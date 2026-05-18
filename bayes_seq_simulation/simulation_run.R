# Sequential Bayesian updating simulation — top-level runner.
#
# Usage:
#   Rscript bayes_seq_simulation/simulation_run.R                 # fresh run
#   Rscript bayes_seq_simulation/simulation_run.R <RUN_ID>        # resume <RUN_ID>
#
# Fresh runs create data/04_simulation/runs/<timestamp>/. Resumed runs reuse
# the named directory and skip any orderings whose trajectory CSV already
# exists. The CONFIG block must match the original run on resume — there is
# no consistency check.
#
# Outputs (under data/04_simulation/runs/<RUN_ID>/):
#   trajectory_<outer_idx>.csv   one CSV per ordering (resume unit)
#   lab_order.csv                full lab order log for all orderings
#   run_config.json              run-level config
#
# Combine the per-ordering CSVs with:
#   source(here::here("bayes_seq_simulation", "simulation_combine.R"))
#   combine_trajectories(<run_dir>)
#
# Prerequisite: run bayes_seq_simulation/simulation_precompute.R once per cohort.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(furrr)
  library(future)
  library(future.callr)
  library(jsonlite)
})

source(here("helper", "ensure_repo_structure.R"))
source(here("bayes_seq_simulation", "simulation_helpers.R"))


COHORT       <- "toddlers"   # "toddlers" | "adults"
N_ORDERINGS  <- 200          # number of random lab orderings
MAX_LABS     <- NA           # NA = use all precomputed labs
TOTAL_CORES  <- 8           # total core budget for the run
SEED         <- 20260515     # seed for ordering permutations + per-fit seeds

# MCMC settings
#MCMC <- list(iter = 4000,  warmup = 1000, chains = 4, cores = 4,
#             adapt_delta = 0.95, max_treedepth = 10, stepsize = 0.1)

MCMC <- list(iter = 10000, warmup = 1000, chains = 4, cores = 4,
              adapt_delta = 0.99, max_treedepth = 12, stepsize = 0.1)

# Stopping rule
STOP_THRESHOLD_HIGH <- 10
STOP_THRESHOLD_LOW  <- 1 / 10
START_BF_AT_N_LABS  <- 3

# Derived: each brm() fit uses MCMC$cores cores; run as many orderings in
# parallel as the budget allows.
N_WORKERS <- max(1L, TOTAL_CORES %/% MCMC$cores)

# RUN_ID: positional CLI arg if given (resume mode); otherwise a fresh timestamp.
cli_args <- commandArgs(trailingOnly = TRUE)
run_id <- if (length(cli_args) >= 1L && nzchar(cli_args[[1L]])) {
  cli_args[[1L]]
} else {
  format(Sys.time(), "%Y%m%d-%H%M%S")
}
out_dir       <- file.path(SIMULATION_RUNS_DIR, run_id)
resuming      <- dir.exists(out_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message(sprintf("[run] %s: %s",
                if (resuming) "resuming" else "fresh run",
                out_dir))

precomp_dir <- file.path(SIMULATION_PRECOMP_DIR, COHORT)
if (!dir.exists(precomp_dir) ||
    length(list.files(precomp_dir, pattern = "\\.rds$")) == 0) {
  stop("No precomputed labs at ", precomp_dir,
       ". Run bayes_seq_simulation/simulation_precompute.R first.")
}

lab_files <- sort(list.files(precomp_dir, pattern = "\\.rds$", full.names = TRUE))
if (!is.na(MAX_LABS)) {
  lab_files <- head(lab_files, MAX_LABS)
}
lab_ids <- tools::file_path_sans_ext(basename(lab_files))
names(lab_files) <- lab_ids
message(sprintf("[run] cohort = %s, %d labs in use (MAX_LABS = %s)",
                COHORT, length(lab_ids),
                if (is.na(MAX_LABS)) "NA (all)" else as.character(MAX_LABS)))

# --------- Generate lab orderings deterministically -------------------------
set.seed(SEED)
orderings <- lapply(seq_len(N_ORDERINGS), function(i) sample(lab_ids))

# Replay-ready lab-order log — write once. Resumed runs trust the existing file.
order_log_path <- file.path(out_dir, "lab_order.csv")
if (!file.exists(order_log_path)) {
  order_log <- tibble(
    outer_idx  = rep(seq_len(N_ORDERINGS), each = length(lab_ids)),
    inner_idx  = rep(seq_len(length(lab_ids)), times = N_ORDERINGS),
    lab_id     = unlist(orderings),
    age_cohort = COHORT
  )
  write_csv(order_log, order_log_path)
}

# run_config: preserve original started_at on resume.
run_config_path <- file.path(out_dir, "run_config.json")
prev_config <- if (file.exists(run_config_path)) {
  jsonlite::fromJSON(run_config_path)
} else NULL

run_config <- list(
  run_id              = run_id,
  cohort              = COHORT,
  n_orderings         = N_ORDERINGS,
  mcmc_settings       = MCMC,
  max_labs            = if (is.na(MAX_LABS)) NA else MAX_LABS,
  seed                = SEED,
  total_cores         = TOTAL_CORES,
  n_workers           = N_WORKERS,
  cores_per_fit       = MCMC$cores,
  center_age_mo       = CENTER_AGE_MO,
  stop_threshold_high = STOP_THRESHOLD_HIGH,
  stop_threshold_low  = STOP_THRESHOLD_LOW,
  start_bf_at_n_labs  = START_BF_AT_N_LABS,
  precomp_dir         = precomp_dir,
  n_labs              = length(lab_ids),
  lab_ids             = lab_ids,
  started_at          = if (!is.null(prev_config) && !is.null(prev_config$started_at)) {
                          prev_config$started_at
                        } else format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  last_resumed_at     = if (resuming) format(Sys.time(), "%Y-%m-%d %H:%M:%S") else NULL
)
jsonlite::write_json(run_config, run_config_path,
                     pretty = TRUE, auto_unbox = TRUE)

# --------- Worker entrypoint ------------------------------------------------
# Each future runs this. We source the helper *inside* the worker so it has
# its own brms / posterior loaded, and we keep the worker's globals lean.
run_one_ordering_worker <- function(outer_idx, lab_order, lab_files_map,
                                    mcmc, seed_base,
                                    high, low, min_n_labs,
                                    here_root, out_dir) {
  suppressPackageStartupMessages({
    library(tidyverse); library(brms); library(posterior); library(cmdstanr)
  })
  source(file.path(here_root, "bayes_seq_simulation", "simulation_helpers.R"))

  res <- run_one_ordering(
    outer_idx     = outer_idx,
    lab_order     = lab_order,
    lab_files_map = lab_files_map,
    mcmc          = mcmc,
    seed_base     = seed_base,
    high          = high,
    low           = low,
    min_n_labs    = min_n_labs
  )

  out_file <- file.path(out_dir, sprintf("trajectory_%05d.csv", outer_idx))
  write_csv(res, out_file)
  list(outer_idx = outer_idx, n_steps = nrow(res), file = out_file)
}

# Skip orderings whose trajectory CSV already exists (resume support).
existing_files <- list.files(out_dir, pattern = "^trajectory_\\d{5}\\.csv$")
existing_idx   <- as.integer(sub("^trajectory_0*(\\d+)\\.csv$", "\\1", existing_files))
to_run         <- setdiff(seq_len(N_ORDERINGS), existing_idx)

if (length(to_run) == 0L) {
  message(sprintf("[run] all %d orderings already on disk — nothing to do.",
                  N_ORDERINGS))
  quit(save = "no", status = 0L)
}

message(sprintf("[run] %d/%d orderings already on disk; running %d remaining.",
                length(existing_idx), N_ORDERINGS, length(to_run)))

plan(future.callr::callr, workers = N_WORKERS)
on.exit(plan(sequential), add = TRUE)

here_root <- here::here()
message(sprintf("[run] launching %d orderings across %d workers...",
                length(to_run), N_WORKERS))
t_start <- Sys.time()

results <- future_map(
  to_run,
  function(i) {
    run_one_ordering_worker(
      outer_idx     = i,
      lab_order     = orderings[[i]],
      lab_files_map = lab_files,
      mcmc          = MCMC,
      seed_base     = as.integer(SEED + i * 1000L),
      high          = STOP_THRESHOLD_HIGH,
      low           = STOP_THRESHOLD_LOW,
      min_n_labs    = START_BF_AT_N_LABS,
      here_root     = here_root,
      out_dir       = out_dir
    )
  },
  .options = furrr_options(seed = TRUE,
                            globals = c("orderings", "lab_files",
                                        "MCMC", "SEED",
                                        "STOP_THRESHOLD_HIGH",
                                        "STOP_THRESHOLD_LOW",
                                        "START_BF_AT_N_LABS",
                                        "here_root", "out_dir",
                                        "run_one_ordering_worker"))
)

elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
message(sprintf("[run] done. %d orderings this batch in %.1f min. Output: %s",
                length(to_run), elapsed, out_dir))

# Write finished_at + total runtime to run_config
run_config$finished_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
run_config$elapsed_minutes <- elapsed
jsonlite::write_json(run_config,
                     file.path(out_dir, "run_config.json"),
                     pretty = TRUE, auto_unbox = TRUE)
