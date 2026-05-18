library(tidyverse)
library(here)

source(here("helper", "ensure_repo_structure.R"))
source(here("bayes_seq_simulation", "simulation_combine.R"))

RUN_ID <- NA

# take latest run if id not specified
if (is.na(RUN_ID)) {
  all_runs <- sort(list.dirs(SIMULATION_RUNS_DIR, recursive = FALSE, full.names = FALSE))
  if (length(all_runs) == 0L) stop("no runs found in ", SIMULATION_RUNS_DIR)
  RUN_ID <- tail(all_runs, 1L)
}
run_dir <- file.path(SIMULATION_RUNS_DIR, RUN_ID)
if (!dir.exists(run_dir)) stop("run dir not found: ", run_dir)

combined_path <- file.path(run_dir, "trajectory_combined.csv")
if (!file.exists(combined_path) ||
    file.info(combined_path)$mtime < max(file.info(
      list.files(run_dir, pattern = "^trajectory_\\d+\\.csv$", full.names = TRUE)
    )$mtime, na.rm = TRUE)) {
  combine_trajectories(run_dir)
}
d <- read_csv(combined_path, show_col_types = FALSE, progress = FALSE)


n_orderings <- length(unique(d$outer_idx))
terminal <- d %>% group_by(outer_idx) %>% slice_tail(n = 1) %>% ungroup()
full_run  <- terminal %>% filter(stop_reason == "max_labs")

print(list(
  orderings       = n_orderings,
  total_fit_rows  = nrow(d),
  stop_reasons    = table(terminal$stop_reason, useNA = "ifany"),
  terminal_lab_number = summary(terminal$inner_idx),
  terminal_bf     = summary(terminal$bf_10),
  full_run_n      = nrow(full_run),
  full_run_bf     = if (nrow(full_run) > 0L) summary(full_run$bf_10)            else NULL,
  full_run_b_mean = if (nrow(full_run) > 0L) summary(full_run$b_condition_mean) else NULL
))
