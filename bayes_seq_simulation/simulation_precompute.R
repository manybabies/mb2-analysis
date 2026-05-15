# Sequential Bayesian updating simulation — one-shot precompute.
#
# Reads the post-exclusion parquet dataset (006), filters to the chosen
# cohort, runs the lab-local per-participant summarization once per lab,
# and writes:
#   data/04_simulation/precomputed/<cohort>/<lab_id>.rds
#
# Run once per cohort. Edit COHORT below and source the file.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(arrow)
  library(assertthat)
})

source(here("helper", "ensure_repo_structure.R"))
source(here("bayes_seq_simulation", "simulation_constants.R"))


# Config — edit and re-run for each cohort.
COHORT <- "toddlers"     # "toddlers" | "adults"


# ===========================================================================
# Per-participant summarization on test-trial first-trial data for ONE lab.
# Mirrors 007a-aoi-analysis.qmd (the canonical
# `summarize_participant_test_first_trial` block) applied lab-locally.
#
# Only intentional deviation from 007a: `age_mo_c` is centered on the
# pre-specified `CENTER_AGE_MO` constant (from simulation_constants.R) instead
# of a data-derived grand mean — a sequential design cannot peek at a grand
# mean it does not yet have.
#
# Input  : `lab_rows` — post-exclusion eye-tracking rows for a single lab,
#           already filtered to the target age_cohort.
# Output : one row per (participant, first test trial), with the same
#           columns 007a emits plus `condition_c`, `method_c`, `age_mo_c`.
# ===========================================================================
summarize_lab_test_first_trial <- function(lab_rows) {
  test_first <- lab_rows %>%
    filter(condition %in% c("knowledge", "ignorance"),
           trial_num == 5)

  if (nrow(test_first) == 0) return(NULL)

  grouping <- c("lab_id", "age_cohort", "age_mo", "age_years_n",
                "participant_lab_id", "participant_id", "participant_trial_id",
                "trial_file_name", "bear_not_visible_ms",
                "point_of_disambiguation", "video_duration_ms",
                "condition", "data_type", "trial_num")
  grouping <- intersect(grouping, names(test_first))

  test_first %>%
    group_by(across(all_of(grouping))) %>%
    filter(t_norm <= 120 & t_norm >= -3880) %>%
    summarize(
      t_min                  = min(t_norm),
      t_max                  = max(t_norm),
      sum_target_general     = sum(aoi == "target_general",     na.rm = TRUE),
      sum_distractor_general = sum(aoi == "distractor_general", na.rm = TRUE),
      prop_general           = sum_target_general /
                               (sum_target_general + sum_distractor_general),
      sum_target_exit        = sum(aoi == "target_exit",        na.rm = TRUE),
      sum_target_box         = sum(aoi == "target_box",         na.rm = TRUE),
      sum_distractor_exit    = sum(aoi == "distractor_exit",    na.rm = TRUE),
      sum_distractor_box     = sum(aoi == "distractor_box",     na.rm = TRUE),
      prop_exit              = sum_target_exit /
                               (sum_target_exit + sum_distractor_exit),
      prop_box               = sum_target_box /
                               (sum_target_box + sum_distractor_box),
      N_general              = sum_target_general + sum_distractor_general,
      N_exit                 = sum_target_exit + sum_distractor_exit,
      N_box                  = sum_target_box + sum_distractor_box,
      .groups = "drop"
    ) %>%
    mutate(
      # Sequential design: fixed center, NOT grand mean
      age_mo_c = age_mo - CENTER_AGE_MO,
      condition_c = case_when(
        condition == "knowledge" ~ -0.5,
        condition == "ignorance" ~  0.5
      ),
      method_c = case_when(
        data_type == "web-based" ~ -0.5,
        data_type == "in-lab"    ~  0.5
      )
    )
}


out_dir <- file.path(SIMULATION_PRECOMP_DIR, COHORT)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message(sprintf("[precompute] cohort = %s, age_center = %s",
                COHORT, format(CENTER_AGE_MO)))

ds <- arrow::open_dataset(
  file.path(INTERMEDIATE_FOLDER, INTERMEDIATE_006_par),
  format = "parquet"
)

lab_ids <- ds %>% select(lab_id) %>% collect() %>% distinct() %>%
  pull(lab_id) %>% sort()
message(sprintf("[precompute] %d distinct lab_ids in post-006 dataset",
                length(lab_ids)))

n_written <- 0L
n_part_total <- 0L

for (lab in lab_ids) {
  lab_rows <- ds %>% filter(lab_id == lab) %>% collect() %>%
    filter(age_cohort == COHORT)
  if (nrow(lab_rows) == 0) next

  # Drop web-based rows; skip the lab if nothing in-lab remains.
  lab_rows <- lab_rows %>% filter(data_type != "web-based")
  if (nrow(lab_rows) == 0) {
    message(sprintf("  [-- ] %-30s  skipped: no in-lab data", lab))
    next
  }

  summarized <- summarize_lab_test_first_trial(lab_rows)
  if (is.null(summarized) || nrow(summarized) == 0) next

  saveRDS(summarized, file.path(out_dir, paste0(lab, ".rds")))
  n_written <- n_written + 1L
  n_part_total <- n_part_total + nrow(summarized)
  message(sprintf("  [%2d] %-30s  participants = %d",
                  n_written, lab, nrow(summarized)))
}

if (n_written == 0L) {
  stop("No labs produced summarized data for cohort '", COHORT,
       "'. Check that 006 has been run and that age_cohort labels match.")
}

message(sprintf("[precompute] done. %d labs, %d participants written to %s",
                n_written, n_part_total, out_dir))
