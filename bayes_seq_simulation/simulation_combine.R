# Concatenate per-ordering trajectory CSVs from a simulation run.
#
# Usage:
#   source(here::here("bayes_seq_simulation", "simulation_combine.R"))
#   combine_trajectories(here::here("data", "04_simulation", "runs", "<timestamp>"))
#
# Writes trajectory_combined.csv into the run directory and returns the path.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

combine_trajectories <- function(run_dir, out_name = "trajectory_combined.csv") {
  if (!dir.exists(run_dir)) {
    stop("run_dir does not exist: ", run_dir)
  }

  files <- list.files(run_dir,
                      pattern = "^trajectory_\\d+\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) {
    stop("No trajectory_*.csv files found in ", run_dir)
  }

  combined <- files %>%
    map_dfr(~ read_csv(.x, show_col_types = FALSE,
                       progress = FALSE)) %>%
    arrange(outer_idx, inner_idx)

  out <- file.path(run_dir, out_name)
  write_csv(combined, out)
  message(sprintf("[combine] %d files -> %s (%d rows)",
                  length(files), out, nrow(combined)))
  invisible(out)
}
