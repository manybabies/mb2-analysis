# SOCIALCOGUMIAMI
# Mike Frank import script
# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

library(tidyverse)
library(here)
library(janitor)

# ------------------------------------------------------------------------------
# preliminaries 

lab_name <- "socialcogUmiami"
lab_dir <- paste0("data/", lab_name, "/")

# eye-tracking data
d <- read_tsv(here(lab_dir, "raw_data/socialcogUmiami_adults_eyetracking.tsv") )
d <- janitor::clean_names(d)

# ------------------------------------------------------------------------------
# xy_timepoints
# lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right

xy_timepoints <- d |>
  rename(participant_id = participant_name, 
         x = gaze_point_x_adc_spx, 
         y = gaze_point_y_adc_spx,
         t = eye_tracker_timestamp) |>
  mutate(lab_id = lab_name, 
         t = t / 1000) |> # microseconds to milliseconds correction
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name), !is.na(t))
    
write_csv(xy_timepoints, here(lab_dir, "processed_data", paste0(lab_name, "_adults_xy_timepoints.csv")))

          