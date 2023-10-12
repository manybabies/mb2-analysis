# GAUGGOETTINGEN
# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

library(tidyverse)
library(here)
library(janitor)


# ------------------------------------------------------------------------------
# preliminaries 

lab_name <- "gaugGöttingen"
lab_dir <- paste0("data/", lab_name)

# eye-tracking data
d_a <- read_tsv(here(lab_dir, "raw_data/gaugGöttingen_adults_eyetrackingdata.tsv") )
d_t <- read_tsv(here(lab_dir, "raw_data/gaugGöttingen_toddlers_eyetrackingdata.tsv") )
d_a <- janitor::clean_names(d_a)
d_t <- janitor::clean_names(d_t)


# ------------------------------------------------------------------------------
# xy_timepoints
# lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right

# adults
xy_timepoints_a <- d_a |>
  rename(participant_id = participant_name, 
         media_name = presented_media_name, 
         x = gaze_point_x, 
         y = gaze_point_y,
         t = eyetracker_timestamp, 
         pupil_left = pupil_diameter_left, 
         pupil_right = pupil_diameter_right) |>
  mutate(lab_id = lab_name, 
         t = t / 1000) |> # microseconds to milliseconds correction
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name), !is.na(t))

write_csv(xy_timepoints_a, here(lab_dir, "processed_data", paste0(lab_name, "_adults_xy_timepoints.csv")))

# toddlers
xy_timepoints_t <- d_t |>
  rename(participant_id = participant_name, 
         media_name = presented_media_name, 
         x = gaze_point_x, 
         y = gaze_point_y,
         t = eyetracker_timestamp, 
         pupil_left = pupil_diameter_left, 
         pupil_right = pupil_diameter_right) |>
  mutate(lab_id = lab_name, 
         t = t / 1000) |> # microseconds to milliseconds correction
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name), !is.na(t))

write_csv(xy_timepoints_t, here(lab_dir, "processed_data", paste0(lab_name, "_toddlers_xy_timepoints.csv")))

