# BABYLAB AMSTERDAM
# Gal Raz import script
# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

library(tidyverse)
library(here)

# ------------------------------------------------------------------------------
# preliminaries 
lab_dir <- "data/babylabAmsterdam/"

# ------------------------------------------------------------------------------
# xy_timepoints

# lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right

# eye-tracking data
d_adults <- read_csv(here(lab_dir, "raw_data/babylabAmsterdam_adults_eyetrackingdata.csv"))

xy_timepoints <- d_adults |>
  rename(x = RIGHT_GAZE_X, 
         y = RIGHT_GAZE_Y,
         t = TIMESTAMP,
         media_name  = videofile, 
         participant_id = Session_Name_,
         pupil_left = LEFT_PUPIL_SIZE,
         pupil_right = RIGHT_PUPIL_SIZE) |>
  mutate_at(c("x", "y", "t", "pupil_left", "pupil_right"), as.numeric) |>
  mutate(lab_id = "babylabAmsterdam") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/babylabAmsterdam_adults_xy_timepoints.csv"))


# eye-tracking data
d_toddlers <- read_csv(here(lab_dir, "raw_data/babylabAmsterdam_toddlers_eyetrackingdata.csv"))

xy_timepoints <- d_toddlers |>
  rename(x = RIGHT_GAZE_X, 
         y = RIGHT_GAZE_Y,
         t = TIMESTAMP,
         media_name  = videofile, 
         participant_id = Session_Name_,
         pupil_left = LEFT_PUPIL_SIZE,
         pupil_right = RIGHT_PUPIL_SIZE)  |>
  mutate_at(c("x", "y", "t", "pupil_left", "pupil_right"), as.numeric) |>
  mutate(lab_id = "babylabAmsterdam") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/babylabAmsterdam_toddlers_xy_timepoints.csv"))



