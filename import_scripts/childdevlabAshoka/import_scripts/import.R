# childdevlabAshoka
# Gal Raz import script
# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

library(tidyverse)
library(here)

# ------------------------------------------------------------------------------
# preliminaries 
lab_dir <- "data/childdevlabAshoka"

# ------------------------------------------------------------------------------
# xy_timepoints

# lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right

# eye-tracking data
d_adults <- read_tsv(here(lab_dir, "raw_data/childdevlabAshoka_adults_eyetrackingdata.tsv"))

xy_timepoints <- d_adults |>
  rename(x = `Gaze point X [DACS px]`, 
         y = `Gaze point Y [DACS px]`,
         t = `Eyetracker timestamp [μs]`,
         media_name  = `Presented Stimulus name`, 
         participant_id = `Participant name`,
         pupil_left = `Pupil diameter left [mm]`,
         pupil_right = `Pupil diameter right [mm]`) |>
  mutate_at(c("x", "y", "t", "pupil_left", "pupil_right"), as.numeric) |>
  mutate(t = t/1000, lab_id = "childdevlabAshoka") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/childdevlabAshoka_adults_xy_timepoints.csv"))


# eye-tracking data
d_toddlers <- read_tsv(here(lab_dir, "raw_data/childdevlabAshoka_children_eyetrackingdata.tsv"))

xy_timepoints <- d_toddlers |>
  rename(x = `Gaze point X [DACS px]`, 
         y = `Gaze point Y [DACS px]`,
         t = `Eyetracker timestamp [μs]`,
         media_name  = `Presented Stimulus name`, 
         participant_id = `Participant name`,
         pupil_left = `Pupil diameter left [mm]`,
         pupil_right = `Pupil diameter right [mm]`) |>
  mutate_at(c("x", "y", "t", "pupil_left", "pupil_right"), as.numeric) |>
  mutate(t = t/1000, lab_id = "childdevlabAshoka") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/childdevlabAshoka_toddlers_xy_timepoints.csv"))

