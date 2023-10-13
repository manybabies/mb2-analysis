# mecdmpihcbs
# Gal Raz import script
# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

library(tidyverse)
library(here)

# preliminaries 
lab_dir <- "data/mecdmpihcbs"

d_adults <- read_csv(here(lab_dir, "raw_data/mecdmpihcbs_adults_eyetracking_data.csv"))

xy_timepoints <- d_adults |>
  rename(x = GazePointX, 
         y = GazePointY,
         t = Timestamp,
         media_name  = StimuliName, 
         participant_id = ID,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |>
  mutate(lab_id = "mecdmpihcbs") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/mecdmpihcbs_adults_xy_timepoints.csv"))


d_toddlers <- read_csv(here(lab_dir, "raw_data/mecdmpihcbs_toddlers_eyetracking_data.csv"))

xy_timepoints <- d_toddlers |>
  rename(x = GazePointX, 
         y = GazePointY,
         t = Timestamp,
         media_name  = StimuliName, 
         participant_id = ID,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |>
  mutate(lab_id = "mecdmpihcbs") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/mecdmpihcbs_toddlers_xy_timepoints.csv"))
