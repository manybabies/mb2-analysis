library(tidyverse)
library(here)

# ------------------------------------------------------------------------------
# preliminaries 
# load point of disambiguation data
# and helper functions for XY and AOI


lab_dir <- "data/babylabChicago/"

# eye-tracking data
d_toddlers <- read_tsv(here(lab_dir, "raw_data/babylabChicago_toddlers_eyetrackingdata.tsv"))

# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

# ------------------------------------------------------------------------------
# xy_timepoints
# note that tobii puts 0,0 at upper left, not lower left so we flip

xy_timepoints <- d_toddlers |>
  rename(x = `GazePointLeftX (ADCSpx)`, 
         y = `GazePointLeftY (ADCSpx)`,
         t = RecordingTimestamp,
         media_name  = MediaName, 
         participant_id = ParticipantName,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |>
  mutate(lab_id = "babylabChicago",
         t = t / 1000,
         average_pupil_size = (pupil_left + pupil_right)/2) |> # microseconds to milliseconds correction, avg pupil size
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/babylabChicago_toddlers_xy_timepoints.csv"))



