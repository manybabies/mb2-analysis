library(tidyverse)
library(here)
library(janitor)

# paths
lab_dir <- here("import_scripts","careylabHarvard")
lab_dataset_name <- "careylabHarvard_adults"

# eye-tracking data
d <- read_tsv(here(lab_dir, "raw_data","careylabHarvard_adults_eyetrackingdata.tsv" ) )
d <- d %>%
  janitor::clean_names()

# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

# xy_timepoints
xy_timepoints <- d |>
  #separate(participant_name,c("lab_id","participant_identifier"),remove=FALSE) |>
  mutate(lab_id = "careylabHarvard") |>
  mutate(participant_name = str_replace(participant_name,"careylabHarvard_","")) |>
  rename(x = gaze_point_x_adc_spx, 
         y = gaze_point_y_adc_spx,
         t = eye_tracker_timestamp,
         participant_id = participant_name) |>
  mutate(t = t / 1000) |> # microseconds to milliseconds correction
  mutate(leading_digits = as.numeric(str_sub(as.character(t), start = 1, end = 3))) |> #these digits are unique to participant
  mutate(t = as.numeric(str_sub(as.character(t), start = 4))) |> #remove this to avoid encoding issue with numbers that are too large
  select(lab_id,participant_id,media_name,x,y,t, pupil_left,pupil_right)

write_csv(xy_timepoints, here(lab_dir, "processed_data",paste0(lab_dataset_name,"_","xy_timepoints.csv")))