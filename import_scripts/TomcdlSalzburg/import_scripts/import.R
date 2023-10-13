library(tidyverse)
library(here)
library(janitor)

# paths
lab_dir <- here("data","TomcdlSalzburg")
lab_dataset_name <- "ToMcdlSalzburg_adults"

#dataset 1
# eye-tracking data
dataset1 <- read_tsv(here(lab_dir, "raw_data","ToMcdlSalzburg_adults_eyetrackingdata_bin1.tsv" ) )
dataset1 <- dataset1 %>%
  janitor::clean_names()

# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

# xy_timepoints
xy_timepoints_1 <- dataset1 |>
  separate(participant_name,c("lab_id","participant_identifier"),remove=FALSE) |>
  rename(x = gaze_point_x_adc_spx, 
         y = gaze_point_y_adc_spx,
         t = eye_tracker_timestamp,
         participant_id = participant_name) |>
  mutate(t = t / 1000) |> # microseconds to milliseconds correction
  select(lab_id,participant_id,media_name,x,y,t, pupil_left,pupil_right)

#dataset2
# eye-tracking data
dataset2 <- read_tsv(here(lab_dir, "raw_data","ToMcdlSalzburg_adults_eyetrackingdata_bin2.tsv" ) )
dataset2 <- dataset2 %>%
  janitor::clean_names()

# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

# xy_timepoints
xy_timepoints_2 <- dataset2 |>
  separate(participant_name,c("lab_id","participant_identifier"),remove=FALSE) |>
  rename(x = gaze_point_x_adc_spx, 
         y = gaze_point_y_adc_spx,
         t = eye_tracker_timestamp,
         participant_id = participant_name) |>
  mutate(t = t / 1000) |> # microseconds to milliseconds correction
  select(lab_id,participant_id,media_name,x,y,t, pupil_left,pupil_right)

xy_timepoints <- bind_rows(xy_timepoints_1,xy_timepoints_2)

write_csv(xy_timepoints, here(lab_dir, "processed_data",paste0(lab_dataset_name,"_","xy_timepoints.csv")))

#toddlers
lab_dataset_name_toddlers <- "ToMcdlSalzburg_toddlers"
# eye-tracking data
d_toddlers <- read_tsv(here(lab_dir, "raw_data","ToMcdlSalzburg_toddlers_eyetrackingdata.tsv" ) )
d_toddlers <- d_toddlers %>%
  janitor::clean_names()

# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

# xy_timepoints
xy_timepoints_toddlers <- d_toddlers |>
  separate(participant_name,c("lab_id","participant_identifier"),remove=FALSE) |>
  rename(x = gaze_point_x_adc_spx, 
         y = gaze_point_y_adc_spx,
         t = eye_tracker_timestamp,
         participant_id = participant_name) |>
  mutate(t = t / 1000) |> # microseconds to milliseconds correction
  select(lab_id,participant_id,media_name,x,y,t, pupil_left,pupil_right)

write_csv(xy_timepoints_toddlers, here(lab_dir, "processed_data",paste0(lab_dataset_name_toddlers,"_","xy_timepoints.csv")))
