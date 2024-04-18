library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'babylabINCC'
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_path_adults <- here(DATA_DIR, "raw_data", "BabylabINCC_adults_eyetrackingdata")
data_adults <- list.files(data_path_adults) |> 
  lapply(\(f) {read.csv(here(data_path_adults, f))}) |> 
  bind_rows()

data_adults_cleaned <- data_adults |> 
  mutate(x = (left_gaze_x_px + right_gaze_x_px) / 2,
         y = (left_gaze_y_px + right_gaze_y_px) / 2,
         event = str_replace(event, "_(start|end)", ""),
         time = time * 1000) |> 
  select(participant_id = subject_ID,
         x,
         y,
         t = time,
         media_name = event,
         pupil_left = left_pupil_measure1,
         pupil_right = right_pupil_measure1) |> 
  mutate(lab_id = LAB_NAME)

write_csv(data_adults_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))

#### Toddler data ####
data_path_toddlers <- here(DATA_DIR, "raw_data", "BabylabINCC_toddlers_eyetrackingdata")
data_toddlers <- list.files(data_path_toddlers) |> 
  lapply(\(f) {read.csv(here(data_path_toddlers, f))}) |> 
  bind_rows()

data_toddlers_cleaned <- data_toddlers |> 
  mutate(x = (left_gaze_x_px + right_gaze_x_px) / 2,
         y = (left_gaze_y_px + right_gaze_y_px) / 2,
         event = str_replace(event, "_(start|end)", ""),
         time = time * 1000) |> 
  select(participant_id = subject_ID,
         x,
         y,
         t = time,
         media_name = event,
         pupil_left = left_pupil_measure1,
         pupil_right = right_pupil_measure1) |> 
  mutate(lab_id = LAB_NAME)

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))


