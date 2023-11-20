library(tidyverse)
library(here)
library(glue)

LAB_NAME <- "irlConcordia"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Toddler data ####
data_path_toddlers <- here(DATA_DIR, "raw_data", "irlConcordia_toddlers_eyetrackingdata_csv_files")
data_toddlers <- list.files(data_path_toddlers, pattern = "*.csv") |> 
  lapply(\(f) {read.csv(here(data_path_toddlers, f), 
                        na = c("", "NA", "."))}) |> 
  bind_rows()
  
data_toddlers_cleaned <- data_toddlers |> 
  select(participant_id = RECORDING_SESSION_LABEL,
         x = AVERAGE_GAZE_X,
         y = AVERAGE_GAZE_Y,
         t = TIMESTAMP,
         media_name = trialtype,
         pupil_left = LEFT_PUPIL_SIZE,
         pupil_right = RIGHT_PUPIL_SIZE) |> 
  mutate(lab_id = LAB_NAME)

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))


