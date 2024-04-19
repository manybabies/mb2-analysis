library(tidyverse)
library(here)
library(glue)
library(vroom)

LAB_NAME <- "ccluniri"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults <- vroom(here(DATA_DIR, "raw_data", "ccluniri_adults_eyetrackingdata.csv"),
                       delim = "\t")

X_DIM = 1920
Y_DIM = 1080

data_adults_cleaned <- data_adults |> 
  mutate(x = rowMeans(cbind(`left_eye_x_top-left00`, `right_eye_x_top-left00`), na.rm = TRUE) * X_DIM,
         y = rowMeans(cbind(`left_eye_y_top-left00`, `right_eye_y_top-left00`), na.rm = TRUE) * Y_DIM) |> 
  select(participant_id = participant_id,
         x,
         y,
         t = time,
         media_name = media,
         pupil_left = left_pupil_measure1,
         pupil_right = right_pupil_measure1) |> 
  mutate(lab_id = LAB_NAME)

write_csv(data_adults_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))
