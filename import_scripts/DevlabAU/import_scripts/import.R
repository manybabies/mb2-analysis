library(tidyverse)
library(here)
library(glue)

LAB_NAME <- "DevlabAU"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults <- read_delim(here(DATA_DIR, "raw_data/DevlabAU_adults_eyetrackingdata.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE)

data_adults_cleaned <- data_adults |> 
  select(participant_id = ParticipantName,
         x = `GazePointX (ADCSpx)`,
         y = `GazePointY (ADCSpx)`,
         t = RecordingTimestamp,
         media_name = MediaName,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |> 
  mutate(lab_id = LAB_NAME)

write_csv(data_adults_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))

#### Toddler data ####
data_toddlers <- read_delim(here(DATA_DIR, "raw_data/DevlabAU_toddlers_eyetrackingdata.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE)

data_toddlers_cleaned <- data_toddlers |> 
  select(participant_id = ParticipantName,
         x = `GazePointX (ADCSpx)`,
         y = `GazePointY (ADCSpx)`,
         t = RecordingTimestamp,
         media_name = MediaName,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |> 
  mutate(lab_id = LAB_NAME)

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

