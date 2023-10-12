library(tidyverse)
library(here)
library(glue)

LAB_NAME <- "babylabUmassb"
DATA_DIR <- here("data", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults <- read_tsv(here(DATA_DIR, "raw_data", "babylabUmassb_adults_eyetrackingdata.tsv"), quote = "'")

data_adults_cleaned <- data_adults |> 
  # filter out rows with incomplete data
  filter(!is.na(RecordingName),
         RecordingName != "UMB\"") |> 
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
data_toddlers <- read_tsv(here(DATA_DIR, "raw_data", "babylabUmassb_toddlers_eyetrackingdata.tsv"))

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
