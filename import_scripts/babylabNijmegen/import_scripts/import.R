library(tidyverse)
library(here)
library(glue)

LAB_NAME <- "babylabNijmegen"
DATA_DIR <- here("data", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults1 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_adults_eyetrackingdata_Bin1_old.csv"),
                           delim = ";", 
                           col_types = cols(StudioEventIndex = col_integer(),
                                            StudioEvent = col_character(),
                                            StudioEventData = col_character()))

# Note: manually edited end of file to correct for wrong number of delimiters
data_adults2 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_adults_eyetrackingdata_Bin1_new.csv"),
                           delim = ";", 
                           col_types = cols(StudioEventIndex = col_integer(),
                                            StudioEvent = col_character(),
                                            StudioEventData = col_character()))

data_adults <- bind_rows(data_adults1, data_adults2)

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
data_toddler1 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_toddlers_eyetrackingdata_Bin4_old.csv"),
                            delim = ";", 
                            col_types = cols(StudioEventIndex = col_integer(),
                                             StudioEvent = col_character(),
                                             StudioEventData = col_character()))
data_toddler2 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_toddlers_eyetrackingdata_Bin4_new.csv"),
                            delim = ";", 
                            col_types = cols(StudioEventIndex = col_integer(),
                                             StudioEvent = col_character(),
                                             StudioEventData = col_character()))
data_toddlers <- bind_rows(data_toddler1, data_toddler2)

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

