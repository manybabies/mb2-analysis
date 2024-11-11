library(tidyverse)
library(here)
library(glue)

LAB_NAME <- "babylabNijmegen"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults1 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_adults_eyetrackingdata_Bin1_old.csv"),
                           delim = ";", 
                           col_types = cols(StudioEventIndex = col_integer(),
                                            StudioEvent = col_character(),
                                            StudioEventData = col_character()),
                           locale = locale(decimal_mark = ",", grouping_mark = ""))

# Note: manually edited end of file to correct for wrong number of delimiters
data_adults2 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_adults_eyetrackingdata_Bin1_new.csv"),
                           delim = ";", 
                           col_types = cols(StudioEventIndex = col_integer(),
                                            StudioEvent = col_character(),
                                            StudioEventData = col_character()),
                           trim_ws = TRUE,
                           locale = locale(decimal_mark = ",", grouping_mark = ""))

data_adults3 <- read_delim(here(DATA_DIR, "raw_data","RU_022.tsv"), 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)

data_adults <- bind_rows(data_adults1, data_adults2, data_adults3)

data_adults_cleaned <- data_adults |> 
  dplyr::select(participant_id = ParticipantName,
         x = `GazePointX (ADCSpx)`,
         y = `GazePointY (ADCSpx)`,
         t = RecordingTimestamp,
         media_name = MediaName,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |> 
  mutate(lab_id = LAB_NAME)

# Renaming participants

data_adults_cleaned <- data_adults_cleaned |>
  mutate(participant_id=case_when(
    participant_id=="R_014" ~ "RU_014", 
    participant_id=="RU_001" ~ "Pilot1",
    participant_id=="RU_002" ~ "Pilot2",
    participant_id=="RU_003" ~ "Pilot3",
    TRUE ~ participant_id))


write_csv(data_adults_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))

#### Toddler data ####
data_toddler1 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_toddlers_eyetrackingdata_Bin4_old.csv"),
                            delim = ";", 
                            col_types = cols(StudioEventIndex = col_integer(),
                                             StudioEvent = col_character(),
                                             StudioEventData = col_character()),
                            locale = locale(decimal_mark = ",", grouping_mark = ""))
data_toddler2 <- read_delim(here(DATA_DIR, "raw_data", "BabylabNijmegen_toddlers_eyetrackingdata_Bin4_new.csv"),
                            delim = ";", 
                            col_types = cols(StudioEventIndex = col_integer(),
                                             StudioEvent = col_character(),
                                             StudioEventData = col_character()),
                            locale = locale(decimal_mark = ",", grouping_mark = ""))

# Renaming participant RU_016 who has been missnamed as RU_017
data_toddler2 <- data_toddler2 |>
  mutate(ParticipantName=case_when(
    ParticipantName=="RU_017" & RecordingName== "RU_016" ~ "RU_016",
    TRUE ~ ParticipantName))

data_toddlers3 <- read_delim(here(DATA_DIR, "raw_data","RU_023.tsv"), 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)

data_toddlers <- bind_rows(data_toddler1, data_toddler2, data_toddlers3)

data_toddlers_cleaned <- data_toddlers |> 
  dplyr::select(participant_id = ParticipantName,
         x = `GazePointX (ADCSpx)`,
         y = `GazePointY (ADCSpx)`,
         t = RecordingTimestamp,
         media_name = MediaName,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |> 
  mutate(lab_id = LAB_NAME)

# Renaming participants

data_toddlers_cleaned <- data_toddlers_cleaned |>
  mutate(participant_id=case_when(
    participant_id=="RU004" ~ "RU_004", 
    participant_id=="RU_001" ~ "Pilot1",
    participant_id=="R_003" ~ "RU_003",
    participant_id=="R_011" ~ "RU_011",
    TRUE ~ participant_id))

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))
