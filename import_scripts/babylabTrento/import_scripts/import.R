library(tidyverse)
library(here)
library(glue)

LAB_NAME <- "babylabTrento"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults <- read_tsv(here(DATA_DIR, "raw_data", "babylabTrento_adults_eyetrackingdata.tsv"))

data_adults_cleaned <- data_adults |>
  select(participant_id = `Participant name`,
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`) |>
  mutate(t = t / 1000,
         lab_id = LAB_NAME)

write_csv(data_adults_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))

#### Toddler data ####
data_toddlers <- read_tsv(here(DATA_DIR, "raw_data", "babylabTrento_toddlers_eyetrackingdata.tsv"))

data_toddlers_cleaned <- data_toddlers |> 
  select(participant_id = `Participant name`,
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`) |> 
  mutate(t = t / 1000,
         lab_id = LAB_NAME)

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

