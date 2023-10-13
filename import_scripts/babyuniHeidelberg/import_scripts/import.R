library(tidyverse)
library(here)
library(glue)
library(vroom)

LAB_NAME <- "babyuniHeidelberg"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults <- vroom(here(DATA_DIR, "raw_data", "babyuniHeidelberg_adults_eyetrackingdata.tsv"),
                     delim = "\t")

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
data_toddlers <- vroom(here(DATA_DIR, "raw_data", "babyuniHeidelberg_toddlers_eyetrackingdata.tsv"),
                       delim = "\t")

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

