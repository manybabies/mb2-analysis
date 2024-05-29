library(tidyverse)
library(here)
library(glue)
library(vroom)

LAB_NAME <- "babyuniHeidelberg"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_adults <- read_delim(here(DATA_DIR, "raw_data/babyuniHeidelberg_adults_eyetrackingdata.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE)

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
data_toddlers <- read_delim(here(DATA_DIR, "raw_data/babyuniHeidelberg_toddlers_eyetrackingdata.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE)

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

