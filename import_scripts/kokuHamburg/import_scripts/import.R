library(tidyverse)
library(here)
library(glue)
library(vroom)

LAB_NAME <- "kokuHamburg"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Toddler data ####
data_toddlers <- read_delim(here(DATA_DIR, "raw_data/kokuHamburg_toddlers_eyetrackingdata.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE)


data_toddlers_cleaned <- data_toddlers |> 
  select(participant_id = `Participant name`,
         x = `Gaze point X [DACS px]`,
         y = `Gaze point Y [DACS px]`,
         t = `Recording timestamp [ms]`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left [mm]`,
         pupil_right = `Pupil diameter right [mm]`) |> 
  mutate(lab_id = LAB_NAME)

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))
