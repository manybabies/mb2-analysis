library(tidyverse)
library(here)
library(glue)

LAB_NAME <- "mpievaCCP"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Toddler data ####
data_toddlers1 <- read_delim(here(DATA_DIR, "raw_data/mpievaCCP_toddlers_eyetrackingdata_TrialBin3.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE) 
  read_tsv(here(DATA_DIR, "raw_data", "mpievaCCP_toddlers_eyetrackingdata_TrialBin3.tsv"))
data_toddlers2 <- read_delim(here(DATA_DIR, "raw_data/mpievaCCP_toddlers_eyetrackingdata_TrialBin4.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE) 
  read_tsv(here(DATA_DIR, "raw_data", "mpievaCCP_toddlers_eyetrackingdata_TrialBin4.tsv"))

data_toddlers1_cleaned <- data_toddlers1 |> 
  select(participant_id = `Recording name`,
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`) |> 
  mutate(lab_id = LAB_NAME)
data_toddlers2_cleaned <- data_toddlers2 |> 
  select(participant_id = `Recording name`,
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`) |> 
  mutate(lab_id = LAB_NAME)
data_toddlers_cleaned <- bind_rows(data_toddlers1_cleaned, data_toddlers2_cleaned) |> 
  mutate(t = t / 1000)

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

