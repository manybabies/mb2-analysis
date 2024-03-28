library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'oxfordBabylab'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
oxf_toddlers <- read_tsv(here(DATA_DIR, "raw_data","oxfordBabylab_toddlers_eyetrackingdata.tsv"))

oxf_toddlers_clean <- oxf_toddlers |> 
  rename(participant_id = `Participant name`,
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`
  ) |> 
  mutate(lab_id = LAB_NAME,
         t = t/1000, ## microseconds to milliseconds
         media_name = str_replace_all(media_name, "_new", ""),
         # fix mislabelled participant
         participant_id = ifelse(`Recording name` == "Recording23", "UoO232", participant_id)) |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right)

write_csv(oxf_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))
