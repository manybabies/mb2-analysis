library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'PKUSu'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
data_toddlers <- read.csv(here(DATA_DIR, "raw_data","PKUSu_toddlers_eyetrackingdata.csv"), sep="\t", fileEncoding="UTF-16LE")

data_toddlers_clean <- data_toddlers %>% 
  select(participant_id = Participant name,
         x = Gaze point X,
         y = Gaze point Y,
         t = Recording timestamp,
         media_name = Presented Media name,
  ) %>% 
  mutate(lab_id = LAB_NAME,
         pupil_left = Pupil diameter left,
         pupil_right = Pupil diameter right)

write.csv(data_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

###Adult data###
data_adults <- read.csv(here(DATA_DIR, "raw_data","PKUSu_adults_eyetrackingdata.csv"),sep="\t", fileEncoding="UTF-16LE")

data_adults_clean <- data_adults %>% 
  select(participant_id = Participant name,
         x = Gaze point X,
         y = Gaze point Y,
         t = Recording timestamp,
         media_name = Presented Media name,
  ) %>% 
  mutate(media_name=str_replace_all(media_name,"_new",""),
         lab_id = LAB_NAME,
         pupil_left = Pupil diameter left,
         pupil_right = Pupil diameter right)

write.csv(data_adults_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))
