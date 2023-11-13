library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'PKUSu'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
PKU_toddlers <- read.csv(here(DATA_DIR, "raw_data","PKUSu_toddlers_eyetrackingdata.csv"), sep="\t", fileEncoding="UTF-16LE")

PKU_toddlers[PKU_toddlers==""]<-NA
PKU_toddlers_clean <- PKU_toddlers %>% 
  rename(participant_id = Participant.name,
         x = Gaze.point.X,
         y = Gaze.point.Y,
         t = Recording.timestamp,
         media_name = Presented.Media.name,
         pupil_left = Pupil.diameter.left,
         pupil_right = Pupil.diameter.right
  ) %>% 
  mutate(media_name=str_replace_all(media_name,"_new",""),
         lab_id = LAB_NAME)%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(PKU_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

###Adult data###
PKU_adults <- read.csv(here(DATA_DIR, "raw_data","PKUSu_adults_eyetrackingdata.csv"),sep="\t", fileEncoding="UTF-16LE")

PKU_adults[PKU_adults==""]<-NA
PKU_adults_clean <- PKU_adults %>% 
  rename(participant_id = Participant.name,
         x = Gaze.point.X,
         y = Gaze.point.Y,
         t = Recording.timestamp,
         media_name = Presented.Media.name,
         pupil_left = Pupil.diameter.left,
         pupil_right = Pupil.diameter.right
  )%>% 
  mutate(media_name=str_replace_all(media_name,"_new",""),
         lab_id = LAB_NAME)%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(PKU_adults_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))
