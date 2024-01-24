library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'gertlabLancaster'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
ger_toddlers <- read_csv(here(DATA_DIR,"raw_data","gertlabLancaster_toddlers_eyetrackingdata.csv"))

ger_toddlers_clean <- ger_toddlers %>% 
  rename(participant_id = Participant.name,
         x = Gaze.point.X,
         y = Gaze.point.Y,
         t = Recording.timestamp,
         media_name = Presented.Media.name,
         pupil_left = Pupil.diameter.left,
         pupil_right = Pupil.diameter.right
  ) %>% 
  mutate(lab_id = LAB_NAME,
         t = t/1000, ## microseconds to milliseconds
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(ger_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))
