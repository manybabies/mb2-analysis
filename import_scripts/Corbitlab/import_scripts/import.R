library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'Corbitlab'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
Cor_toddlers <- read_tsv(here(DATA_DIR,"raw_data","Corbitlab_toddlers_eyetrackingdata.tsv"))

Cor_toddlers_clean <- Cor_toddlers %>% 
  rename(participant_id = `Participant name`,
         x = `Gaze point X (MCSnorm)`,
         y = `Gaze point Y (MCSnorm)`,## the file misses Gaze point X(Y) nor Gaze point left(right) X(Y) 
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`
  ) %>% 
  mutate(lab_id = LAB_NAME,
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(Cor_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

###Adult data###
Cor_adults <- read_tsv(here(DATA_DIR,"raw_data","Corbitlab_adults_eyetrackingdata.tsv"))

Cor_adults_clean <- Cor_adults %>% 
  rename(participant_id = `Participant name`,
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`
  ) %>% 
  mutate(lab_id = LAB_NAME,
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(Cor_adults_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))
