library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'babylabBrookes'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
Bro_toddlers <- read_tsv(here(DATA_DIR,"raw_data","babylabBrookes_toddlers_eyetrackingdata.tsv"))

Bro_toddlers_clean <- Bro_toddlers %>% 
  rename(participant_id = `Participant name`,
         x = `Gaze point X`,
         y = `Gaze point Y`, 
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`
  ) %>% 
  mutate(lab_id = LAB_NAME,
         t = t/1000, ## microseconds to milliseconds
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(Bro_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

###Adult data###
Bro_adults1 <- read_tsv(here(DATA_DIR,"raw_data","babylabBrookes_adults_eyetrackingdata_Bin1.tsv"))
Bro_adults2 <- read_tsv(here(DATA_DIR,"raw_data","babylabBrookes_adults_eyetrackingdata_Bin2.tsv"))
Bro_adults3 <- read_tsv(here(DATA_DIR,"raw_data","babylabBrookes_adults_eyetrackingdata_Bin3.tsv"))
Bro_adults4 <- read_tsv(here(DATA_DIR,"raw_data","babylabBrookes_adults_eyetrackingdata_Bin4.tsv"))
Bro_adults <- rbind(Bro_adults1,Bro_adults2,Bro_adults3,Bro_adults4)

Bro_adults_clean <- Bro_adults %>% 
  rename(participant_id = `Participant name`,
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t = `Recording timestamp`,
         media_name = `Presented Media name`,
         pupil_left = `Pupil diameter left`,
         pupil_right = `Pupil diameter right`
  ) %>% 
  mutate(lab_id = LAB_NAME,
         t = t/1000, ## microseconds to milliseconds
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(Bro_adults_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))

