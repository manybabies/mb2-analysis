library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'SkidLSDLab'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
Skid_toddlers <- read_csv(here(DATA_DIR,"raw_data","SkidLSDLab_toddlers_eyetrackingdata.csv"))

Skid_toddlers[Skid_toddlers=="-"]<-NA
Skid_toddlers$`Point of Regard Right X [px]`<-as.numeric(Skid_toddlers$`Point of Regard Right X [px]`)
Skid_toddlers$`Point of Regard Left X [px]`<-as.numeric(Skid_toddlers$`Point of Regard Left X [px]`)
Skid_toddlers$`Point of Regard Right Y [px]`<-as.numeric(Skid_toddlers$`Point of Regard Right Y [px]`)
Skid_toddlers$`Point of Regard Left Y [px]`<-as.numeric(Skid_toddlers$`Point of Regard Left Y [px]`)

Skid_toddlers<-transform(Skid_toddlers,x = (`Point of Regard Right X [px]`+`Point of Regard Left X [px]`)/2,
                         y = (`Point of Regard Right Y [px]`+`Point of Regard Left Y [px]`)/2)

Skid_toddlers_clean <- Skid_toddlers %>% 
  rename(participant_id = Participant,
         t = RecordingTime..ms.,
         media_name = Stimulus,
         pupil_left = Pupil.Diameter.Left..mm.,
         pupil_right = Pupil.Diameter.Right..mm.
  ) %>% 
  mutate(lab_id = LAB_NAME,
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(Skid_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

###Adult data###
Skid_adults <- read_csv(here(DATA_DIR,"raw_data","SkidLSDLab_adults_eyetrackingdata.tsv"))

Skid_adults[Skid_adults=="-"]<-NA
Skid_adults$`Point of Regard Right X [px]`<-as.numeric(Skid_adults$`Point of Regard Right X [px]`)
Skid_adults$`Point of Regard Left X [px]`<-as.numeric(Skid_adults$`Point of Regard Left X [px]`)
Skid_adults$`Point of Regard Right Y [px]`<-as.numeric(Skid_adults$`Point of Regard Right Y [px]`)
Skid_adults$`Point of Regard Left Y [px]`<-as.numeric(Skid_adults$`Point of Regard Left Y [px]`)

Skid_adults<-transform(Skid_adults,x = (`Point of Regard Right X [px]`+`Point of Regard Left X [px]`)/2,
                       y = (`Point of Regard Right Y [px]`+`Point of Regard Left Y [px]`)/2)

Skid_adults_clean <- Skid_adults %>% 
  rename(participant_id = Participant,
         t = RecordingTime..ms.,
         media_name = Stimulus,
         pupil_left = Pupil.Diameter.Left..mm.,
         pupil_right = Pupil.Diameter.Right..mm.
  ) %>% 
  mutate(lab_id = LAB_NAME,
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(Skid_adults_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))
