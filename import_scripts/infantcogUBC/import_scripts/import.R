library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'infantcogUBC'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
UBC_toddlers <- read_csv(here(DATA_DIR, "raw_data","infantcogUBC_toddlers_eyetrackingdata.csv"))

UBC_toddlers[UBC_toddlers=="-"]<-NA
UBC_toddlers$Point.of.Regard.Right.X..px.<-as.numeric(UBC_toddlers$Point.of.Regard.Right.X..px.)
UBC_toddlers$Point.of.Regard.Left.X..px.<-as.numeric(UBC_toddlers$Point.of.Regard.Left.X..px.)
UBC_toddlers$Point.of.Regard.Right.Y..px.<-as.numeric(UBC_toddlers$Point.of.Regard.Right.Y..px.)
UBC_toddlers$Point.of.Regard.Left.Y..px.<-as.numeric(UBC_toddlers$Point.of.Regard.Left.Y..px.)

UBC_toddlers<-transform(UBC_toddlers,x = (Point.of.Regard.Right.X..px.+Point.of.Regard.Left.X..px.)/2,
                        y = (UBC_toddlers$Point.of.Regard.Right.Y..px.+UBC_toddlers$Point.of.Regard.Left.Y..px.)/2)

UBC_toddlers_clean <- UBC_toddlers %>% 
  rename(participant_id = Participant,
         t = RecordingTime..ms.,
         media_name = Stimulus,
         pupil_left = Pupil.Diameter.Left..mm.,
         pupil_right = Pupil.Diameter.Right..mm.
  ) %>% 
  mutate(lab_id = LAB_NAME,
         media_name=str_replace_all(media_name,"_new",""))%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(UBC_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))
