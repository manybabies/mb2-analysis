library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'jmuCDL'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Adult data###
jmu_1 <- read_tsv('jmuCDL_adults_eyetrackingdata_bin3.tsv')
jmu_2 <- read_tsv('jmuCDL_adults_eyetrackingdata_bin4.tsv')
jmu_adults<-rbind(jmu_1,jmu_2)

jmu_adults_clean <- jmu_adults %>% 
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

write_csv(jmu_adults_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))
