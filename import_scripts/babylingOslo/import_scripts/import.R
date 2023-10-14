library(tidyverse)
library(here)

LAB_NAME <- 'babylingOslo'
DATA_DIR = file.path('import_scripts', LAB_NAME, 'raw_data')

data_toddlers <- read.csv(here(DATA_DIR, "babylingOslo_toddlers_eyetrackingdata.csv"))

data_toddlers <- data_toddlers %>% 
  select(participant_id = RECORDING_SESSION_LABEL,
         x = RIGHT_GAZE_X,
         y = RIGHT_GAZE_Y,
         t = TIMESTAMP,
         media_name = videofile,
         ) %>% 
  mutate(lab_id = LAB_NAME,
         pupil_left = NA,
         pupil_right = NA)

write.csv(data_toddlers, here(file.path('data', LAB_NAME, 'processed_data', paste0(LAB_NAME,'_toddlers_xy_timepoints.csv'))), row.names = FALSE)



