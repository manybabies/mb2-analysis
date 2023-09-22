library(tidyverse)
library(here)

LAB_NAME <- 'WSUMARCS'
DATA_DIR = file.path('data', LAB_NAME, 'raw_data')

data_adults1 <- read.csv(here(DATA_DIR, "WSUMARCS_adults_eyetrackingdata_Bin1_order1.csv"))
data_adults2 <- read.csv(here(DATA_DIR, "WSUMARCS_adults_eyetrackingdata_Bin1 order 2_to_16.csv"))

data_toddlers <- rbind(data_adults1, data_adults2) %>% 
  select(participant_id = ParticipantName,
         x = GazePointX..ADCSpx.,
         y = GazePointY..ADCSpx.,
         t = RecordingTimestamp,
         media_name = MediaName,
         ) %>% 
  mutate(lab_id = LAB_NAME,
         pupil_left = NA,
         pupil_right = NA)

write.csv(data_toddlers, here(file.path('data', LAB_NAME, 'processed_data', paste0(LAB_NAME,'_adults_xy_timepoints.csv'))))



