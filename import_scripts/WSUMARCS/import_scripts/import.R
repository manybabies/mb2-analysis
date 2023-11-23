library(tidyverse)
library(here)

LAB_NAME <- 'WSUMARCS'
DATA_DIR = file.path('import_scripts', LAB_NAME, 'raw_data')

data_adults1 <- read.csv(here(DATA_DIR, "WSUMARCS_adults_eyetrackingdata_Bin1_order1.csv"))
data_adults2 <- read.csv(here(DATA_DIR, "WSUMARCS_adults_eyetrackingdata_Bin1 order 2_to_16.csv"))

data_adults <- rbind(data_adults1, data_adults2) %>% 
  select(participant_id = ParticipantName,
         x = GazePointX..ADCSpx.,
         y = GazePointY..ADCSpx.,
         t = RecordingTimestamp,
         media_name = MediaName,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) %>% 
  mutate(lab_id = LAB_NAME)

write_csv(data_adults, here("import_scripts", LAB_NAME, "processed_data", paste0(LAB_NAME,'_adults_xy_timepoints.csv')))
