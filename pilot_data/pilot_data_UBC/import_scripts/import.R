library(peekds)
library(readxl)
library(tidyverse)

p <- read_csv("../raw_data/InfantLab_UBC_participant_data_total.csv")

# TODO: only BLOCK 1 is here, with the following stimuli:
# "FAM_LL_1200x9001.avi" "FAM_LR_1200x9001.avi" "FAM_RR_1200x9001.avi" "FAM_RL_1200x9001.avi"
# we have to use the Trial column to reconstruct blocks, as below
d = read_csv("../raw_data/InfantLab_UBC_SMI_data_total.csv")

# datasets
# dataset_id, monitor_size_x, monitor_size_y, sample_rate, tracker, lab_dataset_id
datasets <- tibble(dataset_id = 2,
                   monitor_size_x = 1920,
                   monitor_size_y = 1080,
                   sample_rate = 60,
                   tracker = "smi",
                   lab_dataset_id = "ubc_infantlab")

peekds::validate_table(df_table = datasets,
                       table_type = "datasets")
write_csv(datasets, "../processed_data/datasets.csv") 

# subjects
# subject_id, age, sex, lab_subject_id
subjects <- p %>%
  select(age = age_days,
         sex = participant_gender,
         lab_subject_id = subid,
         session_error) %>%
  mutate(subject_id = 0:(nrow(p) -1 ),
         error = session_error == "error") %>%
  select(-session_error)

peekds::validate_table(df_table = subjects,
                       table_type = "subjects")
write_csv(subjects, "../processed_data/subjects.csv") 


# aoi_regions
# aoi_region_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max,
# r_y_min
source("../../../metadata/generate_AOIs.R")
aoi_regions = generate_aoi_regions(screen_width = datasets$monitor_size_x, 
                                   screen_height = datasets$monitor_size_y,
                                   video_width = 1200, 
                                   video_height = 900, # says 9001 in picture name, but that seems wrong
                                   size = "big")
peekds::validate_table(df_table = aoi_regions, 
                       table_type = "aoi_regions")
write_csv(aoi_regions, "../processed_data/aoi_regions.csv")

# TODO: this is a hack because of how the data is formatted
# in order to make sure each subject + trial is unique
d$Stimulus = paste0(d$Stimulus, d$Trial)

# point of disambiguation is 30s plus 18 frames
pod = 30000 + ((1000/30) * 18)

# get the trial_num based on timestamp, for each subject
# assign trial_id based on subject/MediaName combo
trials <- filter(d, grepl("FAM", Stimulus), 
                 is.na(`RecordingTime [ms]`) == F) %>%
  group_by(Participant, Stimulus) %>%
  summarise(firsttime = min(`RecordingTime [ms]`)) %>%
  rename(lab_trial_id = Stimulus,
         lab_subject_id = Participant) %>%
  mutate(trial_num = rank(firsttime),
         condition = substr(lab_trial_id, 5, 6),
         aoi_region_id = 0,
         dataset_id = 2,
         distractor_image = "distractor",
         distractor_label = "distractor",
         full_phrase = NA,
         point_of_disambiguation = pod,
         target_image = "target", 
         target_label = "target", 
         target_side = ifelse(str_sub(condition, start = 2, end = 2) == "L", 
                              "left", "right")) %>%
  ungroup() %>%
  mutate(trial_id = 0:(n()-1)) %>%
  select(-firsttime)

peekds::validate_table(df_table = trials, 
                       table_type = "trials")
write_csv(trials, "../processed_data/trials.csv")

#################################################3
# SMI data
X = (as.numeric(as.character(d$`Point of Regard Left X [px]`))
     + as.numeric(as.character(d$`Point of Regard Right X [px]`)))/2
Y = (as.numeric(as.character(d$`Point of Regard Left Y [px]`))
     + as.numeric(as.character(d$`Point of Regard Right Y [px]`)))/2
xy_data <- tibble(lab_subject_id = d$Participant,
                  x = X,
                  y = Y,
                  t = (d$`RecordingTime [ms]` - d$`RecordingTime [ms]`[1]),
                  lab_trial_id = d$Stimulus) %>%
  filter(str_detect(lab_trial_id, "FAM"), 
         !is.na(t),
         !is.na(lab_trial_id)) %>%
  mutate(xy_data_id = 0:(n() - 1)) %>%
  left_join(trials) %>%
  left_join(subjects) %>%
  select(xy_data_id, subject_id, trial_id, x, y, t)

peekds::validate_table(df_table = xy_data, 
                       table_type = "xy_data")
write_csv(xy_data, "../processed_data/xy_data.csv")

# aoi_data
# aoi_data_id, aoi, subject, t, trial
aoi_data <- generate_aoi("../processed_data/")

peekds::validate_table(df_table = aoi_data, 
                       table_type = "aoi_data")
write_csv(aoi_data, "../processed_data/aoi_data.csv")
