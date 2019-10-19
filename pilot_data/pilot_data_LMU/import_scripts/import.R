library(peekds)
library(readxl)
library(tidyverse)
library(here)

source(lab(here, "metadata/pod.R"))

lab_dir = "pilot_data/pilot_data_LMU"

# parsing errors for adults
p <- bind_rows(readxl::read_xlsx(here(lab_dir, "raw_data/LMU_Munich_participantsheet_children.xlsx")))
               #readxl::read_xlsx(here(lab_dir, "raw_data/LMU_Munich_participantsheet_adults.xlsx"))

d = bind_rows(read_tsv(here(lab_dir, "raw_data/LMU_Munich_rawdata_children.tsv")))
              #read_tsv(here(lab_dir, "raw_data/LMU_Munich_rawdata_adults.tsv"))

# datasets
# dataset_id, monitor_size_x, monitor_size_y, sample_rate, tracker, lab_dataset_id
datasets <- tibble(dataset_id = 4,
                   monitor_size_x = 1280,
                   monitor_size_y = 1024,
                   sample_rate = 60,
                   tracker = "tobii",
                   lab_dataset_id = "lmu_babylab")

peekds::validate_table(df_table = datasets,
                       table_type = "datasets")
write_csv(datasets, here(lab_dir, "processed_data/datasets.csv") )

# subjects
# subject_id, age, sex, lab_subject_id
subjects <- p %>%
  select(age = age_days,
         sex = participant_gender,
         lab_subject_id = subid,
         session_error) %>%
  mutate(subject_id = 0:(nrow(p) -1 ),
         error = session_error == "error",
         dataset_id = 4) %>%
  select(-session_error)

peekds::validate_table(df_table = subjects,
                       table_type = "subjects")
write_csv(subjects, here(lab_dir, "processed_data/subjects.csv") )


# aoi_regions
# aoi_region_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max,
# r_y_min
source(here("metadata/generate_AOIs.R"))
aoi_regions = generate_aoi_regions(screen_width = datasets$monitor_size_x, 
                                   screen_height = datasets$monitor_size_y,
                                   video_width = 1280, # from data #TODO: how do we get this from data?
                                   video_height = 960, 
                                   size = "big")

peekds::validate_table(df_table = aoi_regions, 
                       table_type = "aoi_regions")
write_csv(aoi_regions, here(lab_dir, "processed_data/aoi_regions.csv"))

# trials
# trial_id, aoi_region, dataset, lab_trial_id, distractor_image, distractor_label, 
# full_phrase, point_of_disambiguation, target_image, target_label, target_side

# get the trial_num based on timestamp, for each subject
# assign trial_id based on subject/MediaName combo
trials <- filter(d, grepl("FAM", d$MediaName), 
                 is.na(EyeTrackerTimestamp) == F) %>%
  group_by(ParticipantName, MediaName) %>%
  summarise(firsttime = min(EyeTrackerTimestamp)) %>%
  rename(lab_trial_id = MediaName,
         lab_subject_id = ParticipantName) %>%
  mutate(trial_num = rank(firsttime),
         condition = substr(lab_trial_id, 5, 6),
         aoi_region_id = 0,
         dataset_id = 4,
         distractor_image = "distractor",
         distractor_label = "distractor",
         full_phrase = NA,
         point_of_disambiguation = pod,
         target_image = "target", 
         target_label = "target", 
         target_side = ifelse(str_sub(condition, start = 2, end = 2) == "L", 
                              "left", "right"),
         distractor_id = 0,
         target_id = 0) %>%
  ungroup() %>%
  mutate(trial_id = 0:(n()-1)) %>%
  select(-firsttime)

# TODO: this fails because it is looking for aoi_region and not aoi_region_id
peekds::validate_table(df_table = trials, 
                       table_type = "trials")
write_csv(trials, here(lab_dir, "processed_data/trials.csv"))

# from https://www.tobiipro.com/siteassets/tobii-pro/user-manuals/tobii-pro-studio-user-manual.pdf
# we want ADCSpx coordinates - those are display coordinates
# note tobii gives upper-left indexed coordinates
xy_data <- tibble(lab_subject_id = d$ParticipantName,
                  x = d$`GazePointX (ADCSpx)`,
                  y = d$`GazePointY (ADCSpx)`,
                  t = (d$EyeTrackerTimestamp - d$EyeTrackerTimestamp[1])/1000,
                  lab_trial_id = d$MediaName) %>%
  filter(str_detect(lab_trial_id, "FAM"), 
         !is.na(t),
         !is.na(lab_trial_id)) %>%
  mutate(xy_data_id = 0:(n() - 1)) %>%
  left_join(trials) %>%
  left_join(subjects) %>%
  select(xy_data_id, subject_id, trial_id, x, y, t) %>%
  center_time_on_pod()

peekds::validate_table(df_table = xy_data, 
                       table_type = "xy_data")
write_csv(xy_data, here(lab_dir, "processed_data/xy_data.csv"))

# aoi_data
# aoi_data_id, aoi, subject, t, trial
aoi_data <- generate_aoi(here(lab_dir, "processed_data/"))

peekds::validate_table(df_table = aoi_data, 
                       table_type = "aoi_data")
write_csv(aoi_data, here(lab_dir, "processed_data/aoi_data.csv"))
