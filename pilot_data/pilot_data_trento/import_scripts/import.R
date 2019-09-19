library(peekds)
library(readxl)
library(tidyverse)

p <- readxl::read_xlsx("../raw_data/babylab-trento-participants.xlsx")
d = read_tsv("../raw_data/babylab-trento.tsv") # TODO: row 41892 does not parse correctly

# datasets
# dataset_id, monitor_size_x, monitor_size_y, sample_rate, tracker, lab_dataset_id
datasets <- tibble(dataset_id = 1,
                   monitor_size_x = 1280,
                   monitor_size_y = 1024,
                   sample_rate = 120,
                   tracker = "tobii",
                   lab_dataset_id = "trento_babylab")

peekds::validate_table(df_table = datasets,
                       table_type = "datasets")
write_csv(datasets, "../processed_data/datasets.csv") 

# subjects
# subject_id, age, sex, lab_subject_id
subjects <- p %>%
  select(age = age_days,
         sex = participant_gender,
         lab_subject_id = subid) %>%
  mutate(subject_id = 0:(nrow(p) -1 ))

peekds::validate_table(df_table = subjects,
                       table_type = "subjects")
write_csv(subjects, "../processed_data/subjects.csv") 


# aoi_regions
# aoi_region_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max,
# r_y_min
source("../../../metadata/generate_AOIs.R")
aoi_regions = generate_aoi_regions(screen_width = datasets$monitor_size_x, 
                                   screen_height = datasets$monitor_size_y,
                                   video_width = 1280, # from data #TODO: how do we get this from data?
                                   video_height = 1024, 
                                   size = "big")

peekds::validate_table(df_table = aoi_regions, 
                       table_type = "aoi_regions")
write_csv(aoi_regions, "../processed_data/aoi_regions.csv")

# trials
# trial_id, aoi_region, dataset, lab_trial_id, 
# distractor_image, distractor_label,
# full_phrase, point_of_disambiguation, target_image, 
# target_label, target_side
media <- unique(d$MediaName) 
media <- media[str_detect(media, "FAM") & !is.na(media)]

# point of disambiguation is 30s plus 18 frames
# TODO: is this always the pod? could store in helper script
pod = 30000 + ((1000/30) * 18)

# target side is last letter of the media label
str_sub(media, start = 6, end = 6)

trials <- tibble(aoi_region_id = 0, 
                 aoi_region = 0,
                 dataset_id = 1, 
                 lab_trial_id = media, 
                 distractor_image = "distractor", 
                 distractor_label = "distractor",
                 full_phrase = NA,
                 point_of_disambiguation = pod, 
                 target_image = "target", 
                 target_label = "target", 
                 target_side = ifelse(str_sub(media, start = 6, end = 6) == "L", 
                                      "left", "right")) %>%
  mutate(trial_id = 0:(n() - 1))

peekds::validate_table(df_table = trials, 
                       table_type = "trials")
write_csv(trials, "../processed_data/trials.csv")

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
  select(xy_data_id, subject_id, trial_id, x, y, t)

peekds::validate_table(df_table = xy_data, 
                       table_type = "xy_data")
write_csv(xy_data, "../processed_data/xy_data.csv")
# TODO: do we care if there are many NA x and y?

# aoi_data
# aoi_data_id, aoi, subject, t, trial
aoi_data <- generate_aoi("../processed_data/")

peekds::validate_table(df_table = aoi_data, 
                       table_type = "aoi_data")
write_csv(aoi_data, "../processed_data/aoi_data.csv")
