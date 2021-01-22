library(peekds)
library(readxl)
library(tidyverse)
library(edfR)
library(here)



source(here("metadata/pod.R"))

lab_dir = "pilot_data/pilot_1b_copenhagen/"

# You need EyeLink Developers Kit and edfR
# https://www.sr-support.com/forum/downloads/eyelink-display-software/45-eyelink-developers-kit-for-mac-os-x-mac-os-x-display-software?15-EyeLink-Developers-Kit-for-Mac-OS-X=
subjs = dir(here(lab_dir, "raw_data/children/"))

# This is currently a little funky.The trials, as output by eyelink, seem to start
# about 1s before the video and there does not seem to be an easy way to get
# the time when the video starts (?). Here, I look for the time the first frame
# is displayed (you have to use an offset because it tells you the offset between
# when the frame is displayed and when the message is written down) and use that.
# This seems to make the data line up with the other labs.
d.pretrim <- subjs %>%
  map_df(function(subj) {
    xy = edf.samples(paste0(here(lab_dir, "raw_data/children/"), subj, "/", subj, ".edf"), trials=T) %>%
      mutate(lab_subject_id = subj)
    msg = edf.messages(paste0(here(lab_dir, "raw_data/children/"), subj, "/", subj, ".edf"))
    
    framestart =  filter(msg, grepl("Frame to be displayed 1$", msg)) %>%
      mutate(eyetrial = 1:n(),
             msg = str_replace_all(msg, "\\s", "|")) %>%
      separate(msg, into=c("offset", "a", "b", "c", "d", "frame"), sep="\\|") %>%
      mutate(first_frame_time = time + as.numeric(as.character(offset))) %>%
      select(first_frame_time, eyetrial)
    
    dd = filter(msg, grepl("videofile", msg)) %>%
      separate(msg, into=c("V", "VAR", "videofile", "video_name"), sep=" ") %>%
      mutate(eyetrial=1:n()) %>%
      select(eyetrial, video_name) %>%
      left_join(framestart)
    
    left_join(xy, dd)
  }
  )
d = filter(d.pretrim, time >= first_frame_time)
d$lab_subject_id = gsub("_", "", d$lab_subject_id)

# TODO: look at funniness with start times
group_by(d.pretrim, eyetrial, lab_subject_id) %>% summarise(mintime=min(time),
                                                            first_frame_time=first(first_frame_time),
                                                            timetostart = (first_frame_time - mintime)/1000)

# datasets
# dataset_id, monitor_size_x, monitor_size_y, sample_rate, tracker, lab_dataset_id
datasets <- tibble(dataset_id = 7, 
                   monitor_size_x = 1280,
                   monitor_size_y = 1024,
                   sample_rate = 500, 
                   tracker = "eyelink", 
                   lab_dataset_id = "copenhagen_babylab")

#peekds::validate_table(df_table = datasets, 
#                       table_type = "datasets")
write_csv(datasets, here(lab_dir, "processed_data/datasets.csv"))

# subjects
# subject_id, age, sex, lab_subject_id
# TODO: one subject has age NC
p <- readxl::read_xlsx(here(lab_dir, "raw_data/MB2_pilot1b_KU_CPH_participantsheet_children.xlsx"))

subjects <- p %>%
  rename(lab_subject_id = subid,
         age = age_days, 
         sex = participant_gender) %>%
  mutate(subject_id = 0:(n() - 1),
         lab_subject_id = unique(d$lab_subject_id),
         age = as.numeric(as.character(age)),
         error = session_error == "error",
         dataset_id = 7) %>%
  select(subject_id, age, sex, lab_subject_id, error, dataset_id)

#peekds::validate_table(df_table = subjects, 
#                       table_type = "subjects")
write_csv(subjects, here(lab_dir, "processed_data/subjects.csv"))

# aoi_regions
# aoi_region_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, 
# r_y_min
source(here("metadata/generate_AOIs.R"))
aoi_regions = generate_aoi_regions(screen_width = datasets$monitor_size_x, 
                                   screen_height = datasets$monitor_size_y,
                                   video_width = 1280, # from data
                                   video_height = 960 
)

#peekds::validate_table(df_table = aoi_regions, 
#                       table_type = "aoi_regions")
write_csv(aoi_regions, here(lab_dir, "processed_data/aoi_regions.csv"))

# xy_data
# xy_data_id, subject_id, trial_id, x, y, t

# trials
# trial_id, aoi_region, dataset, lab_trial_id, distractor_image, distractor_label, 
# full_phrase, point_of_disambiguation, target_image, target_label, target_side

# get the trial_num based on timestamp, for each subject
# assign trial_id based on subject/MediaName combo
trials <- filter(d, grepl("FAM", d$video_name), 
                 is.na(time) == F) %>%
  mutate(lab_trial_id = paste(video_name, eyetrial, sep="_")) %>%
  group_by(lab_subject_id, lab_trial_id) %>%
  summarise(firsttime = min(time)) %>%
  mutate(trial_num = rank(firsttime),
         condition = substr(lab_trial_id, 5, 6),
         experiment_num = ifelse(grepl("no_outcome", lab_trial_id), "pilot_1b_no_outcome", "pilot_1b_outcome"),
         has_outcome = grepl("no_outcome", lab_trial_id) == F,
         aoi_region_id = 0,
         dataset_id = 7,
         distractor_image = "distractor",
         distractor_label = "distractor",
         distractor_id = "distractor",
         full_phrase = NA,
         point_of_disambiguation = pod_pilot_1b,
         target_image = "target", 
         target_label = "target", 
         target_id = "target",
         target_side = ifelse(str_sub(condition, start = 2, end = 2) == "L", 
                              "left", "right")) %>%
  ungroup() %>%
  mutate(trial_id = 0:(n()-1)) %>%
  select(-firsttime)

# TODO: this fails because it is looking for aoi_region and not aoi_region_id
#peekds::validate_table(df_table = trials, 
#                       table_type = "trials")
write_csv(trials, here(lab_dir, "processed_data/trials.csv"))

# from https://www.tobiipro.com/siteassets/tobii-pro/user-manuals/tobii-pro-studio-user-manual.pdf
# we want ADCSpx coordinates - those are display coordinates
# note tobii gives upper-left indexed coordinates
xy_data <- tibble(lab_subject_id = d$lab_subject_id,
                  x = d$gxR,
                  y = d$gyR,
                  t = (d$time - d$time[1]),
                  lab_trial_id = paste(d$video_name, d$eyetrial, sep="_")) %>%
  filter(grepl("FAM", lab_trial_id),
         !is.na(t)) %>%
  mutate(xy_data_id = 0:(n() - 1)) %>%
  left_join(trials) %>%
  left_join(subjects) %>%
  select(xy_data_id, subject_id, trial_id, x, y, t, point_of_disambiguation) %>%
  center_time_on_pod()

# round to the precision given to 
# avoid writing out excessively long floating point numbers
xy_data$x = round(xy_data$x, 1)
xy_data$y = round(xy_data$y, 1)
xy_data = filter(xy_data, !(is.na(xy_data$x) == T & is.na(xy_data$y) == T))
#peekds::validate_table(df_table = xy_data, 
#                       table_type = "xy_data")
write_csv(xy_data, here(lab_dir, "processed_data/xy_data.csv"))

# aoi_data
# aoi_data_id, aoi, subject, t, trial
aoi_data <- generate_aoi_small(here(lab_dir, "processed_data/"))

#peekds::validate_table(df_table = aoi_data, 
#                       table_type = "aoi_data")
write_csv(aoi_data, here(lab_dir, "processed_data/aoi_data.csv"))