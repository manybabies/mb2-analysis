library(peekds)
library(readxl)
library(tidyverse)
library(here)
library(glue)

## TO-DO:
# Check that datasets, administrations & trials are following the import guide: https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit#heading=h.8byw2x9ukxkr
# Adapt xy time points from Trento: https://github.com/manybabies/mb2-analysis/blob/master/data/babylabTrento/import_scripts/import.R
#g enerate AOI region sets: ??
# generate AOI timepoints: create_aoi_timepoints()
lab_dir = "data/jmuCDL/"


raw_data = read.csv(here(lab_dir, 'raw_data/jmuCDL_PilotData.csv'))

## datasets
# dataset_id, lab_dataset_id, cite, sortcite, dataset_aux_data
# datasets <- tibble(dataset_id = 0,
#                    lab_dataset_id = "jmuCDL",
#                    dataset_name = '',
#                    cite = NA,
#                    shortcite = NA,
#                    dataset_aux_data = NA)
# 
# write_csv(datasets, here(lab_dir, "processed_data/datasets.csv"))

## subjects
# subject_id, age, sex, lab_subject_id
# p <- read.csv(here(lab_dir, "raw_data/ManyBabies2_ Lab Participants Data Adults_jmuCDL.csv"))
# 
# subjects <- p %>%
#   rename(lab_subject_id = participant_id) %>%
#   mutate(subject_id = 0:(n() - 1),
#          subject_aux_data = NA,
#          native_language=native_lang1,
#          sex = case_when(
#            participant_gender == 'woman' ~ 'female',
#            participant_gender == 'man' ~ 'male',
#            participant_gender == 'other' ~ 'other',
#            TRUE ~ 'unspecified'
#          )) %>%
#   select(subject_id, sex, lab_subject_id, native_language, subject_aux_data)
# 
# 
# #peekds::validate_table(df_table = subjects, 
# #                       table_type = "subjects")
# write_csv(subjects, here(lab_dir, "processed_data/subjects.csv"))
datasets <- tibble(dataset_id = 0,
                   lab_dataset_id = "jmuCDL",
                   dataset_name = '',
                   cite = NA,
                   shortcite = NA,
                   dataset_aux_data = NA)

write_csv(datasets, here(lab_dir, "processed_data/datasets.csv"))

## subjects
# subject_id, age, sex, lab_subject_id
p <- read.csv(here(lab_dir, "raw_data/ManyBabies2_ Lab Participants Data Adults_jmuCDL.csv"))

subjects <- p %>%
  rename(lab_subject_id = participant_id) %>%
  mutate(subject_id = 0:(n() - 1),
         subject_aux_data = NA,
         native_language=native_lang1,
         sex = case_when(
           participant_gender == 'woman' ~ 'female',
           participant_gender == 'man' ~ 'male',
           participant_gender == 'other' ~ 'other',
           TRUE ~ 'unspecified'
         )) %>%
  select(subject_id, sex, lab_subject_id, native_language, subject_aux_data)


#peekds::validate_table(df_table = subjects, 
#                       table_type = "subjects")
write_csv(subjects, here(lab_dir, "processed_data/subjects.csv"))

## administrations
# administration_id, dataset_id, subject_id,age,lab_age,lab_age_units,monitor_size_x,monitor_size_y,sample_rate,tracker,coding_method,administration_aux_data

administrations <- 
  tibble(subject_id = subjects$subject_id,
         dataset_id = datasets$dataset_id,
         lab_age = p$age_years,
         lab_age_units = 'years',
         age = p$age_years * 12,
         monitor_size_x = raw_data$Recording.resolution.width[1],
         monitor_size_y = raw_data$Recording.resolution.height[1],
         sample_rate = 300, # from google sheet (https://docs.google.com/spreadsheets/d/16TQ3fOGjATD0YhFPAw4I6JBTCka3Vh994nMQAFtWAiw/edit#gid=1390267299)
         tracker = "Tobii Pro Spectrum", # from same google sheet
         coding_method = "in-lab eye tracking", # from same google sheet, might be just 'eyetracking' as specific in peekbank column info
         administration_aux_data = NA
         ) %>%
  mutate(administration_id = 0:(n() -1))

write_csv(administrations, here(lab_dir, "processed_data/administrations.csv"))


## trials
# trial_id, trial_order, excluded, exclusion_reason, trial_type_id, trial_aux_data

# first we get all the idx in which the trials start or end
fam_trial_idx = grep('FAM', raw_data$Event.value)
IG_trial_idx = grep('IG', raw_data$Event.value)
KNOW_trial_idx = grep('KNOW', raw_data$Event.value)

# concatenate and sort
all_idx = sort(c(fam_trial_idx, IG_trial_idx, KNOW_trial_idx))

# only look for video stimulus start, because the trial labels appear twice, once for start and once for end
trial_info =  raw_data[all_idx,] %>% filter(Event == 'VideoStimulusStart') %>% select(Event.value, Participant.name)


trials <- tibble(lab_trial_type_id = trial_info$Event.value,	lab_subject_id	= trial_info$Participant.name) %>% 
  group_by(lab_subject_id) %>% mutate(trial_order = 0:(n()-1)) %>% ungroup() %>% 
                                        mutate(fam_or_test = case_when(
                                          startsWith(lab_trial_type_id, 'FAM') ~ 'fam',
                                          TRUE ~ 'test')) %>% 
                                      group_by(lab_subject_id, fam_or_test) %>%
                                      mutate(trial_aux_data = case_when(
                                        fam_or_test == 'fam' ~ paste('fam',row_number(),sep=""),
                                        fam_or_test == 'test' ~ paste('test',row_number(),sep=""),
                                        )
                                      ) %>% ungroup() %>%
                                      mutate(trial_type_id = match(lab_trial_type_id, unique(lab_trial_type_id)) - 1) %>%
                                      mutate(trial_id = 0:(n()-1))
                                    
excluded_trials <- p |>
  select(participant_id, contains("error")) |>
  pivot_longer(contains("error"), names_to = "trial", values_to = "error") |>
  filter(!str_detect(trial, "session_error")) |>
  separate(trial, into = c("trial","type"), extra = "merge") |>
  pivot_wider(names_from = "type", values_from = "error") |>
  filter(trial != "trial") |>
  mutate(excluded = ifelse(error == "error", TRUE, FALSE), 
         exclusion_reason = error_info, 
         lab_subject_id = participant_id) |>
  rename(trial_aux_data = trial) |>  # these are not actual trial types, they are schmatic ordering bits
  select(lab_subject_id, trial_aux_data, excluded, exclusion_reason)

trials <- left_join(trials, excluded_trials, by = c('trial_aux_data', 'lab_subject_id')) %>% select(-c(fam_or_test))

write_csv(trials, here(lab_dir, "processed_data/trials.csv"))


# xy_timepoints
# note that tobii puts 0,0 at upper left, not lower left so we flip
source(here("metadata/generate_AOIs.R"))

source(here("metadata/pod.R"))

xy_timepoints <- raw_data |>
  rename(x = `Gaze.point.X`, 
         y = `Gaze.point.Y`,
         t = `Eyetracker.timestamp`,
         lab_trial_type_id  = `Presented.Stimulus.name`, 
         lab_administration_id = `Participant.name`) |>
  mutate(t = t / 1000) |> # microseconds to milliseconds correction
  select(x, y, t, lab_trial_type_id, lab_administration_id) |>
  filter(lab_trial_type_id %in% unique(trials$lab_trial_type_id)) |>
  left_join(select(trials, lab_trial_type_id, trial_id, lab_subject_id) |>
              rename(lab_administration_id = lab_subject_id)) |>
  left_join(select(administrations, lab_administration_id, administration_id)) |>
  group_by(lab_trial_type_id, lab_administration_id) |>
  mutate(t_zeroed = t - t[1]) |>
  add_pod() |>
  peekds::normalize_times() |>
  select(trial_id, administration_id, lab_trial_type_id, lab_administration_id, x, y, t_norm) |>
  peekds::resample_times(table_type = "xy_timepoints") |>
  xy_trim(xy = administrations$monitor_size_x[1], 
          y_max = administrations$monitor_size_y[1]) |>
  mutate(y = administrations$monitor_size_y[1] - y) 

peekds::validate_table(df_table = xy_timepoints,
                       table_type = "xy_timepoints")
write_csv(xy_timepoints, here(lab_dir, "processed_data/xy_timepoints.csv"))


## aoi_region_sets ##UNDER CONSTRUCTION - NEED TO FIND VIDEO WIDTH/HEIGHT
source(here("metadata/generate_AOIs.R"))
aoi_region_sets = generate_aoi_regions(screen_width = administrations$monitor_size_x, 
                                       screen_height = administrations$monitor_size_y)
#video_width = 1200, 
# video_height = 900) 


#peekds::validate_table(df_table = aoi_regions, 
#                       table_type = "aoi_regions")
write_csv(aoi_region_sets, here(labdir, "processed_data/aoi_region_sets.csv"))

## stimuli ## UNDER CONSTRUCTIONS

stimuli <- tibble(
  stimulus_id = 0,
  
)

write_csv(stimuli, here(labdir, "processed_data/stimuli.csv"))

## trial_types 
trial_types <- tibble(trial_type_id = )


write_csv(trial_types, here(labdir, "processed_data/trial_types.csv"))
