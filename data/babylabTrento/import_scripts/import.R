library(peekds)
library(tidyverse)
library(here)
library(glue)

# ------------------------------------------------------------------------------
# preliminaries 
# load point of disambiguation data
source(here("metadata/pod.R"))

lab_dir <- "data/babylabTrento/"

# participant data
p <- read_csv(here(lab_dir, "raw_data/BLT_Trento_participantdata.csv"))

# eye-tracking data
d <- read_tsv(here(lab_dir, "raw_data/BLT_Trento_eyetrackingdata.tsv") )

# ------------------------------------------------------------------------------
# datasets
datasets <- tibble(dataset_id = 0, # deal with this later
                   lab_dataset_id = "babylabTrento",
                   cite = NA, 
                   shortcite = NA, 
                   dataset_aux_data = NA, 
                   dataset_name = "babylabTrento")

peekds::validate_table(df_table = datasets, table_type = "datasets")
write_csv(datasets, here(lab_dir, "processed_data/datasets.csv") )

# ------------------------------------------------------------------------------
# subjects
subjects <- p |>
  mutate(lab_subject_id = participant_id, 
         sex = case_when(participant_gender == "man" ~ "male",
                         participant_gender == "woman" ~ "female",
                         TRUE ~ "other"),
         native_language = case_when(native_lang1 == "Italian" ~ "ita",
                                     TRUE ~ "other"),
         subject_id = 0:(n()-1)) |>
  select(subject_id, lab_subject_id, sex, native_language)

peekds::validate_table(df_table = subjects, 
                       table_type = "subjects")
write_csv(subjects, here(lab_dir, "processed_data/subjects.csv") )

# ------------------------------------------------------------------------------
# administrations
administrations <- subjects |>
  mutate(administration_id = subject_id, 
         dataset_id = 0, 
         subject_id = subject_id,
         age = p$age_years * 12, 
         lab_age = p$age_years, 
         lab_age_units = "years",
         monitor_size_x = 1920,
         monitor_size_y = 1080,
         sample_rate = 120,
         tracker = "tobii", # ??
         coding_method = "eye-tracking")

peekds::validate_table(df_table = administrations, 
                       table_type = "administrations")
write_csv(subjects, here(lab_dir, "processed_data/administrations.csv") )

# ------------------------------------------------------------------------------
# trial_types
trial_types <- tibble(lab_trial_type_id = unique(d$`Event value`)) |>
  filter(str_detect(lab_trial_type_id, "FAM|KNOW|IG")) |>
  mutate(trial_type_id = 0:(n() -1),
         dataset_id = 0, 
         lab_dataset_id = "babylabTrento",
         aoi_region_set_id = 0) 

peekds::validate_table(df_table = trial_types, 
                       table_type = "trial_types")
write_csv(subjects, here(lab_dir, "processed_data/trial_types.csv") )

# ------------------------------------------------------------------------------
# trials
# note: trial_type_aux_data contains a fam/test numbering for each participant 
# so their exclusion info can be joined in below
# note: this code depends on a LOT of ordering assumptions about trials that 
# could be violated silently, causing badness
trials <- d |>
  filter(`Event value` %in% trial_types$lab_trial_type_id, 
         `Event` == "VideoStimulusStart") |>
    select(`Event value`, `Participant name`) |>
    rename(lab_trial_type_id = `Event value`,
           lab_subject_id = `Participant name`) |>
    group_by(lab_subject_id) |>
    mutate(trial_order = 0:(n() - 1)) |>
    ungroup() |>
    mutate(trial_id = 0:(n() - 1), 
           trial_type_aux_data = case_when(
             str_detect(lab_trial_type_id, "FAM") ~ glue("fam{trial_order+1}"),
             TRUE ~ glue("test{trial_order - 3}")
             ))
  
# parse out exclusions
excluded_trials <- p |>
  select(participant_id, contains("error")) |>
  pivot_longer(contains("error"), names_to = "trial", values_to = "error") |>
  filter(!str_detect(trial, "session_error")) |>
  separate(trial, into = c("trial","type"), extra = "merge") |>
  pivot_wider(names_from = "type", values_from = "error") |>
  filter(trial != "trial") |>
  mutate(excluded = ifelse(error == "error", TRUE, FALSE), 
         excluded_reason = error_info, 
         lab_subject_id = participant_id) |>
  rename(trial_type_aux_data = trial) |>  # these are not actual trial types, they are schmatic ordering bits
  select(lab_subject_id, trial_type_aux_data, excluded, excluded_reason)

# join in exclusions and also trial_type_ids (being lazy and just doing this from 
# the prior table)  
trials <- left_join(trials, excluded_trials) |>
  left_join(select(trial_types, trial_type_id, lab_trial_type_id))

peekds::validate_table(df_table = trials, 
                       table_type = "trials")
write_csv(subjects, here(lab_dir, "processed_data/trial_types.csv") )

# ------------------------------------------------------------------------------
# aoi_region_sets
# note, relies on assumption that all admins within dataset had the same monitor size!
source(here("metadata/generate_AOIs_for_primary_data.R"))
aoi_region_sets <- generate_aoi_regions(screen_width = administrations$monitor_size_x[1], 
                                        screen_height = administrations$monitor_size_y[1],
                                        video_width = 1280, # from data #TODO: how do we get this from data?
                                        video_height = 1024)

#peekds::validate_table(df_table = aoi_regions, 
#                       table_type = "aoi_regions")
write_csv(aoi_regions, here(lab_dir, "processed_data/aoi_regions.csv"))


# TODO: this fails because it is looking for aoi_region and not aoi_region_id
#peekds::validate_table(df_table = trials, 
#                       table_type = "trials")
write_csv(trials, here(lab_dir, "processed_data/trials.csv"))



# ------------------------------------------------------------------------------
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
  select(xy_data_id, subject_id, trial_id, x, y, t, point_of_disambiguation) %>%
  center_time_on_pod() %>%
  xy_trim(datasets)

#peekds::validate_table(df_table = xy_data, 
#                       table_type = "xy_data")
write_csv(xy_data, here(lab_dir, "processed_data/xy_data.csv"))

# ------------------------------------------------------------------------------
# aoi_data
# aoi_data_id, aoi, subject, t, trial
aoi_data <- generate_aoi_small(here(lab_dir, "processed_data/"))

#peekds::validate_table(df_table = aoi_data, 
#                      table_type = "aoi_data")
write_csv(aoi_data, here(lab_dir, "processed_data/aoi_data.csv"))
