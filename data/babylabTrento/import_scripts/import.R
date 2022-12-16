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
         lab_administration_id = lab_subject_id,
         dataset_id = 0, 
         subject_id = subject_id,
         age = p$age_years * 12, 
         lab_age = p$age_years, 
         lab_age_units = "years",
         monitor_size_x = 1920,
         monitor_size_y = 1080,
         sample_rate = 120,
         tracker = "tobii", # ??
         coding_method = "eyetracking")

peekds::validate_table(df_table = administrations, 
                       table_type = "administrations")
write_csv(administrations, here(lab_dir, "processed_data/administrations.csv") )

# ------------------------------------------------------------------------------
# trial_types
trial_types <- tibble(lab_trial_type_id = unique(d$`Event value`)) |>
  filter(str_detect(lab_trial_type_id, "FAM|KNOW|IG|star_calib")) |>
  mutate(trial_type_id = 0:(n() -1),
         dataset_id = 0, 
         lab_dataset_id = "babylabTrento",
         aoi_region_set_id = 0) 

peekds::validate_table(df_table = trial_types, 
                       table_type = "trial_types")
write_csv(trial_types, here(lab_dir, "processed_data/trial_types.csv") )

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
write_csv(trials, here(lab_dir, "processed_data/trials.csv") )

# ------------------------------------------------------------------------------
# aoi_region_sets
# note, relies on assumption that all admins within dataset had the same monitor size!
source(here("metadata/generate_AOIs_for_primary_data.R"))
aoi_region_sets <- generate_aoi_regions(screen_width = administrations$monitor_size_x[1], 
                                        screen_height = administrations$monitor_size_y[1],
                                        video_width = 1280, 
                                        video_height = 1024)

peekds::validate_table(df_table = aoi_region_sets,
                      table_type = "aoi_region_sets")
write_csv(aoi_region_sets, here(lab_dir, "processed_data/aoi_region_sets.csv"))


# ------------------------------------------------------------------------------
# xy_timepoints
# note that tobii puts 0,0 at upper left, not lower left so we flip
source(here("metadata/pod.R"))

xy_timepoints <- d |>
  rename(x = `Gaze point X`, 
         y = `Gaze point Y`,
         t = `Eyetracker timestamp`,
         lab_trial_type_id  = `Presented Stimulus name`, 
         lab_administration_id = `Participant name`) |>
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
  xy_trim(x_max = administrations$monitor_size_x[1], 
          y_max = administrations$monitor_size_y[1]) |>
  mutate(y = administrations$monitor_size_y[1] - y) 

peekds::validate_table(df_table = xy_timepoints,
                      table_type = "xy_timepoints")
write_csv(xy_timepoints, here(lab_dir, "processed_data/xy_timepoints.csv"))

# ------------------------------------------------------------------------------
# aoi_timepoints
# aoi_timepoint_id,  trial_id, aoi,  t_norm,  administration_id
aoi_timepoints <- xy_timepoints |>
  mutate(aoi_region_set_id = 0, 
         target_side = "left") |>
  left_join(aoi_region_sets) |>
  add_aois() |>
  rename(aoi_timepoint_id = xy_timepoint_id) |>
  select(aoi_timepoint_id, trial_id, administration_id, t_norm, aoi, side)

peekds::validate_table(df_table = aoi_timepoints,
                     table_type = "aoi_timepoints")
write_csv(aoi_timepoints, here(lab_dir, "processed_data/aoi_timepoints.csv"))

# ------------------------------------------------------------------------------
# validation

ggplot(xy_timepoints, aes(x = x, y = y)) + 
  geom_point(alpha = .05) + 
  xlim(0, administrations$monitor_size_x[1]) + 
  ylim(0, administrations$monitor_size_y[1]) 

