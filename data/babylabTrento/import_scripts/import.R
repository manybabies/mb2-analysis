library(peekds)
library(tidyverse)
library(here)
library(glue)

# ------------------------------------------------------------------------------
# preliminaries 
# load point of disambiguation data
# and helper functions for XY and AOI
source(here("metadata/generate_AOIs_for_primary_data.R"))

lab_dir <- "data/babylabTrento/"

# participant data
p <- read_csv(here(lab_dir, "raw_data/BLT_Trento_participantdata.csv"))

# eye-tracking data
d <- read_tsv(here(lab_dir, "raw_data/BLT_Trento_eyetrackingdata.tsv") )

# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

# ------------------------------------------------------------------------------
# subjects & administrations
# only administrations is necessary

subjects <- p |>
  mutate(lab_subject_id = participant_id, 
         sex = case_when(participant_gender == "man" ~ "male",
                         participant_gender == "woman" ~ "female",
                         TRUE ~ "other"),
         native_language = case_when(native_lang1 == "Italian" ~ "ita",
                                     TRUE ~ "other"),
         subject_id = 0:(n()-1)) |>
  select(subject_id, lab_subject_id, sex, native_language)

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
# trials
# note: trial_type_aux_data contains a fam/test numbering for each participant 
# so their exclusion info can be joined in below
# note: this code depends on a LOT of ordering assumptions about trials that 
# could be violated silently, causing badness

lab_trial_type_ids <- unique(d$`Event value`)
lab_trial_type_ids <- lab_trial_type_ids[grepl("FAM|KNOW|IG|star_calib",lab_trial_type_ids)]

trials <- d |>
  filter(`Event value` %in% lab_trial_type_ids, 
         `Event` == "VideoStimulusStart") |>
    select(`Event value`, `Participant name`) |>
    rename(lab_trial_id = `Event value`,
           lab_subject_id = `Participant name`) |>
    group_by(lab_subject_id) |>
    mutate(trial_order = 0:(n() - 1)) |>
    ungroup() |>
    mutate(trial_id = 0:(n() - 1), 
           trial_type_aux_data = case_when(
             str_detect(lab_trial_id, "FAM") ~ glue("fam{trial_order}"),
             str_detect(lab_trial_id, "star") ~ "calib check",
             TRUE ~ glue("test{trial_order - 4}")
             )) # note, this part is totally magic-numbered to deal with calib check ordering

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
  left_join(select(trial_types, trial_type_id, lab_trial_id))

write_csv(trials, here(lab_dir, "processed_data/trials.csv") )

# ------------------------------------------------------------------------------
# xy_timepoints
# note that tobii puts 0,0 at upper left, not lower left so we flip

xy_timepoints <- d |>
  rename(x = `Gaze point X`, 
         y = `Gaze point Y`,
         t = `Eyetracker timestamp`,
         lab_trial_id  = `Presented Stimulus name`, 
         lab_administration_id = `Participant name`) |>
  mutate(t = t / 1000) |> # microseconds to milliseconds correction
  select(x, y, t, lab_trial_id, lab_administration_id) |>
  filter(lab_trial_id %in% unique(trials$lab_trial_id)) |>
  left_join(select(trials, lab_trial_id, trial_id, lab_subject_id) |>
              rename(lab_administration_id = lab_subject_id)) |>
  left_join(select(administrations, lab_administration_id, administration_id)) |>
  group_by(lab_trial_id, lab_administration_id) |>
  mutate(t_zeroed = t - t[1]) |>
  left_join(select(trial_types, lab_trial_id, point_of_disambiguation)) |>
  peekds::normalize_times() |>
  select(trial_id, administration_id, lab_trial_id, lab_administration_id, x, y, t_norm) |>
  peekds::resample_times(table_type = "xy_timepoints") |>
  xy_trim(x_max = administrations$monitor_size_x[1], 
          y_max = administrations$monitor_size_y[1]) |>
  mutate(y = administrations$monitor_size_y[1] - y) 

peekds::validate_table(df_table = xy_timepoints,
                      table_type = "xy_timepoints")
write_csv(xy_timepoints, here(lab_dir, "processed_data/xy_timepoints.csv"))

# ------------------------------------------------------------------------------
# validation

ggplot(xy_timepoints, aes(x = x, y = y)) + 
  geom_point(alpha = .05) + 
  xlim(0, administrations$monitor_size_x[1]) + 
  ylim(0, administrations$monitor_size_y[1]) 
