library(peekds)
library(tidyverse)
library(here)


lab_dir <- "data/testLabtestLab/"


source(here("metadata/generate_AOIs_for_primary_data.R"))


# participant data
p <- read_csv(here(lab_dir, "raw_data/participantdata.csv"))

# eye-tracking data
d <- read_csv(here(lab_dir, "raw_data/transformed_data.csv"))



d_clean <- within(d, {
  stimulus[stimulus_version == 1] <- paste(stimulus[stimulus_version == 1],'_new', sep='')
  subid <- substr(subid,1,nchar(subid)-2)
}) 



# administrations

administrations <- p |>
  mutate(lab_subject_id = participant_id, 
         sex = case_when(participant_gender == "boy" ~ "male",
                         participant_gender == "girl" ~ "female",
                         TRUE ~ "other"),
         native_language = lang1, # TODO check for a unified solution to insert the language-codes
         subject_id = 0:(n()-1)) |>
  select(subject_id, lab_subject_id, sex, native_language) |>
  
  mutate(administration_id = subject_id,
         lab_administration_id = lab_subject_id,
         dataset_id = 0,
         subject_id = subject_id,
         age = round(p$age_days/30.417, digit=0),
         lab_age = p$age_days, 
         lab_age_units = "days",
         sample_rate = 30,
         tracker = "mb2-webgazer-custom",
         coding_method = "eyetracking") |> 
  inner_join(
    d_clean[!duplicated(d_clean$subid),] |> select(lab_administration_id=subid, monitor_size_x=win_width, monitor_size_y=win_height),
  )
# TODO: Stimulus stretches the entire screen for webgazer while x and y coordinates are scaled, should the screensize be specified differently?

# trials 1 - leave in subid and stimulus for now to make xy timepoints easier

# TODO exclusion due to low sampling rate, this is a special case for webcam eyetracking
# TODO how to handle exclusion reporting for participants that provided no trial data at all?

trials <- d_clean |>
  group_by(subid, stimulus) |>
  slice(1) |>
  ungroup() |>
  inner_join(trial_types, by=c('stimulus'='lab_trial_id')) |>
  mutate(trial_id = 0:(n()-1),
        trial_order = trial_num-1,
        excluded = manual_exclusion == 'yes',
        exclusion_reason = manual_exclusion_reason,
        trial_aux_data = "",) |>
  select(trial_id, trial_type_id, trial_order, excluded, exclusion_reason, trial_aux_data, subid, stimulus) 



# xy timepoints
# TODO: What to do with screensize vs stimulus size? (see above) - virtual stimulus size and scaled x y are in dataset

trials |> inner_join(trial_types)

d_clean |>
  inner_join(administrations, by=c('subid'='lab_subject_id')) |>
  inner_join( trials , by=c('subid'='subid', 'stimulus'='stimulus')) |>
  mutate(xy_timepoint_id = 0:(n()-1),
         x = x_stim, # TODO: Flip - wait for clarification on coordinates used (screen vs stimulus)
         y = y_stim, # TODO: Flip - wait for clarification on coordinates used (screen vs stimulus)
         t_norm = t,) |> # TODO: Have we implemented our current norming function based on stimulus?
  select(xy_timepoint_id, x, y, t_norm, administration_id, trial_id)


# trials 2
trials <- trials |> select(-subid, -stimulus) 
