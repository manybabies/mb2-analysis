library(peekds)
library(tidyverse)
library(here)

# TODO: figure out exclusions
lab_dir <- "data/testLabtestLab/"


source(here("metadata/generate_AOIs_for_primary_data.R"))


# participant data
p <- read_csv(here(lab_dir, "raw_data/participantdata.csv"))

# eye-tracking data
d <- read_csv(here(lab_dir, "raw_data/transformed_data.csv"))

# exclusion file
e <- read_csv(here(lab_dir, "raw_data/excluded_trials.csv"))



# administrations

subjects <- 

administrations <- p |>
  mutate(lab_subject_id = participant_id, 
         sex = case_when(participant_gender == "man" ~ "male",
                         participant_gender == "woman" ~ "female",
                         TRUE ~ "other"),
         native_language = case_when(native_lang1 == "Italian" ~ "ita",
                                     TRUE ~ "other"),
         subject_id = 0:(n()-1)) |>
  select(subject_id, lab_subject_id, sex, native_language) |>
  
  mutate(administration_id = 0, # TODO: what to put here?? subject_id? what if we test twice?
         lab_administration_id = lab_subject_id,
         dataset_id = 0,
         subject_id = subject_id,
         age = p$age_years * 12, 
         lab_age = p$age_years, 
         lab_age_units = "years",
         monitor_size_x = 1920,
         monitor_size_y = 1080,
         sample_rate = 120,
         tracker = "mb2-webgazer-custom", # ??
         coding_method = "eyetracking")

# trials

# xy timepoints

