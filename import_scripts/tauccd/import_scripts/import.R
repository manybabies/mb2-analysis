library(tidyverse)
library(here)
library(glue)
library(eyelinkReader)

# NOTE: Requires demog data to get true participant IDs

LAB_NAME <- "tauccd"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_path <- here(DATA_DIR, "raw_data", "EDF files ")
data_files <- list.files(data_path, pattern = ".edf", recursive = TRUE)
data_files_adults <- data_files[17:32]
data_demog_adults <- read_csv(here(DATA_DIR, "raw_data", "tauccd_adults_participantdata.csv"))

data_adults_cleaned <- lapply(data_files_adults, \(fp) {
  data_edf <- read_edf(here(data_path, fp),
                       import_samples = TRUE,
                       import_saccades = FALSE,
                       import_blinks = FALSE,
                       import_fixations = FALSE,
                       sample_attributes = c("time", # time
                                             "px", "py", # pupil coords
                                             "pa")) # pupil area
  
  samples <- data_edf$samples |> 
    left_join(data_edf$variables |> 
                filter(variable == "trialtype") |> 
                select(trial, media_name = value),
              by = "trial") |> 
    mutate(participant_id = str_remove(fp, ".*/") |> str_remove("\\.edf"),
           x = rowMeans(cbind(pxL, pxR), na.rm = TRUE),
           y = rowMeans(cbind(pyL, pyR), na.rm = TRUE),
           lab_id = LAB_NAME) |> 
    select(participant_id, 
           x, y, 
           t = time, media_name,
           pupil_left = paL, pupil_right = paR,
           lab_id)
  
  samples
}) |> bind_rows()

data_adults_cleaned <- data_adults_cleaned |> 
  left_join(data_demog_adults |> 
              select(participant_id, test_order) |> 
              mutate(test_order = as.character(test_order)), 
            by = c("participant_id" = "test_order")) |> 
  mutate(participant_id = participant_id.y) |> 
  select(-participant_id.y)

write_csv(data_adults_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))

#### Toddler data ####
data_files_toddlers <- data_files[1:16]
data_demog_toddlers <- read_csv(here(DATA_DIR, "raw_data", "tauccd_toddlers_participantdata.csv"))

data_toddlers_cleaned <- lapply(data_files_toddlers, \(fp) {
  data_edf <- read_edf(here(data_path, fp),
                       import_samples = TRUE,
                       import_saccades = FALSE,
                       import_blinks = FALSE,
                       import_fixations = FALSE,
                       sample_attributes = c("time", # time
                                             "px", "py", # pupil coords
                                             "pa")) # pupil area
  
  variables <- data_edf$variables |> 
    filter(variable == "trialtype") |> 
    select(trial, media_name = value)
  # some variable dfs list both the current trial and all following trials;
  # this removes the extraneous rows
  if (nrow(variables) > 13) { 
    variables <- variables |> 
      group_by(trial) |> 
      slice(1) |> 
      ungroup()
  }
  
  samples <- data_edf$samples |> 
    left_join(variables,
              by = "trial",
              relationship = "many-to-one") |> 
    mutate(participant_id = str_remove(fp, ".*/") |> str_remove("\\.edf"),
           x = rowMeans(cbind(pxL, pxR), na.rm = TRUE),
           y = rowMeans(cbind(pyL, pyR), na.rm = TRUE),
           lab_id = LAB_NAME) |> 
    select(participant_id, 
           x, y, 
           t = time, media_name,
           pupil_left = paL, pupil_right = paR,
           lab_id)
  
  samples
}) |> bind_rows()

data_toddlers_cleaned <- data_toddlers_cleaned |> 
  left_join(data_demog_toddlers |> 
              select(participant_id, test_order) |> 
              mutate(test_order = as.character(test_order)), 
            by = c("participant_id" = "test_order")) |> 
  mutate(participant_id = participant_id.y) |> 
  select(-participant_id.y)

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

