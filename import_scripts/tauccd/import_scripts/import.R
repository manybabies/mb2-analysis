library(tidyverse)
library(here)
library(glue)
library(eyelinkReader)

LAB_NAME <- "tauccd"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Adult data ####
data_path <- here(DATA_DIR, "raw_data", "EDF files")
data_files <- list.files(data_path, pattern = ".edf", recursive = TRUE)
data_files_adults <- data_files[17:32]

data_adults_cleaned <- lapply(data_files_adults, \(fp) {
  data_edf <- read_edf(here(data_path_adults, fp),
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
           x = mean(c(pxL, pxR), na.rm = TRUE),
           y = mean(c(pyL, pyR), na.rm = TRUE),
           lab_id = LAB_NAME) |> 
    select(participant_id, 
           x, y, 
           t = time, media_name,
           pupil_left = paL, pupil_right = paR,
           lab_id)
  
  samples
}) |> bind_rows()

write_csv(data_adults_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))

#### Toddler data ####
data_files_toddlers <- data_files[1:16]

data_toddlers_cleaned <- lapply(data_files_toddlers, \(fp) {
  data_edf <- read_edf(here(data_path_toddlers, fp),
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
           x = mean(c(pxL, pxR), na.rm = TRUE),
           y = mean(c(pyL, pyR), na.rm = TRUE),
           lab_id = LAB_NAME) |> 
    select(participant_id, 
           x, y, 
           t = time, media_name,
           pupil_left = paL, pupil_right = paR,
           lab_id)
  
  samples
}) |> bind_rows()

write_csv(data_toddlers_cleaned,
          here(DATA_DIR, "processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

