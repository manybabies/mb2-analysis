library(tidyverse)
library(here)
library(glue)
library(eyelinkReader)

LAB_NAME <- "babylingOslo"
DATA_DIR <- here("import_scripts", LAB_NAME)
dir.create(here(DATA_DIR, "processed_data"))

#### Toddler data ####
data_path_toddlers <- here(DATA_DIR, "raw_data", "babylingOslo_toddlers_eyetrackingdata (2)")
data_files_toddlers <- list.files(data_path_toddlers, pattern = "P?UIO_[0-9]+.edf", recursive = TRUE)

data_toddlers_cleaned <- lapply(data_files_toddlers, \(fp) {
  data_edf <- read_edf(here(data_path_toddlers, fp),
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
  
  samples <- data_edf$samples |> 
    left_join(variables,
              by = "trial",
              relationship = "many-to-one") |> 
    mutate(participant_id = str_remove(fp, "/.*"),
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
