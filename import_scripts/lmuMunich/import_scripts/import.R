# LMUMUNIC

library(tidyverse)
library(here)
library(janitor)

lab_name <- "lmuMunich"
lab_dir <- paste0("import_scripts/", lab_name, "/")

# ------------------------------------------------------------------------------
# adults 

# eye-tracking data
# hard to merge so just processing both separately. 
d_a1 <- read_delim(here(lab_dir, "raw_data/lmuMunich_adults_eyetrackingdata_Bin1.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE) |> 
  clean_names() |>
  mutate(lab_id = lab_name) |>
  rename(t = recording_timestamp, 
         x = gaze_point_x,
         y = gaze_point_y,
         media_name = presented_media_name,
         participant_id = participant_name, 
         pupil_left = pupil_diameter_left, 
         pupil_right = pupil_diameter_right) |>
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right)

d_a2 <- read_delim(here(lab_dir, "raw_data/lmuMunich_adults_eyetrackingdata_Bin4_new.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE) |>
  clean_names() |>
  mutate(lab_id = lab_name) |>
  rename(t = recording_timestamp, 
         x = gaze_point_x,
         y = gaze_point_y,
         media_name = presented_media_name,
         participant_id = participant_name, 
         pupil_left = pupil_diameter_left, 
         pupil_right = pupil_diameter_right) |>
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right)

d_a <- bind_rows(d_a1, d_a2) |>
  filter(!is.na(media_name), !is.na(t)) |>
  mutate(t = t/1000)

write_csv(d_a, here(lab_dir, "processed_data", paste0(lab_name, "_adults_xy_timepoints.csv")))


# ------------------------------------------------------------------------------
# kids


d_t1 <- read_delim(here(lab_dir, "raw_data/lmuMunich_toddlers_eyetrackingdata_Bin1_new.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE) |>
  clean_names() |>
  mutate(lab_id = lab_name) |>
  rename(t = recording_timestamp, 
         x = gaze_point_x,
         y = gaze_point_y,
         media_name = presented_media_name,
         participant_id = participant_name, 
         pupil_left = pupil_diameter_left, 
         pupil_right = pupil_diameter_right) |>
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right)

d_t2 <- read_delim(here(lab_dir, "raw_data/lmuMunich_toddlers_eyetrackingdata_Bin2.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE) |>
  clean_names() |>
  mutate(lab_id = lab_name) |>
  rename(t = recording_timestamp, 
         x = gaze_point_x,
         y = gaze_point_y,
         media_name = presented_media_name,
         participant_id = participant_name, 
         pupil_left = pupil_diameter_left, 
         pupil_right = pupil_diameter_right) |>
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right)

d_t3 <- read_delim(here(lab_dir, "raw_data/lmuMunich_toddlers_eyetrackingdata_Bin3.tsv"), delim = "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE) |>
  clean_names() |>
  mutate(lab_id = lab_name) |>
  rename(t = recording_timestamp, 
         x = gaze_point_x,
         y = gaze_point_y,
         media_name = presented_media_name,
         participant_id = participant_name, 
         pupil_left = pupil_diameter_left, 
         pupil_right = pupil_diameter_right) |>
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right)

d_t <- bind_rows(d_t1, d_t2, d_t3) |>
  filter(!is.na(media_name), !is.na(t)) |>
  mutate(t = t/1000)

write_csv(d_t, here(lab_dir, "processed_data", paste0(lab_name, "_toddlers_xy_timepoints.csv")))
