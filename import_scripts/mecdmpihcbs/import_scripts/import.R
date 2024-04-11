# mecdmpihcbs
# Gal Raz import script
# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

library(tidyverse)
library(here)

# preliminaries 
lab_dir <- "import_scripts/mecdmpihcbs"

d_adults <- read_csv(here(lab_dir, "raw_data/mecdmpihcbs_adults_eyetracking_data.csv"))

### Replacing GazePoint of 0 with NA and Pupil Size of -1 with NA

d_adults$GazePointX[d_adults$GazePointX==0]<-NA
d_adults$GazePointY[d_adults$GazePointY==0]<-NA
d_adults$PupilLeft[d_adults$PupilLeft==-1]<-NA
d_adults$PupilRight[d_adults$PupilRight==-1]<-NA

xy_timepoints <- d_adults |>
  rename(x = GazePointX, 
         y = GazePointY,
         t = Timestamp,
         media_name  = StimuliName, 
         participant_id = ID,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |>
  mutate(lab_id = "mecdmpihcbs") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/mecdmpihcbs_adults_xy_timepoints.csv"))


d_toddlers <- read_csv(here(lab_dir, "raw_data/mecdmpihcbs_toddlers_eyetracking_data.csv"))

### Replacing GazePoint of 0 with NA and Pupil Size of -1 with NA

d_toddlers$GazePointX[d_toddlers$GazePointX==0]<-NA
d_toddlers$GazePointY[d_toddlers$GazePointY==0]<-NA
d_toddlers$PupilLeft[d_toddlers$PupilLeft==-1]<-NA
d_toddlers$PupilRight[d_toddlers$PupilRight==-1]<-NA

xy_timepoints <- d_toddlers |>
  rename(x = GazePointX, 
         y = GazePointY,
         t = Timestamp,
         media_name  = StimuliName, 
         participant_id = ID,
         pupil_left = PupilLeft,
         pupil_right = PupilRight) |>
  mutate(lab_id = "mecdmpihcbs") |> 
  select(lab_id, participant_id, media_name, x, y, t, pupil_left, pupil_right) |>
  filter(!is.na(media_name)) 

write_csv(xy_timepoints, here(lab_dir, "processed_data/mecdmpihcbs_toddlers_xy_timepoints.csv"))
