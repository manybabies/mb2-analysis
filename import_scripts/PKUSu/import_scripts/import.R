library(tidyverse)
library(here)
library(glue)

LAB_NAME <- 'PKUSu'
DATA_DIR = file.path('import_scripts', LAB_NAME)
dir.create(here(DATA_DIR),"processed_data")

###Toddler data###
PKU_toddlers <- read.csv(here(DATA_DIR, "raw_data","PKUSu_toddlers_eyetrackingdata.csv"), sep="\t", fileEncoding="UTF-16LE")

PKU_toddlers[PKU_toddlers==""]<-NA
PKU_toddlers_clean <- PKU_toddlers %>% 
  rename(participant_id = Participant.name,
         x = Gaze.point.X,
         y = Gaze.point.Y,
         t = Recording.timestamp,
         media_name = Presented.Media.name,
         pupil_left = Pupil.diameter.left,
         pupil_right = Pupil.diameter.right
  ) %>% 
  ## scaling procedure
  ## we rescale and shift x-y values here to account for an atypical video positioning
  ## see https://github.com/manybabies/mb2-analysis/issues/115 for details
  #scale screen
  mutate(
    x_old = x,
    y_old= y,
    x = x_old * 1920 / 2560,
    y = y_old * 1080 / 1440
  ) %>%
  # account for placement of video in the center of the screen (1440x1080)
  #y stays the same
  mutate(
    x = x - (1920-1440)/2
  ) %>%
  #now scale to 1280x960
  mutate(
    x = x * 1280 / 1440,
    y = y * 960 / 1080
  ) %>%
  mutate(media_name=str_replace_all(media_name,"_new",""),
         lab_id = LAB_NAME)%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(PKU_toddlers_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_toddlers_xy_timepoints.csv")))

###Adult data###
PKU_adults <- read.csv(here(DATA_DIR, "raw_data","PKUSu_adults_eyetrackingdata.csv"),sep="\t", fileEncoding="UTF-16LE")

PKU_adults[PKU_adults==""]<-NA
PKU_adults_clean <- PKU_adults %>% 
  rename(participant_id = Participant.name,
         x = Gaze.point.X,
         y = Gaze.point.Y,
         t = Recording.timestamp,
         media_name = Presented.Media.name,
         pupil_left = Pupil.diameter.left,
         pupil_right = Pupil.diameter.right
  )%>% 
  ## scaling procedure
  ## we rescale and shift x-y values here to account for an atypical video positioning
  ## see https://github.com/manybabies/mb2-analysis/issues/115 for details
  #scale screen
  mutate(
    x_old = x,
    y_old= y,
    x = x_old * 1920 / 2560,
    y = y_old * 1080 / 1440
  ) %>%
  # account for placement of video in the center of the screen (1440x1080)
  #y stays the same
  mutate(
    x = x - (1920-1440)/2
  ) %>%
  #now scale to 1280x960
  mutate(
    x = x * 1280 / 1440,
    y = y * 960 / 1080
  ) %>%
  mutate(media_name=str_replace_all(media_name,"_new",""),
         lab_id = LAB_NAME)%>%
  select(lab_id,participant_id,media_name,x,y,t,pupil_left,pupil_right)

write_csv(PKU_adults_clean, here(DATA_DIR,"processed_data", glue("{LAB_NAME}_adults_xy_timepoints.csv")))
