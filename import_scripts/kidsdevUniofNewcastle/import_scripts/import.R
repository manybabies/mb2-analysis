# kidsdevUniofNewcastle
# Gal Raz import script
# following data import guide:
# https://docs.google.com/document/d/1MEEQicPc1baABDHFasbWoujvj2GwfBGarwrzyS2JQtM/edit

library(tidyverse)
library(here)
library(edfR)
# ------------------------------------------------------------------------------
# preliminaries 
lab_dir <- "import_scripts/kidsdevUniofNewcastle/"

subjs = dir(here(lab_dir, "raw_data/Adult EDF Files/"))

d.pretrim <- subjs %>%
  map_df(function(subj) {
    xy = edf.samples(paste0(here(lab_dir, "raw_data/Adult EDF Files/"), subj, "/", subj, ".edf"), trials=T) %>%
      mutate(lab_subject_id = subj)
    msg = edf.messages(paste0(here(lab_dir, "raw_data/Adult EDF Files/"), subj, "/", subj, ".edf"))
    
    
    
    framestart =  filter(msg, grepl("Frame to be displayed 1$", msg)) %>%
      mutate(eyetrial = 1:n(),
             msg = str_replace_all(msg, "\\s", "|")) %>%
      separate(msg, into=c("offset", "a", "b", "c", "d", "frame"), sep="\\|") %>%
      mutate(first_frame_time = time + as.numeric(as.character(offset))) %>%
      select(first_frame_time, eyetrial)
    
    dd = filter(msg, grepl("videofile", msg)) %>%
      separate(msg, into=c("V", "VAR", "videofile", "video_name"), sep=" ") %>%
      mutate(eyetrial=1:n()) %>%
      select(eyetrial, video_name) %>%
      left_join(framestart)
    
    left_join(xy, dd)
  }
  )
