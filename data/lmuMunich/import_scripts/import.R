library(peekds)
library(readxl)
library(tidyverse)
library(here)

source(here("metadata/pod.R"))

lab_dir = "data/lmuMunich"

save_table <- function(table, table_type, validate=F){
  if(validate){
    print(peekds::validate_table(df_table = table, table_type = table_type))
  }
  write_csv(table, here(lab_dir, paste("processed_data/",table_type,".csv", sep="")))
}


p <- read.csv(here(lab_dir, "raw_data/lmuMunich_pilot_demodata.csv"))
e <- bind_rows(read_tsv(here(lab_dir, "raw_data/lmuMunich_pilot_eyetrackingdata.tsv")))

#TODO: global dataset_id
global_dataset_id = 0

### datasets

datasets <- tibble(dataset_id = global_dataset_id,
                   dataset_name = 'lmu_babylab_ds',
                   lab_dataset_id = 'lmu_babylab',
                   shortcite = '',
                   cite = '',
                   dataset_aux_data =''
)

save_table(table = datasets, table_type = "datasets", validate=T) # TODO: "- Column dataset_name should contain characters only."

### administrations

administrations <- tibble(administration_id = 0:(nrow(p)-1),
                          dataset_id = global_dataset_id,
                          subject_id = 0:(nrow(p)-1),
                          age = p$age_days / (365.25/12),
                          lab_age = p$age_days,
                          lab_age_units = 'days',
                          monitor_size_x = 1920,
                          monitor_size_y = 1080,
                          sample_rate = 120,
                          tracker = "tobii",
                          coding_method = 'eyetracking',
                          administration_aux_data = ''
  )

save_table(table = administrations, table_type = "administrations", validate=T) # TODO: weird bugs


### subjects 
#TODO: fix language
#library(ISOcodes)
#ISO_639_2 -> pivot

subjects <- p %>%
  select(sex = participant_gender,
         lab_subject_id = participant_id,
         native_language = lang1) %>%
  mutate(subject_id = 0:(nrow(p)-1),
         subject_aux_data = "")


save_table(table = subjects, table_type = "subjects", validate=T)


### stimuli
#TODO: everything, should this be centralized?

save_table(table = stimuli, table_type = "stimuli", validate=T)

### aois

source(here("metadata/generate_AOIs.R"))

### aoi_region_sets 
# TODO: function?
aoi_region_sets = generate_aoi_regions(screen_width = datasets$monitor_size_x, 
                                   screen_height = datasets$monitor_size_y,
                                   video_width = 1280, #TODO: how do we get this from data?
                                   video_height = 960 
)

save_table(table = aoi_region_sets, table_type = "aoi_region_sets", validate=T)

### aoi_timepoints
# TODO: function?
aoi_timepoints <- generate_aoi_small(here(lab_dir, "processed_data/"))

save_table(table = aoi_timepoints, table_type = "aoi_timepoints", validate=T)

### trialtypes 
#TODO: Everything, should this be centralized?

trialtypes <- e %>%
  select(stimulus_name = `Presented Stimulus name`) %>%
  filter(grepl(paste(c('FAM','KNOW','IG'),collapse="|"), 
               stimulus_name)) %>%
  distinct(stimulus_name)



save_table(table = trialtypes, table_type = "trialtypes", validate=T)


### preparation for trials and xy_timestamps

trial_data <- e %>%
  select(lab_subject_id = `Participant name`, 
         stimulus_name = `Presented Stimulus name`, 
         x = `Gaze point X`,
         y = `Gaze point Y`,
         t_tracker = `Eyetracker timestamp`
         ) %>%
  filter(grepl(paste(c('FAM','KNOW','IG'),collapse="|"), 
               stimulus_name)) %>%
  mutate(xy_timepoint_id = row_number()) %>%
  group_by(lab_subject_id, stimulus_name) %>%
  mutate(trial_id = cur_group_id()-1)
  
  

### trials
# TODO: trial type id, how to handle missing data as exclusion?
trials <- trial_data %>%
  slice_head() %>%
  group_by(lab_subject_id) %>%
  mutate(trialorder = row_number(lab_subject_id)-1) %>%
  ungroup() %>%
  left_join(p, by = c("lab_subject_id" = "participant_id")) %>%
  mutate(excluded = session_error == "error", 
         exclusion_reason = session_error_info, 
         trial_aux_data=''
         ) %>%
  select(trial_id, trialorder, excluded, exclusion_reason, trial_aux_data)


save_table(table = trials, table_type = "trials", validate=T)


### xy_timepoints
# TODO: center timestamps, xy_trim, move coordinates to origin?

# from https://www.tobiipro.com/siteassets/tobii-pro/user-manuals/tobii-pro-studio-user-manual.pdf
# we want ADCSpx coordinates - those are display coordinates
# note tobii gives upper-left indexed coordinates


lab_subject_id_to_administration_id <- inner_join(subjects, administrations) %>% 
  select(administration_id, lab_subject_id)

xy_timepoints <- trial_data %>% 
  ungroup() %>%
  inner_join(lab_subject_id_to_administration_id) %>%
  mutate(t=t_tracker) %>%
  select(-c(t_tracker, lab_subject_id, stimulus_name))

save_table(table = xy_data, table_type = "xy_timepoints", validate=T)