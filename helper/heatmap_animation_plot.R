# needs local ffmpeg installation that is accessible on the command line

library(tidyverse)
library(png)
trials = c('IG_LL',
           'IG_RL',
           'IG_LR',
           'IG_RR',
           'KNOW_LL',
           'KNOW_RL',
           'KNOW_LR',
           'KNOW_RR',
           'FAM_LL',
           'FAM_RL',
           'FAM_LR',
           'FAM_RR',
           )

source(here('helper','ensure_repo_structure.R'))
plot_path <- here("plots")


stimuli_path = here('helper','stimuli')
dir.create(stimuli_path, showWarnings = FALSE)

osfr::osf_retrieve_node('pd35f') %>%
osfr::osf_ls_files() %>%
osfr::osf_download(path = stimuli_path,
                   conflicts = "skip",
                   progress = TRUE,
                   recurse = TRUE)

mp4_files <- list.files(stimuli_path, pattern = "\\.mp4$", recursive = TRUE, full.names = TRUE)
file.rename(from = mp4_files, to = file.path(stimuli_path, basename(mp4_files)))
subdirs <- list.dirs(stimuli_path, recursive = TRUE)
unlink(subdirs[-1], recursive = TRUE)

load(here(INTERMEDIATE_FOLDER, INTERMEDIATE_006))

output_folder <- here(INTERMEDIATE_FOLDER, 'animations')
dir.create(output_folder, showWarnings = FALSE)

animate_trial <- function(trial, cohort){
  frame_split_path <- here('data','02_intermediates', paste0('split_frames_', cohort, '_', trial))
  frame_merge_path <- here('data','02_intermediates', paste0('merge_frames_', cohort, '_', trial))
  dir.create(frame_split_path, showWarnings = FALSE)
  dir.create(frame_merge_path, showWarnings = FALSE)
  #Extract individual frames from videos
  system(paste0('ffmpeg -i ',stimuli_path, '/' , trial,'.mp4 -vf "fps=40" ',frame_split_path,'/',trial,'_%04d.png'))
 
  data_used <- data_preprocessed %>%
    filter(media_name == trial & cohort == age_cohort & data_type != 'web-based') %>%
    select(unique_participant_id, media_name, t_zeroed, x, y) %>% 
    group_by(unique_participant_id, media_name) %>%
    mutate(frame = row_number()) %>% 
    ungroup() %>% 
    group_by(frame) %>%
    group_walk(function(data, grouping){
      
      img_name <- paste0(trial, sprintf('_%04d.png', grouping$frame))
      img_path <- here(frame_split_path, img_name)
      
      if(!file.exists(img_path)){
        return(NA)
      }
      
      img.r <- as.raster(readPNG(img_path),interpolate=F)
      
      tryCatch(
               ggsave(
                 here(frame_merge_path, img_name),
                 plot = 
                   ggplot(data, aes(x,y)) +
                   theme(legend.position = "none") + 
                   annotation_raster(img.r, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
                   stat_density2d(geom = "polygon", aes(fill=..level.., alpha = ..level..), size= 20, bins= 500) + 
                   scale_fill_gradient(low="blue",high="red") +
                   scale_alpha_continuous(range=c(0.01,1.0), guide = FALSE) +
                   scale_x_continuous(limits=c(0,dim(img.r)[2]),expand=c(0,0))+
                   scale_y_continuous(limits=c(0,dim(img.r)[1]),expand=c(0,0))+
                   coord_fixed(),
                 width = 7,
                 height = 5,
                 units = "in"
               ), error = function(e) print(e))
    })
  # 1440/36
  system(paste0('ffmpeg -framerate 40 -i ',frame_merge_path,'/',trial,'_%04d.png -c:v libx264 -vf format=yuv420p ', output_folder, '/', cohort,'_', trial, '.mp4'))

  unlink(frame_split_path, recursive=TRUE)
  unlink(frame_merge_path, recursive=TRUE)
}

walk(trials, function(trial){animate_trial(trial, 'toddlers')})
walk(trials, function(trial){animate_trial(trial, 'adults')})

