library(here)
library(gifski)
library(grid)
library(gganimate)

## command for generating stimulus_frameS
## ffmpeg -i FAM_LL_outcome_1280x960.mp4 -stimulus_frames_pilot1b/FAM_LL_1b_outcome_1280x960_%04d.png

source(here::here("helper/common.R"))
source(here("helper/preprocessing_helper.R"))
source(here("metadata/pod.R"))


resample_times <- function(df) {
  # set sample rates
  SAMPLE_RATE = 40 # Hz
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  MAX_GAP_LENGTH = .100 # S
  MAX_GAP_SAMPLES = MAX_GAP_LENGTH / (1/SAMPLE_RATE)
  
  # center timestamp (0 POD)
  df <- df %>%
    dplyr::group_by(.data$subject_id, .data$trial_id, .data$dataset_id) %>%
    dplyr::mutate(t_trial = .data$t - .data$t[1],
                  t_zeroed = .data$t_trial - .data$point_of_disambiguation)
  
  df %>% dplyr::group_by(.data$subject_id, .data$trial_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = .data$data %>%
        purrr::map(function(df) {
          df_rounded <- df %>%
            dplyr::mutate(t_zeroed = round(SAMPLE_DURATION * round(t_zeroed/SAMPLE_DURATION)))
          
          t_resampled <- tibble::tibble(t_zeroed = round(seq(min(df_rounded$t_zeroed),
                                                             max(df_rounded$t_zeroed),
                                                             SAMPLE_DURATION)))
          
          dplyr::left_join(t_resampled, df_rounded) %>%
            dplyr::group_by(t_zeroed)
        })) %>%
    tidyr::unnest(.data$data)
}

labs <- dir("pilot_data")

xy_orig <- labs %>%
  map_df(function(lab) {
    xy_data <- read_csv(here(paste0("pilot_data/",
                                    lab,"/processed_data/xy_data.csv"))) 
    subjects <- read_csv(here(paste0("pilot_data/",
                                     lab,"/processed_data/subjects.csv"))) 
    trials <- read_csv(here(paste0("pilot_data/",
                                   lab,"/processed_data/trials.csv"))) 
    datasets <- read_csv(here(paste0("pilot_data/",
                                     lab,"/processed_data/datasets.csv")))
    aoiregions <- read_csv(here(paste0("pilot_data/",
                                       lab,"/processed_data/aoi_regions.csv")))
    
    left_join(xy_data, subjects) %>%
      left_join(trials) %>%
      left_join(datasets) %>%
      left_join(aoiregions) %>%
      select(lab_subject_id, lab_dataset_id, lab_trial_id, trial_id, dataset_id, subject_id,
             age, t, x, y, trial_num, error, monitor_size_x, monitor_size_y,
             l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, r_y_min,
             point_of_disambiguation, experiment_num) %>%
      rename(subid = lab_subject_id, 
             lab = lab_dataset_id, 
             stimulus = lab_trial_id) 
    })


# read in all frames
# we enforce 1280x960 and use only that
pngs1a <- data.frame(filename = c(list.files("stimulus_frames/", full.names=T)),
                     files=c(list.files("stimulus_frames/"))) %>%
  separate(files, into=c("FAM", "cond", "res", "fnum"), sep="_") %>%
  mutate(frame = as.numeric(substr(fnum, 1, 4)),
         filename = as.character(filename),
         experiment_num =  "pilot_1a") %>%
  select(-res)

pngs1b <- data.frame(filename = c(list.files("stimulus_frames_pilot1b/", full.names=T)),
                   files=c(list.files("stimulus_frames_pilot1b/"))) %>%
  mutate(files = gsub("no_outcome", "nooutcome", files)) %>%
  separate(files, into=c("FAM", "cond", "pilot", "outcome", "res", "fnum"), sep="_") %>%
  mutate(frame = as.numeric(substr(fnum, 1, 4)),
         filename = as.character(filename),
         experiment_num = ifelse(grepl("no_outcome", filename),
                                 "pilot_1b_no_outcome",
                                 "pilot_1b_outcome")) %>%
  select(-res)

pngs <- bind_rows(pngs1a, pngs1b)

# transform for plotting, assuming the origin is upper left
# and video is centered within monitor
xy <- mutate(xy_orig,
             video_size_x = ifelse(grepl("1200", stimulus), 1200, 1280),
             video_size_y = ifelse(grepl("900", stimulus), 900, 960),
             x_plot = x - (monitor_size_x - video_size_x)/2,
             y_plot = (monitor_size_y - y) - (monitor_size_y - video_size_y)/2,
             res = paste0(video_size_x, "x", video_size_y),
             cond = substr(stimulus, 5, 6),
             frame = 1 + floor(30 * (t + pod_pilot_1b)/1000)) %>%
  left_join(filter(pngs, cond == "LL") %>% select(-cond)) %>%
  filter(is.na(filename) == F)

# exclude subject marked with any error and/or less than 8 trials
xy$experiment = ifelse(grepl("1a", xy$experiment_num), "1a", "1b")

xy <- xy %>% 
  group_by(lab, subid, experiment) %>%
  mutate(error_subj = any(error)) %>%
  exclude_by(quo(error_subj), quiet=FALSE) 

# exclude trials under 32s (which are not complete trials)
# changed from 35s to 32 after pilot 1b because no_outcome
# trials are shorter
xy <- ungroup(xy) %>% 
  group_by(lab, trial_id, subid, experiment) %>%
  mutate(time_range = (max(t) - min(t))/1000) %>%
  exclude_by(quo(time_range <= 32), quiet=FALSE)


# exclude subjects who did not complete 7/8 trials
xy <- ungroup(xy) %>% 
  group_by(lab, subid, experiment) %>%
  mutate(trials_completed = length(unique(trial_id))) %>%
  exclude_by(quo(trials_completed < 7),quiet=FALSE)


# transform all vidoes to 1280x960
transform_all = TRUE
if (transform_all == TRUE) {
  xy$x_plot = (xy$x_plot/(xy$video_size_x)) * 1280
  xy$y_plot = (xy$y_plot/(xy$video_size_y)) * 960
  xy$video_size_x = 1280
  xy$video_size_y = 960
}

# make heat map
xy_resampled = resample_times(xy) %>%
  dplyr::select(-t) %>%
  dplyr::rename(t = t_zeroed)

make_heat_map <- function(xy_resampled, i, i_end) {
  df = filter(xy_resampled, t > i, t < i_end,
              x_plot >= 0, x_plot <= 1500, y_plot >=0, y_plot <= 1500,
              is.na(x_plot) == F,
              is.na(y_plot) == F)  %>%
    mutate(x_plot = if_else(cond %in% c("LR", "RR"), video_size_x - x_plot, x_plot)) %>%
    group_by(dataset_id,
             subject_id,
             trial_id,
             t,
             cond,
             video_size_x,
             video_size_y,
             filename,
             experiment_num) %>%
    summarise(x_plot = median(x_plot),
              y_plot = median(y_plot))
  
  for (e in unique(df$experiment_num))  {
    print(
    ggplot(data=filter(df, experiment_num == e), aes(x=x_plot, 
                        y=y_plot
    )) +
      annotation_custom(rasterGrob(png::readPNG(as.character(first(df$filename)))), 
                        0,
                        first(df$video_size_x), 
                        0,
                        first(df$video_size_y)) +
      stat_density2d(aes(fill=..level..), geom="polygon") +
      scale_fill_gradient(low="blue", high="red") +
      coord_fixed()  +
      xlim(0, first(df$video_size_x)) +
      ylim(0, first(df$video_size_y)) +
      xlab("") + ylab("") +
      ggtitle(paste0("heat map during anticipation window: ", i, "ms to ", i_end)) +
      theme_bw(18) + 
      theme(legend.position = "none") + 
      geom_text(data=tibble(name=c("target", "distractor"), xpos=c(100, 1100), ypos=c(50, 50)),
                aes(x=xpos, y=ypos, label=name), colour="white", size=12) + 
      facet_grid(. ~ experiment_num)
    
  )
    ggsave(paste0(i, "_", i_end, "_", e, "_heatmap_target_left_all.pdf"), width=10, height=10)
    
  }
}
# make heat map
make_heat_map(xy_resampled, -2000, -100)



# plot a single frame of the data
print_single_frame <- function(df) {
  print(
    ggplot(data=df, aes(x=x_plot, 
                        y=y_plot, 
                        group=paste(lab, subid, trial_id), 
                        colour=lab)) +
      annotation_custom(rasterGrob(png::readPNG(as.character(first(df$filename)))), 
                        0,
                        first(df$video_size_x), 
                        0,
                        first(df$video_size_y)) +
      geom_point()  + 
      coord_fixed() + 
      xlim(0, first(df$video_size_x)) +
      ylim(0, first(df$video_size_y)) +
      ggtitle(paste("time:", round(median(df$t/1000), 2), "frame:", first(df$frame)))
  )
}

# This syntax is taken mostly from gifski documentation 
# https://cran.r-project.org/web/packages/gifski/gifski.pdf
# I tried a fancy thing with using tidyr and nest to do this
# but it was slower and leaked memory.
# Each full video takes about 20 minutes to generate.
makeplot <- function(df){
  datalist <- split(df, df$frame)
  lapply(datalist, print_single_frame)
}


# make all the plots and stitch together into a gif
save_gif(makeplot(filter(xy, 
                        is.na(filename) == F,
                        experiment_num == "pilot_1b_outcome",
                        cond == "LL")),
             width = 600,
             height = 450,
             delay=1/30,
             gif_file = paste0("pilot1b_outcome.gif"))


# make all the plots and stitch together into a gif
save_gif(makeplot(filter(xy, 
                         is.na(filename) == F,
                         experiment_num == "pilot_1b_no_outcome",
                         cond == "LL")),
         width = 600,
         height = 450,
         delay=1/30,
         gif_file = paste0("pilot1b_no_outcome.gif"))


head(x)
