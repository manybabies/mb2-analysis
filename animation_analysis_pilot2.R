library(here)
library(gifski)
library(grid)
library(gganimate)

## command for generating stimulus_frameS
## ffmpeg -i FAM_LL_outcome_1280x960.mp4 -r 25 stimulus_frames_pilot1b/FAM_LL_1b_outcome_1280x960_%04d.png

source(here::here("helper/common.R"))
source(here("helper/preprocessing_helper.R"))
source(here("metadata/pod.R"))

labs <- dir("pilot_data")
labs <- labs[grepl("1b", labs)] 

xy <- labs %>%
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
             point_of_disambiguation) %>%
      rename(subid = lab_subject_id, 
             lab = lab_dataset_id, 
             stimulus = lab_trial_id)
  })

# read in all frames
pngs <- data.frame(filename = list.files("stimulus_frames_pilot1b/", full.names=T),
                   files=list.files("stimulus_frames_pilot1b/")) %>%
  mutate(files = gsub("no_outcome", "nooutcome", files)) %>%
  separate(files, into=c("FAM", "cond", "pilot", "outcome", "res", "fnum"), sep="_") %>%
  mutate(frame = as.numeric(substr(fnum, 1, 4)),
         filename = as.character(filename))

# transform for plotting, assuming the origin is upper left
# and video is centered within monitor
xy <- mutate(xy,
             video_size_x = ifelse(grepl("1200", stimulus), 1200, 1280),
             video_size_y = ifelse(grepl("900", stimulus), 900, 960),
             x_plot = x - (monitor_size_x - video_size_x)/2,
             y_plot = (monitor_size_y - y) - (monitor_size_y - video_size_y)/2,
             res = paste0(video_size_x, "x", video_size_y),
             cond = substr(stimulus, 5, 6),
             frame = 1 + floor(30 * (t + pod_pilot2)/1000)) %>%
  left_join(pngs) %>%
  filter(is.na(filename) == F)

# transform all vidoes to 1280x960
transform_all = TRUE
if (transform_all == TRUE) {
  xy$x_plot = (xy$x_plot/(xy$video_size_x)) * 1280
  xy$y_plot = (xy$y_plot/(xy$video_size_y)) * 960
  xy$video_size_x = 1280
  xy$video_size_y = 960
}


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
for (cc in unique(xy$cond)) {
  for (rr in unique(xy$res)) {
    save_gif(makeplot(filter(xy, cond == cc,
                             res == rr,
                             is.na(filename) == F)),
             width = 600,
             height = 450,
             delay=1/30,
             gif_file = paste0("1full_", cc, "_", rr, ".gif"))
  }
}
