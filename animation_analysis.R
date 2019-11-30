library(here)
library(gifski)
library(grid)
library(gganimate)

source(here::here("helper/common.R"))
source(here("helper/preprocessing_helper.R"))
source(here("metadata/pod.R"))

labs <- dir("pilot_data")

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
pngs <- data.frame(filename = list.files("stimulus_frames/", full.names=T),
                   files=list.files("stimulus_frames/")) %>%
  separate(files, into=c("FAM", "cond", "res", "fnum"), sep="_") %>%
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
             frame = 1 + floor(30 * (t + pod)/1000)) %>%
  left_join(pngs)

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
  df = filter(xy_resampled, t > i, t < i_end, x_plot >= 0, x_plot <= 1500, y_plot >=0, y_plot <= 1500,
            is.na(x_plot) == F, is.na(y_plot) == F, cond=="LL" | cond =="RL") %>%
  group_by(dataset_id, subject_id, trial_id, t, cond, video_size_x, video_size_y, filename) %>%
  summarise(x_plot = median(x_plot),
            y_plot = median(y_plot))
  
  print(
    ggplot(data=df, aes(x=x_plot, 
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
                aes(x=xpos, y=ypos, label=name), colour="white", size=12)
    
  )
  ggsave(paste0(i, "_", i_end, "_heatmap_target_left.pdf"), width=10, height=10)
}
# make heat map
for (i in c(-4000, -3000, -2000, -1000)) {
  make_heat_map(xy_resampled, i, i + 1000)
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
             delay = .10,
             gif_file = paste0("full_", cc, "_", rr, ".gif"))
  }
}
