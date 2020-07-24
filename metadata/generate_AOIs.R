library(peekds)
# Generate AOIs:

## Set screen and video dimensions. Note: this only works if the zero point of the 
## eyetracker's coordinate systems is top left, and if the video was displayed centered,
## i.e., the video's and the screen's center were aligned. 

# For the pilot, the dimensions are:
## 1280 x 1024 screen and 1280 x 960 video
## 1920 x 1080 screen and 1200 x 900 video
## 1920 x 1200 screen and 1200 x 900 video

# generate appropriate specification:
# aoi_regions
# aoi_region_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, w_x_max, w_x_min, w_y_max, w_y_min
generate_aoi_regions <- function(screen_width = 1280, 
                                 screen_height = 1024,
                                 video_width = 1280,
                                 video_height = 960) {
      aoi_regions = tibble(
      aoi_region_id = 0, 
      l_x_max = (video_width*.37) + (screen_width-video_width)/2,
      l_x_min = (video_width*.24) + (screen_width-video_width)/2,
      l_y_max = (video_height*0.78) + ((screen_height-video_height)/2),
      l_y_min = (video_height*0.55) + ((screen_height-video_height)/2),
      r_x_max = video_width - (video_width*.24) + (screen_width-video_width)/2,
      r_x_min = video_width - (video_width*.37)  + (screen_width-video_width)/2,
      r_y_max = (video_height*0.78) + ((screen_height-video_height)/2),
      r_y_min =  (video_height*0.55) + ((screen_height-video_height)/2),
      w_x_max = video_width/2 + floor((video_width*.94)/16) + (screen_width-video_width)/2,
      w_x_min = video_width/2 - floor((video_width*.94)/16) + (screen_width-video_width)/2,
      w_y_max = video_height/2 + ((screen_height-video_height)/2),
      w_y_min = video_height/2 - (video_height*2.5)/16 + ((screen_height-video_height)/2),
      lb_x_max = (video_width*.23) + (screen_width-video_width)/2,
      lb_x_min = (video_width*.03) + (screen_width-video_width)/2,
      lb_y_max = (video_height*0.92) + ((screen_height-video_height)/2),
      lb_y_min = (video_height*0.62) + ((screen_height-video_height)/2),
      rb_x_max = video_width - (video_width*.23) + (screen_width-video_width)/2,
      rb_x_min = video_width - (video_width*.03)  + (screen_width-video_width)/2,
      rb_y_max = (video_height*0.92) + ((screen_height-video_height)/2),
      rb_y_min =  (video_height*0.62) + ((screen_height-video_height)/2),
      lbig_x_max = (video_width*7)/16 + (screen_width-video_width)/2,
      lbig_x_min = (screen_width-video_width)/2,
      lbig_y_max = video_height + ((screen_height-video_height)/2),
      lbig_y_min = ((video_height*9)/16) + ((screen_height-video_height)/2),
      rbig_x_max = video_width + (screen_width-video_width)/2,
      rbig_x_min = ((video_width*9)/16) + (screen_width-video_width)/2,
      rbig_y_max = video_height + ((screen_height-video_height)/2),
      rbig_y_min = ((video_height*9)/16) + ((screen_height-video_height)/2)
      )
}


na_mode <- function(x) {
  if (all(is.na(x))) {
    return(as.character(NA))
  } else {
    x_nona <- x[!is.na(x)]
    
    # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
    ux <- unique(x_nona)
    x_mode <- ux[which.max(tabulate(match(x_nona, ux)))]
    
    return(x_mode)
  }
}

add_aois_small <- function (xy_joined) 
{
  # first we assign aois to a side, then to a type. if it is target or distractor but not
  # specifically in one of the other types, case_when below assigns it to general
  # so left_exit + left_box + left_general is a big AOI
  xy_joined = xy_joined %>% dplyr::mutate(
    side = dplyr::case_when(x >  l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "left",
                            x > r_x_min & x < r_x_max & y > r_y_min & y < r_y_max ~ "right",
                            x >  lb_x_min & x < lb_x_max & y > lb_y_min & y < lb_y_max ~ "left",
                            x > rb_x_min & x < rb_x_max & y > rb_y_min & y < rb_y_max ~ "right",
                            x > w_x_min & x < w_x_max & y > w_y_min & y < w_y_max ~ "window",   
                            x > lbig_x_min & x < lbig_x_max & y > lbig_y_min & y < lbig_y_max ~ "left",
                            x > rbig_x_min & x < rbig_x_max & y > rbig_y_min & y < rbig_y_max ~ "right",                            
                            !is.na(x) & !is.na(y) ~ "other", 
                            TRUE ~  as.character(NA)),
    aoitype = dplyr::case_when(x >  l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "exit",
                            x > r_x_min & x < r_x_max & y > r_y_min & y < r_y_max ~ "exit",
                            x >  lb_x_min & x < lb_x_max & y > lb_y_min & y < lb_y_max ~ "box",
                            x > rb_x_min & x < rb_x_max & y > rb_y_min & y < rb_y_max ~ "box",
                            x > w_x_min & x < w_x_max & y > w_y_min & y < w_y_max ~ "window", 
                            x > lbig_x_min & x < lbig_x_max & y > lbig_y_min & y < lbig_y_max ~ "general",
                            x > rbig_x_min & x < rbig_x_max & y > rbig_y_min & y < rbig_y_max ~ "general",                            
                            !is.na(x) & !is.na(y) ~ "other", 
                            TRUE ~  as.character(NA)),    
    aoi = dplyr::case_when(side %in%  c("left", "right") & side == target_side ~ "target", 
                           side %in% c("left", "right") & side != target_side ~ "distractor",
                           TRUE ~ side),
    aoi = paste(aoi, aoitype, sep="_"))
  return(xy_joined)
}


generate_aoi_small <- function (dir) 
{
  SAMPLE_RATE = 40
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  MAX_GAP_LENGTH = 0.1
  MAX_GAP_SAMPLES = MAX_GAP_LENGTH/(1/SAMPLE_RATE)
  xy <- readr::read_csv(file.path(dir, "xy_data.csv"))
  trials <- readr::read_csv(file.path(dir, "trials.csv"))
  aoi_regions <- readr::read_csv(file.path(dir, "aoi_regions.csv"))
  xy_joined <- xy %>% dplyr::left_join(trials) %>% dplyr::left_join(aoi_regions)

  xy_joined <- add_aois_small(xy_joined)
  
  aoi = resample_times(xy_joined) %>% dplyr::select(dataset_id, 
                                                    subject_id, trial_id, t_zeroed, aoi) %>% 
    dplyr::rename(t = t_zeroed) %>% 
    dplyr::group_by(dataset_id, subject_id, trial_id, t) %>% 
    dplyr::summarise(aoi = na_mode(aoi)) %>% dplyr::ungroup() %>% 
    group_by(dataset_id, subject_id, trial_id) %>% dplyr::mutate(aoi = zoo::na.locf(aoi, 
                                                                                    maxgap = MAX_GAP_SAMPLES, na.rm = FALSE)) %>%
    ungroup() %>% 
    dplyr::mutate(aoi_data_id = 0:(n() - 1))
}

