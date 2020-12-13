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
  
## the approach to genrate the AOIs is as follows:
  #1. take the tunnel exit diameter of D units. 

  #2. create a rectangular bounding box that extends D units from 
  # the uppermost, leftmost, rightmost, and bottommost points of the tunnel exit
  
  # we do this calculation for the 1200 x 900 video below, 
  # and express the result in terms of ratios of video heights and widths. 
  # since the x/y ratios are the same for the two video sizes, i.e. (1280 x 960) / (1200 x 900) = 1
  # the AOIs will scale correctly to the 1280 x 960 case. This is done in the function below
      ratios = ratios_of_bounding_box(video_width, video_height)
  
      aoi_regions = tibble(
      aoi_region_id = 0, 
      l_x_max = (video_width*ratios$L_right) + (screen_width-video_width)/2,
      l_x_min = (video_width*ratios$L_left) + (screen_width-video_width)/2,
      l_y_max = (video_height*ratios$bottom) + ((screen_height-video_height)/2),
      l_y_min = (video_height*ratios$top) + ((screen_height-video_height)/2),
      r_x_max = (video_width*ratios$R_right)  + (screen_width-video_width)/2,
      r_x_min = (video_width*ratios$R_left)  + (screen_width-video_width)/2,
      r_y_max = (video_height*ratios$bottom) + ((screen_height-video_height)/2),
      r_y_min =  (video_height*ratios$top) + ((screen_height-video_height)/2),
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

ratios_of_bounding_box <- function(video_width, video_height) {
  

  ## coordinates for left tunnel exit 
  L_left_X = 304; L_left_Y = 618;   # leftmost part
  L_right_X = 370; L_right_Y = 650;   # rightmost part
  L_top_X = 328; L_top_Y = 586;   # topmost part
  L_bottom_X = 339; L_bottom_Y = 678;   # bottommost part

  # horizontal tunnel diameter in 2D (turns out to be 66 pixels)
  D = L_right_X - L_left_X;
  
  ## compute ratios with respect to video dimensions (computed from the left tunnel)
  L_left_ratio = (L_left_X - D) / video_width; # 0.1983
  L_right_ratio = (L_right_X + D) / video_width; # 0.3633
  
  # since the stimuli are symmetric, we can use the ratios
  # to get the bounding box for the right tunnel as well 
  # i.e. R_right_ratio is 1 - L_left_ratio
  R_right_ratio = 1 - L_left_ratio # 0.8017
  R_left_ratio = 1 - L_right_ratio # 0.6367
  
  # ratios of screen height are invariant to left/right tunnel
  top_ratio = (L_top_Y - D) / video_height; # 0.5778
  bottom_ratio = (L_bottom_Y + D) / video_height; # 0.8267
  
  return(list(L_left = L_left_ratio,
              L_right = L_right_ratio, 
              R_right = R_right_ratio, 
              R_left = R_left_ratio,
              top = top_ratio,
              bottom = bottom_ratio))
}
  
  

