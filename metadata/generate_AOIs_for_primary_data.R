# ------------------------------------------------------------------------------
# NOTE THIS FUNCTION IS FOR PRIMARY DATASET
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
# Generate AOIs:

## Set screen and video dimensions. Note: this only works if the zero point of the 
## eyetracker's coordinate systems is top left, and if the video was displayed centered,
## i.e., the video's and the screen's center were aligned. 

## TODO: Trial_types:
## create these from the trial order csvs and pod.R
## delete these general notes
# trial_type_id = rownumber()
# full_phrase = ''
# full_phrase_language = ''
# point_of_disambiguation = pod.R
# target_side = c('left', 'right'...)
# lab_trial_id = c('FAM_LL, FAM_LR'.... 'IG')
# condition = c('FAM', ....'KNOW'....)
# vanilla_trial = '' #
# trial_type_aux_data = '' #
# aoi_region_set_id = 0
# dataset_id = 0
# distractor_id = 0
# target_id = 0

# ------------------------------------------------------------------------------
# do bounding box ratios
ratios_of_bounding_box <- function() {
  
  # compute ratios using these video dimensions
  video_width = 1200
  video_height = 900
  
  ## coordinates for left tunnel exit
  L_left_X = 304; L_left_Y = 618;   # leftmost part
  L_right_X = 370; L_right_Y = 650;   # rightmost part
  L_top_X = 328; L_top_Y = 586;   # topmost part
  L_bottom_X = 339; L_bottom_Y = 678;   # bottommost part
  
  # horizontal tunnel diameter in 2D (turns out to be 66 pixels) * 0.25
  D = 1.25 * (L_right_X - L_left_X);
  
  ## compute ratios with respect to video dimensions (computed from the left tunnel)
  L_left_ratio = (L_left_X - D) / video_width;
  L_right_ratio = (L_right_X + D) / video_width;
  
  # since the stimuli are symmetric, we can use the ratios
  # to get the bounding box for the right tunnel as well
  # i.e. R_right_ratio is 1 - L_left_ratio
  R_right_ratio = 1 - L_left_ratio;
  R_left_ratio = 1 - L_right_ratio;
  
  # ratios of screen height are invariant to left/right tunnel
  top_ratio = (L_top_Y - D) / video_height;
  bottom_ratio = (L_bottom_Y + D) / video_height;
  
  return(list(L_left = L_left_ratio,
              L_right = L_right_ratio,
              R_right = R_right_ratio,
              R_left = R_left_ratio,
              top = top_ratio,
              bottom = bottom_ratio))
}

ratios <-ratios_of_bounding_box()
video_width <- 1280
video_height <- 960

aoi_region_sets = tibble(
  aoi_region_set_id = 0, 
  l_x_max = (video_width*ratios$L_right),
  l_x_min = (video_width*ratios$L_left),
  l_y_max = (video_height*ratios$bottom),
  l_y_min = (video_height*ratios$top),
  r_x_max = (video_width*ratios$R_right),
  r_x_min = (video_width*ratios$R_left),
  r_y_max = (video_height*ratios$bottom),
  r_y_min =  (video_height*ratios$top),
  w_x_max = video_width/2 + floor((video_width*.94)/16),
  w_x_min = video_width/2 - floor((video_width*.94)/16),
  w_y_max = video_height/2,
  w_y_min = video_height/2 - (video_height*2.5)/16,
  lb_x_max = (video_width*.23),
  lb_x_min = (video_width*.03),
  lb_y_max = (video_height*0.92),
  lb_y_min = (video_height*0.62),
  rb_x_max = video_width - (video_width*.23),
  rb_x_min = video_width - (video_width*.03),
  rb_y_max = (video_height*0.92),
  rb_y_min =  (video_height*0.62),
  lbig_x_max = (video_width*7)/16,
  lbig_x_min = 0,
  lbig_y_max = video_height, 
  lbig_y_min = ((video_height*9)/16),
  rbig_x_max = video_width,
  rbig_x_min = ((video_width*9)/16),
  rbig_y_max = video_height,
  rbig_y_min = ((video_height*9)/16)
)


function(xy_timepoints, trials, screen_width, screen_height){
  xy_timepoints %>% 
  left_join(trials) %>%
  left_join(trial_types) %>%
  mutate(x = x+(screen_width-video_width)/2,
         y = y+(screen_height-video_height)/2,
         aoi_timepoint_id = rownumber(),
         side = dplyr::case_when(x >  l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "left",
                            x > r_x_min & x < r_x_max & y > r_y_min & y < r_y_max ~ "right",
                            x >  lb_x_min & x < lb_x_max & y > lb_y_min & y < lb_y_max ~ "left",
                            x > rb_x_min & x < rb_x_max & y > rb_y_min & y < rb_y_max ~ "right",
                            x > w_x_min & x < w_x_max & y > w_y_min & y < w_y_max ~ "window",
                            x > lbig_x_min & x < lbig_x_max & y > lbig_y_min & y < lbig_y_max ~ "left",
                            x > rbig_x_min & x < rbig_x_max & y > rbig_y_min & y < rbig_y_max ~ "right",
                            !is.na(x) & !is.na(y) ~ "other",
                            TRUE ~  as.character(NA)),
         aoitype = dplyr::case_when(x > l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "exit",
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
        aoi = paste(aoi, aoitype, sep="_")) %>%
    select(aoi_timepoint_id,
           trial_id,
           aoi,
           t_norm,
           administration_id)
}


# ------------------------------------------------------------------------------
# xy_trim
# converts off-screen coordinates to NA

xy_trim <- function(xy, x_max, y_max) {
  xy |>
    mutate(x = ifelse(x >= 0 & x <= x_max, x, NA),
           y = ifelse(y >= 0 & y <= y_max, y, NA))
  
}


# 
# 
# add_aois_small <- function (xy_joined) 
# {
#   # first we assign aois to a side, then to a type. if it is target or distractor but not
#   # specifically in one of the other types, case_when below assigns it to general
#   # so left_exit + left_box + left_general is a big AOI

#   return(xy_joined)
# }
# 
# 
# generate_aoi_small <- function (dir) 
# {
#   SAMPLE_RATE = 40
#   SAMPLE_DURATION = 1000/SAMPLE_RATE
#   MAX_GAP_LENGTH = 0.1
#   MAX_GAP_SAMPLES = MAX_GAP_LENGTH/(1/SAMPLE_RATE)
#   xy <- readr::read_csv(file.path(dir, "xy_data.csv"))
#   trials <- readr::read_csv(file.path(dir, "trials.csv"))
#   aoi_regions <- readr::read_csv(file.path(dir, "aoi_regions.csv"))
#   xy_joined <- xy %>% dplyr::left_join(trials) %>% dplyr::left_join(aoi_regions)
#   
#   xy_joined <- add_aois_small(xy_joined)
#   
#   aoi = resample_times(xy_joined) %>% dplyr::select(dataset_id, 
#                                                     subject_id, trial_id, t_zeroed, aoi) %>% 
#     dplyr::rename(t = t_zeroed) %>% 
#     dplyr::group_by(dataset_id, subject_id, trial_id, t) %>% 
#     dplyr::summarise(aoi = na_mode(aoi)) %>% dplyr::ungroup() %>% 
#     group_by(dataset_id, subject_id, trial_id) %>% dplyr::mutate(aoi = zoo::na.locf(aoi, 
#                                                                                     maxgap = MAX_GAP_SAMPLES, na.rm = FALSE)) %>%
#     ungroup() %>% 
#     dplyr::mutate(aoi_data_id = 0:(n() - 1))
# }
# 
# resample_times <- function(df) {
#   # set sample rates
#   SAMPLE_RATE = 40 # Hz
#   SAMPLE_DURATION = 1000/SAMPLE_RATE
#   MAX_GAP_LENGTH = .100 # S
#   MAX_GAP_SAMPLES = MAX_GAP_LENGTH / (1/SAMPLE_RATE)
#   
#   # center timestamp (0 POD)
#   df <- df %>%
#     dplyr::group_by(.data$subject_id, .data$trial_id, .data$dataset_id) %>%
#     dplyr::mutate(t_trial = .data$t - .data$t[1],
#                   t_zeroed = .data$t_trial - .data$point_of_disambiguation)
#   
#   df %>% dplyr::group_by(.data$subject_id, .data$trial_id) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(
#       data = .data$data %>%
#         purrr::map(function(df) {
#           df_rounded <- df %>%
#             dplyr::mutate(t_zeroed = round(SAMPLE_DURATION * round(t_zeroed/SAMPLE_DURATION)))
#           
#           t_resampled <- tibble::tibble(t_zeroed = round(seq(min(df_rounded$t_zeroed),
#                                                              max(df_rounded$t_zeroed),
#                                                              SAMPLE_DURATION)))
#           
#           dplyr::left_join(t_resampled, df_rounded) %>%
#             dplyr::group_by(t_zeroed)
#         })) %>%
#     tidyr::unnest(.data$data) 
# }
# 

# 
# 
# 
