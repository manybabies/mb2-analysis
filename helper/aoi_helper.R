ratios_of_bounding_box <- function() {
  # compute ratios using these video dimensions
  video_width <- 1200
  video_height <- 900

  ## coordinates for left tunnel exit
  L_left_X <- 304 # leftmost part
  L_right_X <- 370 # rightmost part
  L_top_Y <- 304 # topmost part
  L_bottom_Y <- 222 # bottommost part

  # horizontal tunnel diameter in 2D (turns out to be 66 pixels) * 1.25
  D <- 1.25 * (L_right_X - L_left_X)

  ## compute ratios with respect to video dimensions (computed from the left tunnel)
  L_left_ratio <- (L_left_X - D) / video_width
  L_right_ratio <- (L_right_X + D) / video_width

  # since the stimuli are symmetric, we can use the ratios
  # to get the bounding box for the right tunnel as well
  # i.e. R_right_ratio is 1 - L_left_ratio
  R_right_ratio <- 1 - L_left_ratio
  R_left_ratio <- 1 - L_right_ratio

  # ratios of screen height are invariant to left/right tunnel
  top_ratio <- (L_top_Y + D) / video_height
  bottom_ratio <- (L_bottom_Y - D) / video_height

  return(list(
    L_left = L_left_ratio,
    L_right = L_right_ratio,
    R_right = R_right_ratio,
    R_left = R_left_ratio,
    top = top_ratio,
    bottom = bottom_ratio
  ))
}

ratios <- ratios_of_bounding_box()
video_width <- 1280
video_height <- 960

aoi_region_sets <- tibble(
  l_x_max = (video_width * ratios$L_right),
  l_x_min = (video_width * ratios$L_left),
  l_y_max = (video_height * ratios$top),
  l_y_min = (video_height * ratios$bottom),
  r_x_max = (video_width * ratios$R_right),
  r_x_min = (video_width * ratios$R_left),
  r_y_max = (video_height * ratios$top),
  r_y_min = (video_height * ratios$bottom),
  w_x_max = video_width / 2 + floor((video_width * .94) / 16),
  w_x_min = video_width / 2 - floor((video_width * .94) / 16),
  w_y_max = video_height / 2 + (video_height * 2.5) / 16,
  w_y_min = video_height / 2,
  lb_x_max = (video_width * .23),
  lb_x_min = (video_width * .03),
  lb_y_max = (video_height * 0.38),
  lb_y_min = (video_height * 0.08),
  rb_x_max = (video_width * .97),
  rb_x_min = (video_width * .77),
  rb_y_max = (video_height * 0.38),
  rb_y_min = (video_height * 0.08),
  lbig_x_max = (video_width * 7) / 16,
  lbig_x_min = 0,
  lbig_y_max = ((video_height * 7) / 16),
  lbig_y_min = 0,
  rbig_x_max = video_width,
  rbig_x_min = ((video_width * 9) / 16),
  rbig_y_max = ((video_height * 7) / 16),
  rbig_y_min = 0,
  bearwatch_x_max = video_width / 2 + floor((video_width * 2.2) / 16),
  bearwatch_x_min = video_width / 2 - floor((video_width * 2.2) / 16),
  bearwatch_y_max = video_height / 2 + (video_height * 7.7) / 16,
  bearwatch_y_min = video_height / 2 + (video_height * 2.5) / 16,
  mousepath_x_max = (video_width * .98),
  mousepath_x_min = (video_width * .02),
  mousepath_y_max = (video_height * 0.42),
  mousepath_y_min = (video_height * 0.03)
)

## Code Code used to test new aois if we need them for exploratory analyses
# library(imager)
# draw_aoi <- function(img, aoi_name, color){
#  return(
#    draw_rect(
#      img,
#      x0 = aoi_region_sets[[1,paste(aoi_name,'_x_min',sep="")]],
#      x1 = aoi_region_sets[[1,paste(aoi_name,'_x_max',sep="")]],
#      y0 = aoi_region_sets[[1,paste(aoi_name,'_y_min',sep="")]],
#      y1 = aoi_region_sets[[1,paste(aoi_name,'_y_max',sep="")]],
#      opacity=0.7,
#      color=color
#    )
#  )
# }

# load.image(here("helper","stim.png")) %>%
#  mirror('y') %>% # flip image so that inverting the y axis later will result in a correct image
#  #draw_aoi("lbig", "blue") %>%
#  #draw_aoi("rbig", "blue") %>%
#  #draw_aoi("l", "red") %>%
#  #draw_aoi("r", "red") %>%
#  #draw_aoi("lb", "yellow") %>%
#  #draw_aoi("rb", "yellow") %>%
#  #draw_aoi("w", "white") %>%
#  draw_aoi("bearwatch", "yellow") %>%
#  draw_aoi("mousepath", "white") %>%
#  plot(axes = T, ylim = c(1, height(.))) # plot with inverted y axis



create_aoi_timepoints <- function(xy_timepoints) {
  xy_timepoints |>
    cross_join(aoi_region_sets) |>
    mutate(
      x_screen = x,
      y_screen = y,
      x = x - (screen_width - video_width) / 2,
      y = y - (screen_height - video_height) / 2,
      side = dplyr::case_when(
        x > l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "left",
        x > r_x_min & x < r_x_max & y > r_y_min & y < r_y_max ~ "right",
        x > lb_x_min & x < lb_x_max & y > lb_y_min & y < lb_y_max ~ "left",
        x > rb_x_min & x < rb_x_max & y > rb_y_min & y < rb_y_max ~ "right",
        x > w_x_min & x < w_x_max & y > w_y_min & y < w_y_max ~ "window",
        x > lbig_x_min & x < lbig_x_max & y > lbig_y_min & y < lbig_y_max ~ "left",
        x > rbig_x_min & x < rbig_x_max & y > rbig_y_min & y < rbig_y_max ~ "right",
        !is.na(x) & !is.na(y) ~ "other",
        TRUE ~ as.character(NA)
      ),
      aoitype = dplyr::case_when(
        x > l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "exit",
        x > r_x_min & x < r_x_max & y > r_y_min & y < r_y_max ~ "exit",
        x > lb_x_min & x < lb_x_max & y > lb_y_min & y < lb_y_max ~ "box",
        x > rb_x_min & x < rb_x_max & y > rb_y_min & y < rb_y_max ~ "box",
        x > w_x_min & x < w_x_max & y > w_y_min & y < w_y_max ~ "window",
        x > lbig_x_min & x < lbig_x_max & y > lbig_y_min & y < lbig_y_max ~ "general",
        x > rbig_x_min & x < rbig_x_max & y > rbig_y_min & y < rbig_y_max ~ "general",
        !is.na(x) & !is.na(y) ~ "other",
        TRUE ~ as.character(NA)
      ),
      aoi = dplyr::case_when(
        side %in% c("left", "right") & side == target_side ~ "target",
        side %in% c("left", "right") & side != target_side ~ "distractor",
        TRUE ~ side
      ),
      aoi = paste(aoi, aoitype, sep = "_"),
      aoi_bearmouse_special = dplyr::case_when(
        x > bearwatch_x_min & x < bearwatch_x_max & y > bearwatch_y_min & y < bearwatch_y_max ~ "bear",
        x > mousepath_x_min & x < mousepath_x_max & y > mousepath_y_min & y < mousepath_y_max ~ "mouse",
        !is.na(x) & !is.na(y) ~ "other",
        TRUE ~ as.character(NA)
      ),
    ) %>%
    select(-colnames(aoi_region_sets))
}
