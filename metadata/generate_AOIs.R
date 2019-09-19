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
# aoi_region_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, 


generate_aoi_regions <- function(screen_width = 1280, 
                                 screen_height = 1024,
                                 video_width = 1280,
                                 video_height = 960, 
                                 size = "big") {
  
  if (size == "small") {
    # not implemented
    # ### Tunnel AOIs
    # #### Left AOI:
    # Tunnel_leftAOI_top <- ((video_height*11)/16) + ((screen_height-video_height)/2)
    # Tunnel_leftAOI_left <- ((video_width*5)/16) + (screen_width-video_width)/2
    # Tunnel_leftAOI_radius <- ((video_height*5)/32)
    # #### Right AOI:
    # Tunnel_rightAOI_top <- ((video_height*11)/16) + ((screen_height-video_height)/2)
    # Tunnel_rightAOI_left <- ((video_width*11)/16) + (screen_width-video_width)/2
    # Tunnel_rightAOI_radius <- ((video_height*5)/32)
  } else if (size == "big") {
    aoi_regions = tibble(
      aoi_region_id = 0, 
      l_x_max = (video_width*7)/16 + (screen_width-video_width)/2,
      l_x_min = (screen_width-video_width)/2,
      l_y_max = video_height + ((screen_height-video_height)/2),
      l_y_min = ((video_height*9)/16) + ((screen_height-video_height)/2),
      r_x_max = video_width + (screen_width-video_width)/2,
      r_x_min = ((video_width*9)/16) + (screen_width-video_width)/2,
      r_y_max = video_height + ((screen_height-video_height)/2),
      r_y_min = ((video_height*9)/16) + ((screen_height-video_height)/2))
  }
}
