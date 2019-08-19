# Generate AOIs:

## Set screen and video dimensions. Note: this only works if the zero point of the 
## eyetracker's coordinate systems is top left, and if the video was displayed centered,
## i.e., the video's and the screen's center were aligned. For the pilot, the dimensions are:
## 1280 x 1024 screen and 1280 x 960 video
## 1920 x 1080 screen and 1200 x 900 video
## 1920 x 1200 screen and 1200 x 900 video

screen_width <- 1280
screen_height <- 1024
video_width <- 1280
video_height <- 960

### Big AOIs
#### Left AOI
Big_leftAOI_top <- ((video_height*9)/16) + ((screen_height-video_height)/2)
Big_leftAOI_bottom <- video_height + ((screen_height-video_height)/2)
Big_leftAOI_left <- (screen_width-video_width)/2
Big_leftAOI_right <- (video_width*7)/16 + (screen_width-video_width)/2
#### Right AOI
Big_rightAOI_top <- ((video_height*9)/16) + ((screen_height-video_height)/2)
Big_rightAOI_bottom <- video_height + ((screen_height-video_height)/2)
Big_rightAOI_left <- ((video_width*9)/16) + (screen_width-video_width)/2
Big_rightAOI_right <- video_width + (screen_width-video_width)/2

### Tunnel AOIs
#### Left AOI:
Tunnel_leftAOI_top <- ((video_height*11)/16) + ((screen_height-video_height)/2)
Tunnel_leftAOI_left <- ((video_width*5)/16) + (screen_width-video_width)/2
Tunnel_leftAOI_radius <- ((video_height*5)/32)
#### Right AOI:
Tunnel_rightAOI_top <- ((video_height*11)/16) + ((screen_height-video_height)/2)
Tunnel_rightAOI_left <- ((video_width*11)/16) + (screen_width-video_width)/2
Tunnel_rightAOI_radius <- ((video_height*5)/32)
