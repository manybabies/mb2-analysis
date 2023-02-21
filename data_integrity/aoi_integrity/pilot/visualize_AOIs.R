library(R.matlab)
library(countcolors)
library(imager)

# this file is in the AOI_tests folder in case you are looking for it
frame = readMat('stim.mat')/255

stimframe = as.cimg(frame$frame)


video_width = 1200
video_height = 900

ratios <- ratios_of_bounding_box(video_width, video_height)

l_x_max = (video_width*ratios$L_right) 
l_x_min = (video_width*ratios$L_left)
l_y_max = (video_height*ratios$bottom) 
l_y_min = (video_height*ratios$top) 
r_x_max = (video_width*ratios$R_right)  
r_x_min = (video_width*ratios$R_left)  
r_y_max = (video_height*ratios$bottom) 
r_y_min =  (video_height*ratios$top) 



stimframe %>% imrotate(90) %>% mirror('x') %>% 
  draw_rect(l_x_min,l_y_max,l_x_max,l_y_min,opacity=0.2) %>% 
  draw_rect(r_x_min,r_y_max,r_x_max,r_y_min,opacity=0.2) %>% 
  plot


