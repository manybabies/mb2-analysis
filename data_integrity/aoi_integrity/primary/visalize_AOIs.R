library(imager)
library(tidyverse)
library(here)

source(here("metadata/generate_AOIs_for_primary_data.R"))


#verify boundaries
for (aoi_name in c('r','l','w','lb','rb','lbig','rbig')) {
  print(aoi_name)
  stopifnot(aoi_region_sets[[1,paste(aoi_name,'_x_min',sep="")]] <= aoi_region_sets[[1,paste(aoi_name,'_x_max',sep="")]])
  stopifnot(aoi_region_sets[[1,paste(aoi_name,'_y_min',sep="")]] <= aoi_region_sets[[1,paste(aoi_name,'_y_max',sep="")]])
}


draw_aoi <- function(img, aoi_name, color){
  return(
    draw_rect(
      img, 
      x0 = aoi_region_sets[[1,paste(aoi_name,'_x_min',sep="")]], 
      x1 = aoi_region_sets[[1,paste(aoi_name,'_x_max',sep="")]], 
      y0 = aoi_region_sets[[1,paste(aoi_name,'_y_min',sep="")]], 
      y1 = aoi_region_sets[[1,paste(aoi_name,'_y_max',sep="")]], 
      opacity=0.7, 
      color=color
    )
  )
}


load.image("stim.png") %>% 
  imresize(scale = 960/900) %>% # img is 900 pixels wide, we need 960 -> rescale the image
  mirror('y') %>% # flip image so that inverting the y axis later will result in a correct image
  
  draw_aoi("lbig", "blue") %>%
  draw_aoi("rbig", "blue") %>%
  draw_aoi("l", "red") %>%
  draw_aoi("r", "red") %>%
  draw_aoi("lb", "yellow") %>%
  draw_aoi("rb", "yellow") %>%
  draw_aoi("w", "white") %>%
  
  plot(axes = T, ylim = c(1, height(.))) # plot with inverted y axis