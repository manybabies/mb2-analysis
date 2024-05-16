
### Procedure for each trial

#Extract individual frames from videos
#ffmpeg -i FAM_LL.mp4 -vf "fps=40" stimulus_frames/FAM_LL_%04d.png

#filter data to specific trial

# Integrating t_norm with frame data
## use just a running counter within a give trial (?think about this?)

# group by individual frame / counter (across participants and labs)
# create heatmap for that specific frame

# library(png)
# img.r <- as.raster(readPNG(here('helper', 'stim.png')),interpolate=F)
# 
# data_with_aois %>% 
#   filter(!(media_name %in% c('star_calib'))) %>% 
#   group_by(lab_id, media_name) %>%
#   group_walk(function(data, grouping){
#     print(grouping$lab_id)
#     
#     if(!(grouping$lab_id %in% labs_to_check)){
#       return(NA)
#     }
#     
#     labdir = here(CHECKING_HEATMAP_DIR, grouping$lab_id)
#     dir.create(labdir, showWarnings = FALSE)
#     tryCatch(
#       ggsave(
#         here(labdir, paste0(grouping$media_name, '__', grouping$lab_id, '.png')),
#         plot = 
#           ggplot(data, aes(x,y)) + 
#           annotation_raster(img.r, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
#           stat_density2d(geom = "polygon", aes(fill=..level.., alpha = ..level..), size= 20, bins= 500) + 
#           scale_fill_gradient(low="blue",high="red") +
#           scale_alpha_continuous(range=c(0.01,1.0), guide = FALSE) +
#           scale_x_continuous(limits=c(0,dim(img.r)[2]),expand=c(0,0))+
#           scale_y_continuous(limits=c(0,dim(img.r)[1]),expand=c(0,0))+
#           coord_fixed(),
#         width = 7,
#         height = 5,
#         units = "in"
#       ), error = function(e) print(e))
#   })

## stitch all frames into a final video (make sure to set to 40 fps)
## ffmpeg or R
