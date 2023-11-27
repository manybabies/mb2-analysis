library(patchwork)
cur_trial_d <- data_with_aois %>%
  filter(participant_id=="1810EmCh") %>%
  filter(event_num == 1)

# cur_trial_d_long <- cur_trial_d %>%
#   pivot_longer(cols=c(x,y),values_to="gaze_position",names_to="gaze_type")
max_t <- max(cur_trial_d_long$t_norm)
min_t <- min(cur_trial_d_long$t_norm)

x_p1 <- cur_trial_d %>%
  ggplot(aes(x,t_norm))+
  scale_y_reverse() +
  geom_point(color="#2ca25f")+
  geom_hline(yintercept=0,size=1)+
  geom_line(aes(y=screen_height),size=1, linetype="dashed",color="black")+
  geom_line(aes(y=screen_width),size=1, linetype="dashed",color="black")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))
x_p1

y_p1 <- cur_trial_d %>%
  ggplot(aes(t_norm,y))+
  scale_y_reverse() +
  geom_point(color="#8856a7")+
  geom_hline(yintercept=0,size=1)+
  geom_line(aes(y=screen_height),size=1, linetype="dashed",color="black")+
  geom_line(aes(y=screen_width),size=1, linetype="dashed",color="black")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))
y_p1 
x_p1+y_p1


  
  
  
  # draw_aoi_x("lbig",color="red")+
  # draw_aoi_y("lbig",color="blue")+
  # draw_aoi_x("rbig",color="red")+
  # draw_aoi_y("rbig",color="blue")+
  # draw_aoi_x("w",color="red")+
  # draw_aoi_y("w",color="blue")+
  # draw_aoi_y("lbig",color="blue")+
  geom_point()+
  geom_hline(yintercept=0,size=1)+
  geom_line(aes(y=screen_height),size=1, linetype="dashed",color="black")+
  geom_line(aes(y=screen_width),size=1, linetype="dashed",color="black")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

ggplot(cur_trial_d_long,aes(-t_norm,gaze_position,color=gaze_type))+
  # draw_aoi_x("lbig",color="red")+
  # draw_aoi_y("lbig",color="blue")+
  # draw_aoi_x("rbig",color="red")+
  # draw_aoi_y("rbig",color="blue")+
  # draw_aoi_x("w",color="red")+
  # draw_aoi_y("w",color="blue")+
  # draw_aoi_y("lbig",color="blue")+
  facet_wrap(~gaze_type)+
  geom_point()+
  coord_flip()+
  geom_hline(yintercept=0,size=1)+
  geom_line(aes(y=screen_height),size=1, linetype="dashed",color="black")+
  geom_line(aes(y=screen_width),size=1, linetype="dashed",color="black")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  
  
  
  annotate("text",y = -100, x=1500 ,label="Screen End X/Y Left/Top")

  
draw_aoi_x <- function(aoi_name, color){
    return(
      annotate("rect",
               xmin=min_t,
               xmax=max_t,
               ymin=aoi_region_sets[[1,paste(aoi_name,'_x_min',sep="")]],
               ymax=aoi_region_sets[[1,paste(aoi_name,'_x_max',sep="")]],
               fill=color,color=NA,alpha=0.1)
    )
  }

draw_aoi_y <- function(aoi_name, color){
  return(
    annotate("rect",
             xmin=min_t,
             xmax=max_t,
             ymin=aoi_region_sets[[1,paste(aoi_name,'_y_min',sep="")]],
             ymax=aoi_region_sets[[1,paste(aoi_name,'_y_max',sep="")]],
             fill=color,color=NA,alpha=0.1)
  )
}
