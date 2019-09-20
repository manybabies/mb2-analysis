
# point of disambiguation is 30s plus 18 frames
get_pod <- function() { return (pod = 30000 + ((1000/30) * 18)) }

get_trials <- function() {
  trialorder = c("LL", "LR", "RR", "RR", "LL", "RL", "LR", "RL", rev(order1))
  trialnums = c(seq(1, 8), seq(1, 8))
  
  trials <- tibble(aoi_region_id = 0, 
                   dataset_id = 0, 
                   lab_trial_id = trialorder, 
                   distractor_image = "distractor", 
                   distractor_label = "distractor",
                   full_phrase = NA,
                   point_of_disambiguation = pod, 
                   target_image = "target", 
                   target_label = "target", 
                   target_side = ifelse(str_sub(trialorder, start = 2, end = 2) == "L", 
                                        "left", "right")) %>%
    mutate(trial_id = trialnums)
  return(trials)
}