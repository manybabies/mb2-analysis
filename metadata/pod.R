## point of disambiguation is 31s plus 17 frames
pod = 31000 + ((1000/30) * 17)

center_time_on_pod <- function(d) {
  group_by(d, subject_id, trial_id) %>%
    mutate(t = (t - min(t)) - pod) %>%
    ungroup()
}