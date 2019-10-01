## point of disambiguation is 30s plus 18 frames
pod = 30000 + ((1000/30) * 18)

center_time_on_pod <- function(d) {
  group_by(d, subject_id, trial_id) %>%
    mutate(t = (t - min(t)) - pod) %>%
    ungroup()
}