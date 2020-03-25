pod_pilot_1a = 31130
pod_pilot_1b = 29960

center_time_on_pod <- function(d) {
  group_by(d, subject_id, trial_id) %>%
    mutate(t = (t - min(t)) - pod) %>%
    ungroup()
}