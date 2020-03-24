pod = 31130
pod_pilot2 = 29960

center_time_on_pod <- function(d) {
  group_by(d, subject_id, trial_id) %>%
    mutate(t = (t - min(t)) - pod) %>%
    ungroup()
}

center_time_on_pod_pilot2 <- function(d) {
  group_by(d, subject_id, trial_id) %>%
    mutate(t = (t - min(t)) - pod_pilot2) %>%
    ungroup()
}