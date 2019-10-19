pod = 31130

center_time_on_pod <- function(d) {
  group_by(d, subject_id, trial_id) %>%
    mutate(t = (t - min(t)) - pod) %>%
    ungroup()
}