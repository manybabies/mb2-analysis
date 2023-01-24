# Used for pilot data, deprecated for real data

# point of disambiguation information

pod_pilot_1a = 31130
pod_pilot_1b = 29960

# this function adds point of disambiguation information to xy_timepoints tables
# based on the lab_trial_type_id column 
add_pod <- function(xy) {
  xy |>
    mutate(point_of_disambiguation = ifelse(str_detect(lab_trial_type_id, "FAM"), 
                                            31000,
                                            35000))
}


# DEPRECATED
# this function is used for pilot data analysis but superseded by 
# peekds::normalize_times()
center_time_on_pod <- function(d) {
  group_by(d, subject_id, trial_id) %>%
    mutate(t = (t - min(t)) - point_of_disambiguation) %>%
    ungroup()
}