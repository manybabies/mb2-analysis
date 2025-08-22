#example code
decision_grid <- expand_grid(
  wt_cutoff = c(3, 3.5),
  cyl_filter = c("all", "cyl4_only")
)

multiverse <- decision_grid %>%
  mutate(
    data = pmap(list(wt_cutoff, cyl_filter), function(wt_cut, cyl_opt) {
      df <- mtcars %>% filter(wt < wt_cut)
      if (cyl_opt == "cyl4_only") {
        df <- df %>% filter(cyl == 4)
      }
      return(df)
    })
  )

# pupillometry case
pupillometry_case_function <- function(plausible_case, gaze_within_case) {
  df <- data_pupillometry
  
  #apply filtering based on implausible values
  if (plausible_case == "plausible") {
    df <- df %>%
      mutate(average = ifelse(eyetracker_type != "EyeLink" & average <= 2 , NA, average),
             average = ifelse(eyetracker_type != "EyeLink" & average >= 8 , NA, average),
             average_z = ifelse(eyetracker_type != "EyeLink" & average <= 2 , NA, average_z),
             average_z = ifelse(eyetracker_type != "EyeLink" & average >= 8 , NA, average_z)
      )
  }
  
  #filter based on whether gaze is within the screen
  if (gaze_within_case == "within") {
    df <- df %>%
      mutate(x = ifelse(x <= 0 | x >= 1280, NA, x),
             y = ifelse(y <= 0 | y >= 960, NA, y),
            average = ifelse(is.na(x) | is.na(y), NA, average),
            average_z = ifelse(is.na(x) | is.na(y), NA, average_z)
            )
  }

  return(df)
}

decision_grid_pupil <- expand_grid(
  filter = c("implausible", "plausible"),
  gaze_within = c("outside", "within")
)

multiverse_pupil <- decision_grid_pupil %>%
  mutate(
    data = pmap(list(filter, gaze_within), pupillometry_case_function)
  )
