rezero_times <- function(df_table,trial_col_name = "event_num") {
  # first check if this data frame has all the correct columns required for
  # normalize
  required_columns <- c(trial_col_name, "participant_id", "t")
  
  if (!all(required_columns %in% colnames(df_table))) {
    stop(.msg("Rezero times function requires the following columns to be
              present in the dataframe:
              {paste(required_columns, collapse = ', ')}. Rezeroing times should
              be the first step in the time standardization process."))
  }
  # center timestamp (0 POD)
  df_out <- df_table %>%
    dplyr::group_by(.data$participant_id, .data[[trial_col_name]]) %>%
    dplyr::mutate(t_zeroed = (.data$t - .data$t[1])) #%>%
    #dplyr::select(-.data$t) #keep old timepoint info
  return(df_out)
}

normalize_times <- function(df_table,trial_col_name = "event_num") {
  # first check if this data frame has all the correct columns required for
  # normalize
  required_columns <- c(trial_col_name, "participant_id", "t_zeroed","point_of_disambiguation")
  
  if (!all(required_columns %in% colnames(df_table))) {
    stop(.msg("Normalize times function requires the following columns to be
              present in the dataframe:
              {paste(required_columns, collapse = ', ')}. Times should be
              re-zeroed first to the starting point of a given trial before
              being normalized."))
  }
  # center timestamp (0 POD)
  df_out <- df_table %>%
    dplyr::group_by(.data$participant_id, .data[[trial_col_name]]) %>%
    dplyr::mutate(t_norm = .data$t_zeroed - .data$point_of_disambiguation) #%>%
    #dplyr::select(-.data$t_zeroed)
  return(df_out)
}

resample_xy_trial <- function(df_trial, timepoint_col_name = "t_norm",trial_col_name = "event_num",resample_pupil_size=TRUE) {
  MISSING_CONST <- -10000
  # set sample rates
  SAMPLE_RATE = 40 # Hz
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  
  t_origin <- df_trial[[timepoint_col_name]]
  x_origin <- df_trial$x
  y_origin <- df_trial$y
  
  #also set up for resampling pupil size
  if (resample_pupil_size) {
    pupil_left_origin <- df_trial$pupil_left
    pupil_right_origin <- df_trial$pupil_right
  }
  
  # create the new timestamps for resampling
  t_start <- min(t_origin) - (min(t_origin) %% SAMPLE_DURATION)
  t_resampled <- seq(from = t_start, to = max(t_origin),
                     by = SAMPLE_DURATION)
  
  # because of the behavior of approx, we need numerical values for missingness
  x_origin[is.na(x_origin)] <- MISSING_CONST
  y_origin[is.na(y_origin)] <- MISSING_CONST
  
  if (resample_pupil_size) {
    pupil_left_origin[is.na(pupil_left_origin)] <- MISSING_CONST
    pupil_right_origin[is.na(pupil_right_origin)] <- MISSING_CONST
  }
  
  # resample we use constant interpolation for two reasons: 1) the numerical
  # missingness needs to be constant, if you interpolate it you won't be able to
  # back it out, 2) linear interpolation might "slow down" saccades by choosing
  # intermediate locations (minor).
  x_resampled <- stats::approx(x = t_origin, y = x_origin, xout = t_resampled,
                               method = "constant", rule = 2,
                               ties = "ordered")$y
  y_resampled <- stats::approx(x = t_origin, y = y_origin, xout = t_resampled,
                               method = "constant", rule = 2,
                               ties = "ordered")$y
  
  if (resample_pupil_size) {
    pupil_left_resampled <- stats::approx(x = t_origin, y = pupil_left_origin, xout = t_resampled,
                                 method = "constant", rule = 2,
                                 ties = "ordered")$y
    pupil_right_resampled <- stats::approx(x = t_origin, y = pupil_right_origin, xout = t_resampled,
                                 method = "constant", rule = 2,
                                 ties = "ordered")$y
  }
  
  # replace missing values
  x_resampled[x_resampled == MISSING_CONST] <- NA
  y_resampled[y_resampled == MISSING_CONST] <- NA
  
  if (resample_pupil_size) {
    pupil_left_resampled[pupil_left_resampled == MISSING_CONST] <- NA
    pupil_right_resampled[pupil_right_resampled == MISSING_CONST] <- NA
  }
  
  # adding back the columns to match schema
  # note, no IDs here because they won't be unique.
  # dplyr::tibble(
  #   lab_id = df_trial$lab_id[1],
  #   participant_id = df_trial$participant_id[1],
  #   {{trial_col_name}} := df_trial[[trial_col_name]][1],
  #   {{timepoint_col_name}} := t_resampled,
  #   x = x_resampled,
  #   y = y_resampled,
  #   media_name = df_trial$media_name[1],
  #   media_version = df_trial$media_version[1],
  #   trial_num = df_trial$trial_num[1],
  #   age_cohort = df_trial$age_cohort[1],
  #   target = df_trial$target[1]
  #   )
  if (resample_pupil_size) {
    #first create a dataset with all constant columns and expand to the length of the resampled trial data frame
    df_trial_col_data_single_row <- df_trial %>%
      ungroup() %>%
      select(-x,-y,-!!sym(timepoint_col_name),-!!sym(trial_col_name),-pupil_left,-pupil_right,-t,-t_zeroed) %>%
      summarize(across(everything(), first))
    df_trial_col_data <- df_trial_col_data_single_row[rep(1, length(t_resampled)), ]
    #add the new resampled columns
    df_trial_new <- df_trial_col_data %>%
      mutate(
        {{trial_col_name}} := df_trial[[trial_col_name]][1],
        {{timepoint_col_name}} := t_resampled,
        x = x_resampled,
        y = y_resampled,
        pupil_left = pupil_left_resampled,
        pupil_right = pupil_right_resampled
      )
    
  } else {
    #first create a dataset with all constant columns and expand to the length of the resampled trial data frame
    df_trial_col_data_single_row <- df_trial %>%
      ungroup() %>%
      select(-x,-y,-!!sym(timepoint_col_name),-!!sym(trial_col_name),-pupil_left,-pupil_right,-t,-t_zeroed) %>%
      summarize(across(everything(), first))
    df_trial_col_data <- df_trial_col_data_single_row[rep(1, length(t_resampled)), ]
    #add the new resampled columns
    df_trial_new <- df_trial_col_data %>%
      mutate(
        {{trial_col_name}} := df_trial[[trial_col_name]][1],
        {{timepoint_col_name}} := t_resampled,
        x = x_resampled,
        y = y_resampled
      )
  }

  df_trial_new
}

resample_times <- function(df_table,timepoint_col_name = "t_norm",trial_col_name = "event_num") {
  
  # first check if this data frame has all the correct columns required for
  # re-sampling
  required_columns <- c(trial_col_name, "participant_id", timepoint_col_name, "x", "y","pupil_left","pupil_right")
  
  # re-zero and normalize times first
  # this is mandatory, comes from our decision that not linking resampling and
  # centering causes a lot of problems
  if (!all(required_columns %in% colnames(df_table))) {
    stop(.msg("Resample times function requires the following columns to be
              present in the dataframe:
              {paste(required_columns, collapse = ', ')}. Times should be
              re-zeroed and normalized first before being resampled!"))
  }
  
  # main resampling call
    df_out <- df_table %>%
      dplyr::mutate(participant_trial_id = paste(.data$participant_id,
                                           .data[[trial_col_name]], sep = "_")) %>%
      split(.$participant_trial_id) %>%
      purrr::map_df(resample_xy_trial) %>%
      dplyr::arrange(.data$participant_id, .data[[trial_col_name]]) #%>%
      #dplyr::mutate(xy_timepoint_id = 0:(dplyr::n() - 1)) # add IDs
  
  return(df_out)
}