## -----------------------------------------------------------------------------
# takes rle_data dataframe (already rle'd)
get_first_look <- function (rle_data, SAMPLING_RATE = 40, MINIMUM_ANTICIPATORY_LOOK_MS = 150) {
  
  # end if no data
  if (is.null(rle_data$values) | is.null(rle_data$lengths) | sum(rle_data$values!="NA_NA") == 0) {
    return(tibble(
      first_look = NA,
      first_look_rt = NA,
      first_shift = NA,
      first_shift_rt = NA,
      shift_type = NA,
      shift_type_all = NA))
  }
  
  #minimum look duration
  #look must be a specific duration to count (e.g. 150 ms)
  min_look_length = ceiling(MINIMUM_ANTICIPATORY_LOOK_MS / (1000/SAMPLING_RATE))
  #any values we want to skip as a potential looking/ landing target
  values_to_skip = c("NA_NA")
  
  rle_data_idx <- rle_data |> 
    #create an overall index
    mutate(idx = seq_along(values)) |> 
    #create an index that skips specific values (NA_NA)
    mutate(
      include = !(values %in% values_to_skip),
      cumulative_index_skipping_nas = cumsum(include)
    ) |> 
    mutate(
      idx_skip_nas = if_else(include,cumulative_index_skipping_nas,NA)
    ) |> 
    select(-include,-cumulative_index_skipping_nas)
  
  #determine onsets
  onset_aoi <- filter(rle_data_idx,idx == 1)$values
  onset_aoi_skip_nas <- filter(rle_data_idx,idx_skip_nas == 1)$values # zero point AOI, excluding NAs
  
  #find the first valid look within the rle data
  first_look_rle <- rle_data_idx |> 
    #filter to valid looks to target or distractor
    filter(values %in% c("target_exit", "distractor_exit"),
           lengths >= min_look_length) |> 
    #get the first look
    slice(1)
  
  #finds the first valid "shift" within the rle data
  first_shift_landing_rle <- rle_data_idx |> 
    filter(idx_skip_nas != 1, # first shift landing is post the initial look location, not counting NAs
           values %in% c("target_exit", "distractor_exit"),
           lengths >= min_look_length) |> 
    slice(1)
  
  # end if no anticipatory look to the AOIs
  if (nrow(first_look_rle) == 0) {
    return(tibble(
      first_look = NA,
      first_look_rt = NA,
      first_shift = NA,
      first_shift_rt = NA,
      shift_type = "no anticipatory look",
      shift_type_all = NA))
  }
  
  #determine first look
  first_look <- first_look_rle$values
  
  # rt is the number of samples happening before arrival
  # (first sample of arrival)
  # times the length of a sample
  #need to keep NAs here for valid looking times
  #MZ: we used to add 1 here but I think that might be wrong? because the first sample is 0 ms, i.e. right at the start of the window
  first_look_rt <- ((rle_data_idx |> 
                       filter(idx < first_look_rle$idx) |> 
                       pull(lengths) |> sum())) * (1000/SAMPLING_RATE)
  
  # if there are valid shifts, add them
  if (nrow(first_shift_landing_rle) == 0) {
    first_shift_landing <- NA
    landing_time_rt <- NA
    shift_type <- NA
    shift_type_all <- NA
  } else {
    #consolidate shift location information
    first_shift_landing <- first_shift_landing_rle$values
    shift_type <- paste0(onset_aoi_skip_nas, "_TO_", first_shift_landing)
    shift_type_all <- rle_data_idx |> 
      filter(idx_skip_nas <= first_shift_landing_rle$idx_skip_nas) |> 
      pull(values) |> 
      paste(collapse = "_TO_")
    #compute RT
    landing_time_rt <- ((rle_data_idx |> 
                           filter(idx < first_shift_landing_rle$idx) |> 
                           pull(lengths) |> sum())) * (1000/SAMPLING_RATE)
  }
  
  return(tibble(
    first_look = first_look, #first valid look location, regardless of whether there was a shift to the location
    first_look_rt = first_look_rt, #reaction time for first valid look location, regardless of whether there was a shift to the location
    first_shift = first_shift_landing, #first shift to a valid look location
    first_shift_rt = landing_time_rt, #reaction time for first shift to a valid look location
    shift_type = shift_type, #shift in locations (START_LOCATION_TO_LANDING_LOCATION) for first shift to a valid look location
    shift_type_all = shift_type_all) # all shifts during the anticipatory window
  )
}