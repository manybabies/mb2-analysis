################################ exclude_by ################################
# excludes on a column and outputs the percentages for exclusion
# - flag action flips the column polarity
# - logfile is the CSV for writing exclusions
# - analysis stage and exclusion criterion are book keeping on the exclusiosn
# 
# note the "any" vs. "all" distinction is 

# columns for logfile:
# analysis_stage, exclusion_criterion, original_n, n_excluded, prop_excluded, n_remaining, timestamp

exclude_by <- function(d, col, logfile, analysis_stage = "exclusions", 
                       exclusion_criterion = "unspecified",
                       action = "exclude") {
  
  log <- read_csv(logfile)
  
  # if this is an include-by variable, flip polarity
  if (action == "include") {
    d <- d %>%
      mutate(!! quo_name(col) := ! (!! col))
  }
  
  print(paste("filtering by", quo_name(col)))

  # original_n, n_excluded, prop_excluded, n_remaining, timestamp
  original_n <- length(unique(d$unique_participant_id))
    
    
  percent_sub <- d %>%
    group_by(unique_participant_id) %>%
    summarise(any = any(!! col), 
              all = all(!! col)) %>%
    ungroup %>%
    summarise(any_sum = sum(any, na.rm=TRUE), 
              any_mean = mean(any, na.rm=TRUE), 
              all_sum = sum(all, na.rm=TRUE), 
              all_mean = mean(all, na.rm=TRUE))
  
  
    
    
    print(paste(percent_sub$all_sum, " subjects,", 
                round(percent_sub$all_mean*100, digits = 1),
                "%, have all trials where", quo_name(col), "is",
                ifelse(action == "include", "true:", "false:"), 
                action)
    )
  
  # return the data frame
  if (action=="NA out") {
    mutate(d, looking_time = ifelse(!! col, NA, looking_time))
  } else {
    filter(d, !( !! col))
  }
}