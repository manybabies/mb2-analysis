################################ exclude_by ################################
# excludes on a column and outputs the percentages for exclusion
# - flag return_pcts means you get a list with the data and other exclusion stats
# - flag action flips the column polarity

exclude_by <- function(d, col, setting = "all", action = "exclude", 
                       return_pcts = FALSE, quiet = TRUE) {
  
  # if this is an include-by variable, flip polarity
  if (action == "include") {
    d <- d %>%
      mutate(!! quo_name(col) := ! (!! col))
  }
  
  if (!quiet) print(paste("filtering by", quo_name(col)))
  
  percent_trial <- d %>%
    ungroup %>%
    summarise(trial_sum = sum(!! col, na.rm=TRUE),
              trial_mean = mean(!! col, na.rm=TRUE)) 
  
  percent_sub <- d %>%
    group_by(lab, subid) %>%
    summarise(any = any(!! col), 
              all = all(!! col)) %>%
    ungroup %>%
    summarise(any_sum = sum(any, na.rm=TRUE), 
              any_mean = mean(any, na.rm=TRUE), 
              all_sum = sum(all, na.rm=TRUE), 
              all_mean = mean(all, na.rm=TRUE))
  
  if (!quiet) {
    print(paste("This variable excludes", percent_trial$trial_sum, "trials, which is ", 
                round(percent_trial$trial_mean*100, digits = 1), "% of all trials."))
    
    if (setting == "any") {
      print(paste(percent_sub$any_sum, " subjects,", 
                  round(percent_sub$any_mean*100, digits = 1),
                  "%, have any trials where", quo_name(col), "is true."))
    } else if (setting == "all") {
      print(paste(percent_sub$all_sum, " subjects,", 
                  round(percent_sub$all_mean*100, digits = 1),
                  "%, have all trials where", quo_name(col), "is",
                  ifelse(action == "include", "true:", "false:"), 
                  action))
    }
  }
  
  if (action=="NA out") {
    d <- mutate(d, 
                looking_time = ifelse(!! col, NA, looking_time))
  } else {
    d <- filter(d, !( !! col))
  }
  
  if (return_pcts) {
    return(list(data = d, 
                percents = percent_sub, 
                percent_trials = percent_trial))
  } else {
    return(d)
  }
}