na_mode <- function(x) {
    if (all(is.na(x))) {
      return(as.character(NA))
    } else {
      x_nona <- x[!is.na(x)]

      # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
      ux <- unique(x_nona)
      x_mode <- ux[which.max(tabulate(match(x_nona, ux)))]

      return(x_mode)
    }
  }