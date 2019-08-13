diff_means_est <- function(.y,
                           .z) {
  
    return(mean(.y[.z == 1]) - mean(.y[.z == 0])) }