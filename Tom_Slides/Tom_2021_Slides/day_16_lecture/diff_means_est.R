diff_means_est <- function(.V,
                           .Z) {
  
    return(mean(.V[.Z == 1]) - mean(.V[.Z == 0])) }