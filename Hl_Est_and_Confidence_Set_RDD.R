
Confidence_Set_and_HL_Est_RDD <-
  function(.Y,
           .Z,
           .R,
           .block,
           .tau_range,
           .tau_length,
           .sims,
           .alpha = .05,
           .cores = parallel::detectCores()) {
    
    # Preliminaries
    requireNamespace("dplyr", quietly = TRUE)
    requireNamespace("magrittr", quietly = TRUE)
    requireNamespace("doParallel", quietly = TRUE)
    
    taus <- taus_sup <- taus_inf <- c(NULL)
    
    cl <- parallel::makeCluster(.cores)
    doParallel::registerDoParallel(cl)
    
    # Start loop over the range of possible taus
    pvals <- foreach(tau = seq(from = .tau_range[1],
                               to = .tau_range[2],
                               length.out = .tau_length)) %dopar% {
                                 
                                 # Adjusted outcome
                                 Y0 <- .Y - .Z * tau
                                 if ( is.null(.block) ) {
                                   
                                   resid_Y0 <- resid(lm(Y0 ~ .R))
                                   t_stat_obs <- coef(lm(resid_Y0 ~ .Z))[2]
                                   t_stats <- replicate(.sims,
                                                        coef(lm(resid_Y0 ~ sample(.Z)))[2])
                                   }
                                 else {
                                   resid_Y0 <- resid(lm(Y0 ~ .R + .block))
                                   t_stat_obs <- coef(lm(resid_Y0 ~ .Z + .block))[2]
                                   t_stats <- replicate(.sims,
                                                        { Z_sim <- .Z %>%
                                                          split(., .block) %>%
                                                          lapply(., sample) %>%
                                                          unsplit(., .block)
                                                        
                                                        return(coef(lm(resid_Y0 ~ Z_sim + .block))[2])
                                                        }
                                   )}
                                 
                                   pval <- ( sum(abs(t_stats) >= abs(t_stat_obs), na.rm = T) / .sims )
                                   
                                   pval
                               }
    parallel::stopCluster(cl)
    
    taus_df <- data.frame(taus = seq(from = .tau_range[1],
                                     to = .tau_range[2],
                                     length.out = .tau_length),
                          pvals = unlist(pvals))
    
    taus_df <- taus_df[which(taus_df$pvals >= .alpha), ] %>%
      arrange(taus) 
    
    HL_est <- taus_df$taus[which.max(taus_df$pvals)[1]]
    
    # Check for irregularities
    
    if ( any( abs( c(min(taus_df$taus), max(taus_df$taus), HL_est) ) == Inf ) ) {
      
      stop("Warning: Tau range could be wrong\n")
      }
    
    return(list("lower_cs" = min(taus_df$taus),
                "upper_cs" = max(taus_df$taus),
                "HL_est" = HL_est,
                "taus_df" = taus_df))
    }

    
