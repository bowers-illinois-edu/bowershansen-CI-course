set.seed(1:5)

library(pacman)

p_load(doParallel)

fake_data <- data_frame(y0 = rnorm(n = 20,
                                  mean = 10,
                                  sd = 2),
                        z = c(rep(x = 0,
                                times = 10),
                              rep(x = 1,
                                  times = 10)),
                        tau = rep(x = 5,
                                  times = 20),
                        y1 = y0 + tau,
                        y = z * y1 + (1 - z) * y0)

Confidence_Set_and_HL_Est <-
  function(.Y,
           .Z,
           .block = NULL,
           .tau_range = c(-10, 10),
           .tau_length = length(seq(-10, 10, by = .0001)),
           .sims = 1000,
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
                                   
                                   t_stat_obs <- coef(lm(Y0 ~ .Z))[2]
                                   t_stats <- replicate(.sims,
                                                        coef(lm(Y0 ~ sample(.Z)))[2])
                                   }
                                 else {
                                   t_stat_obs <- coef(lm(Y0 ~ .Z + .block))[2]
                                   t_stats <- replicate(.sims,
                                                        { Z_sim <- .Z %>%
                                                          split(., .block) %>%
                                                          lapply(., sample) %>%
                                                          unsplit(., .block)
                                                        
                                                        return(coef(lm(Y0 ~ Z_sim + .block))[2])
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

Confidence_Set_and_HL_Est(.Y = fake_data$y,
                          .Z = fake_data$z)

    
