
#' Complete sampling
#'
#' Complete sampling with heterogeneous probabilities.
#' This function samples units gives an ordered vector of probabilities --- or more generally an expected number of cases assigned  to a unit --- using a version of systematic sampling
#' @param p vector of probabilities for each unit (or expected number of cases assiged to a unit)
#' @param n number of units -- generally length of p, not required
#' @param m number of units to be sampled, generally sum of p, not required
#' @param seed seed
#' @param mywarning Warning for rescaling probabilities, negative probabilities etc
#' @param reweight reweight in case of incompatibilities,
#' @param systematic defaults FALSE; if FALSE order is randomized prior to randomization and then restored; if TRUE then true systematic sampling. Systematic sampling uses orderings which introduces correlations in assignments.
#' @keywords sampling
#' @export
#' @examples
#' # Simple specifications with uniform probabilities
#' complete_sampling(m = 3,   n = 14, seed = 1)
#' complete_sampling(p = 3/7, n = 14, seed = 1)
#' complete_sampling(p = 3/7, n = 14, seed = 1, systematic = TRUE)
#' # Nonuniform probabilities
#' complete_sampling(c(0, 1.8, .2))
#' complete_sampling(c(0, 1.8, .2), m = 1)
#' complete_sampling(c(0, 1.8, .2), m = 1, reweight = TRUE)
#' apply(replicate(1000, complete_sampling(c(.2,.4,.6,.8), m = 2)), 1, mean)
#' # Illustration of ordered versus unordered systematic
#' complete_sampling(.6, n = 40, systematic = TRUE, seed = 1)
#' complete_sampling(.6, n = 40, systematic = FALSE, seed = 1)
#' # probabilities need not sum to an integer
#' uneven <- replicate(100000,complete_sampling(p = c(.3,.4)))
#' apply(uneven, 1, mean)
#' table(apply(uneven, 2, sum))
#' # May assign exactly or have randomization over residuals only
#' complete_sampling(c(10, 11.5, 12.5))

complete_sampling <- function(
  p  = NULL,  # vector of probabilities
  n  = NULL,  # scalar: total number of units
  m  = NULL,  # scalar: number of units to be sampled /assigned to treatment
  seed = NULL,
  mywarning = TRUE,
  systematic = FALSE,
  reweight = FALSE
){
  # Housekeeping to allow flexible specifications of p, n, and m
  if(!is.null(seed)) set.seed(seed)
  
  if(is.null(p) & is.null(n) & is.null(m)) stop("Please define either p or both m and n")
  
  if(is.null(p) & !is.null(n) & !is.null(m)) p <- rep(m/n, n)
  
  if(length(p)==1 & !is.null(n))   p <- rep(p, n)
  
  if(min(p) < -.0001){ stop("stopping because value for p < -.0001 found")}
  if(min(p) < 0) { if(mywarning) print(paste("small negative p found: min p", min(p), "; scaling up")); p <- p-min(p)}
  if(max(p) == 0) { if(mywarning) print("no positive probabilities"); return(rep(0, length(p)))}
  
  
  if(is.null(m) & (sum(p)%%1) >0) {m <- ceiling(sum(p)); p <- c(p, m - sum(p)); uneven <- TRUE} else {uneven <- FALSE}
  if(is.null(m)) {m <- sum(p)}
  if(reweight) {
    if(sum(p)!=m & mywarning & !uneven) {print("m specified and p does not sum to m: reweighting p")}
    p   <- {p*m/sum(p)}}
  
  n <- length(p)
  
  # if(sum(p) > m | m>n) stop("Incompatible specificiation of p, m and n")
  if(!systematic) {neworder <- sample(1:n); p[neworder] <- p}
  
  # Randomization starts here:
  
  base <- floor(p)
  p    <- p-base
  m    <- m - sum(base)
  
  if(sum(p) <= 0) {
    add <- rep(0,n)
  } else {
    s <- ((cumsum(p) +m*runif(1))%%m)
    e <- s - floor(s)
    add <- 1*(e < (c(e[n], e[-n])-.Machine$double.eps^.5))
  }
  out <- base + add
  
  if(!systematic) out <- out[neworder]
  if(uneven) out <- out[-n]
  return(out)
}

gen_Omega_and_probs <- function(.total_n,
                                .total_n_t,
                                .y,
                                .z,
                                .block,
                                .probs,
                                .gamma,
                                .p_value,
                                .exact = NULL,
                                .seed = NULL,
                                .n_sims) {
  
  obs_stat = mean(.y[.z == 1]) - mean(.y[.z == 0])
  
  if (.gamma == 1) {
    p_u = p_l = .probs 
  } else {
    odds_u = ifelse(test = .z == 1, yes = .gamma * .probs / (1 - .probs), no = 1)
    odds_l = ifelse(test = .z == 1, yes = .probs / ( .gamma * (1 - .probs) ), no = 1)
    p_u = odds_u / (1 + odds_u)
    p_l = odds_l / (1 + odds_l)
  }
  
  if(.exact == TRUE){
    
    if(choose(n = .total_n, k = .total_n_t) > choose(n = 20, k = 10)) 
      { stop("Error: You have too many units for the exact method; use simulations instead") }
    
    treated = combn(x = 1:.total_n,
                    m = .total_n_t,
                    simplify = TRUE) 
    
    all_z = apply(X = treated,
                  MARGIN = 2,
                  FUN = function(x) as.integer(1:.total_n %in% x))
    
    if (!is.null(.block)) {
      
      which_z = apply(X = all_z,
                      MARGIN = 2, 
                      FUN = function(x) { 
                        all(do.call(what = "c",
                                    args = lapply(X = split(x = x,
                                                            f = .block),
                                                  FUN = sum)) == 
                              do.call(what = "c",
                                      args = lapply(X = split(x = .z,
                                                              f = .block),
                                                    FUN = sum))) })
      
      all_z = all_z[, which_z] 
      
    }
    
    probs_u = apply(X = all_z,
                    MARGIN = 2,
                    FUN = function(x) prod(ifelse(test = x == 1,
                                                  yes = p_u,
                                                  no =  (1 - p_u))))
    
    total_prob_u = sum(probs_u)
    
    all_probs_u = probs_u / total_prob_u
    
    probs_l = apply(X = all_z,
                    MARGIN = 2,
                    FUN = function(x) prod(ifelse(test = x == 1,
                                                  yes = p_l,
                                                  no =  (1 - p_l))))
    
    total_prob_l = sum(probs_l)
    
    all_probs_l = probs_l / total_prob_l
    
    null_dist = apply(X = all_z,
                      MARGIN = 2,
                      FUN = function(x) { mean(.y[x == 1]) - mean(.y[x == 0])})
    
    if(.p_value == "two.sided"){
    
    p_value_u = 2 * min(sum((null_dist >= obs_stat) * all_probs_u), sum((null_dist <= obs_stat) * all_probs_u)) 
    
    p_value_l = 2 * min(sum((null_dist >= obs_stat) * all_probs_l), sum((null_dist <= obs_stat) * all_probs_l)) 
    
    return(list(upperp=p_value_u, lowerp=p_value_l)) }
    
    if(.p_value == "lower"){
      
      p_value_u = sum((null_dist <= obs_stat) * all_probs_u)
      
      p_value_l = sum((null_dist <= obs_stat) * all_probs_l)
      
      return(list(upperp=p_value_u, lowerp=p_value_l)) }
    
    if(.p_value == "upper"){
      
      p_value_u = sum((null_dist >= obs_stat) * all_probs_u)
      
      p_value_l = sum((null_dist >= obs_stat) * all_probs_l)
      
      return(list(upperp=p_value_u, lowerp=p_value_l)) }
    
    }
  
  else {
    
    if(!is.null(.block)){
      
      data = data.frame(p_u = p_u,
                        p_l = p_l,
                        z = .z,
                        block = .block)
      
      data_by_blocks = sapply(X = unique(data$block),
                              FUN = function(x) { list(subset(x = data,
                                                              subset = block == x)) })
      
      set.seed(.seed)
      z_sims_u = replicate(n = .n_sims,
                         expr = unlist(lapply(X = 1:length(data_by_blocks),
                                              FUN = function(x) { complete_sampling(p = data_by_blocks[[x]]$p_u,
                                                                                    n = nrow(data_by_blocks[[x]]),
                                                                                    m = sum(data_by_blocks[[x]]$z))})))
      z_sims_l = replicate(n = .n_sims,
                           expr = unlist(lapply(X = 1:length(data_by_blocks),
                                                FUN = function(x) { complete_sampling(p = data_by_blocks[[x]]$p_l,
                                                                                      n = nrow(data_by_blocks[[x]]),
                                                                                      m = sum(data_by_blocks[[x]]$z))})))
      
      null_dist_u = apply(X = z_sim_u,
                        MARGIN = 2,
                        FUN = function(x) { mean(.y[x == 1]) - mean(.y[x == 0])})
      
      null_dist_l = apply(X = z_sim_l,
                          MARGIN = 2,
                          FUN = function(x) { mean(.y[x == 1]) - mean(.y[x == 0])})
      
      probs_u = apply(X = z_sim_u,
                    MARGIN = 2,
                    FUN = function(x) prod(ifelse(test = x == 1,
                                                  yes = p_u,
                                                  no =  (1 - p_u))))
      
      total_prob_u = sum(probs_u)
      
      all_probs_u = probs_u / total_prob_u
      
      probs_l = apply(X = z_sim_l,
                      MARGIN = 2,
                      FUN = function(x) prod(ifelse(test = x == 1,
                                                    yes = p_l,
                                                    no =  (1 - p_l))))
      
      total_prob_l = sum(probs_l)
      
      all_probs_l = probs_l / total_prob_l
      
      if(.p_value == "two.sided"){
        
        p_value_u = 2 * min(sum((null_dist_u >= obs_stat) * all_probs_u), sum((null_dist_u <= obs_stat) * all_probs_u)) 
        
        p_value_l = 2 * min(sum((null_dist_l >= obs_stat) * all_probs_l), sum((null_dist_l <= obs_stat) * all_probs_l)) 
        
        return(list(upperp=p_value_u, lowerp=p_value_l)) }
      
      if(.p_value == "lower"){
        
        p_value_u = sum((null_dist_u <= obs_stat) * all_probs_u)
        
        p_value_l = sum((null_dist_l <= obs_stat) * all_probs_l)
        
        return(list(upperp=p_value_u, lowerp=p_value_l)) }
      
      if(.p_value == "upper"){
        
        p_value_u = sum((null_dist_u >= obs_stat) * all_probs_u)
        
        p_value_l = sum((null_dist_l >= obs_stat) * all_probs_l)
        
        return(list(upperp=p_value_u, lowerp=p_value_l)) }
      
       }
    
    else {      
      
      set.seed(.seed)
      z_sim_u = replicate(n = .n_sims,
                        expr = complete_sampling(p = p_u,
                                                 n = length(p_u),
                                                 m = sum(.z)))
      
      z_sim_l = replicate(n = .n_sims,
                          expr = complete_sampling(p = p_l,
                                                   n = length(p_l),
                                                   m = sum(.z)))
      
      probs = apply(X = z_sim_u,
                    MARGIN = 1,
                    FUN = mean)
      
            null_dist_u = apply(X = z_sim_u,
                          MARGIN = 2,
                          FUN = function(x) { mean(.y[x == 1]) - mean(.y[x == 0])})
      
      null_dist_l = apply(X = z_sim_l,
                          MARGIN = 2,
                          FUN = function(x) { mean(.y[x == 1]) - mean(.y[x == 0])})
      
      probs_u = apply(X = z_sim_u,
                    MARGIN = 2,
                    FUN = function(x) prod(ifelse(test = x == 1,
                                                  yes = p_u,
                                                  no =  (1 - p_u))))
      
      total_prob_u = sum(probs_u)
      
      all_probs_u = probs_u / total_prob_u
      
      probs_l = apply(X = z_sim_l,
                      MARGIN = 2,
                      FUN = function(x) prod(ifelse(test = x == 1,
                                                    yes = p_l,
                                                    no =  (1 - p_l))))
      
      total_prob_l = sum(probs_l)
      
      all_probs_l = probs_l / total_prob_l
      
      if(.p_value == "two.sided"){
        
        p_value_u = 2 * min(sum((null_dist_u >= obs_stat) * all_probs_u), sum((null_dist_u <= obs_stat) * all_probs_u)) 
        
        p_value_l = 2 * min(sum((null_dist_l >= obs_stat) * all_probs_l), sum((null_dist_l <= obs_stat) * all_probs_l)) 
        
        return(list(upperp=p_value_u, lowerp=p_value_l)) }
      
      if(.p_value == "lower"){
        
        p_value_u = sum((null_dist_u <= obs_stat) * all_probs_u)
        
        p_value_l = sum((null_dist_l <= obs_stat) * all_probs_l)
        
        return(list(upperp=p_value_u, lowerp=p_value_l)) }
      
      if(.p_value == "upper"){
        
        p_value_u = sum((null_dist_u >= obs_stat) * all_probs_u)
        
        p_value_l = sum((null_dist_l >= obs_stat) * all_probs_l)
        
        return(list(upperp=p_value_u, lowerp=p_value_l)) }
      
    }
  }
}
#' @examples
#' unit_index <- 1:8
#' 
#' block_index <- c(1, 1, 1, 2, 2, 2, 3, 3)
#' 
#' total_n = length(unit_index)
#' 
#' hyp_z <- c(1, 0, 0, 0, 1, 0, 1, 0)
#' 
#' y = c(8, 11, 21, 27, 27, 33,  6, 34)
#' 
#' total_n_t <- sum(hyp_z)
#' 
#' pis <- c(rep(x = 0.5,
#'              times = length(hyp_z)))
#' 
#' gen_Omega_and_probs(.total_n = total_n,
#'                     .total_n_t = total_n_t,
#'                     .y = y,
#'                     .z = hyp_z,
#'                     .block = NULL,
#'                     .probs = pis,
#'                     .gamma = 2,
#'                     .p_value = "two.sided",
#'                     .exact = TRUE,
#'                     .seed = 1:5,
#'                     .n_sims = 10000)


