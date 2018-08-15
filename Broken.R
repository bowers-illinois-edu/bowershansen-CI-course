
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


p_u <- c(2/3, 1/2, 1/2, 1/2, 2/3, 1/2, 2/3, 1/2)
z <- c(1, 0, 0, 0, 1, 0, 1, 0)

set.seed(1:5)
z_sim_u <- replicate(n = 10^5,
                    expr = complete_sampling(p = p_u,
                                             n = length(p_u),
                                             m = sum(z)))
probs <- apply(X = z_sim_u,
              MARGIN = 1,
              FUN = mean)

