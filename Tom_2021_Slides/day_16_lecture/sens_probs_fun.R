sens_probs <- function(.Gamma, .Omega, .u){
  
  gamma = log(.Gamma)
  
  unnorm_probs = sapply(X = 1:ncol(.Omega),
                        FUN = function(x) { exp(gamma * t(.Omega[,x]) %*% .u) })
  
  total_prob = sum(unnorm_probs)
  
  return(unnorm_probs / total_prob)
  
  
}


