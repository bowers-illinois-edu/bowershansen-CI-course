true_diff_means_var <- function(.n, .n_1, .y_c, .y_t){
  
  var_y_c = mean((.y_c - mean(.y_c))^2)
  
  var_y_t = mean((.y_t - mean(.y_t))^2)
  
  cov_y_c_y_t = mean((.y_c - mean(.y_c)) * (.y_t - mean(.y_t)))
  
  var = (1/(.n - 1)) * ((.n_1 * var_y_c) / (.n - .n_1) +
                          
                          (((.n - .n_1) * var_y_t) / .n_1) +
                          
                          (2 * cov_y_c_y_t))
  
  consv_var = (.n/(.n - 1)) * ((var_y_c) / (.n - .n_1) +
                                 
                                 ((var_y_t) / .n_1))
  
  return(list("var" = var, "consv_var" = consv_var))
  
}