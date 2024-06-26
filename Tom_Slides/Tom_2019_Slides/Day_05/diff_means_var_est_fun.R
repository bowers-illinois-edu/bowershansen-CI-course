diff_means_var_est <- function(.n,
                               .n_1,
                               .z,
                               .y) {
  
  obs_y_c = .y[which(.z == 0)]
  obs_y_t = .y[which(.z == 1)]
  
  est_mean_y_c = mean(obs_y_c)
  est_mean_y_t = mean(obs_y_t)
  
  est_var_y_c = ((.n - 1)/(.n * ((.n - .n_1) - 1))) * sum((obs_y_c - est_mean_y_c)^2)
  
  est_var_y_t = ((.n - 1)/(.n * (.n_1 - 1))) * sum((obs_y_t - est_mean_y_t)^2)
  
  return((.n/(.n - 1)) * ( (est_var_y_c/(.n - .n_1)) + (est_var_y_t/.n_1)))

  
}