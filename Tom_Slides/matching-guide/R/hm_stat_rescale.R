#' Rescale outcomes for harmonic-mean weighting
#' 
#' @param data   Data frame with outcome, treat, and strata
#' @param outcome Bare name of outcome column
#' @param treat   Bare name of 0/1 treatment column
#' @param strata  Bare name of matched set id column
#' @return Data frame of matched units with <outcome>_hm_scaled added

hm_stat_rescale <- function(data, outcome, treat, strata) {
  
  # get column names from bare names
  out_nm <- deparse(substitute(outcome))
  trt_nm <- deparse(substitute(treat))
  str_nm <- deparse(substitute(strata))
  
  # fail if columns missing
  need <- c(out_nm, trt_nm, str_nm)
  miss <- need[!(need %in% names(data))]
  if (length(miss)) stop("Column(s) not found in `data`: ", paste(miss, collapse = ", "))
  
  # drop unmatched units
  d <- data[!is.na(data[[str_nm]]), , drop = FALSE]
  
  # per set counts
  s <- d[[str_nm]]
  m_by <- tapply(d[[trt_nm]] == 1L, s, sum)
  n_by <- tapply(d[[trt_nm]] == 0L, s, sum)
  
  # keep sets with at least one treated and one control
  valid_sets <- names(which(m_by > 0 & n_by > 0))
  d <- d[s %in% valid_sets, , drop = FALSE]
  s <- d[[str_nm]]
  
  # recompute per set summaries on filtered data
  m_by <- tapply(d[[trt_nm]] == 1L, s, sum)
  n_by <- tapply(d[[trt_nm]] == 0L, s, sum)
  h_by <- 1 / (1 / m_by + 1 / n_by)
  sumy_by <- tapply(d[[out_nm]], s, sum)
  
  # total harmonic weight
  H <- sum(h_by)
  
  # per row adjustment using its set's values
  idx <- match(s, names(h_by))
  adj <- (h_by[idx] * sumy_by[idx]) / (m_by[idx] * n_by[idx])
  
  new_col <- paste0(out_nm, "_hm_scaled")
  d[[new_col]] <- (d[[out_nm]] - adj) / H
  
  d
}