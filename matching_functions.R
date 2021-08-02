## Some helper functions, especially for use with designmatch objects

## Convert the output into a factor variable for use in analysis
nmatch_to_dat <- function(obj, origid) {
  ## We want a factor that we can merge onto our
  ## existing dataset. Here returning a data.frame so that
  ## we can merge --- seems less error prone than using
  ## rownames even if it is slower.
  matchesdat <- data.frame(
    bm = obj$group_id,
    match_id = c(obj$id_1, obj$id_2)
  )
  matchesdat$id <- origid[matchesdat$match_id]
  return(matchesdat)
}


bmatch_to_dat <- function(obj, origid) {
  ## This is pair matching. Make sure of it.
  stopifnot(length(obj$t_id) == length(obj$c_id))
  ## We want a factor that we can merge onto our
  ## existing dataset. Here returning a data.frame so that
  ## we can merge --- seems less error prone than using
  ## rownames even if it is slower.
  matchesdat <- data.frame(
    bm = obj$group_id,
    match_id = c(obj$t_id, obj$c_id)
  )
  matchesdat$id <- origid[matchesdat$match_id]
  return(matchesdat)
}
