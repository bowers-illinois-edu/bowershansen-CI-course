# Some customization.  You can alter or delete as desired (if you know what you are doing).
# knitr settings to control how R chunks work.
rm(list = ls())

require(knitr)
## Setup for most r markdown files

## This plus size="\\scriptsize" from https://stackoverflow.com/questions/26372138/beamer-presentation-rstudio-change-font-size-for-chunk

knitr::knit_hooks$set(mysize = function(before, options, envir) {
  if (before) {
    return(options$size)
  } else {
    return("\\normalsize")
  }
})

knit_hooks$set(plotdefault = function(before, options, envir) {
  if (before) par(mar = c(3, 3, .1, .1), oma = rep(0, 4), mgp = c(1.5, .5, 0))
})

opts_chunk$set(
  tidy = "styler", # display code as typed
  echo = FALSE,
  results = "markup",
  strip.white = TRUE,
  fig.path = "figs/fig",
  cache = FALSE,
  highlight = TRUE,
  width.cutoff = 132,
  #size = "\\scriptsize",
  size = "\\normalsize",
  out.width = ".8\\textwidth",
  fig.retina = FALSE,
  message = FALSE,
  comment = NA,
  mysize = TRUE,
  plotdefault = TRUE
)

options(
  digits = 4,
  scipen = 8,
  width = 132
)
