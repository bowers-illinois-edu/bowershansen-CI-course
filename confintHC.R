confint.HC<-function (object, parm, level = 0.95, thevcov, ...) {
  ## a copy of the confint.lm function adding "thevcov" argument
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qt(a, object$df.residual)
    pct <- stats:::format.perc(a, 3)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
        pct))
    ## The original version extracts  the var-cov matrix itself
    ## ses <- sqrt(diag(vcov(object)))[parm]
    ses <- sqrt(diag(thevcov))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}

