


#' Given a vector of existing probabilities, produce odds, and then
#' adjust odds, and convert back to probabilities of assignment.
#' @param p is a scalar probability 0<=p<=1 (not sure the point of allowing 0 or 1)
#' @param z is a vector of 0 and 1 or FALSE and TRUE indicating treatment assignment
#' @param s is a strata factor like a matched set or blocking factor
#' @param G multiplies the odds of treatment assignment
#' @return  an nx1 vector (where n is length(z) where each unit has a new probability of treatment.
probTC<-function(p,z,s,G){
  odds<-p/(1-p) ## calculate the odds of treatment
  newodds<-G*odds ## new odds to reflect the WhatIf thought experiment
  if(identical(newodds,odds) & G==1){ 
    return(p) ## control and treated all get p
  } else {
    newpt<-newodds/(1+newodds) ## convert odds back to probabilities
    ## Make sure that prob of treatment+prob control adds to 1 within strata
    ## so prob of treatment for controls must go down if prob of treatment for
    ## treated goes up
    ptpc<-unsplit(lapply(split(data.frame(z=z,probt=newpt),s),
			 function(dat){
			   ##stopifnot(length(unique(dat$probt))==1)
			   with(dat,
				z*probt + (1-z)*(1-unique(probt))
				)
			 }),
		  s)
    return(ptpc)
  }
}


#' Sample without replacement from a vector with fixed probabilities of
#' treatment
#' @param prob is the probability of treatment assignment. It should be n x 1.
#' @return A vector the same length as the vector prob with a draw from the
#' set of possible samples consistent with the vector of probabilities.
biasedsample<-function(prob){
	## prob is the translation from odds to probabilities of treatment
	require(sampling)
	newz<-UPrandomsystematic(prob)
	newz
}

#' Execute biased sampling across sets/blocks/strata
#' @param set a vector (ideally a factor) indicating set membership
#' @param prob a vector of probabilities of treatment assignment the same
#' length as set
#' @return a vector indicating new treatment status 
biasedsampleStrat<-function(set,prob){ 
  unsplit(
	  lapply(split(prob,set),function(ps){
		 biasedsample(ps)
			 }),
	  set)
}




