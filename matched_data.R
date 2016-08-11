## Create a matched design for use in other handouts.

library(MASS)
library(RItools)
library(optmatch)
load(url("http://jakebowers.org/Matching/meddat.rda"))
meddat<-transform(meddat, HomRate03=(HomCount2003/Pop2003)*1000)
meddat<-transform(meddat, HomRate08=(HomCount2008/Pop2008)*1000)

# Ingredients: Distance Matrices

## Scalar distance on baseline outcome
tmp <- meddat$HomRate03
names(tmp) <- rownames(meddat)
absDist <- match_on(tmp, z = meddat$nhTrt)

## Propensity score using "bias-reduced" logistic regression (i.e. another
## form of penalized logistic regression.
## install.packages("brglm",dependencies=TRUE)
balfmla<-reformulate(c(names(meddat)[c(6:7,9:24)],"HomRate03"),response="nhTrt")

library(brglm)
brglm1<-brglm(balfmla,data=meddat,family=binomial)
pScore2<-predict(brglm1)
meddat$pScore2<-pScore2
psDist2<-match_on(nhTrt~pScore2,data=meddat)

## Overlapping propensity score plot
## with(meddat,boxplot(split(pScore2,nhTrt)))

## Mahalanobis distance
mahalDist<-match_on(balfmla,data=meddat,method="rank_mahalanobis")

fm1<-fullmatch(psDist2+caliper(psDist2,4)
	       +caliper(absDist,2)
	       +caliper(mahalDist,40),data=meddat,tol=.00001,min.controls=1)

summary(fm1)

xb1<-xBalance(balfmla,strata=list(fm1=~fm1),data=meddat,report="all")
meddat$fm1<-NULL ## this line exists to prevent confusion with new fm1 objects
meddat[names(fm1),"fm1"]<-fm1

fm2 <- fullmatch(psDist2+exactMatch(nhTrt~I(nhOwn>.5),data=meddat),data=meddat)

xb2<-xBalance(update(balfmla,.~.+pScore2),
	      strata=list(raw=NULL,fm1=~fm1),
	      data=meddat,
	      report=c("std.diffs","z.scores","adj.means",
		       "adj.mean.diffs", "chisquare.test","p.values"))
xb2$overall
zapsmall(xb2$results["HomRate03",,])
zapsmall(xb2$results["pScore2",,])

save(fm1,meddat,file="matched_Data.Rdata")
