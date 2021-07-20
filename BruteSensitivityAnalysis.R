
## Finally, here is a general but very slow version of Rosenbaum's sensitivity analysis. Here, I am using the harmonic weighted mean difference as a test statistic for tests of the sharp null of no effect.

source(url("http://jakebowers.org/ICPSR/bruteSens.R"))

tmpdat<-na.omit(meddat[,c("nhTrt","HomRate0803","bestfm")])

origProb<-unsplit(tapply(tmpdat$nhTrt,tmpdat$bestfm,mean),tmpdat$bestfm)

library(sampling)

obscoef<-coef(lm(HomRate0803~nhTrt+bestfm,data=tmpdat))[["nhTrt"]]

set.seed(12345)
G1rands<-replicate(5000,biasedsampleStrat(tmpdat$bestfm,origProb))
G1randdist<-apply(G1rands,2,function(newz){
			  coef(lm(HomRate0803~newz+bestfm,data=tmpdat))[["newz"]]
	 })
mean(G1randdist<=obscoef)

set.seed(12345)
G2p<-probTC(p=origProb,z=tmpdat$nhTrt,G=2,s=tmpdat$bestfm)
G2rands<-replicate(5000,biasedsampleStrat(tmpdat$bestfm,G2p))
G2randdist<-apply(G2rands,2,function(newz){
			  coef(lm(HomRate0803~newz+bestfm,data=tmpdat))[["newz"]]
	 })
mean(G2randdist<=obscoef)

set.seed(12345)
G3p<-probTC(p=origProb,z=tmpdat$nhTrt,G=3,s=tmpdat$bestfm)
G3rands<-replicate(5000,biasedsampleStrat(tmpdat$bestfm,G3p))
G3randdist<-apply(G3rands,2,function(newz){
			  coef(lm(HomRate0803~newz+bestfm,data=tmpdat))[["newz"]]
	 })
mean(G3randdist<=obscoef)

set.seed(12345)
G5p<-probTC(p=origProb,z=tmpdat$nhTrt,G=5,s=tmpdat$bestfm)
G5rands<-replicate(5000,biasedsampleStrat(tmpdat$bestfm,G5p))
G5randdist<-apply(G5rands,2,function(newz){
			  coef(lm(HomRate0803~newz+bestfm,data=tmpdat))[["newz"]]
	 })
mean(G5randdist<=obscoef)


#Here is a figure showing how the randomization distributions change as the odds of treatment assignment for the treated change:

boxplot(list(G1randdist,G2randdist,G3randdist,G5randdist))
abline(h=obscoef)

#And here are the results for a range of $\Gamma$ --- using the parallel package to speed up computation.


library(parallel)
options(mc.cores=4)

theGs<-c(1,2,3,4,5,10)

sensresults<-mclapply(theGs,function(theG){
			      ##sensresults<-sapply(theGs,function(theG){
			      message("odds are:",theG,appendLF=FALSE)
			      set.seed(12345)
			      Gtop<-probTC(p=origProb,z=tmpdat$nhTrt,G=theG,s=tmpdat$bestfm)
			      Grands<-replicate(5000,biasedsampleStrat(tmpdat$bestfm,Gtop))
			      Granddist<-apply(Grands,2,function(newz){
						       coef(lm(HomRate0803~newz+bestfm,data=tmpdat))[["newz"]]
				})
			      cbind(theG,mean(Granddist<=obscoef))
	 })

sensresults


