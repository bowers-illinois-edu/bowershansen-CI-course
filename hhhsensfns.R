specpars.ps <- function(dat, covs, dropcov, type=NULL,outcome,treatment,
			fitfunction=glm,thecaliper=NULL,fmcontrol=NULL,fmmaker=NULL,...){
  ## outcome, treatment, dropcov are character names of variables in dat
  ## covs is a vector of character names of covariates
  ## dat is a data frame
  ## type says whether the covariate is a factor (2) or numeric (1)
  ## fmfmla is either psdist or psdist+caliper(...)

  require(lmtest)
  require(sandwich)

  if(is.null(type)){ type<-1 }

  ##New dataset without the cov
  newdat <- dat[,-grep(dropcov,names(dat))]
  newcovs <- covs[ -grep(dropcov,covs)]

  if(is.null(fmmaker)){
  ##Make distances without the cov
  newfmla<-reformulate(newcovs,response=treatment)
  psmod<-fitfunction(newfmla,data=newdat,...)
  psdist<-match_on(psmod,data=newdat)
  if(!is.null(thecaliper)){
    psdist<-psdist+caliper(psdist,thecaliper)
  }

  ##Do matching
  if(is.null(fmcontrol)){
  thefm<-fullmatch(psdist, data=newdat)}
  else{
    fmcontrol[["x"]]<-psdist
    fmcontrol[["data"]]<-newdat
    thefm<-do.call("fullmatch",fmcontrol)
  }
  } else {
   thefm<-fmmaker(newdat,newcovs,treatment=treatment,...)
  }



  dat[names(thefm),"thefm"]<-thefm

  mod2fmla<-reformulate(c(treatment,"thefm"),response=outcome)
  mod1fmla<-reformulate(c(treatment,"thefm",dropcov),response=outcome)

  ##Fit calibration models:
  ##trtmod = lm(infhrs~postbomb+thefm+dat[,dropcov], data=dat)
  trtmod = lm(mod1fmla, data=dat)
  r.with = summary(trtmod)$r.squared
  b.add = coef(trtmod)[treatment]
  se.b.add = coeftest(trtmod,vcov.=vcovHC(trtmod,type="HC2"))[treatment,"Std. Error"]

  ## trtmod2 = lm(infhrs~postbomb+thefm, data=dat)
  trtmod2 = lm(mod2fmla, data=dat)
  r.without = summary(trtmod2)$r.square
  b = coef(trtmod2)[treatment]
  se.b = coeftest(trtmod2,vcov.=vcovHC(trtmod2,type="HC2"))[treatment,"Std. Error"]

  r.par = (r.with - r.without)/(1-r.without)

  if(type==1){
    bigmodfmla<-reformulate(c(dropcov,"thefm"),response=treatment)
    ##bigmod = lm(postbomb~dat[,dropcov]+thefm, data=dat)
    bigmod = lm(bigmodfmla, data=dat)
    t.w = coeftest(bigmod,vcov.=vcovHC(bigmod,type="HC2"))[dropcov,"t value"]
    df =  bigmod$df 
  }

  if(type==2){
    smmodfmla<-reformulate(c("thefm"),response=treatment)
    smmod = lm(smmodfmla, data=dat)
    bigmodfmla<-reformulate(c(dropcov,"thefm"),response=treatment)
    ##bigmod = lm(postbomb~dat[,dropcov]+thefm, data=dat)
    bigmod = lm(bigmodfmla, data=dat)
    Fw=((deviance(smmod)-deviance(bigmod))/(deviance(bigmod)/df.residual(bigmod))) 
    df = summary(bigmod)$df[2] 
    k= length(levels(dat[,dropcov]))-1 
    t.w = sqrt((k*df/(df+1-k))*Fw)
  }

  specpars = cbind(r.par, t.w, b, se.b, df, b.add, se.b.add)

  return(specpars)
}

make.ci = function(dat, specpars, covnm, tquant, r.par)
{
  T = abs(as.numeric(specpars["t.w",covnm]))
  k = length(levels(dat[,covnm]))-1
  df = specpars["df",covnm]

  g = (T^2*(df+k))/(T^2*(df+k) + tquant^2*(T^2+df))

  if(r.par<=g) {
    ci = cor2(dat, specpars, covnm, tquant, r.par)
  }
  if(r.par>g) {
    ci = cor1(dat, specpars, covnm, tquant)
  }

  return(ci)
}


cor2 = function(dat, specpars, covnm, tquant, r.par)
{
  T = abs(as.numeric(specpars["t.w",covnm]))
  df = specpars["df",covnm]
  b = specpars["b",covnm]
  se.b = specpars["se.b",covnm]

  adj1 = T*sqrt(r.par)
  adj2 = tquant*sqrt(((T^2)+df)/(df-1))*sqrt(1-r.par)
  adj = adj1+adj2

  lb = b - adj*se.b
  ub = b + adj*se.b

  ci = cbind(lb,ub)
  return(ci)
} 

cor1 = function(dat, specpars, covnm, tquant) 
{ 
  T = abs(as.numeric(specpars["t.w",covnm]))
  df = specpars["df",covnm]
  b = specpars["b",covnm]
  se.b = specpars["se.b",covnm]

  k = as.numeric(length(names(dat))-1)

  adj = sqrt((T^2)+(tquant^2)*(((T^2)+df-k+1)/(df-k))) 

  lb = b - adj*se.b 
  ub = b + adj*se.b 

  ci = cbind(lb,ub) 
  return(ci) 
} 


