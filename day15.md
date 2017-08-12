---
title: Information, Balance, Estimation, Testing
date: 'August 12, 2017'
author: ICPSR 2017 Session 2
bibliography:
 - refs.bib
 - BIB/master.bib
 - BIB/misc.bib
fontsize: 10pt
geometry: margin=1in
graphics: yes
biblio-style: authoryear-comp
output:
  beamer_presentation:
    slide_level: 2
    keep_tex: true
    latex_engine: xelatex
    citation_package: biblatex
    template: icpsr.beamer
    includes:
        in_header:
           - defs-all.sty
---


<!-- Make this document using library(rmarkdown); render("day12.Rmd") -->









\scriptsize

\scriptsize

## Today

\begin{enumerate}
\item Agenda: Estimation and Testing after matching; Plus more on  Navigating implementation tradeoffs in matching: Information and blocked designs (some blockings have more information about treatment effects than others); Balance tests and the Sequential Intersection Union Principle
\item Reading for this week: DOS 8--9, 13 and \cite[\S~9.5]{gelman2006dau}, and \cite{ho:etal:07} and \cite{hansen2011}.
\item Questions arising from the reading or assignments or life?
\item Next week: Sensitivity Analysis; Non-bipartite matching (matching where the treatment variable not binary).
\end{enumerate}


\scriptsize

# Matching structure and effective sample size

## Matching with a varying number of controls and full matching

### Fixed vs flexible ratio matching:

\begin{itemize}[<+->]
\item Pair matching \& sample size
\item Effective vs real sample size
\item If we limit ourselves to fixed matching ratios, we gain in
  simplicity but pay a price in sample size (effective \& real).
\item How big a price?  Trying is the best way to find out.
\end{itemize}

\scriptsize

## Matching with a varying number of controls

\begin{minipage}[t]{2in}
\begin{center}
Existing site\\
{\small
% latex table generated in R 3.0.2 by xtable 1.7-3 package
% Thu Jul 31 13:51:34 2014
\begin{tabular}{lrr}
  \hline
 & z.date & z.cap \\
  \hline
A & -1.6 & {1.2} {\mlpnode{NA}} \\
  B & -0.9 & {1.2} {\mlpnode{NB}} \\
  C & -0.4 & {0} {\mlpnode{NC}} \\
  D & -0.4 & {-1.4} {\mlpnode{ND}} \\
  E & 0.1 & {1.1} {\mlpnode{NE}} \\
  F & 2.2 & {0} {\mlpnode{NF}} \\
  G & 1.3 & {0} {\mlpnode{NG}} \\
   \hline
\end{tabular}}
\end{center}
\end{minipage}
\begin{minipage}[t]{2in}
\begin{center}
New site\\
{\scriptsize
% latex table generated in R 3.0.2 by xtable 1.7-3 package
% Thu Jul 31 13:51:34 2014
\begin{tabular}{lrr}
  \hline
 & z.date & z.cap \\
  \hline
{\mlpnode{NH}\mbox{}} {H} & -0.3 & -0.7 \\
  {\mlpnode{NI}\mbox{}} {I} & -1.6 & 1.2 \\
  {\mlpnode{NJ}\mbox{}} {J} & -0.9 & 1.2 \\
  {\mlpnode{NK}\mbox{}} {K} & -0.9 & -1.5 \\
  {\mlpnode{NL}\mbox{}} {L} & -0.7 & -0.0 \\
  {\mlpnode{NM}\mbox{}} {M} & -0.4 & -1.8 \\
  {\mlpnode{NN}\mbox{}} {N} & -0.5 & -0.2 \\
  {\mlpnode{NO}\mbox{}} {O} & -0.3 & -1.3 \\
  {\mlpnode{NP}\mbox{}} {P} & -0.1 & -0.2 \\
  {\mlpnode{NQ}\mbox{}} {Q} & -0.4 & -1.4 \\
  {\mlpnode{NR}\mbox{}} {R} & 0.1 & 1.1 \\
  {\mlpnode{NS}\mbox{}} {S} & 0.1 & 0.1 \\
  {\mlpnode{NT}\mbox{}} {T} & -0.4 & -0.2 \\
  {\mlpnode{NU}\mbox{}} {U} & 0.7 & 0.1 \\
  {\mlpnode{NV}\mbox{}} {V} & 0.4 & 1.3 \\
  {\mlpnode{NW}\mbox{}} {W} & -0.1 & 0.4 \\
  {\mlpnode{NX}\mbox{}} {X} & 0.9 & -0.2 \\
  {\mlpnode{NY}\mbox{}} {Y} & 1.7 & -1.4 \\
  {\mlpnode{NZ}\mbox{}} {Z} & 2.3 & 1.5 \\
   \hline
\end{tabular}}
\end{center}
\end{minipage}
\begin{tikzpicture}[overlay]
  \path[draw,gray] (NA) edge (NI);
 \path[draw,gray] (NB) edge (NJ);
 \path[draw,gray] (NC) edge (NH);
 \path[draw,gray] (NC) edge (NL);
 \path[draw,gray] (NC) edge (NN);
 \path[draw,gray] (NC) edge (NP);
 \path[draw,gray] (NC) edge (NS);
 \path[draw,gray] (NC) edge (NT);
 \path[draw,gray] (NC) edge (NW);
 \path[draw,gray] (ND) edge (NK);
 \path[draw,gray] (ND) edge (NM);
 \path[draw,gray] (ND) edge (NO);
 \path[draw,gray] (ND) edge (NQ);
 \path[draw,gray] (NE) edge (NR);
 \path[draw,gray] (NE) edge (NV);
 \path[draw,gray] (NF) edge (NZ);
 \path[draw,gray] (NG) edge (NU);
 \path[draw,gray] (NG) edge (NX);
 \path[draw,gray] (NG) edge (NY);
 \end{tikzpicture}

\scriptsize

```r
fmnukeMC1<-fullmatch(pr~date+cap,data=nuke.nopt,min.controls=1)
summary(fmnukeMC1)
```

```
Structure of matched sets:
 1:1  1:2  1:3  1:4 1:5+ 
   3    1    1    1    1 
Effective Sample Size:  9.18 
(equivalent number of matched pairs).
```


\note{
  Observe that now no control plants are left out.  (This is something you can change if you want.)

Discuss effective sample size.
}



## Matching so as to maximize effective sample size

\begin{minipage}[t]{2in}
\begin{center}
Existing site\\
{\small
% latex table generated in R 3.0.2 by xtable 1.7-3 package
% Thu Jul 31 13:51:34 2014
\begin{tabular}{lrr}
  \hline
 & z.date & z.cap \\
  \hline
A & -1.6 & {1.2} {\mlpnode{NA}} \\
  B & -0.9 & {1.2} {\mlpnode{NB}} \\
  C & -0.4 & {0} {\mlpnode{NC}} \\
  D & -0.4 & {-1.4} {\mlpnode{ND}} \\
  E & 0.1 & {1.1} {\mlpnode{NE}} \\
  F & 2.2 & {0} {\mlpnode{NF}} \\
  G & 1.3 & {0} {\mlpnode{NG}} \\
   \hline
\end{tabular}}
\end{center}
\bigskip
\bigskip
\bigskip
\bigskip
\end{minipage}
\begin{minipage}[t]{2in}
\begin{center}
New site\\
{\scriptsize
% latex table generated in R 3.0.2 by xtable 1.7-3 package
% Thu Jul 31 13:51:34 2014
\begin{tabular}{lrr}
  \hline
 & z.date & z.cap \\
  \hline
{\mlpnode{NH}\mbox{}} {H} & -0.3 & -0.7 \\
  {\mlpnode{NI}\mbox{}} {I} & -1.6 & 1.2 \\
  {\mlpnode{NJ}\mbox{}} {J} & -0.9 & 1.2 \\
  {\mlpnode{NK}\mbox{}} {K} & -0.9 & -1.5 \\
  {\mlpnode{NL}\mbox{}} {L} & -0.7 & -0.0 \\
  {\mlpnode{NM}\mbox{}} {M} & -0.4 & -1.8 \\
  {\mlpnode{NN}\mbox{}} {N} & -0.5 & -0.2 \\
  {\mlpnode{NO}\mbox{}} {O} & -0.3 & -1.3 \\
  {\mlpnode{NP}\mbox{}} {P} & -0.1 & -0.2 \\
  {\mlpnode{NQ}\mbox{}} {Q} & -0.4 & -1.4 \\
  {\mlpnode{NR}\mbox{}} {R} & 0.1 & 1.1 \\
  {\mlpnode{NS}\mbox{}} {S} & 0.1 & 0.1 \\
  {\mlpnode{NT}\mbox{}} {T} & -0.4 & -0.2 \\
  {\mlpnode{NU}\mbox{}} {U} & 0.7 & 0.1 \\
  {\mlpnode{NV}\mbox{}} {V} & 0.4 & 1.3 \\
  {\mlpnode{NW}\mbox{}} {W} & -0.1 & 0.4 \\
  {\mlpnode{NX}\mbox{}} {X} & 0.9 & -0.2 \\
  {\mlpnode{NY}\mbox{}} {Y} & 1.7 & -1.4 \\
  {\mlpnode{NZ}\mbox{}} {Z} & 2.3 & 1.5 \\
   \hline
\end{tabular}}
\end{center}
\end{minipage}
\begin{tikzpicture}[overlay]
  \path[draw,gray] (NA) edge (NI);
 \path[draw,gray] (NA) edge (NJ);
 \path[draw,gray] (NB) edge (NL);
 \path[draw,gray] (NB) edge (NN);
 \path[draw,gray] (NB) edge (NW);
 \path[draw,gray] (NC) edge (NH);
 \path[draw,gray] (NC) edge (NO);
 \path[draw,gray] (NC) edge (NT);
 \path[draw,gray] (ND) edge (NK);
 \path[draw,gray] (ND) edge (NM);
 \path[draw,gray] (ND) edge (NQ);
 \path[draw,gray] (NE) edge (NR);
 \path[draw,gray] (NE) edge (NS);
 \path[draw,gray] (NE) edge (NV);
 \path[draw,gray] (NF) edge (NY);
 \path[draw,gray] (NF) edge (NZ);
 \path[draw,gray] (NG) edge (NP);
 \path[draw,gray] (NG) edge (NU);
 \path[draw,gray] (NG) edge (NX);
 \end{tikzpicture}
\scriptsize

```r
fmnuke<-fullmatch(pr~date+cap, min=2, max=3, data=nuke.nopt)
summary(fmnuke)
```

```
Structure of matched sets:
1:2 1:3 
  2   5 
Effective Sample Size:  10.2 
(equivalent number of matched pairs).
```

## Showing matches

\centering
\scriptsize<img src="figs/figunnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width=".8\textwidth" />

## Matching so as to maximize effective sample size

\tiny
\scriptsize

```r
effectiveSampleSize(fmnuke)
```

```
[1] 10.17
```

```r
nukewts <- cbind(nuke.nopt,fmnuke) %>% 
  group_by(fmnuke) %>% summarise(nb = n(),
                                 nTb = sum(pr),
                                 nCb = nb - nTb,
                                 hwt = ( 2*( nCb * nTb ) / (nTb + nCb)))
nukewts
```

```
# A tibble: 7 x 5
  fmnuke    nb   nTb   nCb   hwt
  <fctr> <int> <dbl> <dbl> <dbl>
1    1.1     3     1     2 1.333
2    1.2     4     1     3 1.500
3    1.3     4     1     3 1.500
4    1.4     4     1     3 1.500
5    1.5     4     1     3 1.500
6    1.6     3     1     2 1.333
7    1.7     4     1     3 1.500
```

```r
dim(nukewts)
```

```
[1] 7 5
```

```r
sum(nukewts$hwt)
```

```
[1] 10.17
```

## Matching so as to maximize effective sample size

\scriptsize

```r
stratumStructure(fmnuke)
```

```
1:2 1:3 
  2   5 
```

So effective sample size for this match = $2 * 4/3 + 5* 3/2 = 10.17$ --- compare to 7 for pairs, 9.33 for triples.

\scriptsize

```r
nukemh <- match_on(pr~date+cap,data=nuke.nopt)
pmnuke <- pairmatch(nukemh,data=nuke.nopt)
levels(pmnuke)
```

```
[1] "1.1" "1.2" "1.3" "1.4" "1.5" "1.6" "1.7"
```

```r
effectiveSampleSize(pmnuke)
```

```
[1] 7
```

## Matching so as to maximize effective sample size

\scriptsize

```r
mds <- matched.distances(fmnuke,nukemh)
mean(unlist(mds))
```

```
[1] 0.6913
```

Mean of matched distances is 0.6913 --- compare to 0.29 for pairs, 0.57 for triples.

Note variance/bias tradeoff. Covariate balance has to do with bias and extrapolation (i.e. confounding). We want balance **and** precision.


## Inspect distances after matching

What kinds of distances remain after matching?

\scriptsize

```r
str(mds)
```

```
List of 7
 $ 1.1: Named num [1:2] 0 2.7
  ..- attr(*, "names")= chr [1:2] "I" "K"
 $ 1.2: Named num [1:3] 0 1.17 1.05
  ..- attr(*, "names")= chr [1:3] "J" "L" "W"
 $ 1.3: Named num [1:3] 0.17 0.369 0.21
  ..- attr(*, "names")= chr [1:3] "N" "P" "T"
 $ 1.4: Named num [1:3] 0.348 0.217 0
  ..- attr(*, "names")= chr [1:3] "M" "O" "Q"
 $ 1.5: Named num [1:3] 0 0.954 0.318
  ..- attr(*, "names")= chr [1:3] "R" "S" "V"
 $ 1.6: Named num [1:2] 1.47 1.45
  ..- attr(*, "names")= chr [1:2] "Y" "Z"
 $ 1.7: Named num [1:3] 1.654 0.602 0.449
  ..- attr(*, "names")= chr [1:3] "H" "U" "X"
 - attr(*, "dim")= int 7
 - attr(*, "dimnames")=List of 1
  ..$ : chr [1:7] "1.1" "1.2" "1.3" "1.4" ...
```

```r
quantile(unlist(mds))
```

```
    0%    25%    50%    75%   100% 
0.0000 0.1898 0.3693 1.1132 2.6972 
```


##  Tracking effective sample size

In 2-sample comparisons, total sample size can be a misleading as a measure of information content.  Example:
\begin{itemize}
\item say $Y$ has same variance, $\sigma^{2}$,in the Tx and the Ctl population.
\item Ben H. samples 10 Tx and 40 Ctls, and
\item Justin M. samples 25 Tx and 25 Ctls
\end{itemize}
--- so that total sample sizes are the same.  However,

\begin{eqnarray*}
  V_{BH}(\bar{y}_{t} - \bar{y}_{c}) &=& \frac{\sigma^{2}}{10} + \frac{\sigma^{2}}{40}=.125\sigma^{2}\mbox{;}\\
  V_{JM}(\bar{y}_{t} - \bar{y}_{c}) &=& \frac{\sigma^{2}}{25} + \frac{\sigma^{2}}{25}=.08\sigma^{2}.\\
\end{eqnarray*}

Similarly, a matched triple is roughly $[(\sigma^{2}/1 + \sigma^{2}/2)/(\sigma^{2}/1 + \sigma^{2}/1)]^{-1}= 1.33$ times as informative as a matched pair.

## Details

Use pooled 2-sample t statistic SE formula to compare 1-1 vs 1-2 matched sets' contribution to variance:

$$
\begin{array}{c|c}
  \atob{1}{1} & \atob{1}{2} \\
M^{-2}\sum_{m=1}^{M} (\sigma^{2}/1 + \sigma^{2}/1) & M^{-2}\sum_{m=1}^{M} (\sigma^{2}/1 + \sigma^{2}/2) \\
\frac{2\sigma^{2}}{M} & \frac{1.5\sigma^{2}}{M} \\
\end{array}
$$

So 20 matched pairs is comparable to 15 matched triples.

(Correspondingly, h-mean of $n_{t},n_{c}$ for a pair is 1, while for a triple it's $[(1/1 + 1/2)/2]^{-1}=4/3$.)

The variance of the `pr`-coeff in `v~pr + match` is
$$
 \frac{2 \sigma^{2}}{\sum_{s} h_{s}}, \hspace{3em} h_{s} = \left( \frac{n_{ts}^{-1} + n_{cs}^{-1} }{2}  \right)^{-1} ,
$$

assuming the OLS model and homoskedastic errors.  (This is b/c the anova formulation is equivalent to harmonic-mean weighting, under which $V(\sum_{s}w_{s}(\bar{v}_{ts} - \bar v_{cs})) = \sum_{s} w_{s}^{2}(n_{ts}^{-1} + n_{cs}^{-1}) \sigma^{2} = \sigma^{2} \sum_{s} w_{s}^{2} 2 h_{s}^{-1} = 2\sigma^{2} \sum_{s}w_{s}/\sum_{s}h_{s} = 2\sigma^{2}/\sum_{s} h_{s}$.)

For matched pairs, of course, $h_{s}=1$.  Harmonic mean of 1, 2 is $4/3$. Etc.

# Balance


 Hansen and Sales (2015) suggest one way to stop iterating between
	\texttt{fullmatch} and \texttt{xBalance} when you have one caliper. The idea
	is that if you would reject the null of balance with one caliper, you would
	also certainly reject it with a wider caliper. That is, the idea is that
	hypothesis tests about balance using calipers can be understood as nested,
	or ordered. Rosenbaum (2008) talks about this in his paper ``Testing
	Hypotheses in Order'' and Hansen and Sales (2008) how these ideas can
	help us choose a matched design:

  ``The SIUP[sequential intersection union principle] states that if a
  researcher pre-specifies a sequence of hypotheses and corresponding
  level-$\alpha$ tests, tests those hypotheses in order, and stops testing
  after the first non-rejected hypothesis, then the probability of incorrectly
  rejecting at least one correct hypothesis is at most $\alpha$.'' (page 2)

	Let us try this out and also try to assess it. Say, we start by saying that
	we will reject the null of balance at $\alpha=.50$.


Imagine, for example we had this matched design:

\scriptsize

```r
balfmla <- nhTrt ~ nhPopD + nhAboveHS + HomRate03
mhdist <- match_on(balfmla,data=meddat)
psmod <- arm::bayesglm(balfmla,data=meddat,family=binomial(link="logit"))
psdist <- match_on(psmod,data=meddat)
tmp <- meddat$HomRate03
names(tmp) <- rownames(meddat)
absdist <- match_on(tmp, z = meddat$nhTrt,data=meddat)

summary(psdist)
```

```
Membership: 22 treatment, 23 control
Total eligible potential matches: 506 
Total ineligible potential matches: 0 

Summary of minimum matchable distance per treatment member:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.002   0.032   0.105   0.406   0.362   4.345 
```

```r
summary(mhdist)
```

```
Membership: 22 treatment, 23 control
Total eligible potential matches: 506 
Total ineligible potential matches: 0 

Summary of minimum matchable distance per treatment member:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.113   0.312   0.589   0.926   0.960   5.519 
```

```r
summary(absdist)
```

```
Membership: 22 treatment, 23 control
Total eligible potential matches: 506 
Total ineligible potential matches: 0 

Summary of minimum matchable distance per treatment member:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.001   0.014   0.048   0.699   0.099   8.911 
```

```r
fmMh <- fullmatch(mhdist,data=meddat,tol=.00001)
summary(fmMh,min.controls=0,max.controls=Inf)
```

```
Structure of matched sets:
5:1 2:1 1:1 1:2 1:4 
  1   1  11   3   1 
Effective Sample Size:  19.6 
(equivalent number of matched pairs).
```

## Principled Balance Search using SIUP

If we just want to find the set of calipers and optmatch options which maximize
balance, why not do a search?

> The SIUP[sequential intersection union principle] states that if a researcher pre-specifies a sequence of hypotheses and corresponding level-$\alpha$ tests, tests those hypotheses in order, and stops testing after the first non-rejected hypothesis, then the probability of incorrectly rejecting at least one correct hypothesis is at most $\alpha$.'' \autocite{saleshansen2014}(page 2)


\scriptsize

```r
matchAndBalance<-function(x,balfmla,distmat,thedata){
	#x is a caliper width
	thefm<-fullmatch(distmat+caliper(distmat,x),data=thedata,tol=.00001)
  ## This next is very annoying but there are scope problems with balanceTest and balfmla
  ## And I don't want to add thefm to meddat at each iteration, which would really slow things down.
	thexb<-balanceTest(update(balfmla,.~.+strata(thefm)),
			data=cbind(thedata,thefm),
			report=c("chisquare.test"))
	return(c(x=x,d2p=thexb$overall["thefm","p.value"]))
}
```


## Balance Search using SIUP


\scriptsize

```r
## Start with the the largest distance between a treated and control unit.
maxpsdist<-max(as.vector(psdist))
minpsdist<-min(as.vector(psdist))
psdistsum <- summary(psdist)
quantile(as.vector(psdist),seq(0,1,.1))
```

```
      0%      10%      20%      30%      40%      50%      60%      70%      80%      90%     100% 
0.002004 0.211379 0.486704 0.797592 1.185445 1.402650 1.719665 2.197976 2.789425 3.653218 9.076767 
```

\scriptsize

```r
results1<-sapply(seq(3,minpsdist,length=100),function(thecal){
                   matchAndBalance(thecal,balfmla,distmat=psdist,thedata=meddat)})

apply(results1,1,summary)
```

```
               x     d2p
Min.    0.002004 0.08066
1st Qu. 0.751503 0.83745
Median  1.501002 0.86024
Mean    1.501002 0.84913
3rd Qu. 2.250501 0.89153
Max.    3.000000 0.99053
```

```r
apply(results1[,results1["d2p",]>.8],1,summary)
```

```
             x    d2p
Min.    0.1231 0.8167
1st Qu. 0.8651 0.8374
Median  1.5767 0.8630
Mean    1.5761 0.8699
3rd Qu. 2.2884 0.8915
Max.    3.0000 0.9905
```


\scriptsize

## Balance Search using SIUP

So, you can see that keeping the maximum produces a set of nested tests so
that if I reject some caliper at some $p$, I know that any caliper tighter
than the chosen one would have less balance (a smaller $p$, more information
against the null that our design is like a well randomized block randomized
study).


\scriptsize<img src="figs/figunnamed-chunk-20-1.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" width=".8\textwidth" />

\note{
  Sometimes we want our matched designs to relate well not only to an
	equivalent block-randomized experiment, but also to help us make the
	argument that our comparisons are comparing specific kinds of like
	with like and/or that our comparisons are statistically powerful.
	That is, among matched designs that we might call "balanced", we might
	one which drops the fewest observations, and perhaps one that has
	specially good balance on certain special covariates (like baseline
	outcomes). So, here is one example, of doing such a search.

	In this case, we are not doing strictly nested hypothesis testing, but are
	using the $p$ values to tell us about information against the null of
	balance rather than using them strictly speaking to reject this null, or
	not-reject it.
  }

## Design Search for both precision and balance

Here I demonstrate searching for two calipers.

\scriptsize

```r
findbalance<-function(x){
	##message(paste(x,collapse=" "))
	thefm<-try(fullmatch(psdist+caliper(mhdist,x[2])+caliper(psdist,x[1]),data=meddat,tol=.00001))

	if(inherits(thefm,"try-error")){
		return(c(x=x,d2p=NA,maxHR03diff=NA,n=NA,effn=NA))
	}

	thexb<-try(balanceTest(update(balfmla,.~.+strata(thefm)),
			    data=cbind(meddat,thefm),
			    report=c("chisquare.test","p.values")),silent=TRUE)

	if(inherits(thexb,"try-error")){
		return(c(x=x,d2p=NA,maxHR03diff=NA,n=NA,effn=NA))
	}

	maxHomRate03diff<-max(unlist(matched.distances(thefm,distance=absdist)))

	return(c(x=x,d2p=thexb$overall["thefm","p.value"],
           maxHR03diff=maxHomRate03diff,
           n=sum(!is.na(thefm)),
           effn=summary(thefm)$effective.sample.size))

}
```

\scriptsize

```
   user  system elapsed 
 96.744   0.977 102.715 
```

\scriptsize


## Which matched design might we prefer?

Now, how might we interpret the results of this search for matched designs?
Here are a few ideas.

\scriptsize

```r
if(class(results)=="list"){
resAnyNA<-sapply(results,function(x){ any(is.na(x)) })
resNoNA<-simplify2array(results[!resAnyNA])
} else {
resAnyNA<-apply(results,2,function(x){ any(is.na(x)) })
resNoNA<-simplify2array(results[,!resAnyNA])
}
apply(resNoNA,1,summary)
```

```
             x1     x2     d2p maxHR03diff    n  effn
Min.    0.01966 0.1253 0.03664      0.1822  2.0  1.00
1st Qu. 2.39106 1.8780 0.36788      2.2729 38.0 13.14
Median  4.64892 3.8220 0.70463      4.2171 43.0 13.40
Mean    4.58132 3.7425 0.62036      3.9373 38.9 12.60
3rd Qu. 6.77169 5.5706 0.83745      4.2171 44.0 13.42
Max.    9.06302 7.1751 0.98316     10.0664 45.0 14.15
```

```r
highbalres<-resNoNA[,resNoNA["d2p",]>.5]
apply(highbalres,1,summary)
```

```
            x1    x2    d2p maxHR03diff     n  effn
Min.    0.1096 0.386 0.5051      0.5357 17.00  7.50
1st Qu. 2.3493 3.579 0.6348      3.7767 42.00 13.36
Median  4.6654 4.892 0.8374      4.2171 44.00 13.42
Mean    4.5516 4.801 0.7748      4.9549 42.91 13.33
3rd Qu. 6.7280 6.082 0.8406      4.2171 44.00 13.43
Max.    9.0567 7.175 0.9832     10.0664 45.00 13.58
```

## Which matched design might we prefer?

\scriptsize<img src="figs/figunnamed-chunk-24-1.png" title="plot of chunk unnamed-chunk-24" alt="plot of chunk unnamed-chunk-24" width=".8\textwidth" />

\scriptsize

```r
interestingDesigns<- (resNoNA["d2p",]>.5 & resNoNA["n",]>=10 &
		      resNoNA["maxHR03diff",]<=1 & resNoNA["effn",] > 6)
candDesigns <- resNoNA[,interestingDesigns]
str(candDesigns)
```

```
 num [1:6, 1:2] 3.39 0.386 0.592 0.536 17 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:6] "x1" "x2" "d2p" "maxHR03diff" ...
  ..$ : NULL
```

```r
apply(candDesigns,1,summary)
```

```
           x1     x2    d2p maxHR03diff  n effn
Min.    3.390 0.3860 0.5924      0.5357 17  7.5
1st Qu. 3.861 0.3862 0.5924      0.5357 17  7.5
Median  4.331 0.3865 0.5924      0.5357 17  7.5
Mean    4.331 0.3865 0.5924      0.5357 17  7.5
3rd Qu. 4.801 0.3867 0.5924      0.5357 17  7.5
Max.    5.271 0.3870 0.5924      0.5357 17  7.5
```

```r
candDesigns<-candDesigns[,order(candDesigns["d2p",],decreasing=TRUE)]
```

## How would we use this information in \texttt{fullmatch}?

\scriptsize

```r
fm4<-fullmatch(psdist+caliper(psdist,candDesigns["x1",2])
	       +caliper(mhdist,candDesigns["x2",2]),data=meddat,tol=.00001)

summary(fm4,min.controls=0,max.controls=Inf)
```

```
Structure of matched sets:
1:0 3:1 2:1 1:1 1:2 0:1 
 13   1   1   2   2  15 
Effective Sample Size:  7.5 
(equivalent number of matched pairs).
```

```r
meddat$fm4<-NULL ## this line exists to prevent confusion with new fm4 objects
meddat[names(fm4),"fm4"]<-fm4

xb3<-balanceTest(update(balfmla,.~.+strata(fm4)),
	      data=meddat, report=c("all"))
xb3$overall[,1:3]
```

```
        chisquare df  p.value
fm4         1.905  3 0.592406
Unstrat    13.585  3 0.003529
```

```r
zapsmall(xb3$results["HomRate03",,])
```

```
           strata
stat           fm4 Unstrat
  Control   0.7642  0.9833
  Treatment 0.8860  1.8657
  std.diff  0.5291  0.5257
  adj.diff  0.1218  0.8824
  pooled.sd 0.2301  1.6785
  z         0.8825  1.7389
  p         1.0000  0.1641
```

## Another approach: more fine tuned optimization

\scriptsize

```r
matchAndBalance2<-function(x,distmat,alpha){
	#x is a caliper widths
	if(x>max(as.vector(distmat)) | x<min(as.vector(distmat))){ return(99999) }
	thefm<-fullmatch(distmat+caliper(distmat,x),data=meddat,tol=.00001)
	thexb<-xBalance(balfmla,
			strata=data.frame(thefm=thefm),
			data=meddat,
			report=c("chisquare.test"))
	return(thexb$overall[,"p.value"])
}

maxpfn<-function(x,distmat,alpha){
	## here x is the targeted caliper width and x2 is the next wider
	## caliper width
	p1<-matchAndBalance2(x=x[1],distmat,alpha)
	p2<-matchAndBalance2(x=x[2],distmat,alpha)
	return(abs( max(p1,p2) - alpha) )
}

maxpfn(c(minpsdist,minpsdist+1),distmat=psdist,alpha=.25)
```

```
[1] 0.6415
```

```r
quantile(as.vector(psdist),seq(0,1,.1))
```

```
      0%      10%      20%      30%      40%      50%      60%      70%      80%      90%     100% 
0.002004 0.211379 0.486704 0.797592 1.185445 1.402650 1.719665 2.197976 2.789425 3.653218 9.076767 
```

```r
sort(as.vector(psdist))[1:10]
```

```
 [1] 0.002004 0.002247 0.004788 0.006297 0.011016 0.014412 0.020658 0.023649 0.028959 0.030626
```

## Another approach: more fine tuned optimization

\scriptsize

```r
library(Rsolnp)
### This takes a long time
results3<-gosolnp(fun=maxpfn,
		ineqfun=function(x,distmat,alpha){ x[2] - x[1] },
		ineqLB = 0,
		ineqUB = maxpsdist,
		LB=c(minpsdist,minpsdist+.01),
		UB=c(maxpsdist-.01,maxpsdist),
		n.restarts=2,
		alpha=.25,distmat=psdist,
		n.sim=500,
		rseed=12345,
		control=list(trace=1)
		)
```

```

Calculating Random Initialization Parameters...ok!

Excluding Inequality Violations...

...Excluded 485/1000 Random Sequences

Evaluating Objective Function with Random Sampled Parameters...ok!

Sorting and Choosing Best Candidates for starting Solver...ok!

Starting Parameters and Starting Objective Function:
        par1  par2   objf
[1,] 0.01231 3.129 0.5874
[2,] 2.14655 4.163 0.5874

gosolnp-->Starting Solver

Iter: 1 fn: 0.5874	 Pars:  0.01231 3.12880
solnp--> Completed in 1 iterations

Iter: 1 fn: 0.5874	 Pars:  2.14655 4.16344
solnp--> Completed in 1 iterations

gosolnp-->Done!
```

## Another approach: more fine tuned optimization

\scriptsize

```r
maxpfn(results3$pars,distmat=psdist,alpha=.25)
```

```
[1] 0.5874
```

```r
matchAndBalance2(results3$pars[1],distmat=psdist,alpha=.25)
```

```
[1] 0.2828
```

```r
matchAndBalance(results3$par[1],balfmla=balfmla,distmat=psdist,thedata=meddat)
```

```
      x     d2p 
0.01231 0.28282 
```

# Estimation and Testing

## Overview: Estimate and Test "as if block-randomized"

This means we have to *define* our estimands in weighted terms (because different blocks provide different amounts of information).  Here are some matched sets and their associated weights:

\scriptsize

```r
meddat[names(fmMh),"fmMh"] <- fmMh
setmeanDiffs <- meddat %>% group_by(fmMh) %>%
  summarise(Y=mean(HomRate08[nhTrt==1])-mean(HomRate08[nhTrt==0]),
            nb=n(),
            nTb = sum(nhTrt),
            nCb = sum(1-nhTrt),
            hwt = ( 2*( nCb * nTb ) / (nTb + nCb))
            )
setmeanDiffs
```

```
# A tibble: 17 x 6
     fmMh        Y    nb   nTb   nCb   hwt
   <fctr>    <dbl> <int> <int> <dbl> <dbl>
 1    1.1 -0.01568     6     5     1 1.667
 2   1.10 -0.89826     2     1     1 1.000
 3   1.11  0.35066     2     1     1 1.000
 4   1.12 -1.10143     2     1     1 1.000
 5   1.14  0.18374     2     1     1 1.000
 6   1.15 -0.88037     2     1     1 1.000
 7   1.16 -0.35340     2     1     1 1.000
 8   1.17 -0.14819     3     1     2 1.333
 9   1.19 -0.06923     2     1     1 1.000
10    1.2 -0.27176     3     1     2 1.333
11   1.21 -0.07299     2     1     1 1.000
12   1.22 -0.09781     5     1     4 1.600
13    1.3 -0.35311     2     1     1 1.000
14    1.6 -0.18775     3     1     2 1.333
15    1.7 -0.17037     3     2     1 1.333
16    1.8 -1.62551     2     1     1 1.000
17    1.9  0.17668     2     1     1 1.000
```

## Using the weights: Set size weights

First, we could calculate the set-size weighted ATE:

\scriptsize

```r
## The set-size weighted version
with(setmeanDiffs, sum(Y*nb/sum(nb)))
```

```
[1] -0.2712
```

## Using the weights: Set size weights

Sometimes it is convenient to use `lm` because there are R functions for standard errors and confidence intervals. This requires a bit more work:

\scriptsize

```r
source("http://jakebowers.org/ICPSR/confintHC.R")
## See Gerber and Green section 4.5 and also Chapter 3 on block randomized experiments. Also Hansen and Bowers 2008.
meddat <- meddat %>% group_by(fmMh) %>% mutate(trtprob=mean(nhTrt),
                                               nbwt=nhTrt/trtprob + (1-nhTrt)/(1-trtprob),
                                               gghbwt= 2*( n()/nrow(meddat) )*(trtprob*(1-trtprob)), ## GG version,
                                               nb = n(),
                                               nTb = sum(nhTrt),
                                               nCb = nb - nTb,
                                               hbwt = ( 2*( nCb * nTb ) / (nTb + nCb))
                                               )
row.names(meddat) <- meddat$id ## dplyr strips row.names
```

```
Warning: Setting row names on a tibble is deprecated.
```

```r
lm0b<-lm(HomRate08~nhTrt,data=meddat,weight=nbwt)
coef(lm0b)["nhTrt"]
```

```
  nhTrt 
-0.2712 
```

```r
coeftest(lm0b,vcov=vcovHC(lm0b,type="HC2"))[1:2,]
```

```
            Estimate Std. Error t value  Pr(>|t|)
(Intercept)   0.6763     0.1271   5.322 0.0000035
nhTrt        -0.2712     0.1576  -1.721 0.0925142
```

```r
theci0 <- confint.HC(lm0b,parm="nhTrt",thevcov=vcovHC(lm0b,type="HC2"))
```


## Using the weights: Set size weights

There is an even more elaborate version of this that we illustrate below (from Winston Lin via the Green Lab SOP).

\scriptsize

```r
meddat <- meddat %>% group_by(fmMh) %>% mutate(fmwt=n()/nrow(meddat),nb=n())
X <- model.matrix(~fmMh-1,data=meddat)
XminusXbar <- apply(X,2,function(x){ x - mean(x) })
wrkdat <- cbind.data.frame(meddat,data.frame(XminusXbar))
tmpfmla <- reformulate(grep("fmMh1",names(wrkdat),value=TRUE)[-1],response="HomRate08")
lmfmla <- update(tmpfmla,.~nhTrt*(.))
lm0 <- lm(lmfmla,data=wrkdat)
coef(lm0)["nhTrt"]
```

```
  nhTrt 
-0.2712 
```

```r
## But, in this case, the HC2 Standard Error is undefined because some of our blocks have too few observations.
coeftest(lm0,vcov=vcovHC(lm0,type="HC2"))[1:2,]
```

```
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.6763        NaN     NaN      NaN
nhTrt        -0.2712        NaN     NaN      NaN
```

```r
## See Gerber and Green 4.5
meddat$Zf <- factor(meddat$trtprob)
Z <- model.matrix(~Zf-1,data=meddat)
ZminusZbar <- apply(Z,2,function(z){ z - mean(z) })
wrkdat <- cbind.data.frame(wrkdat,ZminusZbar)
tmpfmla <- reformulate(grep("Zf0",names(wrkdat),value=TRUE)[-1],response="HomRate08")
lmfmlaZ <- update(tmpfmla,.~nhTrt*(.))
lm0a <- lm(lmfmlaZ,data=wrkdat)
coef(lm0a)["nhTrt"]
```

```
  nhTrt 
-0.2712 
```

```r
coeftest(lm0a,vcov=vcovHC(lm0a,type="HC2"))[1:2,]
```

```
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.6763        NaN     NaN      NaN
nhTrt        -0.2712        NaN     NaN      NaN
```

## Using the weights: precision weights

Set size weighting is easy to explain. But set size weighting leaves information on the table. (For example, if we used a set-size weighted mean difference as a test statistic, the null reference distribution would be wider than if we used a harmonic weighted mean difference test statistic.) See this for example:

\scriptsize

```r
## The mean diff used as the observed value in the testing
## or from lm
with(setmeanDiffs, sum(Y*hwt/sum(hwt)))
```

```
[1] -0.2991
```

```r
lm1 <- lm(HomRate08~nhTrt+fmMh,data=meddat)
xbOutcome <- balanceTest(nhTrt~HomRate08+strata(fmMh),data=meddat,report="all")
## lm1a <- lm(HomRate08~nhTrt,data=meddat,weights=1/hbwt)
coeftest(lm1,parm="nhTrt",vcov=vcovHC(lm1,type="HC2"))["nhTrt",]
```

```
  Estimate Std. Error    t value   Pr(>|t|) 
  -0.29915    0.11429   -2.61748    0.01434 
```

```r
coeftest(lm0b,parm="nhTrt",vcov=vcovHC(lm0b,type="HC2"))["nhTrt",]
```

```
  Estimate Std. Error    t value   Pr(>|t|) 
  -0.27120    0.15762   -1.72059    0.09251 
```

```r
theci1 <- confint.HC(lm1,parm="nhTrt",thevcov=vcovHC(lm1,type="HC2"))
theci1
```

```
        2.5 %   97.5 %
nhTrt -0.5336 -0.06465
```

```r
diff(theci1[1,])
```

```
97.5 % 
 0.469 
```

```r
diff(theci0[1,]) ## set size weighting
```

```
97.5 % 
0.6357 
```

## `balanceTest`

`balanceTest` reports the ATT (the set mean differences weighted by number of treated) but uses the precision-weighted mean for the $p$-value.

\scriptsize

## The direct permutation approach

\scriptsize

```r
library(permute)

newExp <- function(z,b){
  n <- length(z)
  h1 <- how(blocks=b)
  z[shuffle(n,control=h1)]
}

hwtfn <- function(data){
  tapply(data$Tx.grp,data$stratum.code,function(x){ 2* sum((x-mean(x))^2) })
}

setsizewtfn<- function(data){
  tapply(data$Tx.grp,data$stratum.code,function(x){ length(x) })
}

wtMeanDiffTZ<-function(y,z,b,wtfn){
  tzb <- mapply(function(yb,zb){ mean(yb[zb==1]) - mean(yb[bz==0]) },
                y=split(y,b),
                z=split(z,b))
  wts <- wtfn(data.frame(Tx.grp=z,stratum.code=b))
  sum(tzb*wts/sum(wts))
}
```

\scriptsize

# Your reports

## Your reports?

What did you do? What kind of overall balance did you achieve so far? What did you do? What kinds of confidence intervals and/or hypothesis tests did you produce?

## Anything Else?

 - Recall the utility of `fill.NAs()`: You can and should match on missingness. No reason to throw away observations only because of covariate missingness.

 - EXTRA: How would we assess the claim that the sequential intersection union principle controls the family-wise error rate for balance tests?



## References
