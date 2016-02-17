## Unit 01, documenting with experiments

- TN-VPK debate
- Freedman et al 2012, coffee and mortality (abstract only)
- Lawlor et al 2004
- DOS Sec 1.1, 1.2
- DOE ch1
    1. Is it essential that assignment to treatment conditions come "last in time" of experimental manipulations, intentional or not, that might affect outcomes observed for experimental units?  Explain the role, for Fisher, of treatment assignment being last in time.  What does he recommend if they aren't last in time, and how does this affect the inference?
    2. Recall our in-class experiment from Monday.  Considering it as comparison of Ben's coffee and coffee-making technique to that of the Statistics Department staff, critique in light of Fisher's argument from the first paragraph of section 9, "Randomisation". 
    3.  "to insist that 'all the cups must be exactly alike' in every respect except that to be tested... is a totally impossible requirement in our example, and equally in all other forms of experimentation" [p.18].  Does this mean that alikeness in respects other than the treatment/control distinction doesn't matter? If not, ie if it does matter, then should it concern us that it's not possible?  Explain.
    4.  "no isolated experiment... can suffice for the experimental demonstration of any natural phenomenon..." [p.13]. What's Fisher's point here?  Would he/can you relate it to replicability, and/or contemporary debates or discussions of replicability? 
    5.  citing Mosteller, Rosenbaum (DOS, pp.33-35) interprets Fisher's phrase "reasoned basis for inference" to indicate the absence of assumptions, and/or the possibility of proof.  Do you agree.  If not, how would you characterize the overarching theme of  _Design of Experiments_, Chapter 1?
- DOS Sec 2.1, 2.3. 
- P. Holland  (1986), "Statistics and Causal Inference," Sections 1-4 (pp. 945-949); 
- P. Holland (2003), "The false linking of race and causality: lessons from standardized testing,"  Sections 1-3 (pp. 219-228).

##  Unit 02, estimation in experiments

- Readings on probabilistic analysis of completely randomized designs:
    - Chapter 2 of Gerber and Green (2012), _Field experiments: Design, analysis, and interpretation_.  Read for understanding of text boxes 2.3, 2.4 and 2.5. (Stats PhD students will be able to skim it, while social scientists will want to read it more carefully.  If you haven't calculated expected values before, or it's a while since you have, then you should expect to have to through one or more of the examples for yourself, to get the hang of things.)  
    - (For Stats PhD students.) Footnote 11, ppA32-32, Freedman, Purves and Pisani (1998), _Statistics_.  
    - (For social scientists.)  Chapter 6, section 6.0 and 6.1.0 (pp.2-12 in the PDF) of Dunning (2012), _Natural experiments in the social sciences: A design-based approach_, on simple random sampling and completely randomized designs.     

- More about probabilistic analysis of completely randomized designs: 
    - Fincan et al (1974), "Moments without tears in simple random sampling from a finite population."  
How you approach this should depend on your mathematical background. Everyone should read the first page and a half.  Stats PhDs should expect to linger a bit, to get the hand of the flavor of the arguments; they'll also go on to selectively read some later parts of the paper, as they later work on the exercises below.  There are somewhat different versions of the exercises depending on your mathematical background.

     1. (Everyone) Suppose $x_1, x_2, \ldots, x_n$ are observations in a simple random sample (without replacement) from a population of size $N$.  Derive expressions for $\mathbf{E}(\bar x)$ and $\mathrm{Var}(\bar x)$, in terms of $n$, $N$ and the population mean and variance $\mu_x$ and $\sigma^2_x$. Hint: for the variance, you can adapt formulas given by Finucan et al 1974, first page or two, for the variance of the sample _total_.
     2.  (Stats PhD students, & other mathy types.) Also, assuming that $y_1, x_2, \ldots, y_n$ are observations on the same sample, and that $\sigma_{xy}$ is the population covariance of $x$'s and $y$'s, derive an expression for $\mathrm{Cov}(\bar x, \bar y) $.  Justify your result with a mathematically correct and complete argument.  You're encouraged to use and adapt Finucan et al's style of reasoning.  
     3. (Everyone)  Derive a formula for $\mathbf{E}(s^2_x)$, the expected value of the sample variance  $s^2_x = [(x_1 - \bar x)^2 + \cdots + (x_n - \bar x)^2 ]/(n-1) $.  If you're a social scientist, you may use the formula Finucan et al give for $\mathbf{E}(v_n)$ in section 4 of their paper.  If you're a Stats PhD student, use the arguments of that section of the paper but not that specific result.
     4. (Social scientists) Do exercises 4-6 of the Unit 1 R exercises. 

- Prep for next time: 
    - Little and Yau 1998 pp.147-151 (1-5 of PDF), on "Bloom's method". 
    - Middleton and Aronow (2015), pp.39-47 (1-9 of PDF). 

- Prep for next time:
    -  Section 1.1, pp. 874-875, of [my 2009 paper with Jake Bowers](http://www.stat.lsa.umich.edu/~bbh/hansenBowers2009.pdf), "Attributing effects to a cluster-randomized get out the vote campaign." 

- Exercises:
   1.  Use the `acorn` data set to give an unbiased estimate of the effect (ACE) of the campaign at the precinct level, on turnout percentage. 
   2. Attach to your estimate a design-based standard error (SE) that's based on (i.e. the square root of) an unbiased estimate _or overestimate_ of your ACE estimator's sampling variance.
   3. Use the `acorn` data set to give an unbiased estimate of the the campaign's ACE, now expressed in total votes irrespective of precinct, on the entire study population.
   4. Attach to your estimate a design-based standard error (SE) that's based on (i.e. the square root of) an unbiased estimate _or overestimate_ of your ACE estimator's sampling variance.
   3.  Use the `acorn` data set to give an unbiased estimate of the total number of votes attributable to the 2003 Kansas City GOTV campaign.
   6. Attach to your estimate a design-based SE that's based on (i.e. the square root of) an unbiased estimate of the variance (of the underlying estimate of voter turnout would have been if, counter to fact, there had been no GOTV campaign). 
   7. Estimate the number of votes _per voter contact_  that were generated by the campaign. Is your estimate unbiased? If not, could you have estimated it without bias (using any of the techniques  we've studied, or a simple modification of one of them)?
   8. Estimate the CACE associated with this campaign (as averaged across compliers in the treatment and control groups).   Is your estimate unbiased? If not, could you have estimated it without bias (using any of the techniques  we've studied, or a simple modification of one of them)?
   9. Imagine that, due to a logistical slipup in the GOTV campaign, canvassers were sent to one of the 14 precincts that had been assigned to control, while one of the 14 treatment precincts did not get any canvassers.  Can we still estimate the votes per contact using same techniques as were applicable otherwise?  How about the CACE?  (Hint: In practice your preferred answer might depend on how "random" the logistical slipup seemed to be.  Make sure your answer addresses the circumstance that it was not plausibly a random event -- e.g., canvassers decided that they'd prefer to canvass in a precinct randomized to control rather than one of the precincts that had been allocated to treatment.) 
   10. **Either** (a) Use formula 21 of Schochet and Chiang (2011) to estimate a standard error for your CACE; or (b) use the characterization given by Middleton and Aronow of the bias of ratio estimators to assess how large your CACE estimator's bias might plausibly be. (Afterwards it will be interesting to compare the results of these two exercises.)

## Unit 03, models of effects

- BFP Sec 1-4??