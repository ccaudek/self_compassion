## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

##########
### Mauricio Garnier-Villarreal & Terrence Jorgensen
### last edited: March 21, 2019
### Adapting Fit Indices for Bayesian Structural Equation Modeling: Comparison to Maximum Likelihood


library(lavaan)
library(blavaan)
library(semTools)
library(simsem)
library(coda)
library(rstan)
options(mc.cores = parallel::detectCores()) ## Run chains in parallel

summary(HolzingerSwineford1939)

# Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
              
'

### ML example
fit_ml <- cfa(
  HS.model, 
  data=HolzingerSwineford1939,
  std.lv=T,
  meanstructure=T
)

summary(fit_ml, fit.measures=T)
fitmeasures(fit_ml)
## save the fit indices
fffs_ml <- c(fitmeasures(fit_ml), moreFitIndices(fit_ml))


### MCMC example, with Gibbs sampler (JAGS)
fit <- bcfa(HS.model, data=HolzingerSwineford1939,
            std.lv=T, 
            burnin=1500, sample=2000)
summary(fit,neff=T)
fitMeasures(fit)

### MCMC null model: needed to extimate the incremental fit indices
fit_null <- bcfa(lav_partable_independence(fit_ml), 
                 data=HolzingerSwineford1939,
                 burnin=1500, sample=2000)
summary(fit_null,neff=T)
fitMeasures(fit_null)


## Compute bayesian approximate fit indices

## pD = LOO
## method = DevM
ff_dv <- blavFitIndices(fit, baseline.model=fit_null, pD="loo", rescale="devM")
ff_dv
summary(ff_dv) ### summary with the 3 central tendency measures
sm_dv <- summary(ff_dv, central.tendency="mean") ## save summary of fit indices
sm_dv

## pD = LOO
## method = PPMC
ff_pp <- blavFitIndices(fit, baseline.model=fit_null, pD="loo", rescale="ppmc")
ff_pp
summary(ff_pp) ### summary with the 3 central tendency measures
sm_pp <- summary(ff_pp, central.tendency="mean") ## save summary of fit indices
sm_pp

## save ML fit indices for comparison
ff_sum <- fffs_ml[c("chisq","rmsea",
                    "gammaHat","adjGammaHat",
                    "mfi","cfi","tli","nfi")]
round(fffs_ml,3)

## combine ML, DevM, and PPMC fit indices in one table
fit_all <- cbind(ff_sum, 
                 rbind(chisq=c(mean(ff_dv@details$chisq),sd(ff_dv@details$chisq),
                               HPDinterval(as.mcmc(ff_dv@details$chisq),prob=.9)),
                       sm_dv), 
                 rbind(chisq=c(mean(ff_pp@details$chisq),sd(ff_pp@details$chisq),
                               HPDinterval(as.mcmc(ff_pp@details$chisq),prob=.9)),
                       sm_pp))
colnames(fit_all) <- c("ML", 
                       paste("DevM",c("mean","sd","90%LB","90%UB"),sep="_"),
                       paste("PPMC",c("mean","sd","90%LB","90%UB"),sep="_"))
round(fit_all,3)

fffs_ml["npar"] ## ML number of parameters
ff_dv@details$pD ## MCMC number of parameters

fffs_ml["df"] ## ML degrees of freedom
ff_dv@details$df ## MCMC degrees of freedom



##### Different prior results

# wd - Use different non-informative priors for all parameters (e.g., use uniform priors and run Stan instead of JAGS, to see if the results are the same).  I expect results will not differ.
# crp - Use our current posterior estimates as informative priors for all parameters, to see the effect of incorporating information that is consistent with the data (i.e., "correct" priors).  I expect the fit indices will have the same posterior means but smaller posterior SDs.
# oth - Use "incorrect" informative priors reflecting the strong assumption of nearly orthogonal factors (so 3 informative near-zero small-variance priors for the factor correlations, but noninformative priors for all other parameters).  This will make the model fit poorly, which I expect will be reflected in the model fit indices.  As a comparison, we can also fit an orthogonal-factors model using MLE and see whether fit looks worse because MLE uses "priors" with zero variance instead of small variance.
# cl - Use small-variance priors for all possible cross-loadings, since that is something popularized by Bengt that many people who want to use our indices will certainly do (as Reviewer 3 correctly fears).  This is distinct from the certainly-incorrect priors in the orthogonal-factors model above because these are "approximately correct" priors, so I think this might even provide the most interesting results to discuss.


## function to summarize the chi-square realized values distribution
chisq_sum <- function(x, prob=.9, decimals=3){
  temp <- c(mean=mean(x@details$chisq),sd=sd(x@details$chisq),
            HPD=HPDinterval(as.mcmc(x@details$chisq),prob=prob))
  return(round(temp,decimals))
}


### MCMC example, with NUTS sampler (Stan)
fit_st <- bcfa(HS.model, data=HolzingerSwineford1939,
               std.lv=T, target="stan",
               burnin=1000, sample=1000) 
summary(fit_st,neff=T, standardized=T)
fitMeasures(fit_st)


neffs <- cbind(blavInspect(fit_st, "neff"), blavInspect(fit, "neff"), 
               blavInspect(fit_st, "neff") - blavInspect(fit, "neff"))
colMeans(neffs)


### Stan MCMC null model
fit_null_st <- bcfa(lav_partable_independence(fit_ml), 
                    data=HolzingerSwineford1939,target="stan",
                    burnin=1000, sample=1000,
                    dp=dpriors(lambda="normal(0,10)",
                               beta="normal(0,10)",
                               nu="normal(0,10)",
                               target="stan"))
summary(fit_null_st,neff=T)
fitMeasures(fit_null_st)

fit_st_null <- blavFitIndices(fit_null_st, pD="loo", rescale="devM")
summary(fit_st_null)

chisq_sum(fit_st_null)

## compute bayesian approximate fit indices
## pD = LOO
## method = DevM
ff_dv_st <- blavFitIndices(fit_st, baseline.model=fit_null_st, pD="loo", rescale="devM")
ff_dv_st
summary(ff_dv_st) ### summary with the 3 central tendency measures
sm_dv_st <- summary(ff_dv_st, central.tendency="mean") ## save summary of fit indices
sm_dv_st

## pD = LOO
## method = PPMC
ff_pp_st <- blavFitIndices(fit_st, baseline.model=fit_null_st, pD="loo", rescale="ppmc")
ff_pp_st
summary(ff_pp_st) ### summary with the 3 central tendency measures
sm_pp_st <- summary(ff_pp_st, central.tendency="mean") ## save summary of fit indices
sm_pp_st

## save ML fit indices for comparison
ff_sum <- fffs_ml[c("chisq","rmsea",
                    "gammaHat","adjGammaHat",
                    "mfi","cfi","tli","nfi")]
round(fffs_ml,3)

## combine ML, DevM, and PPMC fit indices in one table
fit_all_st <- cbind(ff_sum, 
                    rbind(chisq=c(mean(ff_dv_st@details$chisq),sd(ff_dv_st@details$chisq),
                                  HPDinterval(as.mcmc(ff_dv_st@details$chisq),prob=.9)),
                          sm_dv_st), 
                    rbind(chisq=c(mean(ff_pp_st@details$chisq),sd(ff_pp_st@details$chisq),
                                  HPDinterval(as.mcmc(ff_pp_st@details$chisq),prob=.9)),
                          sm_pp_st))
colnames(fit_all_st) <- c("ML", 
                          paste("DevM",c("mean","sd","90%LB","90%UB"),sep="_"),
                          paste("PPMC",c("mean","sd","90%LB","90%UB"),sep="_"))
round(fit_all_st,3)

fffs_ml["npar"] ## ML number of parameters
ff_dv_st@details$pD ## MCMC number of parameters

fffs_ml["df"] ## ML degrees of freedom
ff_dv_st@details$df ## MCMC degrees of freedom


#########
### Wide priors
##########
fit_st_wd <- bcfa(HS.model, data=HolzingerSwineford1939,
                  std.lv=T, target="stan",
                  burnin=10000, sample=10000,
                  dp=dpriors(lambda="normal(0,1000)",
                             beta="normal(0,1000)",
                             itheta="uniform(0.01,1000)[sd]",
                             nu="normal(0,1000)",
                             ipsi="uniform(0.01,1000)[sd]", target="stan") )
summary(fit_st_wd,neff=T)


## method = DevM
ff_dv_st_wd <- blavFitIndices(fit_st_wd, baseline.model=fit_null_st, pD="loo", rescale="devM")
ff_dv_st_wd
summary(ff_dv_st_wd)
ff_dv_st_wd@details$pD

blavCompare(fit_st_wd, fit_st)


chisq_sum(ff_dv_st_wd)

########
## cros-loadings with small variances
########
HS.model_cl <- ' 
visual  =~ x1 + x2 + x3 + prior("normal(0,.1)")*x4 + prior("normal(0,.1)")*x5 + prior("normal(0,.1)")*x6 + prior("normal(0,.1)")*x7 + prior("normal(0,.1)")*x8 + prior("normal(0,.1)")*x9
textual =~ x4 + x5 + x6 + prior("normal(0,.1)")*x1 + prior("normal(0,.1)")*x2 + prior("normal(0,.1)")*x3 + prior("normal(0,.1)")*x7 + prior("normal(0,.1)")*x8 + prior("normal(0,.1)")*x9
speed   =~ x7 + x8 + x9 + prior("normal(0,.1)")*x4 + prior("normal(0,.1)")*x5 + prior("normal(0,.1)")*x6 + prior("normal(0,.1)")*x1 + prior("normal(0,.1)")*x2 + prior("normal(0,.1)")*x3 
'

fit_st_cl <- bcfa(HS.model_cl, data=HolzingerSwineford1939,
                  std.lv=T, target="stan",
                  burnin=10000, sample=10000,
                  dp=dpriors(lambda="normal(0,10)",
                             beta="normal(0,10)",
                             itheta="cauchy(0,2.5)[sd]",
                             nu="normal(0,10)",
                             ipsi="cauchy(0,2.5)[sd]", target="stan") )
summary(fit_st_cl,neff=T, standardized=T)
fitMeasures(fit_st_cl)


## method = DevM
ff_dv_st_cl <- blavFitIndices(fit_st_cl, baseline.model=fit_null_st, pD="loo", rescale="devM")
ff_dv_st_cl
summary(ff_dv_st_cl)
ff_dv_st_cl@details$pD

blavCompare(fit_st_cl, fit_st)


summary(ff_dv_st_cl)$EAP - summary(ff_dv_st_wd)$EAP

chisq_sum(ff_dv_st_cl)

###########
## Orthogonal correlations
###########
### prior that 95% of the correlation distribution is between -.1 and .1
pr_cr <- -1 + 2*rbeta(10000, 200, 200)
hist(pr_cr)
sum(pr_cr > -.1 & pr_cr < .1)/10000

fit_st_oth <- bcfa(HS.model, data=HolzingerSwineford1939,
                   std.lv=T, target="stan",
                   burnin=10000, sample=10000,
                   dp=dpriors(lambda="normal(0,10)",
                              beta="normal(0,10)",
                              itheta="cauchy(0,2.5)[sd]",
                              nu="normal(0,10)",
                              rho="beta(200, 200)",
                              ipsi="cauchy(0,2.5)[sd]", target="stan") )
summary(fit_st_oth,neff=T, standardized=T)
fitMeasures(fit_st_oth)

blavCompare(fit_st_oth, fit_st)

## method = DevM
ff_dv_st_oth <- blavFitIndices(fit_st_oth, baseline.model=fit_null_st, pD="loo", rescale="devM")
ff_dv_st_oth
summary(ff_dv_st_oth)
ff_dv_st_oth@details$pD

# ML orthogonal
fit_ml_oth <- cfa(HS.model, data=HolzingerSwineford1939,
                  std.lv=T, orthogonal=T)
summary(fit_ml_oth)
fitMeasures(fit_ml_oth)[c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","nfi","mfi")]
moreFitIndices(fit_ml_oth)[c("gammaHat","adjGammaHat")]


chisq_sum(ff_dv_st_oth)


#########
### Posterior estimates as informative priors
#########
summary(fit_st,neff=T, standardized=T)


HS.model_correctprior <- ' 
visual  =~ prior("normal(0.907, .1)")*x1 + prior("normal(0.503, .1)")*x2 + prior("normal(0.663, .1)")*x3
textual =~ prior("normal(1.002, .1)")*x4 + prior("normal(1.116, .1)")*x5 + prior("normal(0.927, .1)")*x6
speed   =~ prior("normal(0.617, .1)")*x7 + prior("normal(0.733, .1)")*x8 + prior("normal(0.681, .1)")*x9 

#visual ~~ prior("beta(8, 3)")*textual + prior("beta(8, 3)")*speed
#textual ~~ prior("beta(8, 3)")*speed

x1 ~prior("normal(4.934, .1)")*1
x2 ~prior("normal(6.087, .1)")*1
x3 ~prior("normal(2.25, .1)")*1
x4 ~prior("normal(3.059, .1)")*1
x5 ~prior("normal(4.338, .1)")*1
x6 ~prior("normal(2.183, .1)")*1
x7 ~prior("normal(4.184, .1)")*1
x8 ~prior("normal(5.526, .1)")*1
x9 ~prior("normal(5.373, .1)")*1

x1 ~~ prior("lognormal(.559, .1)")*x1
x2 ~~ prior("lognormal(1.152, .1)")*x2
x3 ~~ prior("lognormal(.857, .1)")*x3
x4 ~~ prior("lognormal(.378, .1)")*x4
x5 ~~ prior("lognormal(.455, .1)")*x5
x6 ~~ prior("lognormal(.364, .1)")*x6
x7 ~~ prior("lognormal(.823, .1)")*x7
x8 ~~ prior("lognormal(.503, .1)")*x8
x9 ~~ prior("lognormal(.568, .1)")*x9
'

fit_st_crp <- bcfa(HS.model_correctprior, data=HolzingerSwineford1939,
                   std.lv=T, target="stan",inits="prior",
                   burnin=10000, sample=10000,
                   dp=dpriors(lambda="normal(0,10)",
                              beta="normal(0,10)",
                              itheta="lognormal(0,1)[sd]",
                              nu="normal(0,10)",
                              #rho="beta(200, 200)",
                              ipsi="lognormal(0,1)[sd]", target="stan") )
summary(fit_st_crp,neff=T, standardized=T)
#fitMeasures(fit_st_crp)

com_priors <- blavCompare(fit_st_crp, fit_st)
com_priors


## method = DevM
ff_dv_st_crp <- blavFitIndices(fit_st_crp, baseline.model=fit_null_st, pD="loo", rescale="devM")
ff_dv_st_crp
summary(ff_dv_st_crp)
ff_dv_st_crp@details$pD


chisq_sum(ff_dv_st_crp)


### multiple priors results
mult_priors <- cbind(summary(ff_dv_st)[,c("EAP","lower","upper")], ## standard weakly informative priors
                     summary(ff_dv_st_wd)[,c("EAP","lower","upper")], ## wide priors
                     summary(ff_dv_st_cl)[,c("EAP","lower","upper")], ## all cross-loadings
                     summary(ff_dv_st_oth)[,c("EAP","lower","upper")], ## orthogonal priors
                     summary(ff_dv_st_crp)[,c("EAP","lower","upper")]) ## "strict" priors
colnames(mult_priors) <- c(paste0(c("EAP","lower","upper")),
                           paste0(c("EAP","lower","upper"),"_wide"),
                           paste0(c("EAP","lower","upper"),"_crossl"),
                           paste0(c("EAP","lower","upper"),"_orth"),
                           paste0(c("EAP","lower","upper"),"_stri"))
round(mult_priors[,c(-1,-2,-3)],3)
round(mult_priors,3)



#########
### Small sample N=75
#########

### select random sample N=75
hlz_75 <- HolzingerSwineford1939[sample(1:nrow(HolzingerSwineford1939),75, replace = F),]
head(hlz_75)
dim(hlz_75)

### Stan MCMC null model
fit_null_st_75 <- bcfa(lav_partable_independence(fit_ml), 
                       data=hlz_75,target="stan",
                       burnin=10000, sample=10000,
                       dp=dpriors(lambda="normal(0,10)",
                                  beta="normal(0,10)",
                                  itheta="cauchy(0,2.5)[sd]",
                                  nu="normal(0,10)",
                                  ipsi="cauchy(0,2.5)[sd]", target="stan"))
summary(fit_null_st_75,neff=T)
fitMeasures(fit_null_st_75)

########
## small sample size with simulation priors
########
fit_st_75 <- bcfa(HS.model, data=hlz_75,
                  std.lv=T, target="stan",
                  burnin=10000, sample=10000,
                  dp=dpriors(lambda="normal(0,10)",
                             beta="normal(0,10)",
                             itheta="cauchy(0,2.5)[sd]",
                             nu="normal(0,10)",
                             ipsi="cauchy(0,2.5)[sd]", target="stan") )
summary(fit_st_75,neff=T, standardized=T)
fitMeasures(fit_st_75)

## method = DevM
ff_dv_st_75 <- blavFitIndices(fit_st_75, baseline.model=fit_null_st_75, pD="loo", rescale="devM")
ff_dv_st_75
summary(ff_dv_st_75)

chisq_sum(ff_dv_st_75)

#######
## small sample size with wide priors
#######
fit_st_75wd <- bcfa(HS.model, data=hlz_75,
                    std.lv=T, target="stan",
                    burnin=10000, sample=10000,
                    dp=dpriors(lambda="normal(0,1000)",
                               beta="normal(0,1000)",
                               itheta="uniform(0.01,1000)[sd]",
                               nu="normal(0,1000)",
                               ipsi="uniform(0.01,1000)[sd]", target="stan") )
summary(fit_st_75wd,neff=T, standardized=T)
fitMeasures(fit_st_75wd)

## method = DevM
ff_dv_st_75wd <- blavFitIndices(fit_st_75wd, baseline.model=fit_null_st_75, pD="loo", rescale="devM")
ff_dv_st_75wd
summary(ff_dv_st_75wd)

chisq_sum(ff_dv_st_75wd)

########
## strict priors small sample size
########

HS.model_correctprior_75 <- ' 
visual  =~ prior("normal(1.277, .1)")*x1 + prior("normal(0.615, .1)")*x2 + prior("normal(0.659, .1)")*x3
textual =~ prior("normal(0.910, .1)")*x4 + prior("normal(1.170, .1)")*x5 + prior("normal(0.939, .1)")*x6
speed   =~ prior("normal(0.693, .1)")*x7 + prior("normal(0.817, .1)")*x8 + prior("normal(0.491, .1)")*x9 

#visual ~~ prior("beta(8, 3)")*textual + prior("beta(8, 3)")*speed
#textual ~~ prior("beta(8, 3)")*speed

x1 ~prior("normal(4.693, .1)")*1
x2 ~prior("normal(6.171, .1)")*1
x3 ~prior("normal(2.249, .1)")*1
x4 ~prior("normal(2.838, .1)")*1
x5 ~prior("normal(4.115, .1)")*1
x6 ~prior("normal(2.111, .1)")*1
x7 ~prior("normal(4.121, .1)")*1
x8 ~prior("normal(5.646, .1)")*1
x9 ~prior("normal(5.300, .1)")*1

x1 ~~ prior("lognormal(0.336, .1)")*x1
x2 ~~ prior("lognormal(1.391, .1)")*x2
x3 ~~ prior("lognormal(0.986, .1)")*x3
x4 ~~ prior("lognormal(0.388, .1)")*x4
x5 ~~ prior("lognormal(0.352, .1)")*x5
x6 ~~ prior("lognormal(0.497, .1)")*x6
x7 ~~ prior("lognormal(0.755, .1)")*x7
x8 ~~ prior("lognormal(0.348, .1)")*x8
x9 ~~ prior("lognormal(0.775, .1)")*x9
'

fit_st_75crp <- bcfa(HS.model_correctprior_75, data=hlz_75,
                     std.lv=T, target="stan",inits="prior",
                     burnin=10000, sample=10000,
                     dp=dpriors(lambda="normal(0,10)",
                                beta="normal(0,10)",
                                itheta="lognormal(0,1)[sd]",
                                nu="normal(0,10)",
                                #rho="beta(200, 200)",
                                ipsi="lognormal(0,1)[sd]", target="stan") )
summary(fit_st_75crp,neff=T, standardized=T)
#fitMeasures(fit_st_75crp)

#blavCompare(fit_st_75crp, fit_st_75)


## method = DevM
ff_dv_st_75crp <- blavFitIndices(fit_st_75crp, baseline.model=fit_null_st_75, pD="loo", rescale="devM")
ff_dv_st_75crp
summary(ff_dv_st_75crp)
ff_dv_st_75crp@details$pD


chisq_sum(ff_dv_st_75crp)


###########
###### Missing data
###########

## 20% of missing data comletely at random
hlz_20mis <- imposeMissing(HolzingerSwineford1939, pmMCAR=.2)
summary(hlz_20mis)

## 50% of missing data comletely at random
hlz_50mis <- imposeMissing(HolzingerSwineford1939, pmMCAR=.5)
summary(hlz_50mis)
sort(rowSums(is.na(hlz_50mis[,paste0("x",1:9)])))

##########
## 20% Missing data with simulation priors
##########

### Stan MCMC null model
fit_null_st_20mis <- bcfa(lav_partable_independence(fit_ml), 
                          data=hlz_20mis,target="stan",
                          burnin=10000, sample=2000,
                          dp=dpriors(lambda="normal(0,10)",
                                     beta="normal(0,10)",
                                     itheta="cauchy(0,2.5)[sd]",
                                     nu="normal(0,10)",
                                     ipsi="cauchy(0,2.5)[sd]", target="stan"))
summary(fit_null_st_20mis,neff=T)
#fitMeasures(fit_null_st_20mis)

fit_st_null_20 <- blavFitIndices(fit_null_st_20mis, pD="loo", rescale="devM")
summary(fit_st_null_20)

chisq_sum(fit_st_null_20)


fit_st_20mis <- bcfa(HS.model, data=hlz_20mis,
                     std.lv=T, target="stan",
                     burnin=10000, sample=2000,
                     dp=dpriors(lambda="normal(0,10)",
                                beta="normal(0,10)",
                                itheta="cauchy(0,2.5)[sd]",
                                nu="normal(0,10)",
                                ipsi="cauchy(0,2.5)[sd]", target="stan") )
summary(fit_st_20mis,neff=T, standardized=T)
#fitMeasures(fit_st_20mis)

## method = DevM
ff_dv_st_20mis <- blavFitIndices(fit_st_20mis, baseline.model=fit_null_st_20mis, pD="loo", rescale="devM")
ff_dv_st_20mis
summary(ff_dv_st_20mis)
ff_dv_st_20mis@details$pD

f20_ml <- cfa(HS.model, data=hlz_20mis,
              std.lv=T, missing="fiml")
summary(f20_ml)
fitMeasures(f20_ml)
moreFitIndices(f20_ml)

chisq_sum(ff_dv_st_20mis)

##########
## 50% Missing data with simulation priors
##########

### Stan MCMC null model
fit_null_st_50mis <- bcfa(lav_partable_independence(fit_ml), 
                          data=hlz_50mis,target="stan",
                          burnin=10000, sample=2000,
                          dp=dpriors(lambda="normal(0,10)",
                                     beta="normal(0,10)",
                                     itheta="cauchy(0,2.5)[sd]",
                                     nu="normal(0,10)",
                                     ipsi="cauchy(0,2.5)[sd]", target="stan"))
summary(fit_null_st_50mis,neff=T)
#fitMeasures(fit_null_st_20mis)

fit_st_null_50 <- blavFitIndices(fit_null_st_50mis, pD="loo", rescale="devM")
summary(fit_st_null_50)

chisq_sum(fit_st_null_50)


# 
fit_st_50mis <- bcfa(HS.model, data=hlz_50mis,
                     std.lv=T, target="stan",
                     burnin=15000, sample=2000,
                     dp=dpriors(lambda="normal(0,10)",
                                beta="normal(0,10)",
                                itheta="cauchy(0,2.5)[sd]",
                                nu="normal(0,10)",
                                ipsi="cauchy(0,2.5)[sd]", target="stan") )
summary(fit_st_50mis,neff=T, standardized=T)
#fitMeasures(fit_st_20mis)

## method = DevM
ff_dv_st_50mis <- blavFitIndices(fit_st_50mis, baseline.model=fit_null_st_50mis, pD="loo", rescale="devM")
ff_dv_st_50mis
summary(ff_dv_st_50mis)
ff_dv_st_50mis@details$pD


f50_ml <- cfa(HS.model, data=hlz_50mis,
              std.lv=T, missing="fiml")
summary(f50_ml)
fitMeasures(f50_ml)
moreFitIndices(f50_ml)

chisq_sum(ff_dv_st_50mis)

