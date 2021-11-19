# Script name: 501_factor_invariance.R
# Project: self compassion
# Script purpose: evaluate factor invariance between rescue-workers and non
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun  8 13:31:01 2021
# Last Modified Date: Tue Jun  8 13:31:01 2021
# 
# Notes: 

library("tidyverse")
library("lavaan")

freq_dframer <- function(data){
  library(dplyr)
  library(questionr)
  quest_out   <- apply(data, 2 , questionr::freq)
  templist <- list()
  for (i in 1:length(names(quest_out))) {
    temp <- data.frame(Item = rep(names(quest_out[i]), nrow(quest_out[[i]])),
                       Category = row.names(quest_out[[i]]),
                       quest_out[[i]])
    templist[[i]] <- temp
  }
  freqs <- dplyr::bind_rows(templist) 
  row.names(freqs) <- NULL
  names(freqs)[3:5] <- c("Frequency", "Percent", "Pcnt_of_nonMissing") 
  freqs$Item <- factor(freqs$Item, levels = unique(freqs$Item))
  freqs
}


the_dir <- "/Users/corrado/Documents/papers/self_compassion/scripts/_mplus/"


source(here("R", "funs_data-cleaning.R"))

# Get non-first responders Red Cross participants
nrw <- get_data_scs_not_rescue_workers()
dim(nrw)

# Get first responders Red Cross participants
rw <- get_data_scs_rescue_workers()
dim(rw)

# merge data
d <- bind_rows(rw, nrw)
dim(d)

d$group <- factor(d$is_rescue_worker)
d$where <- factor(d$where)
d$sex <- factor(d$sex)

# Make a copy of the data
mydata <- d


# Configural invariance

# A 2-factor ESEM is fitted to each group separately to test for configural 
# invariance.

# Rescue workers
rwonly <- mydata[mydata$group == "1", ]
# select only the SCS items
d1 <- rwonly[, 1:26]

f2_efa <- psych::fa(d1, nfact = 2, rotate = "geominQ", fm = "ml")
psych::fa.diagram(f2_efa)

f_loadmat <- zapsmall(matrix(round(f2_efa$loadings, 2), nrow = 26, ncol = 2))
rownames(f_loadmat) <- colnames(d1)

# This will turn the loading matrix into a lavaan-compatible equation set. 
terms <- vector()
for (i in 1:2) {
  terms[i] <-
    paste0("F",i,"=~ ", 
           paste0(c(f_loadmat[,i]), "*", 
                  names(f_loadmat[,1]), collapse = "+")
    )
}

m_esem <- paste(terms, collapse = "\n")
m_esem

items <- c("u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
           "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
           "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
           "c22", "c23", "u24", "u25", "c26")
fit_rw_ESEM <- lavaan::cfa(
  m_esem, 
  data = rwonly, 
  verbose = FALSE,
  estimator = "WLSMV",
  ordered = items
)

summary(
  fit_rw_ESEM, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitMeasures(
  fit_rw_ESEM, 
  c("chisq", "df", "pvalue", 
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi", "cfi.scaled", 
    "tli", "tli.scaled", 
    "rmsea", 
    "rmsea.ci.lower",
    "rmsea.ci.upper",
    "rmsea.scaled",
    "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled",          
    "srmr"), 
  output = "matrix")
# chisq                 2351.261
# df                     322.000
# pvalue                   0.000
# chisq.scaled          1575.510
# df.scaled              322.000
# pvalue.scaled            0.000
# cfi                      0.969
# cfi.scaled               0.939
# tli                      0.969
# tli.scaled               0.938
# rmsea                    0.090
# rmsea.ci.lower           0.086
# rmsea.ci.upper           0.093
# rmsea.scaled             0.071
# rmsea.ci.lower.scaled    0.067
# rmsea.ci.upper.scaled    0.074
# srmr                     0.070

fs_rw <- lavPredict(fit_rw_ESEM, newdata = d1) %>% 
  as.data.frame()



# Non rescue workers
nrw <- mydata[mydata$group == "0", ]
# select only the SCS items
d1 <- nrw[, 1:26]

f2_efa <- psych::fa(d1, nfact = 2, rotate = "geominQ", fm = "ml")
psych::fa.diagram(f2_efa)

f_loadmat <- zapsmall(matrix(round(f2_efa$loadings, 2), nrow = 26, ncol = 2))
rownames(f_loadmat) <- colnames(d1)

# This will turn the loading matrix into a lavaan-compatible equation set. 
terms <- vector()
for (i in 1:2) {
  terms[i] <-
    paste0("F",i,"=~ ", 
           paste0(c(f_loadmat[,i]), "*", 
                  names(f_loadmat[,1]), collapse = "+")
    )
}

m_esem <- paste(terms, collapse = "\n")
m_esem

items <- c("u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
           "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
           "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
           "c22", "c23", "u24", "u25", "c26")
fit_nrw_ESEM <- lavaan::cfa(
  m_esem, 
  data = nrw, 
  verbose = FALSE,
  estimator = "WLSMV",
  ordered = items
)

summary(
  fit_rw_ESEM, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitMeasures(
  fit_nrw_ESEM, 
  c("chisq", "df", "pvalue", 
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi", "cfi.scaled", 
    "tli", "tli.scaled", 
    "rmsea", 
    "rmsea.ci.lower",
    "rmsea.ci.upper",
    "rmsea.scaled",
    "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled",          
    "srmr"), 
  output = "matrix")

fs_nrw <- lavPredict(fit_nrw_ESEM, newdata = d1) %>% 
  as.data.frame()

# factor scores for the two groups and the two factors.
# Not useful.
fs <- bind_rows(fs_rw, fs_nrw)




# Bifactor model ----

items <- c("u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
           "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
           "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
           "c22", "c23", "u24", "u25", "c26")

mod_bifactor <- '
  sj =~ u1 + u8 + u11 + u16 + u21
  is =~ u4 + u13 + u18 + u25
  oi =~ u2 + u6 + u20 + u24
  
  sk =~ c5 + c12 + c19 + c23 + c26
  cu =~ c3 + c7  + c10 + c15
  mi =~ c9 + c14 + c17 + c22
  
  g  =~ u1 + u8  + u11 + u16 + u21 +
        u4 + u13 + u18 + u25 +
        u2 + u6  + u20 + u24 +
        c5 + c12 + c19 + c23 + c26 +
        c3 + c7  + c10 + c15 +
        c9 + c14 + c17 + c22
  sj ~~ NA*is
  sj ~~ NA*oi
  sj ~~ NA*sk
  sj ~~ NA*cu
  sj ~~ NA*mi
  is ~~ NA*oi
  is ~~ NA*sk
  is ~~ NA*cu
  is ~~ NA*mi
  oi ~~ NA*sk
  oi ~~ NA*cu
  oi ~~ NA*mi
  sk ~~ NA*cu
  sk ~~ NA*mi
  cu ~~ NA*mi
  g ~~ 0*sj
  g ~~ 0*is
  g ~~ 0*oi
  g ~~ 0*sk
  g ~~ 0*cu
  g ~~ 0*mi
'


fit_bif_rw <- lavaan::cfa(
  mod_bifactor,
  data = rwonly,
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE

)

summary(
  fit_bif_rw, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitMeasures(
  fit_bif_rw, 
  c("chisq", "df", "pvalue", 
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi", "cfi.scaled", 
    "tli", "tli.scaled", 
    "rmsea", 
    "rmsea.ci.lower",
    "rmsea.ci.upper",
    "rmsea.scaled",
    "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled",          
    "srmr"), 
  output = "matrix")



# remove multivariate outliers
psych::outlier(mydata[, 2:27])

md <- mahalanobis(
  nrw[, 2:27],
  center = colMeans(nrw[, 2:27]),
  cov = cov(nrw[, 2:27])
)
alpha <- .01
cutoff <- (qchisq(p = 1 - alpha, df = ncol(nrw[, 2:27])))
names_outliers_MH <- which(md > cutoff)
bad_ids <- names_outliers_MH

nrw2 <- nrw[-bad_ids, ]

fit_bif_nrw <- lavaan::cfa(
  mod_bifactor,
  data = nrw2,
  #estimator = "WLSMV",
  #ordered = items,
  std.lv = TRUE
)

summary(
  fit_bif_nrw, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitMeasures(
  fit_bif_nrw, 
  c("chisq", "df", "pvalue", 
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi", "cfi.scaled", 
    "tli", "tli.scaled", 
    "rmsea", 
    "rmsea.ci.lower",
    "rmsea.ci.upper",
    "rmsea.scaled",
    "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled",          
    "srmr"), 
  output = "matrix")








factanal(nrw[, 2:27], factors = 3)

temp <- rwonly[, 2:27] %>% 
  as.data.frame()

temp <- nrw[, 2:27] %>% 
  as.data.frame()


psych::outlier(temp)

psych::KMO(temp)
psych::cortest.bartlett(temp)
det(cor(temp))


library("nFactors")
n_p  <- sum(complete.cases(temp)) # The number of persons in our data
n_nu <- ncol(temp) # The number of variables in our data
set.seed(123)     # To reproduce our randomly generated results.
ReducedEig <- eigenComputes(temp, model = "factors", use = "complete")
n_factors  <- length(ReducedEig)
paral <- parallel(subject = n_p,  
                  var = n_nu, 
                  rep = 100,
                  quantile = .95, 
                  model  = "factors")

ParallelAna <- data.frame(Nfactor  = 1:n_factors,
                          ReducedEig,
                          RandEigM = paral$eigen$mevpea,
                          RandEig95= paral$eigen$qevpea)
ParallelAna <- round(ParallelAna, 3)
ParallelAna

scree <- data.frame(Factor_n = as.factor(1:n_factors), 
                    Eigenvalue = ReducedEig)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the reduced correlation matrix)")

fafit <- psych::fa(temp, nfactors = 2, fm = "pa", rotate = "promax")
n_factors <- length(fafit$e.values)
fafit

print(fafit, cut = .4, sort = TRUE, digits = 3)



temp <- rwonly[, 2:27] %>% 
  as.data.frame()

temp <- nrw[, 2:27] %>% 
  as.data.frame()




freqout <- freq_dframer(temp)
# write.csv(freqout, "freqout.csv", row.names = F)

library("gridExtra")
ggplot(freqout, aes(x = Category, y = Frequency)) +
  geom_col() +
  facet_wrap(. ~ Item, nrow = 3 )



# mydata$c19[1] <- 6
# mydata$u20[1] <- 6
# mydata$u21[1] <- 6
# mydata$c22[1] <- 6
# mydata$c23[1] <- 6
# mydata$u24[1] <- 6
# mydata$u25[1] <- 6
# mydata$c26[1] <- 6


# 
# new_item_names <- c("group", 
#                     "u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
#                     "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
#                     "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
#                     "c22", "c23", "u24", "u25", "c26", "where", "sex", "age")
# names(nrw) <- new_item_names

# new_names <- c(
#   "group", 
#   "scsj1", "scoi2", "scch3", "scis4", "scsk5", "scoi6", "scch7", 
#   "scsj8", "scmi9", "scch10", "scsj11", "scsk12", "scis13", "scmi14", 
#   "scch15", "scsj16", "scmi17", "scis18", "scsk19", "scoi20", 
#   "scsj21", "scmi22", "scsk23", "scoi24", "scis25", "scsk26",
#   "where", "sex", "age" 
# )
# names(nrw) <- new_names

# # remove multivariate outliers
# psych::outlier(mydata[, 2:27])
# 
# md <- mahalanobis(
#   mydata[, 2:27], 
#   center = colMeans(mydata[, 2:27]), 
#   cov = cov(mydata[, 2:27])
# )
# alpha <- .001
# cutoff <- (qchisq(p = 1 - alpha, df = ncol(mydata[, 2:27])))
# names_outliers_MH <- which(md > cutoff)
# bad_ids <- names_outliers_MH
# 
# mydata2 <- mydata[-bad_ids, ]







# d_s <- scale(d1)

f2_efa <- psych::fa(d1, nfact = 2, rotate = "geominQ", fm = "ml")
psych::fa.diagram(f2_efa)

f_loadmat <- zapsmall(matrix(round(f2_efa$loadings, 2), nrow = 26, ncol = 2))
rownames(f_loadmat) <- colnames(d1)

# This will turn the loading matrix into a lavaan-compatible equation set. 
terms <- vector()
for (i in 1:2) {
  terms[i] <-
    paste0("F",i,"=~ ", 
           paste0(c(f_loadmat[,i]), "*", 
                  names(f_loadmat[,1]), collapse = "+")
    )
}

m_esem <- paste(terms, collapse = "\n")
m_esem

scs_items <- c("u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
                "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
                "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
                "c22", "c23", "u24", "u25", "c26")

# mydata$c19[1] <- 6
# mydata$u20[1] <- 6
# mydata$u21[1] <- 6
# mydata$c22[1] <- 6
# mydata$c23[1] <- 6
# mydata$u24[1] <- 6
# mydata$u25[1] <- 6
# mydata$c26[1] <- 6

new_item_names <- c("group", 
  "u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
                    "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
                    "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
                    "c22", "c23", "u24", "u25", "c26", "where", "sex", "age")
names(mydata2) <- new_item_names


fit00_ESEM <- lavaan::cfa(
  m_esem, 
  data = mydata2, 
  verbose = FALSE,
  # estimator = "WLSMV",
  estimator = "MLR"
  #ordered = scs_items,
)



fit0_ESEM <- lavaan::cfa(
  m_esem, 
  data = mydata2, 
  verbose = FALSE,
  # estimator = "WLSMV",
  estimator = "MLR"
  #ordered = scs_items,
)

summary(
  fit0_ESEM, 
  standardized=TRUE, 
  fit.measures=TRUE
)


fit_ESEM <- lavaan::cfa(
  m_esem, 
  data = mydata2, 
  verbose = FALSE, 
  # estimator = "WLSMV",
  #estimator = "MLR",
  # ordered = scs_items
  group = "group"
)

summary(
  fit_ESEM, 
  standardized=TRUE, 
  fit.measures=TRUE
)








mi <- modindices(fit_ESEM)
mi[mi$op == "=~",]

# Adding residual correlations based on modification indices.
# modification indices and analyzed correlations between items’ residuals in order to improve the goodness of fit.
# m_esem1 <- 
#   c("F1=~ 0.51*u1+0.81*u2+-0.22*c3+0.8*u4+-0.13*c5+0.8*u6+-0.33*c7+0.7*u8+-0.08*c9+-0.23*c10+0.69*u11+0.03*c12+0.73*u13+0.16*c14+-0.03*c15+0.73*u16+0.19*c17+0.79*u18+0.05*c19+0.64*u20+0.56*u21+0.06*c22+0.25*c23+0.63*u24+0.77*u25+0.08*c26\nF2=~ -0.05*u1+0.04*u2+0.44*c3+0.04*u4+0.66*c5+0.02*u6+0.51*c7+-0.01*u8+0.49*c9+0.56*c10+-0.02*u11+0.7*c12+0.03*u13+0.49*c14+0.67*c15+0.02*u16+0.6*c17+-0.03*u18+0.72*c19+-0.06*u20+-0.01*u21+0.65*c22+0.59*c23+0.02*u24+0.05*u25+0.66*c26\nc9~~NA*c14\nc7~~NA*c10\nu13~~NA*u18")

m_esem1 <- 
  c(
    "F1=~ 0.38*u1+0.75*u2+0.03*c3+0.75*u4+-0.04*c5+0.76*u6+0.21*c7+0.61*u8+-0.09*c9+0.11*c10+0.6*u11+-0.19*c12+0.66*u13+-0.33*c14+-0.2*c15+0.58*u16+-0.36*c17+0.68*u18+-0.02*c19+0.61*u20+0.57*u21+-0.03*c22+-0.1*c23+0.63*u24+0.75*u25+-0.03*c26\nF2=~ -0.2*u1+-0.06*u2+0.06*c3+-0.06*u4+0.39*c5+-0.01*u6+0.32*c7+-0.02*u8+0.09*c9+0.4*c10+-0.13*u11+0.46*c12+-0.02*u13+-0.04*c14+0.18*c15+-0.12*u16+0.19*c17+-0.08*u18+0.82*c19+0.59*u20+0.44*u21+0.75*c22+0.81*c23+0.53*u24+0.37*u25+0.78*c26\nc9~~NA*c14"
  )



fit_ESEM <- lavaan::cfa(
  m_esem1, 
  data = mydata, 
  verbose = FALSE, 
  estimator = "WLSMV",
  ordered = scs_items
)

summary(
  fit_ESEM, 
  standardized=TRUE, 
  fit.measures=TRUE
)


mi <- modindices(fit_ESEM)













# # Bifactor version 2 ----
# 
# SCS_corr_mat <- lavaan::lavCor(d, ordered = names(d))
# 
# ## Conduct tranditional Schmid-Leiman procedure, do not round output
# SLOutput <- 
#   SchmidLeiman(R          = SCS_corr_mat,   ## Corr matrix
#                numFactors = c(2, 1),        ## Number of factors per level
#                facMethod  = "fals",         ## Fac extraction method
#                rotate     = "geominQ",      ## Rotational criterion
#                rotateControl = 
#                  list(numberStarts = 100,   ## Random start configs
#                       delta        = .01,   ## Geomin tuning param
#                       standardize  = "none"))$B    
# 
# # ## Add column names to the SL Bifactor output
# # colnames(SLOutput) <- c("Performance", "Boss", "Peer", "Subordinate", "Self")
# # 
# # ## Sort the row order into 'hallow staircase' pattern
# # SortedSL <- faSort(fmat     = SLOutput,
# #                    BiFactor = TRUE)$loadings
# # 
# # ## Compute the item communalities
# # Communality <- apply(SortedSL^2, 1, sum)
# # 
# # ## Compute the item explained common variance (I-ECV)
# # IECV <- apply(SortedSL^2, 1, function(x) x[1] / sum(x))
# # 
# # ## Add columns for the communality and and I-ECV values
# # SL <- cbind(SortedSL, 
# #             Communality, 
# #             IECV) %>% round(2)
# # 
# # ## Give clear row and column names
# # dimnames(SL) <- 
# #   list(rep(c("Technical", "Interpersonal", "Leadership"), 4), 
# #        c("Performance", "Boss", "Peer", "Subordinate", "Self", 
# #          "$h^2$", "I-ECV"))
# 
# 
# f_loadmat <- SLOutput
# rownames(f_loadmat) <- colnames(d[, 1:26])
# 
# # This will turn the loading matrix into a lavaan-compatible equation set. 
# terms <- vector()
# for (i in 1:3) {
#   terms[i] <-
#     paste0("F",i,"=~ ", 
#            paste0(c(f_loadmat[,i]), "*", 
#                   names(f_loadmat[,1]), collapse = "+")
#     )
# }
# 
# m_esem <- paste(terms, collapse = "\n")
# m_esem
# 
# 
# # m_esem <- "F1=~ 0.237489281801502*u1+0.429887259500751*u2+0.125541390817628*u3+0.425429144546642*u4+0.258130199595277*u5+0.409125142187821*u6+0.0808427342147022*u7+0.356082044998831*u8+0.217548495279002*u9+0.16541897318553*u10+0.341800878982743*u11+0.360741223339561*u12+0.379204200545866*u13+0.343121430473647*u14+0.332359301659812*u15+0.380014704177943*u16+0.410325824368322*u17+0.38226462047563*u18+0.378939476044336*u19+0.297448970007021*u20+0.277595395056909*u21+0.363413056718487*u22+0.427395668678096*u23+0.350260328410562*u24+0.412127780691352*u25+0.373392400410804*u26\nF2=~ 0.490249882328467*u1+0.737663354482553*u2+-0.21200723535891*u3+0.726176813922544*u4+-0.119889136321777*u5+0.726178413247306*u6+-0.342876666743195*u7+0.665523107407188*u8+-0.0693593054240087*u9+-0.241623443104029*u10+0.655509021751017*u11+0.032897501331412*u12+0.654515303711587*u13+0.154415759871224*u14+-0.0313597448195801*u15+0.674323686490117*u16+0.181107410509167*u17+0.710064711711802*u18+0.0550508065984046*u19+0.58159747356244*u20+0.529031823832191*u21+0.0579294615716469*u22+0.24949931498909*u23+0.602138907360023*u24+0.697546443489501*u25+0.0913568390859534*u26\nF3=~ -0.0592430910865683*u1+0.0425163947157969*u2+0.439845694483885*u3+0.0459121372120675*u4+0.588356042306952*u5+0.0163212625828756*u6+0.489593887852244*u7+-0.0192885553903295*u8+0.464176613297618*u9+0.541833626495867*u10+-0.0351926058836949*u11+0.62179274810147*u12+0.0336824307837687*u13+0.468297255356066*u14+0.634541139201513*u15+0.0153449883655846*u16+0.563571320181907*u17+-0.0163127822491008*u18+0.632666493339644*u19+-0.0417730017023263*u20+-0.0252385596502421*u21+0.601609757560617*u22+0.526158577233593*u23+0.0335301177951796*u24+0.0504025628403547*u25+0.586293365396466*u26\nF1~~0*F2\nF1~~0*F3\nF2~~0*F3\nc9~~NA*c14"
# 
# d_s <- scale(d)
# 
# fit_3 <- lavaan::cfa(
#   m_esem, 
#   data = d_s, 
#   verbose = FALSE, 
#   estimator = "WLSMV",
#   ordered = items
# )
# 
# 
# summary(
#   fit_3, 
#   standardized=TRUE, 
#   fit.measures=TRUE
# )



# Network analysis ----


new_item_names <- c("u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
                    "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
                    "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
                    "c22", "c23", "u24", "u25", "c26")

names(d) <- new_item_names

corMat <- cor_auto(d)

Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring")

Graph_pcor <- qgraph(
  corMat, graph = "pcor", layout = "spring", threshold = "bonferroni",
  sampleSize = nrow(d), alpha = 0.05)


Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
                      sampleSize = nrow(d))

centRes <- centrality(Graph_lasso)
centRes$OutDegree # Or InDegree, it's the same in unweighted networks
centRes$Closeness
centRes$Betweenness

centralityPlot(Graph_lasso)

Groups <- c("sj", "oi", "cu", "is", "sk", "oi", "cu", "sj",
            "mi", "cu", "sj", "sk", "is", "mi", "cu", "sj",
            "mi", "is", "sk", "oi", "sj", "mi", "sk", "oi",
            "is", "sk")

qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(d), groups = Groups, palette = "colorblind")


ega.rand <- EGA(data = d, model = "glasso", palette = "colorblind")

set.seed(0)
boot_scs <- bootEGA(
  data = d, 
  model = "glasso",
  iter = 1000, 
  type = "resampling",
  plot.typicalStructure = FALSE
)

boot_scs$summary.table
# n.Boots median.dim    SE.dim   CI.dim Lower.CI Upper.CI Lower.Quantile Upper.Quantile
# 1    1000          4 0.5975559 1.172609 2.827391 5.172609              3              5

tab1 <- boot_scs$summary.table

knitr::kable(
  tab1, 
  digits = 2,
  booktabs = TRUE, 
  caption = "bootEGA Summary Table") %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position"),
    position = "center"
  )



# lslx

sj =~ u1 + u8 + u11 + u16 + u21
is =~ u4 + u13 + u18 + u25
oi =~ u2 + u6 + u20 + u24

sk =~ u5 + u12 + u19 + u23 + u26
cu =~ u3 + u7 + u10 + u15
mi =~ u9 + u14 + u17 + u22




items_ord <- c(
  "u1", "u8", "u11", "u16", "u21", "u4", "u13", "u18", 
  "u25", "u2", "u6", "u20", "u24",
  "c5", "c12", "c19", "c23", "c26", "c3", "c7", "c10",
  "c15", "c9", "c14", "c17", "c22"
)


model_fa <- "

  sj :=> u1 + u8 + u11 + u16 + u21
  sj :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  is :=> u4 + u13 + u18 + u25
  is :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  oi :=> u2 + u6 + u20 + u24
  oi :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  sk :=> c5 + c12 + c19 + c23 + c26
  sk :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  cu :=> c3 + c7  + c10 + c15
  cu :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  mi :=> c9 + c14 + c17 + c22
  mi :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  sj <=> fix(1) * sj
  is <=> fix(1) * is
  oi <=> fix(1) * oi
  sk <=> fix(1) * sk
  cu <=> fix(1) * cu
  mi <=> fix(1) * mi

"

# initialize lslx object via specified model and raw data
lslx_fa <- lslx$new(
  model = model_fa, 
  data = d
)

# fit with mcp under specified penalty levels and convexity levels
lslx_fa$fit(
  penalty_method = "mcp", 
  lambda_grid = seq(.01, .60, .01), 
  delta_grid = c(1.5, 3.0, Inf)
)

# summarize fitting result under penalty level selected by 'bic'
lslx_fa$summarize(selector = "bic")


lslx_fa$plot_numerical_condition()
lslx_fa$plot_information_criterion()
lslx_fa$plot_fit_index()
lslx_fa$plot_coefficient(block = "y<-f")
lslx_fa$extract_coefficient_matrix(selector = "bic", block = "y<-f")









lslx_fa$fit(
  penalty_method = "none"
)




mi :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22



model_fa <- "
  unc :=> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  com :=> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  unc :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24 + c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  com :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24 + c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  unc <=> 1 * unc
  com <=> 1 * com
"

