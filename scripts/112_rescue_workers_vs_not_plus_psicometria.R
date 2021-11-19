# Script name: 112_rescue_workers_vs_not_plus_psicometria.R
# Project: self-compassion and SCS
# Script purpose: SCS, CS, and RUS group comparisons 
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jul 15 06:44:58 2021
# Last Modified Date: Thu Jul 15 06:44:58 2021
#
# Notes: age of the first 127 observations in the
# rescue workers group has been imputed. Change age 
# in other files as well!

library("here")
library("tidyverse")
library("brms")
library("cmdstanr")
set_cmdstan_path("/Users/corrado/cmdstan")
library("posterior")
library("bayesplot")
library("bayestestR")
library("effectsize")
library("patchwork")
library("readxl")
library("mice")

options(max.print = .Machine$integer.max) 

source(here("R", "funs_data-cleaning.R"))

# Get controls participants
nrw <- get_data_scs_not_rescue_workers()
dim(nrw)

# Get first responders Red Cross participants
rw <- get_data_scs_rescue_workers()
dim(rw)

# Get controls participants Psicometria
temp1 <- read_excel(here("data", "raw", "self_compassion_controlli_psicometria.xlsx"))

temp1$`Inserisci il tuo codice anonimo (esempio: ma_ro_1997_05_04_174_m)` <- NULL

temp3 <- temp1[,] %>%
  rename(
    age = `Età (anni compiuti)`,
    sex = "Genere"
  ) 

names(temp3) <- c("sex", "age", 
  "u1", "u2", "c3", "u4", "c5", "u6", "c7", "u8", "c9", 
  "c10", "u11", "c12", "u13", "c14", "c15", "u16", "c17", 
  "u18", "c19", "u20", "u21", "c22", "c23", "u24", "u25",
  "c26" 
)

temp3$is_rescue_worker <- 0
temp3$neg_self_compassion <- temp3$u1 + temp3$u2 + temp3$u4 + temp3$u6 + 
    temp3$u8 + temp3$u11 + temp3$u13 + temp3$u16 + temp3$u18 + 
    temp3$u20 + temp3$u21 + temp3$u24 + temp3$u25
temp3$pos_self_compassion <- temp3$c3 + temp3$c5 + temp3$c7 + temp3$c9 +
    temp3$c10 + temp3$c12 + temp3$c14 + temp3$c15 + temp3$c17 + 
    temp3$c19 + temp3$c22 + temp3$c23 + temp3$c26
temp3$where <- "toscana"

temp4 <- temp3 %>% 
  drop_na()

# merge data
d1 <- bind_rows(rw, nrw)
dim(d1)

d <- bind_rows(d1, temp4) 

d$group <- factor(d$is_rescue_worker)
d$where <- factor(d$where)
d$sex <- factor(d$sex)

# Correct age: it is always 40 in the observations 1:127 (only rescue
# workers)!!
# The mean and sd are taken from the remaining rescue workers.

d$age[1:127] <- NA
set.seed(12345)
imp1 <- mice::mice(d, m = 5)
d1 <- mice::complete(imp1)
d <- d1

# Correct sex
d$sex <- dplyr::recode_factor(
  d$sex, 
  "Altro" = "Maschio" 
)

d$scs_total_score <- d$pos_self_compassion + d$neg_self_compassion

mydata <- d
mydata$group <- ifelse(
  mydata$is_rescue_worker == 1, "rescue_worker", "not_rescue_worker"
)
mydata$group <- factor(mydata$group)
contrasts(mydata$group) <- contr.sum
contrasts(mydata$group)

mydata$sex <- factor(mydata$sex)
contrasts(mydata$sex) <- contr.sum
contrasts(mydata$sex)

mydata$scs <- as.numeric(scale(mydata$scs_total_score)[, 1])
mydata$age_s <- as.numeric(scale(mydata$age)[, 1])


# SCS total score ---------------------------------------------------------

eq_var_scs <- bf(
  scs ~ group * age_s * sex
)

mod1_scs <- brm(
  eq_var_scs,
  family = student(),
  iter = 20000,
  prior = c(prior(student_t(3, 0, 1), class = Intercept),
            prior(gamma(2, .1), class = nu),
            prior(student_t(3, 0, 1), class = b),
            prior(cauchy(0, 1), class = sigma)),
  data = mydata,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstan"
)

mod1_scs <- add_criterion(mod1_scs, "loo")
loo(mod1_scs)

pp_check(mod1_scs, resp = "scs_total_score")

round(bayes_R2(mod1_scs), 2)
#    Estimate Est.Error Q2.5 Q97.5
# R2     0.09      0.02 0.06  0.12

parameters::model_parameters(mod1_scs, ci = 0.95, standardize = "refit")
# Parameter         | Median |         95% CI |     pd | % in ROPE |  Rhat |      ESS
# -----------------------------------------------------------------------------------
# (Intercept)       |  -0.09 | [-0.18, -0.01] | 98.44% |    54.83% | 1.000 | 23658.00
# group1            |  -0.05 | [-0.13,  0.04] | 85.69% |    88.80% | 1.000 | 22329.00
# age_s             |   0.13 | [ 0.05,  0.22] | 99.89% |    21.87% | 1.000 | 20682.00
# sex1              |   0.05 | [-0.03,  0.14] | 87.99% |    86.25% | 1.000 | 17940.00
# group1:age_s      |  -0.19 | [-0.28, -0.11] |   100% |     1.47% | 1.000 | 21577.00
# group1:sex1       |  -0.08 | [-0.17,  0.01] | 96.41% |    68.31% | 1.000 | 18332.00
# age_s:sex1        |  -0.06 | [-0.14,  0.03] | 91.02% |    83.46% | 1.000 | 18041.00
# group1:age_s:sex1 |   0.02 | [-0.07,  0.10] | 67.50% |    96.44% | 1.000 | 18329.00

draws <- as.data.frame(mod1_scs)
draws$cohens_d <- draws$`b_group1:age_s` / draws$sigma
median(draws$cohens_d)
#[1] -0.2114889

conditional_effects(mod1_scs, "age_s:group")

mydata %>% 
  group_by(group) %>% 
  summarise(
    avg_scs_tot = mean(scs_total_score, na.rm = TRUE),
    se_scs_tot = sd(scs_total_score, na.rm = TRUE) / sqrt(n())
  )
#   group             avg_scs_tot se_scs_tot
# 1 not_rescue_worker        79.2      0.753
# 2 rescue_worker            81.6      0.573


# Reduced Uncompassionate Responding --------------------------------------

mydata$rus <- scale(mydata$neg_self_compassion)[, 1] 

eq_var_rus <- bf(
  rus ~ group * age_s * sex
)

mod1_rus <- brm(
  eq_var_rus,
  family = student(),
  iter = 20000,
  prior = c(prior(student_t(3, 0, 1), class = Intercept),
            prior(gamma(2, .1), class = nu),
            prior(student_t(3, 0, 1), class = b),
            prior(cauchy(0, 1), class = sigma)),
  data = mydata,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstan"
)

mod1_rus <- add_criterion(mod1_rus, "loo")
loo(mod1_rus)
pp_check(mod1_rus, resp = "neg_self_compassion")

parameters::model_parameters(mod1_rus, ci = 0.95, standardize = "refit")
# Parameter         |    Median |         95% CI |     pd | % in ROPE |  Rhat |      ESS
# --------------------------------------------------------------------------------------
# (Intercept)       |     -0.14 | [-0.23, -0.06] | 99.92% |    17.90% | 1.000 | 26057.00
# group1            |     -0.10 | [-0.19, -0.02] | 99.10% |    46.09% | 1.000 | 24415.00
# age_s             |      0.08 | [-0.01,  0.17] | 96.32% |    69.46% | 1.000 | 22053.00
# sex1              |      0.08 | [-0.01,  0.17] | 96.51% |    67.43% | 1.000 | 20694.00
# group1:age_s      |     -0.21 | [-0.29, -0.12] |   100% |     0.72% | 1.000 | 23224.00
# group1:sex1       |     -0.08 | [-0.17,  0.01] | 96.44% |    68.21% | 1.000 | 21074.00
# age_s:sex1        | -1.67e-03 | [-0.09,  0.08] | 51.54% |    97.71% | 1.000 | 20387.00
# group1:age_s:sex1 |  4.15e-03 | [-0.08,  0.09] | 53.74% |    97.72% | 1.000 | 20212.00

draws <- as.data.frame(mod1_rus)
draws$cohens_d <- draws$`b_group1:age_s` / draws$sigma
median(draws$cohens_d)
# [1] -0.2184588

conditional_effects(mod1_rus, effects = "age_s:group")

round(bayes_R2(mod1_rus), 2)
#    Estimate Est.Error Q2.5 Q97.5
# R2     0.09      0.02 0.06  0.13


# Compassionate responding ------------------------------------------------

mydata$cs <- scale(mydata$pos_self_compassion)[, 1]

eq_var_cs <- bf(
  cs ~ group * age_s * sex
)

mod1_cs <- brm(
  eq_var_cs,
  family = student(),
  iter = 20000,
  prior = c(prior(student_t(3, 0, 1), class = Intercept),
            prior(gamma(2, .1), class = nu),
            prior(student_t(3, 0, 1), class = b),
            prior(cauchy(0, 1), class = sigma)),
  data = mydata,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstan"
)

mod1_cs <- add_criterion(mod1_cs, "loo")
loo(mod1_cs)
pp_check(mod1_cs, resp = "pos_self_compassion")

parameters::model_parameters(mod1_cs, ci = 0.95, standardize = "refit")
# Parameter         | Median |         95% CI |     pd | % in ROPE |  Rhat |      ESS
# -----------------------------------------------------------------------------------
# (Intercept)       |   0.02 | [-0.07,  0.11] | 67.34% |    95.38% | 1.000 | 24308.00
# group1            |   0.06 | [-0.03,  0.15] | 89.09% |    82.88% | 1.000 | 23061.00
# age_s             |   0.13 | [ 0.04,  0.22] | 99.82% |    23.72% | 1.000 | 21493.00
# sex1              |  -0.02 | [-0.11,  0.07] | 70.28% |    94.66% | 1.000 | 19835.00
# group1:age_s      |  -0.05 | [-0.14,  0.04] | 85.70% |    87.57% | 1.000 | 22296.00
# group1:sex1       |  -0.04 | [-0.12,  0.05] | 78.24% |    91.94% | 1.000 | 19582.00
# age_s:sex1        |  -0.10 | [-0.19, -0.02] | 98.85% |    46.66% | 1.000 | 19173.00
# group1:age_s:sex1 |   0.01 | [-0.07,  0.10] | 62.79% |    96.52% | 1.000 | 19552.00

conditional_effects(mod1_cs, effects = "age_s:sex")
# increase of CS with age for females, but not for males

draws <- as.data.frame(mod1_cs)
draws$cohens_d <- draws$`b_age_s:sex1` / draws$sigma
median(draws$cohens_d)
# -0.1071845

round(bayes_R2(mod1_cs), 2)
#    Estimate Est.Error Q2.5 Q97.5
# R2     0.03      0.01 0.02  0.06


# Descriptive stats
mydata %>% 
  group_by(group) %>% 
  summarise(
    n = n(),
    avg_age = mean(age),
    sd_age = sqrt(var(age, na.rm = TRUE)),
    avg_scs = mean(scs_total_score),
    se_scs = sd(scs_total_score) / sqrt(n()),
    avg_rus = mean(neg_self_compassion),
    se_rus = sd(neg_self_compassion) / sqrt(n()),
    avg_cs = mean(pos_self_compassion),
    se_cs = sd(pos_self_compassion) / sqrt(n())
  )
#   group                 n avg_age sd_age avg_scs se_scs avg_rus se_rus avg_cs se_cs
# 1 not_rescue_worker   324    25.6   10.8    79.2  0.753    40.7  0.664   38.6 0.551
# 2 rescue_worker       783    41.1   13.9    81.6  0.573    43.2  0.420   38.5 0.319

mydata %>% 
  group_by(group) %>% 
  summarise(
    n = n()
  )
# group                 n
# 1 not_rescue_worker   324
# 2 rescue_worker       783

mydata %>% 
  group_by(group, sex) %>% 
  summarise(
    n = n()
  )
# group             sex         n
# 1 not_rescue_worker Maschio    83
# 2 not_rescue_worker Femmina   241
# 3 rescue_worker     Maschio   425
# 4 rescue_worker     Femmina   358



# e  n  d  ----------------------------------------------------------






# there is no evidence that an unequal variance model is justified
if (0) {
  uneq_var_scs <- bf(
    scs ~ group + age_s + sex + (1 | where), 
    sigma ~ group + (1 | where)
  )
  
  get_prior(
    uneq_var_scs,
    data = mydata,
    family = student
  )
  
  hist(mydata$scs)
  
  mod_scs <- brm(
    uneq_var_scs,
    family = student,
    iter = 20000,
    prior = c(prior(student_t(3, 0, 1), class = Intercept),
              prior(gamma(2, .1), class = nu),
              prior(student_t(3, 0, 1), class = b),
              prior(cauchy(0, 1), dpar = sigma)),
    data = mydata,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.999),
    backend = "cmdstan"
  )
  
  mod_scs <- add_criterion(mod_scs, "loo")
  loo(mod_scs)
  
  pp_check(mod_scs, resp = "scs_total_score")
  
  round(bayes_R2(mod_scs), 2)
  
  summary(mod_scs)
}


# Comparison with the data of Neff (2003b) --------------------------

mydata %>% 
  summarise(
    n = n(),
    avg_scs = mean(scs_total_score),
    sd_scs = sd(scs_total_score)
  )



# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t_test <- function(
  m1, m2, s1, s2, n1, n2, m0=0, equal.variance=FALSE)
{
  if (equal.variance==FALSE) {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ((s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt((1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0) / se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t), df), df)    
  names(dat) <- c("Mean Diff", "Std Error", "t", "p-value", "df")
  return(dat) 
}

# all sample 
y <- mydata$scs_total_score
y30 <- y * 30 / (26*5)
m1 <- mean(y30)
m1
s1 <- sd(y30)
n1 <- length(y30)

m2 <- 18.25
s2 <- 3.75
n2 <- 391

# t-test between average total score of the SCS scale reported by Neff (2003b)
# and the present data (rescue-workers + not-rescue-workers)
round(t_test(m1, m2, s1, s2, n1, n2), 3)



# first time responders
y <- mydata[mydata$is_rescue_worker == 1, ]$scs_total_score
y30 <- y * 30 / (26*5)
m1 <- mean(y30)
m1
s1 <- sd(y30)
s1
n1 <- length(y30)

m2 <- 18.25
s2 <- 3.75
n2 <- 391

# t-test between average total score of the SCS scale reported by Neff (2003b)
# and the present data (rescue-workers + not-rescue-workers)
round(t_test(m1, m2, s1, s2, n1, n2), 3)
# Mean Diff Std Error         t   p-value        df 
#     0.583     0.231     2.522     0.012   770.164  

# effect size
std_pooled <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
std_pooled

(m1 - m2) / std_pooled
# 0.1568862


# NON first time responders
y <- mydata[mydata$is_rescue_worker == 0, ]$scs_total_score
y30 <- y * 30 / (26*5)
m1 <- mean(y30)
m1
s1 <- sd(y30)
n1 <- length(y30)

m2 <- 18.25
s2 <- 3.75
n2 <- 391

# t-test between average total score of the SCS scale reported by Neff (2003b)
# and the present data (rescue-workers + not-rescue-workers)
round(t_test(m1, m2, s1, s2, n1, n2), 3)
# Mean Diff Std Error         t   p-value        df 
#     0.583     0.231     2.522     0.012   770.164  

# effect size
std_pooled <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
std_pooled
(m1 - m2) / std_pooled


# RUS component
rw_rus <- mydata[mydata$is_rescue_worker == 1, ]$neg_self_compassion / 13
m1 <- mean(rw_rus)
m1
s1 <- sd(rw_rus)
s1
n1 <- length(rw_rus)
n1

m2 <- (3.01 + 3.39 + 3.05) / 3
s2 <- (0.92 + 0.76 + 0.96) / 3
n2 <- 391
round(t_test(m1, m2, s1, s2, n1, n2), 3)

# effect size
std_pooled <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
std_pooled
(m1 - m2) / std_pooled
m1/m2

# CS component
rw_cs <- mydata[mydata$is_rescue_worker == 1, ]$pos_self_compassion / 13
m1 <- mean(rw_cs)
m1
s1 <- sd(rw_cs)
s1
n1 <- length(rw_cs)
n1

m2 <- (3.05 + 3.14 + 2.99) / 3
s2 <- (0.75 + 0.79 + 0.79) / 3
n2 <- 391
round(t_test(m1, m2, s1, s2, n1, n2), 3)

# effect size
std_pooled <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
std_pooled
(m1 - m2) / std_pooled
m1/m2



GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

p1 <- mydata %>% 
  ggplot(aes(x=group,y=pos_self_compassion, fill=sex)) + 
  geom_split_violin() +
  ylim(10, 70) + 
  theme(legend.position = "none")

p2 <- mydata %>% 
  ggplot(aes(x=group,y=neg_self_compassion, fill=sex)) + 
  geom_split_violin() +
  ylim(10, 70)

p1 + p2


