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

d$scs_total_score <- d$pos_self_compassion + d$neg_self_compassion

# get fs from 501_factor_invariance_test.R
dd <- bind_cols(d, fs)

dd <- dd %>% 
  dplyr::rename(
    fs_cs = F2,
    fs_rus = F1
  )

cor(dd$pos_self_compassion, dd$fs_cs)
cor(dd$neg_self_compassion, dd$fs_rus)

d %>% 
  group_by(is_rescue_worker) %>% 
  summarise(
    n = n()
  )

d %>% 
  group_by(is_rescue_worker) %>% 
  summarise(
    n = n(),
    age = mean(age),
    avg_scs = mean(scs_total_score),
    sd_scs = sd(scs_total_score),
    avg_rus = mean(neg_self_compassion),
    sd_rus = sd(neg_self_compassion),
    avg_cs = mean(pos_self_compassion),
    sd_cs = sd(pos_self_compassion)
  )
#   is_rescue_worker sex         n   age avg_scs sd_scs avg_rus sd_rus avg_cs sd_cs
# 1                0 Femmina   105  30.8    73.4   16.2    36.2   10.6   37.2  9.46
# 2                0 Maschio    48  32.6    77.6   14.8    39.7   11.0   37.9  8.83
# 3                1 Femmina   358  37.5    78.9   17.7    40.5   11.9   38.4  9.66
# 4                1 Maschio   425  42.1    83.9   14.1    45.4   11.2   38.5  8.26

mydata <- dd
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
  #    Estimate Est.Error Q2.5 Q97.5
  # R2     0.12      0.02 0.08  0.15
  
  summary(mod_scs)
  #                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
  # Intercept          -0.07      0.37    -0.99     0.74 1.00     1532     1158
  # sigma_Intercept    -0.09      0.51    -1.20     1.11 1.01     1410     1148
  # group1             -0.10      0.05    -0.21     0.00 1.00     3581     3248
  # age_s               0.27      0.03     0.21     0.33 1.00     3678     2785
  # sex1               -0.11      0.03    -0.17    -0.05 1.00     3630     2962
  # sigma_group1        0.01      0.04    -0.07     0.08 1.00     3632     2410
}


eq_var_scs <- bf(
  scs ~ group + age_s + sex 
)

mod1_scs <- brm(
  eq_var_scs,
  family = student,
  prior = c(prior(student_t(3, 0, 1), class = Intercept),
            prior(gamma(2, .1), class = nu),
            prior(student_t(3, 0, 1), class = b),
            prior(cauchy(0, 1), class = sigma)),
  data = mydata,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.999),
  backend = "cmdstan"
)

mod1_scs <- add_criterion(mod1_scs, "loo")
loo(mod1_scs)

pp_check(mod1_scs, resp = "scs_total_score")

round(bayes_R2(mod1_scs), 2)
#    Estimate Est.Error Q2.5 Q97.5
# R2     0.12      0.02 0.08  0.15

summary(mod1_scs)
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept    -0.07      0.04    -0.15     0.01 1.00     3323     2636
# group1       -0.10      0.04    -0.19    -0.02 1.00     2867     2752
# age_s         0.27      0.03     0.20     0.33 1.00     3060     2630
# sex1         -0.11      0.03    -0.17    -0.05 1.00     3020     2635

effectsize::standardize_parameters(mod1_scs, ci = 0.95)
# Component   |   Parameter | Median (std.) |         95% CI
# ----------------------------------------------------------
# conditional | b_Intercept |         -0.07 | [-0.16,  0.01]
# conditional |    b_group1 |         -0.10 | [-0.19, -0.02]
# conditional |     b_age_s |          0.27 | [ 0.21,  0.33]
# conditional |      b_sex1 |         -0.11 | [-0.17, -0.05]
# sigma       |       sigma |          0.91 | [ 0.86,  0.96]

draws <- as.data.frame(mod1_scs)
draws$cohens_d <- draws$b_group1 / draws$sigma
median(draws$cohens_d)
#[1] -0.1141712

mydata %>% 
  group_by(group) %>% 
  summarise(
    avg_scs_tot = mean(scs_total_score, na.rm = TRUE),
    se_scs_tot = sd(scs_total_score, na.rm = TRUE) / sqrt(n())
  )


# Reduced Uncompassionate Responding --------------------------------------

mydata$rus <- scale(mydata$neg_self_compassion)[, 1] 
# mydata$rus <- scale(mydata$fs_rus)[, 1] 

# there is no evidence of unequal variance
if(0) {
  uneq_var_rus <- bf(
    rus ~ group + age_s + sex + (1 | where), 
    sigma ~ group + (1 | where)
  )
  
  mod_rus <- brm(
    uneq_var_rus,
    family = student,
    prior = c(prior(student_t(3, 0, 1), class = Intercept),
              prior(gamma(2, .1), class = nu),
              prior(student_t(3, 0, 1), class = b),
              prior(cauchy(0, 1), dpar = sigma)),
    data = mydata,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.999),
    backend = "cmdstan", 
  )
  
  mod_rus <- add_criterion(mod_rus, "loo")
  loo(mod_rus)
  
  pp_check(mod_rus, resp = "neg_self_compassion")
  
  round(bayes_R2(mod_rus), 2)
  #    Estimate Est.Error Q2.5 Q97.5
  # R2     0.12      0.02 0.09  0.16
  
  summary(mod_rus)
  #                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
  # Intercept          -0.78      0.41    -1.68     0.08 1.01     1291     1185
  # sigma_Intercept    -0.10      0.64    -1.53     1.22 1.00     1571     1315
  # group1             -0.15      0.05    -0.26    -0.05 1.00     3388     2714
  # age                 0.02      0.00     0.01     0.02 1.00     6135     3197
  # sex1               -0.16      0.03    -0.22    -0.09 1.00     3255     2675
  # sigma_group1       -0.05      0.04    -0.13     0.03 1.00     2814     2636
}


eq_var_rus <- bf(
  rus ~ group + age_s + sex
)

mod1_rus <- brm(
  eq_var_rus,
  family = student,
  prior = c(prior(student_t(3, 0, 1), class = Intercept),
            prior(gamma(2, .1), class = nu),
            prior(student_t(3, 0, 1), class = b),
            prior(cauchy(0, 1), class = sigma)),
  data = mydata,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.999),
  backend = "cmdstan"
)

mod1_rus <- add_criterion(mod1_rus, "loo")
loo(mod1_rus)
pp_check(mod1_rus, resp = "neg_self_compassion")

conditional_effects(mod1_rus, effects = "group")


parameters::model_parameters(mod1_rus, ci = 0.95, standardize = "refit")
# Parameter   | Median |         95% CI |     pd | % in ROPE |  Rhat |     ESS
# ----------------------------------------------------------------------------
# (Intercept) |  -0.09 | [-0.18, -0.01] | 98.60% |    58.00% | 1.000 | 3999.00
# group1      |  -0.14 | [-0.22, -0.06] | 99.80% |    20.50% | 1.001 | 3798.00
# age_s       |   0.23 | [ 0.17,  0.30] |   100% |        0% | 1.000 | 4120.00
# sex1        |  -0.16 | [-0.23, -0.10] |   100% |     2.80% | 1.000 | 3639.00


effectsize::standardize_parameters(mod1_rus, ci = 0.95)
# Component   |   Parameter | Median (std.) |         95% CI
# ----------------------------------------------------------
# conditional | b_Intercept |         -0.09 | [-0.17, -0.01]
# conditional |    b_group1 |         -0.13 | [-0.22, -0.05]
# conditional |     b_age_s |          0.23 | [ 0.17,  0.29]
# conditional |      b_sex1 |         -0.16 | [-0.22, -0.10]
# sigma       |       sigma |          0.93 | [ 0.88,  0.97]

draws <- as.data.frame(mod1_rus)
draws$cohens_d <- draws$b_group1 / draws$sigma
median(draws$cohens_d)
# [1] -0.1451386


# Compassionate responding ------------------------------------------------

mydata$cs <- scale(mydata$pos_self_compassion)[, 1]

# uneq_var_cs <- bf(
#   cs ~ group + age + sex + (1 | where), 
#   sigma ~ group + (1 | where)
# )
# 
# mod_cs <- brm(
#   uneq_var_cs,
#   family = student,
#   prior = c(prior(student_t(3, 0, 1), class = Intercept),
#             prior(gamma(2, .1), class = nu),
#             prior(student_t(3, 0, 1), class = b),
#             prior(cauchy(0, 1), dpar = sigma)),
#   data = mydata,
#   cores = parallel::detectCores(),
#   control = list(adapt_delta = 0.999),
#   backend = "cmdstan"
# )
# 
# # prior predictive check
# # hyp1 <- hypothesis(mod_cs, "group1 = 0")
# # plot(hyp1, plot = FALSE)[[1]] +
# #   coord_cartesian(xlim = c(-2.5, 2.5))
# 
# # loo
# mod_cs <- add_criterion(mod_cs, "loo")
# loo1 <- loo(mod_cs, save_psis = TRUE)
# plot(loo1)
# 
# pp_check(mod_cs, resp = "pos_self_compassion")
# pp_check(mod_cs, type = "stat", stat = 'median', nsamples = 100)
# pp_check(mod_cs, type = "stat", stat = 'var', nsamples = 100)
# 
# 
# round(bayes_R2(mod_cs), 2)
# #    Estimate Est.Error Q2.5 Q97.5
# # R2     0.04      0.01 0.02  0.06

eq_var_cs <- bf(
  cs ~ group + age_s + sex
)

mod1_cs <- brm(
  eq_var_cs,
  family = student,
  prior = c(prior(student_t(3, 0, 1), class = Intercept),
            prior(gamma(2, .1), class = nu),
            prior(student_t(3, 0, 1), class = b),
            prior(cauchy(0, 1), class = sigma)),
  data = mydata,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.999),
  backend = "cmdstan"
)

mod1_cs <- add_criterion(mod1_cs, "loo")
loo(mod1_cs)
pp_check(mod1_cs, resp = "pos_self_compassion")

parameters::model_parameters(mod1_cs, ci = 0.95, standardize = "refit")
# Parameter   |    Median |        95% CI |     pd | % in ROPE |  Rhat |     ESS
# ------------------------------------------------------------------------------
# (Intercept) | -1.86e-03 | [-0.09, 0.08] | 51.38% |    97.15% | 1.001 | 2852.00
# group1      | -4.63e-03 | [-0.10, 0.08] | 54.02% |    96.85% | 1.000 | 3165.00
# age_s       |      0.18 | [ 0.11, 0.24] |   100% |     0.60% | 1.002 | 3042.00
# sex1        |      0.01 | [-0.05, 0.08] | 66.97% |    99.75% | 1.001 | 2937.00

draws <- as.data.frame(mod1_cs)
draws$cohens_d <- draws$b_group1 / draws$sigma
median(draws$cohens_d)
# -0.00566491

mydata %>% 
  group_by(is_rescue_worker) %>% 
  summarise(
    n = n(),
    age = mean(age),
    avg_scs = mean(scs_total_score),
    se_scs = sd(scs_total_score) / sqrt(n()),
    avg_rus = mean(neg_self_compassion),
    se_rus = sd(neg_self_compassion) / sqrt(n()),
    avg_cs = mean(pos_self_compassion),
    se_cs = sd(pos_self_compassion) / sqrt(n())
  )
#   is_rescue_worker     n   age avg_scs se_scs avg_rus se_rus avg_cs se_cs
# 1                0   153  31.3    74.7  1.28     37.3  0.874   37.4 0.748
# 2                1   783  40.0    81.6  0.573    43.2  0.420   38.5 0.319



# Comparison with the data of Neff (2003b) --------------------------------

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


