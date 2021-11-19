library("cmdstanr")
set_cmdstan_path("/Users/corrado/cmdstan")
library("brms")
library("tidyverse")
library("here")
library("MplusAutomation")
library("tidyLPA")
library("emmeans")
library("tidybayes")
library("patchwork")

theme_set(theme_bw())



# (1) In the file used for the analysis of the factor structure of the SCS, 
# considered by itself, there are no missing values. There are __783__ 
# observations. There is no column with a subjID code.
scs_items_data <- read.table(here("scripts", "_mplus", "selfcompassionitems.dat"))

# (2) In the data.frame `clean_dat` that had been used for the main SEM analyses
# there are __731__ observations. This can be justified by saying that not all 
# participants completed all tests.

# (3) I must obtain 731 observations also for the LPA. Actually, I got 732, and
# they can be get as follows:

df <- readRDS(here("data", "processed", "rescue_workers_for_LPA.rds"))

df1 <- df %>% 
  dplyr::rename(
    age = age_imp
  ) %>% 
  mutate(
    ptg = life_appreciation + new_possibilities + personal_strength,
    ies = avoiding + intrusivity + iperarousal,
    pos_self_compassion = self_kindness + common_humanity + mindfulness,
    neg_self_compassion = self_judgment + isolation + over_identification,
    self_comp = pos_self_compassion + neg_self_compassion
  )

df1 %>% 
  summarise(
    n = n(),
    mean_age = round(mean(age, na.rm = TRUE), 3),
    sd_age = round(sd(age, na.rm = TRUE), 3),
    percent_female = round(100*sum(gender=="female") / n, 2),
    mean_education_years = mean(edu_years, na.rm = TRUE),
    sd_education_years = sd(edu_years, na.rm = TRUE),
    mean_experience_years = mean(years_experience, na.rm = TRUE),
    sd_experience_years = sd(years_experience, na.rm = TRUE),
    mean_last_training_months = mean(last_training_months, na.rm = TRUE),
    sd_last_training_months = sd(last_training_months, na.rm = TRUE),
    mean_activity_rate_per_week = mean(activity_rate_per_week, na.rm = TRUE),
    sd_activity_rate_per_week = sd(activity_rate_per_week, na.rm = TRUE),
    mean_ptg = mean(ptg, na.rm = TRUE),
    sd_ptg = sd(ptg, na.rm = TRUE),
    mean_ies = mean(ies, na.rm = TRUE),
    sd_ies = sd(ies, na.rm = TRUE)
  ) %>% 
  round(2) %>% 
  t()

# Standardize numeric variables
ds <- df1 %>% 
  mutate_if(is.numeric, scale) %>% 
  as.vector()



# Gender effects ----------------------------------------------------------


y1 <- ds[ds$gender == "female", ]$self_comp %>% na.omit()
y2 <- ds[ds$gender == "male", ]$self_comp %>% na.omit()

BESTout <- BEST::BESTmcmc(y1, y2, parallel=TRUE)
summary(BESTout)
#             mean median   mode HDI%   HDIlo    HDIup compVal %>compVal
# mu1       -0.168 -0.168 -0.170   95 -0.2869  -0.0485                  
# mu2        0.153  0.153  0.154   95  0.0645   0.2402                  
# muDiff    -0.321 -0.321 -0.321   95 -0.4722  -0.1758       0     3e-03
# sigma1     1.071  1.070  1.069   95  0.9785   1.1599                  
# sigma2     0.864  0.864  0.864   95  0.7936   0.9349                  
# sigmaDiff  0.207  0.207  0.208   95  0.0992   0.3115       0     1e+02
# nu        46.450 38.311 24.797   95  8.9603 107.3895                  
# log10nu    1.590  1.583  1.564   95  1.0997   2.0852                  
# effSz     -0.330 -0.330 -0.327   95 -0.4827  -0.1755       0     3e-03


# Self-compassion and the five personality traits of the NEO-FFI ----------


m1 <- brms::brm(
  self_comp ~ neuroticism + extraversion + openness +
    agreeableness + conscientiousness +
  (1 | where/red_cross_commeetee_location), 
  family = student(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 0.5), class = b),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .999, max_treedepth = 15),
  backend = "cmdstan", 
  data = ds
)
m1 <- add_criterion(m1, "loo")
loo(m1)

pp_check(m1)
bayes_R2(m1)
#    Estimate  Est.Error      Q2.5     Q97.5
# R2 0.473479 0.01934904 0.4338349 0.5087492
summary(m1)
#                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             0.01      0.38    -0.86     0.92 1.01     1067      881
# neuroticism          -0.61      0.03    -0.67    -0.55 1.00     3602     2883
# extraversion          0.13      0.03     0.06     0.19 1.00     3845     3296
# openness              0.01      0.03    -0.05     0.06 1.00     3958     2427
# agreeableness         0.08      0.03     0.02     0.14 1.00     3814     2423
# conscientiousness    -0.03      0.03    -0.09     0.03 1.00     3906     2873

m1a <- brms::brm(
  self_comp ~ age + edu_years + years_experience + 
    last_training_months + activity_rate_per_week +
    (1 | where/red_cross_commeetee_location), 
  family = student(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 0.5), class = b),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99, max_treedepth = 15),
  backend = "cmdstan", 
  data = ds
)
m1a <- add_criterion(m1a, "loo")
loo(m1a)

pp_check(m1a)
bayes_R2(m1a)
#    Estimate  Est.Error       Q2.5    Q97.5
# R2 0.103146 0.02232974 0.06393313 0.149966
summary(m1a)
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                  0.01      0.38    -0.90     0.85 1.00     1370      968
# age                        0.23      0.04     0.15     0.31 1.00     4102     2653
# edu_years                 -0.03      0.04    -0.10     0.05 1.00     4962     3147
# years_experience           0.09      0.04     0.01     0.16 1.00     4581     3320
# last_training_months      -0.04      0.03    -0.11     0.03 1.00     4717     3200
# activity_rate_per_week     0.03      0.04    -0.05     0.10 1.00     5877     2968

hist(ds$ptg)
m3 <- brms::brm(
  ptg ~ neuroticism + extraversion + openness +
    agreeableness + conscientiousness + 
    gender + age + edu_years + years_experience + 
    last_training_months + activity_rate_per_week +
    (1 | where/red_cross_commeetee_location), 
  family = student(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = b),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99, max_treedepth = 15),
  backend = "cmdstan", 
  data = ds
)
m3 <- add_criterion(m3, "loo")
loo(m3)
pp_check(m3)
bayes_R2(m3)
#     Estimate  Est.Error       Q2.5     Q97.5
# R2 0.1173605 0.02246319 0.07636047 0.1633898
summary(m3)
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                  0.09      0.36    -0.76     0.85 1.00     2098     1573
# neuroticism                0.17      0.05     0.08     0.26 1.00     5869     2993
# extraversion               0.23      0.05     0.14     0.33 1.00     5573     3007
# openness                   0.04      0.04    -0.04     0.12 1.00     7555     2783
# agreeableness             -0.06      0.04    -0.14     0.02 1.00     6033     2967
# conscientiousness          0.12      0.04     0.04     0.21 1.00     6787     2157
# gendermale                -0.17      0.08    -0.33    -0.01 1.01     7637     3238
# age                        0.09      0.04     0.01     0.18 1.00     5326     3065
# edu_years                 -0.01      0.04    -0.09     0.06 1.00     5423     2673
# years_experience          -0.03      0.04    -0.11     0.05 1.00     6116     3129
# last_training_months      -0.02      0.04    -0.09     0.05 1.00     7563     2954
# activity_rate_per_week     0.06      0.04    -0.02     0.13 1.00     7552     2701


hist(ds$ies)
m4 <- brms::brm(
  ies ~ neuroticism + extraversion + openness +
    agreeableness + conscientiousness + 
    gender + age + edu_years + years_experience + 
    last_training_months + activity_rate_per_week +
    (1 | where/red_cross_commeetee_location), 
  family = skew_normal(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = b),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99),
  backend = "cmdstan", 
  data = ds
)
m4 <- add_criterion(m4, "loo")
loo(m4)
pp_check(m4)
bayes_R2(m4)
#     Estimate  Est.Error       Q2.5     Q97.5
# R2 0.03466239 0.01248015 0.014653 0.06429405

summary(m4)
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                  0.07      0.28    -0.57     0.70 1.00      979      946
# neuroticism                0.14      0.03     0.08     0.21 1.00     2484     2895
# extraversion               0.04      0.03    -0.01     0.10 1.00     3157     2935
# openness                   0.03      0.02    -0.00     0.08 1.00     4188     3166
# agreeableness              0.01      0.02    -0.03     0.05 1.00     3598     2835
# conscientiousness          0.02      0.02    -0.03     0.06 1.00     3869     3119
# gendermale                -0.08      0.04    -0.17     0.01 1.00     3344     2905
# age                       -0.05      0.02    -0.10    -0.01 1.00     3351     3067
# edu_years                 -0.05      0.02    -0.09    -0.01 1.00     3078     2404
# years_experience           0.02      0.02    -0.03     0.06 1.00     2914     2670
# last_training_months      -0.00      0.02    -0.04     0.04 1.00     4418     3132
# activity_rate_per_week     0.00      0.02    -0.03     0.04 1.00     4305     3020


# Latent profile analysis -------------------------------------------------


df1$cope <- df1$positive_attitude + df1$problem_orientation
df1$rel_sc <- (df1$neg_self_compassion + df1$pos_self_compassion) / 2


m6 <- df1 %>%
  select(neg_self_compassion, pos_self_compassion, rel_sc,
         neuroticism, ptg, ies, cope) %>% 
  rename(
    rus=neg_self_compassion,
    cs=pos_self_compassion,
    neu=neuroticism
  ) %>% 
  na.omit() %>% 
  scale() %>%
  estimate_profiles(
    1:6, 
    variances = c("equal", "varying"),
    covariances = c("zero", "varying"),
    package = "MplusAutomation") %>% 
  compare_solutions(statistics = c("AIC", "BIC"))

get_estimates(m6)

df1 %>%
  select(neg_self_compassion, pos_self_compassion, neuroticism,
         ptg, ies, cope) %>% 
  rename(
    rus=neg_self_compassion,
    cs=pos_self_compassion,
    neu=neuroticism
  ) %>% 
  single_imputation() %>%
  scale() %>%
  estimate_profiles(1:6, 
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))



m2 <- df1 %>%
  select(neg_self_compassion, pos_self_compassion, neuroticism,
         ptg, ies, cope) %>% 
  rename(
    rus=neg_self_compassion,
    cs=pos_self_compassion,
    neu=neuroticism
  ) %>% 
  single_imputation() %>%
  scale() %>%
  estimate_profiles(2, package = "mplus")

get_estimates(m2)

lpa2 <- get_data(m2)

get_fit(m2)


lpa2$Class <- lpa2$Class - 1

summary(fm <- lm(ies ~ Class, lpa2))
summary(fm <- lm(ptg ~ Class, lpa2))


#--------------
df1$selfco <- df1$pos_self_compassion + df1$neg_self_compassion

m2 <- df1 %>%
  select(selfco, neuroticism, cope) %>% 
  rename(
    neu=neuroticism
  ) %>% 
  single_imputation() %>%
  scale() %>%
  estimate_profiles(2, package = "mplus")

get_estimates(m2)

lpa2 <- get_data(m2)

get_fit(m2)


lpa2$Class <- lpa2$Class - 1

df1$grp <- lpa2$Class

summary(fm <- lm(ies ~ grp, df1))
summary(fm <- lm(ptg ~ grp, df1))


#--------------
df1$selfco <- df1$pos_self_compassion + df1$neg_self_compassion

m2 <- scs %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(2, package = "mplus")

get_estimates(m2)

lpa2 <- get_data(m2)

get_fit(m2)


lpa2$Class <- lpa2$Class - 1

df1$grp <- lpa2$Class

summary(fm <- lm(ies ~ grp, df1))
summary(fm <- lm(ptg ~ grp, df1))


summary(fm <- lm(neg_self_compassion ~ grp, df1))
summary(fm <- lm(pos_self_compassion ~ grp, df1))


all <- cbind(df1, scs) 
all <- all[!is.na(all$sk26), ]



m6 <- all[, 69:94] %>%
  poms() %>%
  estimate_profiles(1:6, package = "mplus")




# Latent profile analysis -------------------------------------------------


m6_comp <- df1 %>%
  dplyr::select(
    self_kindness,             
    self_judgment, 
    common_humanity,
    isolation,    
    mindfulness, 
    over_identification
  ) %>% 
  scale() %>%
  estimate_profiles(
    1:6, 
    package = "mplus",
    ANALYSIS = "starts = 1000, 250;"
  ) %>%
  compare_solutions(statistics = c("AIC", "BIC"))
# Compare tidyLPA solutions:
#   
#   Model Classes AIC       BIC      
# 1     1       12481.952 12537.101
# 1     2       11492.200 11579.520
# 1     3       11119.128 11238.618
# 1     4       10910.152 11061.813
# 1     5       10793.676 10977.507
# 1     6       10700.768 10916.770
# 
# Best model according to AIC is Model 1 with 6 classes.
# Best model according to BIC is Model 1 with 6 classes.
# 
# An analytic hierarchy process, based on the fit indices AIC, AWE, BIC, CLC, 
# and KIC (Akogul & Erisoglu, 2017), suggests the best solution is Model 1 with
# 6 classes.

aic_bic <- c(
  12481.952, 12537.101,
  11492.200, 11579.520,
  11119.128, 11238.618,
  10910.152, 11061.813,
  10793.676, 10977.507,
  10700.768, 10916.770
)

M <- matrix(
  aic_bic, byrow = TRUE,
  ncol = 2
)

plot(1:6, M[, 1], type = 'l')
points(1:6, M[, 2], type = 'l', col = "blue")


# Plot --------------------------------------------------------------------


m6 <- df1 %>%
  dplyr::select(
    self_kindness,             
    self_judgment, 
    common_humanity,
    isolation,    
    mindfulness, 
    over_identification
  ) %>% 
  scale() %>%
  estimate_profiles(
    6, 
    package = "MplusAutomation",
    ANALYSIS = "starts = 1000, 250;"
  )

out <- get_estimates(m6) %>% 
  dplyr::filter(Category == "Means") %>% 
  as.data.frame()

out$Parameter <- factor(
  out$Parameter, 
  levels = c("self_kindness", "common_humanity", "mindfulness",
             "self_judgment", "over_identification", "isolation")
)

p <- out %>% 
  ggplot(aes(fill=Parameter, y=Estimate, x=Parameter)) + 
    geom_bar(position="dodge", stat="identity") +
    facet_wrap( ~Class) +
  labs(
    title = "Latent profile analysis patterns of the six SCS indicators",
    x = "",
    y = "z-standardized mean scores"
  ) +
  scale_fill_grey(
    name = "SCS scales", 
    labels = c("Self kindness", "Common humanity", "Mindfulness",
               "Self judgment", "Overidentification", "Isolation"),
    start = 0.9, end = 0.1
  ) +
  papaja::theme_apa() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="bottom"
  ) 
p

ggsave(
  p, 
  filename = "lpa.png", 
  width = 10, 
  height = 4, 
  dpi = 200, 
  device='png'
)



# Add classes to data.frame -----------------------------------------------


ds$class <- get_data(m6)$Class

table(ds$class) / sum(table(ds$class))


# PTG ---------------------------------------------------------------------


hist(ds$ptg)

m_ptg <- brms::brm(
  ptg ~ factor(class) +
    (1 | where/red_cross_commeetee_location), 
  family = student(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = b),
    prior(student_t(3, 0, 10), class = sigma)
  ),
  control = list(adapt_delta = .99, max_treedepth = 15),
  backend = "cmdstan", 
  data = ds
)

m_ptg <- add_criterion(m_ptg, "loo")
loo(m_ptg)
pp_check(m_ptg)
bayes_R2(m_ptg)
summary(m_ptg)


#get the adjusted means
m3_em <- emmeans(m_ptg, ~ class)
m3_em
# class  emmean lower.HPD upper.HPD
#     1 -0.0677    -1.012     0.776
#     2 -0.1059    -0.971     0.778
#     3 -0.3187    -1.194     0.554
#     4  0.2057    -0.673     1.075
#     5  0.1229    -0.656     1.103
#     6  0.2136    -0.590     1.178
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

#get all possible contrasts
cont_png <- contrast(m3_em, "tukey")
cont_png
# contrast estimate lower.HPD upper.HPD
# 1 - 2     0.04068   -0.2042    0.2986
# 1 - 3     0.25864   -0.0168    0.5233
# 1 - 4    -0.27031   -0.5397    0.0249
# 1 - 5    -0.18402   -0.4382    0.0927
# 1 - 6    -0.27888   -0.5331   -0.0337
# 2 - 3     0.21733   -0.0279    0.4444
# 2 - 4    -0.31157   -0.5834   -0.0678
# 2 - 5    -0.22388   -0.4597    0.0125
# 2 - 6    -0.31658   -0.5354   -0.0828
# 3 - 4    -0.52649   -0.7959   -0.2537
# 3 - 5    -0.44116   -0.6958   -0.1745
# 3 - 6    -0.53534   -0.7764   -0.2929
# 4 - 5     0.08744   -0.1902    0.3519
# 4 - 6    -0.00555   -0.2664    0.2520
# 5 - 6    -0.09441   -0.3373    0.1377
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

#get the posterior draws from the contrasts
cont_png_posterior <- gather_emmeans_draws(cont_png)

#plot
g1 <- ggplot(cont_png_posterior,
       aes(y = contrast, x = .value)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, color = "red", lty = 2) +
  labs(
    title = "PTG",
    x = "Value",
    y = "Contrast"
  ) +
  papaja::theme_apa()
g1


cntr <- c(
  0.04068,
  0.25864,
  -0.27031,
  -0.18402,
  -0.27888, 
  0.21733,
  -0.31157, 
  -0.22388,
  -0.31658, 
  -0.52649, 
  -0.44116, 
  -0.53534, 
  0.08744,
  -0.00555,
  -0.09441
)






# IES-R -------------------------------------------------------------------

hist(ds$ies)

m_ies <- brms::brm(
  ies ~ factor(class) +
    (1 | where/red_cross_commeetee_location), 
  family = skew_normal(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = b),
    prior(student_t(3, 0, 10), class = sigma)
  ),
  control = list(adapt_delta = .99, max_treedepth = 15),
  backend = "cmdstan", 
  data = ds
)

m_ies <- add_criterion(m_ies, "loo")
loo(m_ies)
pp_check(m_ies)
bayes_R2(m_ies)
summary(m_ies)

#get the adjusted means
m4_em <- emmeans (m_ies, ~ class)
m4_em
# class   emmean lower.HPD upper.HPD
# 1  0.15569    -0.545     0.820
# 2  0.00183    -0.727     0.615
# 3 -0.10420    -0.801     0.519
# 4  0.17153    -0.490     0.849
# 5 -0.04683    -0.763     0.582
# 6  0.07991    -0.643     0.705
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

#get all possible contrasts
cont_ies <- contrast(m4_em, "tukey")
cont_ies
# contrast estimate lower.HPD upper.HPD
# 1 - 2      0.1505    0.0106    0.3045
# 1 - 3      0.2548    0.0893    0.4293
# 1 - 4     -0.0194   -0.1555    0.1328
# 1 - 5      0.1983    0.0490    0.3504
# 1 - 6      0.0704   -0.0649    0.2061
# 2 - 3      0.1053   -0.0170    0.2338
# 2 - 4     -0.1694   -0.3080   -0.0470
# 2 - 5      0.0478   -0.0591    0.1549
# 2 - 6     -0.0795   -0.1959    0.0220
# 3 - 4     -0.2766   -0.4309   -0.1251
# 3 - 5     -0.0571   -0.1864    0.0560
# 3 - 6     -0.1843   -0.3250   -0.0633
# 4 - 5      0.2170    0.0960    0.3668
# 4 - 6      0.0907   -0.0372    0.2225
# 5 - 6     -0.1269   -0.2480   -0.0190
# 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

#get the posterior draws from the contrasts
cont_ies_posterior <- gather_emmeans_draws(cont_ies)

#plot
g2 <- ggplot(cont_ies_posterior,
       aes(y = contrast, x = .value)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, color = "red", lty = 2) +
  labs(
    title = "IES-R",
    x = "Value",
    y = ""
  ) +
  papaja::theme_apa()

g <- g1 + g2

fig <- p / (g1 + g2)

ggsave(
  fig, 
  filename = "lpa_contrasts.png", 
  width = 10, 
  height = 10, 
  dpi = 200, 
  device='png'
)



pred_data <- data.frame(
  profile = c("theone", "theother"),
  sample_size = c(13, 13),
  success = c(13, 5))

(my_prior <- get_prior(family = binomial,
                       success | trials(sample_size) ~ profile,
                       data = pred_data))



##################



hist(df1$ptg)
m1 <- brms::brm(
  ptg ~ lpc +
    (1 | where/red_cross_commeetee_location), 
  family = skew_normal(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 0.5), class = b),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .999, max_treedepth = 15),
  backend = "cmdstan", 
  data = temp1
)
m1 <- add_criterion(m1, "loo")
loo(m1)

pp_check(m1)
bayes_R2(m1)
summary(m1)


m2 <- brms::brm(
  ptg ~ lpc +
    (1 | where/red_cross_commeetee_location), 
  family = student(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = b),
    prior(student_t(3, 0, 10), class = sigma)
  ),
  control = list(adapt_delta = .999, max_treedepth = 15),
  backend = "cmdstan", 
  data = temp
)
m2 <- add_criterion(m2, "loo")
loo(m2)
pp_check(m2)
bayes_R2(m2)
summary(m2)




ds$class <- get_data(m6)$Class


















get_estimates(m6)

ggplot(m6) + geom_bar(aes(y = class))



m6 %>% 
  compare_solutions(statistics = c("AIC", "BIC"))




temp <- df1[df1$class %in% c(2, 3, 5), ]

temp$lpc <- ifelse(temp$class == 3, 1, 0) 

fm <- lm(ptg ~ lpc, temp)
summary(fm)

fm2 <- lm(ies ~ lpc, temp)
summary(fm2)


temp1 <- df1[!df1$class %in% c(2, 3, 5), ]

temp1$lpc <- ifelse(temp1$class == 6, 1, 0) 

fm <- lm(ptg ~ lpc, temp1)
summary(fm)

fm2 <- lm(ies ~ lpc, temp1)
summary(fm2)







ch <- outForest(scs)
outliers(ch)
oo <- outliers(ch)$row
length(oo)
length(unique(oo))


md <- mahalanobis(scs, center = colMeans(scs), cov = cov(scs))
alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(scs)))
names_outliers_MH <- which(md > cutoff)
excluded_mh <- names_outliers_MH
data_clean_mh <- scs[-excluded_mh, ]
scs[excluded_mh, ]


colnames(scs) <- c(
  "sj1",  "oi2",  "ch3",  "is4",  "sk5",  "oi6",  "ch7", 
  "sj8",  "mi9",  "ch10", "sj11", "sk12", "is13", "mi14", 
  "ch15", "sj16", "mi17", "is18", "sk19", "oi20", 
  "sj21", "mi22", "sk23", "oi24", "is25", "sk26"
)



