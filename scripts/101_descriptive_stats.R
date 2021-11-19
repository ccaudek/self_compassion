library("cmdstanr")
set_cmdstan_path("/Users/corrado/cmdstan")
library("brms")
library("tidyverse")
library("here")
library("MplusAutomation")
library("tidyLPA")

# data comes from df1 created by 01_import_data.Rmd

# Recode rate of activity as a numeric variable
df1$activity_rate_per_week <- as.numeric(
  as.character(fct_recode(
    df1$rate_of_activity,
    "1" = "1 turno a settimana",
    "2" = "Più di 1 turno a settimana",
    "0.5" = "1 turno ogni due settimane",
    "0.25" = "1 turno al mese",
    "0.08" = "Meno di 1 turno al mese"
  ))
)

# Recode education as a numeric variable
df1$edu_years <- as.numeric(
  as.character(fct_recode(
    df1$education,
    "8" = "Scuola media primaria",
    "13" = "Diploma",
    "16" = "Laurea breve",
    "18" = "Laurea magistrale",
    "21" = "Dottorato"
  ))
)

# Recode last_training as a numeric variable
df1$last_training_months <- as.numeric(
  as.character(fct_recode(
    df1$last_training,
    "1" = "Meno di 2 mesi fa",
    "3" = "Più di 2 mesi fa",
    "5" = "Meno di 6 mesi fa",
    "9" = "Più di 6 mesi fa",
    "10.5" = "Meno di 1 anno fa",
    "18" = "Più di 1 anno fa"
  ))
)

df1$job_qualification <- fct_recode(
  df1$job_qualification,
  "Soccorritore" = "Infermiere",
  "Soccorritore" = "Medico"
)

df1$years_experience <- as.numeric(as.character(df1$years_experience))
df1$years_experience <- ifelse(df1$years_experience == 2011, 9, df1$years_experience)

df1 %>% 
  dplyr::filter(!is.na(neuroticism)) %>% 
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

ds <- df1 %>% 
  dplyr::filter(!is.na(neuroticism)) %>% 
  mutate_if(is.numeric, scale) %>% 
  as.vector()

ds$self_comp <- scale(ds$pos_self_compassion + ds$neg_self_compassion) %>% 
  as.vector()

y1 <- ds[ds$gender == "female", ]$self_comp %>% na.omit()
y2 <- ds[ds$gender == "male", ]$self_comp %>% na.omit()

BESTout <- BEST::BESTmcmc(y1, y2, parallel=TRUE)
summary(BESTout)
#            mean median   mode HDI%   HDIlo    HDIup compVal %>compVal
# mu1       -0.145 -0.145 -0.143   95 -0.2588  -0.0279                  
# mu2        0.132  0.132  0.134   95  0.0486   0.2160                  
# muDiff    -0.277 -0.277 -0.274   95 -0.4188  -0.1353       0     1e-02
# sigma1     1.090  1.090  1.090   95  1.0011   1.1786                  
# sigma2     0.849  0.848  0.849   95  0.7812   0.9151                  
# sigmaDiff  0.242  0.241  0.237   95  0.1401   0.3453       0     1e+02
# nu        46.203 38.273 25.053   95  9.2311 105.0848                  
# log10nu    1.590  1.583  1.546   95  1.1186   2.0802                  
# effSz     -0.284 -0.283 -0.277   95 -0.4321  -0.1394       0     1e-02



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
  control = list(adapt_delta = .999),
  backend = "cmdstan", 
  data = ds
)
m1 <- add_criterion(m1, "loo")
loo(m1)

pp_check(m1)
bayes_R2(m1)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.4383731 0.01965339 0.3980618 0.4752477
summary(m1)
#                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept            -0.00      0.33    -0.82     0.75 1.00     2052     1593
# neuroticism          -0.59      0.03    -0.65    -0.53 1.00     4405     3075
# extraversion          0.10      0.03     0.04     0.17 1.00     4470     3423
# openness              0.02      0.03    -0.03     0.07 1.00     5569     3375
# agreeableness         0.06      0.03     0.01     0.12 1.00     5686     3487
# conscientiousness    -0.01      0.03    -0.07     0.05 1.00     4962     3011

m1a <- brms::brm(
  self_comp ~ age + edu + years_experience + 
    last_training_months + activity_rate_per_week +
    (1 | where/red_cross_commeetee_location), 
  family = student(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 0.5), class = b),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99),
  backend = "cmdstan", 
  data = ds
)
m1a <- add_criterion(m1a, "loo")
loo(m1a)

pp_check(m1a)
bayes_R2(m1a)
#     Estimate  Est.Error       Q2.5     Q97.5
# R2 0.1206113 0.02362119 0.07777765 0.1706863
summary(m1a)
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.03      0.40    -0.95     0.89 1.00     2173     1794
# age                        0.32      0.04     0.23     0.40 1.00     5773     3033
# edu                        0.01      0.04    -0.07     0.08 1.00     7125     3086
# years_experience           0.02      0.04    -0.06     0.10 1.00     6613     3331
# last_training_months      -0.03      0.04    -0.11     0.04 1.00     7598     2710
# activity_rate_per_week    -0.00      0.04    -0.08     0.07 1.00     6509     3292

hist(ds$ptg)
m3 <- brms::brm(
  ptg ~ neuroticism + extraversion + openness +
    agreeableness + conscientiousness + 
    gender + age + edu + years_experience + 
    last_training_months + activity_rate_per_week +
    (1 | where/red_cross_commeetee_location), 
  family = student(),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = b),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99),
  backend = "cmdstan", 
  data = ds
)
m3 <- add_criterion(m3, "loo")
loo(m3)
pp_check(m3)
bayes_R2(m3)
#    Estimate  Est.Error       Q2.5     Q97.5
# R2 0.146466 0.02651229 0.09841614 0.2013411
summary(m3)
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                  0.11      0.38    -0.77     0.97 1.00     1844     1594
# neuroticism                0.20      0.05     0.11     0.30 1.00     4607     2965
# extraversion               0.24      0.05     0.15     0.33 1.00     4682     3038
# openness                  -0.01      0.04    -0.09     0.07 1.00     5409     3189
# agreeableness              0.01      0.04    -0.07     0.09 1.00     5992     3099
# conscientiousness          0.10      0.04     0.01     0.18 1.00     4595     3139
# gendermale                -0.21      0.08    -0.37    -0.05 1.00     5093     3014
# age                        0.15      0.04     0.06     0.24 1.00     5469     3261
# edu                       -0.03      0.04    -0.10     0.05 1.00     5689     2867
# years_experience          -0.05      0.04    -0.13     0.04 1.00     4904     3144
# last_training_months      -0.05      0.04    -0.12     0.02 1.00     6090     3034
# activity_rate_per_week     0.08      0.04     0.01     0.16 1.00     5445     2656


hist(ds$ies)
m4 <- brms::brm(
  ies ~ neuroticism + extraversion + openness +
    agreeableness + conscientiousness + 
    gender + age + edu + years_experience + 
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
#    Estimate  Est.Error       Q2.5     Q97.5
# R2 0.05567536 0.01825069 0.02483078 0.09561347

summary(m4)
# Intercept                  0.02      0.37    -0.91     0.83 1.00     1129      806
# neuroticism                0.18      0.04     0.11     0.25 1.00     2582     2847
# extraversion               0.04      0.03    -0.02     0.10 1.00     3726     2288
# openness                   0.04      0.02    -0.01     0.08 1.00     4389     3044
# agreeableness              0.02      0.02    -0.02     0.07 1.00     4150     3165
# conscientiousness          0.03      0.03    -0.03     0.08 1.00     4342     2773
# gendermale                -0.07      0.05    -0.17     0.03 1.00     3922     3141
# age                       -0.05      0.03    -0.10    -0.00 1.00     3411     2864
# edu                       -0.06      0.02    -0.10    -0.01 1.00     4194     3222
# years_experience           0.04      0.03    -0.01     0.09 1.00     2951     2866
# last_training_months       0.01      0.02    -0.04     0.05 1.00     5323     3260
# activity_rate_per_week    -0.02      0.02    -0.06     0.03 1.00     4811     2682



# Latent profile analysis -------------------------------------------------

library("outForest")

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
  single_imputation() %>%
  scale() %>%
  estimate_profiles(1:6, package = "mplus") %>% 
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

m6 <- all %>%
  dplyr::select(
    self_kindness,             
    self_judgment, 
    common_humanity,
    isolation,    
    mindfulness, 
    over_identification
  ) %>% 
  scale() %>%
  estimate_profiles(6, package = "mplus",
                    variances = c("varying"),
                    covariances = c("varying")
                    ) 

out <- get_estimates(m6) %>% 
  dplyr::filter(Category == "Means") %>% 
  as.data.frame()

out$Parameter <- factor(
  out$Parameter, 
  levels = c("self_kindness", "common_humanity", "mindfulness",
             "self_judgment", "over_identification", "isolation")
)

out %>% 
  ggplot(aes(fill=Parameter, y=Estimate, x=Parameter)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("six profiles") +
    facet_wrap(~Class) +
    theme(legend.position="none") +
    xlab("")







get_estimates(m6)

ggplot(m6) + geom_bar(aes(y = class))



m6 %>% 
  compare_solutions(statistics = c("AIC", "BIC"))



plot(
  jitter(df1$last_training_months), jitter(df1$ies)
)


cor(df1$last_training_months, log(df1$ies+1))


rwd <- df
rwd$ies <- ies_r_score

hist(rwd$ies)

rwd$probable_ptss_case <- ifelse(rwd$ies >= 33, 1, 0)
sum(rwd$probable_ptss_case) / length(rwd$probable_ptss_case)

rwd %>% 
  dplyr::filter(probable_ptss_case == 1) %>% 
  group_by(gender) %>% 
  summarise(
    n = n()
  )

mean(rwd$ies)


rwd$ptg <- ptg
mean(rwd$ptg)
sd(rwd$ptg)



temp <- rwd %>% 
  dplyr::select(starts_with("X"), age) 

set.seed(123)
imp <- mice::mice(temp, method = "norm.predict", m = 1) # Impute data
data_det <- complete(imp) # Store data

rwd$age <- round(data_det$age)
mean(rwd$age)
sd(rwd$age)


rwd$years_experience <- as.numeric(as.character(df$years_experience))
rwd$years_experience <- 
  ifelse(rwd$years_experience == 2011, 9, rwd$years_experience)
mean(rwd$years_experience)
sd(rwd$years_experience)



df %>% 
  group_by(X140) %>% 
  summarise(
    ies_avg = mean(ies, na.rm = TRUE)
  )

df$t <- case_when(
  df$X140 == "6 mesi - 1 anno" ~ 9,
  df$X140 == "1 - 2 anni" ~ 18,
  df$X140 == "2 - 5 anni" ~ 42,
  df$X140 == "Più di 5 anni" ~ 84
)

df$t_personal <- case_when(
  df$X138 == "6 mesi - 1 anno" ~ 9,
  df$X138 == "1 - 2 anni" ~ 18,
  df$X138 == "2 - 5 anni" ~ 42,
  df$X138 == "Più di 5 anni" ~ 84
)

df$t_tr <- case_when(
  df$X167 == "6 mesi - 1 anno" ~ 9,
  df$X167 == "1 - 2 anni" ~ 18,
  df$X167 == "2 - 5 anni" ~ 42,
  df$X167 == "Più di 5 anni" ~ 84
)


fm <- lm(
  ies ~ I(neg_self_compassion + pos_self_compassion),
  data = df
)
summary(fm)

fm1 <- lm(
  ies ~ (pos_self_compassion + neg_self_compassion) +
    (X142 + X143 + X144 + cope_nvi +
    t_tr + t_personal + t) ,
  data = df
)
car::Anova(fm1)

defusing_ptg + 
when_defusiong_ptg + 
when_debriefing_ptg


df %>% 
  group_by(X142) %>% 
  summarise(
    m = mean(ptg, na.rm = T),
    n = n()
  )




foo <- ifelse(df$X142 == "Mai", 0, 1)


df %>% 
  group_by(X142) %>% 
  summarise(
    ies_avg = mean(ies, na.rm = TRUE), 
    n = n()
  )
