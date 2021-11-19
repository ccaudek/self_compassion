#-------------------------------------------------------------------- 
# Script: 30_bayesian_med.R
#
# Self-compassion project: "protezione civile", ambulance operators
#
# Question: does the positive component of SC add something to the
# predictive abilities of the negative component of the SC?
# To answer, we will consider the phenomenon of PTG and its 
# relation with the personality factors (Panjikidze, Beelmann, 
# Martskvishvili, Chitashvili, 2019). The relation between
# personality factors and PTG has been discussed in terms of a
# mediation model, with Social Support as a mediating variable.
# 
# Moreover, the relation between PTG (or PTSD) and personality has 
# also been described as mediated by the coping strategies (Mattson, 
# James, Engdahl, 2018).
#
# PURPOSE: 
# 1. To determine whether such mediation effects are present in our 
# data;
# 2. Establish whether SC can also mediate the relation between
# personality factors and PTG (or PTSD);
# 3. Determine whether, in its mediating role, positive and
# negative SC have the same role.


#
# Corrado Caudek
# This file was last modified on "Mon Oct 14 09:31:24 2019"
#-------------------------------------------------------------------- 


library("here")
suppressPackageStartupMessages(library("brms")) 
suppressPackageStartupMessages(library("tidyverse")) 
library("ggthemes")
library("viridis")
library("tidyverse")
library("tidyr")
library("rstanarm")
library("projpred")
library("bayesplot")
theme_set(theme_classic())
library("mice")

options(max.print=99999999)


temp <- read.csv(
  file=here("data", "rescue_workers.csv"), 
  header=TRUE, 
  sep=";"
)


# mydat <- data.frame(
#   where = thedat$where,
#   ptg = thedat$ptg, 
#   neuroticism = thedat$neuroticism, 
#   extraversion = thedat$extraversion, 
#   openness = thedat$openness, 
#   agreeableness = thedat$agreeableness, 
#   conscientiousness = thedat$conscientiousness, 
#   coping = thedat$coping, 
#   pos_self_compassion = thedat$pos_self_compassion, 
#   neg_self_compassion = thedat$neg_self_compassion, 
#   education = thedat$education,
#   age = thedat$age, 
#   employment = thedat$employment, 
#   gender = thedat$gender,
#   rescue_worker_qualification = thedat$rescue_worker_qualification,
#   years_experience = as.numeric(as.character(thedat$years_experience)),
#   last_training = thedat$last_training,
#   rate_of_activity = thedat$rate_of_activity,
#   job_qualification = thedat$job_qualification
# )

temp$is_rescue_worker <- NULL
summary(temp)

mydata <- temp[!is.na(temp$friends), ]
summary(mydata)


# Multiple imputation -----------------------------------------------------
# for age!

# select only the numeric columns of 'mydata'
nums <- unlist(lapply(mydata, is.numeric))  
dat_num <- mydata[, nums]

dat_num$years_experience <- ifelse(
  dat_num$years_experience == 2011, NA, dat_num$years_experience
)

# one value of 'years_experience' is miscoded: 2011;
# il will be recoded as NA and the multiply imputed and
# added again to the 'mydata' data.frame
tail(sort(mydata$years_experience))

imp <- mice(
  dat_num, 
  method = "norm.predict", 
  m = 1, 
  seed = 12345
)

# imputed_data <- mice::mice(
#   dat_num, m=5, maxit = 50, method = 'pmm', seed = 500
# )

d <- mice::complete(imp)

# add imputed 'age' to mydata
mydata$age_imp <- d$age
mydata$years_experience <- d$years_experience

# mydata$ptg <- mydata$ptg + 1

hist(d$ptg)

mod <- bf(
  ptg ~ 1 + age + gender + education + years_experience + 
    rate_of_activity + last_training + rate_of_activity + 
    job_qualification + is_job_qualification_invariant + 
    is_team_invariant + 
    neuroticism + extraversion + openness + agreeableness + 
    conscientiousness + cope_nvi + neg_self_compassion + 
    pos_self_compassion 
)

mod1 <- bf(
  ptg ~ 1 + 
    neuroticism + extraversion + openness + agreeableness + 
    conscientiousness + cope_nvi + neg_self_compassion + 
    pos_self_compassion 
)

mod_baseline <- bf(
  ptg ~ 1 
)


fit1 <-
  brm(
    data = mydata,
    #family = skew_normal(),
    family = shifted_lognormal(),
    mod,
    chains = 4,
    cores = 4,
    inits = 0
  )


pp_check(fit1)

print(fit1, digits = 3)

bayes_R2(fit1)

loo0 <- loo(fit0)
loo1 <- loo(fit1)
loo_baseline <- loo(fit_baseline)

print(loo::compare(loo_baseline, loo1), digits = 3)




# Mediational analysis (1)

# We consider the path analysis model of Mattson, James, & Engdahl 
# (2018) described in their Figure 2.
# X: openness
# Y: ptg
# M: coping

d <- data.frame(
  neuroticism = mydata$neuroticism,
  extraversion = mydata$extraversion, 
  openness = mydata$openness,
  agreeableness = mydata$agreeableness,
  conscientiousness = mydata$conscientiousness,
  cope_nvi = mydata$cope_nvi,
  pos_self_compassion = mydata$pos_self_compassion, 
  neg_self_compassion = mydata$neg_self_compassion, 
  ptg = mydata$ptg
)

d <- d[complete.cases(d), ]
nrow(d)

d <- d %>% 
  dplyr::mutate(
    X = extraversion - mean(extraversion, na.rm = TRUE),
    Y = ptg - mean(ptg, na.rm = TRUE),
    M = pos_self_compassion - mean(pos_self_compassion, na.rm = TRUE),
    M1 = cope_nvi - mean(cope_nvi, na.rm = TRUE)
  ) 











model0 <- 
  brm(data = d, 
      family = gaussian,
      cbind(X, Y, M) ~ 1,
      chains = 4, 
      cores = 4
  )

print(model0, digits = 3)

posterior_samples(model0) %>% 
  select(rescor__X__Y, rescor__X__M, rescor__Y__M) %>% 
  gather() %>% 
  ggplot(aes(x = value, fill = key)) +
  geom_density(alpha = .85, color = "transparent") +
  scale_fill_viridis(discrete = T, option = "D", direction = -1,
                     labels = c(expression(paste(rho["X, M"])),
                                expression(paste(rho["X, Y"])),
                                expression(paste(rho["Y, M"]))),
                     guide = guide_legend(label.hjust = 0,
                                          label.theme = element_text(size = 15, angle = 0, color = "white"),
                                          title.theme = element_blank())) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(title = "Correlation density plot",
       x = NULL) +
  theme_black() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())



# Model 1 -----------------------------------------------------------------

hist(d$Y)
hist(d$M)


y_model <- bf(Y ~ 1 + X + M, family = skew_normal())
m_model <- bf(M ~ 1 + X, family = skew_normal())

model1 <-
  brm(
    data = d,
    y_model + m_model + set_rescor(FALSE),
    chains = 4,
    cores = 4
  )

summary(model1)

# putting the posterior draws into a data frame
post <- posterior_samples(model1)


# Computing the indirect effect with multiplication
post <-
  post %>% 
  mutate(indirect_eff = b_M_X * b_Y_M)

# getting the posterior median and 95% intervals with `quantile()`
quantile(post$indirect_eff, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)

# the direct effect
posterior_summary(model1)["b_Y_X", ] %>% 
  round(digits = 3)

# the total effect
post <-
  post %>% 
  mutate(total_eff = b_Y_X + indirect_eff)

quantile(post$total_eff, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)



post %>% 
  mutate(direct_eff   = b_Y_X,
         indirect_eff = indirect_eff) %>% 
  mutate(total_eff = direct_eff + indirect_eff) %>% 
  select(direct_eff, indirect_eff, total_eff) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)



# Posterior plots
post %>% 
  ggplot(aes(x = indirect_eff)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(3)[3]) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .5, .975)), 
             color = "white", linetype = c(2, 1, 2), size = c(.5, .8, .5)) +
  # scale_x_continuous(breaks = quantile(post$ab, probs = c(.025, .5, .975)),
  #                    labels = quantile(post$ab, probs = c(.025, .5, .975)) %>% 
  #                      round(2) %>% 
  #                      as.character()) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("The indirect effect, the ", b[mx] * b[ym], " pathway")),
       x = NULL) +
  theme_classic()


post %>% 
  ggplot(aes(x = b_Y_X)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(4)[4]) +
  geom_vline(xintercept = 0, color = "white", linetype = 2) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("0 is not a credible value for the direct effect ", b[yx], ".")),
       x = NULL) +
  theme_classic()


ll <- quantile(post$total_eff, probs = .025) %>% round(digits = 3)
ul <- quantile(post$total_eff, probs = .975) %>% round(digits = 3)

post %>% 
  ggplot(aes(x = total_eff)) +
  geom_histogram(color = "white", size = .25, 
                 fill = colorblind_pal()(5)[5],
                 binwidth = .025, boundary = 0) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .975)),
             linetype = 3, color = colorblind_pal()(6)[6]) +
  labs(x = expression(paste("Total effect (", b[yx] + b[mx] * b[ym], ")")),
       y = "Frequency in 4,000 HMC posterior draws",
       subtitle = paste("95% of the posterior draws are between", ll, "and", ul)) +
  theme_classic()


# Evaluate the model's fit

pp_check(model1, resp = "Y")
pp_check(model1, resp = "M")

print(model1, digits = 3)

bayes_R2(model1)
(loo1 <- loo(model1))
(loo2 <- loo(model2))
plot(loo1)
loo_compare(loo1, loo2)    


# Partially-standardized coefficients

SD_y <- sd(d$Y)

post %>% 
  mutate(direct_eff_ps   = b_Y_X / SD_y,
         indirect_eff_ps = indirect_eff / SD_y) %>% 
  mutate(total_eff_ps = direct_eff_ps + indirect_eff_ps) %>% 
  select(direct_eff_ps, indirect_eff_ps, total_eff_ps) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)


post %>% 
  mutate(direct_eff_ps   = b_Y_X / SD_y,
         indirect_eff_ps = indirect_eff / SD_y) %>% 
  mutate(total_eff_ps = direct_eff_ps + indirect_eff_ps) %>% 
  select(direct_eff_ps, indirect_eff_ps, total_eff_ps) %>% 
  gather() %>%
  ggplot(aes(x = value, fill = key)) +
  geom_density(alpha = .85, color = "transparent") +
  scale_fill_viridis(discrete = T, option = "D") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Partially-standardized coefficients",
       x = NULL) +
  theme_black() +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  facet_wrap(~key, ncol = 3)


# Standardized coefficients

standardize <- function(x){
  (x - mean(x))/sd(x)
}


d <-
  d %>% 
  mutate(
    Yz = standardize(Y), 
    Xz = standardize(X), 
    Mz = standardize(M)
  )

y_model <- bf(Yz ~ 1 + Xz + Mz, family = skew_normal())
m_model <- bf(Mz ~ 1 + Xz, family = gaussian())

model2 <-
  brm(data = d, 
      y_model + m_model + set_rescor(FALSE),
      chains = 4, 
      cores = 4
  )

fixef(model2) %>% round(digits = 3)

post <- posterior_samples(model2)

post %>% 
  mutate(indirect_eff_s = b_Mz_Xz * b_Yz_Mz,
         direct_eff_s = b_Yz_Xz) %>%
  mutate(total_eff_s = indirect_eff_s + direct_eff_s) %>% 
  select(direct_eff_s, indirect_eff_s, total_eff_s) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 4)



# Multiple mediators ------------------------------------------------------

hist(d$neg_self_compassion)


d <- d %>% 
  dplyr::mutate(
    X = extraversion - mean(extraversion, na.rm = TRUE),
    Y = ptg - mean(ptg, na.rm = TRUE),
    M = pos_self_compassion - mean(pos_self_compassion, na.rm = TRUE),
    M1 = cope_nvi - mean(cope_nvi, na.rm = TRUE),
    M2 = neg_self_compassion - mean(neg_self_compassion, na.rm = TRUE)
  ) 


m1_model <- bf(M   ~ 1 + X, family = skew_normal()) # pos SC
m2_model <- bf(M1  ~ 1 + X) # cope nvi
m3_model <- bf(M2  ~ 1 + X, family = skew_normal()) # neg SC
y_model  <- bf(Y   ~ 1 + X + M + M1 + M2, family = skew_normal())


model2 <-
  brm(
    data = d, 
    y_model + m1_model + m2_model + m3_model + set_rescor(FALSE),
    chains = 4, 
    cores = 4, 
    inits = 0
  )



# putting the posterior draws into a data frame
post <- posterior_samples(model2)


# Computing the indirect effect with multiplication
post <-
  post %>% 
  mutate(
    indirect_eff_M  = b_M_X * b_Y_M,
    indirect_eff_M1 = b_M1_X * b_Y_M1,
    indirect_eff_M2 = b_M2_X * b_Y_M2
  )


# # getting the posterior median and 95% intervals with `quantile()`
# # pos self compassion
# quantile(post$indirect_eff_M, probs = c(.5, .025, .975)) %>% 
#   round(digits = 3)
# 
# # cope nvi
# quantile(post$indirect_eff_M1, probs = c(.5, .025, .975)) %>% 
#   round(digits = 3)
# 
# 
# # the direct effect
# posterior_summary(model1)["b_Y_X", ] %>% 
#   round(digits = 3)
# 
# # the total effect
# post <-
#   post %>% 
#   mutate(total_eff = b_Y_X + indirect_eff_M + indirect_eff_M1)
# 
# quantile(post$total_eff, probs = c(.5, .025, .975)) %>% 
#   round(digits = 3)



post %>% 
  mutate(
    direct_eff = b_Y_X,
    indirect_eff_M = indirect_eff_M,
    indirect_eff_M1 = indirect_eff_M1,
    indirect_eff_M2 = indirect_eff_M2
  ) %>% 
  mutate(total_eff = direct_eff + indirect_eff_M + indirect_eff_M1 + indirect_eff_M2) %>% 
  select(direct_eff, indirect_eff_M, indirect_eff_M1, indirect_eff_M2, total_eff) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)



# Posterior plots
post %>% 
  ggplot(aes(x = indirect_eff_M)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(3)[3]) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .5, .975)), 
             color = "white", linetype = c(2, 1, 2), size = c(.5, .8, .5)) +
  # scale_x_continuous(breaks = quantile(post$ab, probs = c(.025, .5, .975)),
  #                    labels = quantile(post$ab, probs = c(.025, .5, .975)) %>% 
  #                      round(2) %>% 
  #                      as.character()) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("The indirect effect, the ", b[mx] * b[ym], " pathway")),
       x = NULL) +
  theme_classic()

post %>% 
  ggplot(aes(x = indirect_eff_M1)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(3)[3]) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .5, .975)), 
             color = "white", linetype = c(2, 1, 2), size = c(.5, .8, .5)) +
  # scale_x_continuous(breaks = quantile(post$ab, probs = c(.025, .5, .975)),
  #                    labels = quantile(post$ab, probs = c(.025, .5, .975)) %>% 
  #                      round(2) %>% 
  #                      as.character()) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("The indirect effect, the ", b[mx] * b[ym], " pathway")),
       x = NULL) +
  theme_classic()


post %>% 
  ggplot(aes(x = b_Y_X)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(4)[4]) +
  geom_vline(xintercept = 0, color = "white", linetype = 2) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("0 is not a credible value for the direct effect ", b[yx], ".")),
       x = NULL) +
  theme_classic()


ll <- quantile(post$total_eff, probs = .025) %>% round(digits = 3)
ul <- quantile(post$total_eff, probs = .975) %>% round(digits = 3)

post %>% 
  ggplot(aes(x = total_eff)) +
  geom_histogram(color = "white", size = .25, 
                 fill = colorblind_pal()(5)[5],
                 binwidth = .025, boundary = 0) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .975)),
             linetype = 3, color = colorblind_pal()(6)[6]) +
  labs(x = expression(paste("Total effect (", b[yx] + b[mx] * b[ym], ")")),
       y = "Frequency in 4,000 HMC posterior draws",
       subtitle = paste("95% of the posterior draws are between", ll, "and", ul)) +
  theme_classic()


# Evaluate the model's fit

pp_check(model2, resp = "Y")
pp_check(model2, resp = "M")
pp_check(model2, resp = "M1")
pp_check(model2, resp = "M2")

print(model2, digits = 3)

bayes_R2(model2)

(loo1 <- loo(model2))
plot(loo1)
# loo_compare(loo1, loo2)    


# Partially-standardized coefficients

SD_y <- sd(d$Y)

post %>% 
  mutate(direct_eff_ps = b_Y_X / SD_y,
         indirect_eff_M_ps = indirect_eff_M / SD_y,
         indirect_eff_M1_ps = indirect_eff_M1 / SD_y
         ) %>% 
  mutate(total_eff_ps = direct_eff_ps + indirect_eff_M_ps + indirect_eff_M1_ps) %>% 
  select(direct_eff_ps, indirect_eff_M_ps, indirect_eff_M1_ps, total_eff_ps) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)


post %>% 
  mutate(
    direct_eff_ps = b_Y_X / SD_y,
    indirect_eff_M_ps = indirect_eff_M / SD_y, 
    indirect_eff_M1_ps = indirect_eff_M1 / SD_y
  ) %>% 
  mutate(total_eff_ps = direct_eff_ps + indirect_eff_M_ps + indirect_eff_M1_ps) %>% 
  select(direct_eff_ps, indirect_eff_M_ps, indirect_eff_M1_ps, total_eff_ps) %>% 
  gather() %>%
  ggplot(aes(x = value, fill = key)) +
  geom_density(alpha = .85, color = "transparent") +
  scale_fill_viridis(discrete = T, option = "D") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Partially-standardized coefficients",
       x = NULL) +
  theme_black() +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  facet_wrap(~key, ncol = 3)


# Standardized coefficients

standardize <- function(x){
  (x - mean(x))/sd(x)
}



# d <- d %>% 
#   dplyr::mutate(
#     X = extraversion - mean(extraversion, na.rm = TRUE),
#     Y = ptg - mean(ptg, na.rm = TRUE),
#     M = pos_self_compassion - mean(pos_self_compassion, na.rm = TRUE),
#     M1 = cope_nvi - mean(cope_nvi, na.rm = TRUE)
#   ) 



d <- data.frame(
  neuroticism = mydata$neuroticism,
  extraversion = mydata$extraversion, 
  openness = mydata$openness,
  agreeableness = mydata$agreeableness,
  conscientiousness = mydata$conscientiousness,
  cope_nvi = mydata$cope_nvi,
  pos_self_compassion = mydata$pos_self_compassion, 
  neg_self_compassion = mydata$neg_self_compassion, 
  ptg = mydata$ptg,
  ies = mydata$ies
)

d <- d[complete.cases(d), ]
nrow(d)




dz <-
  d %>% 
  mutate(
    Yz = standardize(ies), 
    Xz = standardize(neuroticism), 
    Mz = standardize(pos_self_compassion),
    M1z = standardize(cope_nvi),
    M2z = standardize(neg_self_compassion)
  )

m1_model <- bf(Mz   ~ 1 + Xz, family = skew_normal())
m2_model <- bf(M1z  ~ 1 + Xz)
m3_model <- bf(M2z  ~ 1 + Xz, family = skew_normal())
y_model  <- bf(Yz   ~ 1 + Xz + Mz + M1z + M2z, family = skew_normal())


model3 <-
  brm(data = dz, 
      y_model + m1_model + m2_model + m3_model + set_rescor(FALSE),
      chains = 4, 
      cores = 4,
      inits = 0
  )

fixef(model3) %>% round(digits = 3)

post <- posterior_samples(model3)




post %>%
  mutate(
    indirect_eff_M_s = b_Mz_Xz * b_Yz_Mz,
    indirect_eff_M1_s = b_M1z_Xz * b_Yz_M1z,
    indirect_eff_M2_s = b_M2z_Xz * b_Yz_M2z,
    direct_eff_s = b_Yz_Xz
  ) %>%
  mutate(total_eff_s = indirect_eff_M_s + indirect_eff_M1_s + indirect_eff_M2_s + direct_eff_s) %>%
  select(direct_eff_s, indirect_eff_M_s, indirect_eff_M1_s, indirect_eff_M2_s, total_eff_s) %>%
  gather() %>%
  group_by(key) %>%
  summarize(mean = mean(value),
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>%
  mutate_if(is_double, round, digits = 4)

















# Imputed data 4 ----------------------------------------------------------

imp_data4 <- complete(temp_data4, 1)

# rename and center variables
d <- center_rename_yxm(
  imp_data4$neuroticism,
  imp_data4$ies_r_score,
  imp_data4$neg_self_compassion
)

# check the shape of the distributions
hist(d$Y)
hist(d$M)

# compute correlations and credible intervals

get_correlations(d)


model0 <- 
  brm(
    mvbind(X, Y, M) ~ 1,
    data = imp_data4, 
    family = gaussian,
    chains = 4, 
    cores = 4
  )

print(model0, digits = 3)

posterior_samples(model0) %>% 
  select(rescor__X__Y, rescor__X__M, rescor__Y__M) %>% 
  gather() %>% 
  ggplot(aes(x = value, fill = key)) +
  geom_density(alpha = .85, color = "transparent") +
  scale_fill_viridis(discrete = T, option = "D", direction = -1,
                     labels = c(expression(paste(rho["X, M"])),
                                expression(paste(rho["X, Y"])),
                                expression(paste(rho["Y, M"]))),
                     guide = guide_legend(label.hjust = 0,
                                          label.theme = element_text(size = 15, angle = 0, color = "white"),
                                          title.theme = element_blank())) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(title = "Our correlation density plot",
       x = NULL) +
  theme_black() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())


y_model <- bf(Y ~ 1 + X + M, family = skew_normal())
m_model <- bf(M ~ 1 + X, family = skew_normal())

model1 <-
  brm(
    data = imp_data4,
    y_model + m_model + set_rescor(FALSE),
    chains = 4,
    cores = 4
  )

print(model1, digits = 3)


pp_check(model1, resp = "Y")
pp_check(model1, resp = "M")
bayes_R2(model1)


# putting the posterior draws into a data frame
post <- posterior_samples(model1)

# computing the ab coefficient with multiplication
post <-
  post %>% 
  mutate(ab = b_M_X * b_Y_M)

# getting the posterior median and 95% intervals with `quantile()`
quantile(post$ab, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)

posterior_summary(model1)["b_Y_X", ]




post %>% 
  mutate(c = b_Y_X + ab) %>% # total effect
  rename(c_prime = b_Y_X) %>% # direct effect
  select(c_prime, c) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)

