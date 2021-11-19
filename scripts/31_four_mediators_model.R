#-------------------------------------------------------------------- 
# Script: 31_four_mediators_model.R
#
# Self-compassion project: ambulance operators
#
# The Self-Compassion Scale assesses trait levels of self-compassion.
# It includes items that measure how often people respond to feelings of 
# inadequacy or suffering with 
# - self-kindness (e.g., "I try to be loving toward myself when I’m feeling 
# emotional pain"), 
# - self-judgment (e.g., "I'm disapproving and judgmental about my own flaws 
# and inadequacies"), 
# - common humanity (e.g., "I try to see my failings as part of the human 
# condition"), 
# - isolation (e.g., "When I think about my inadequacies it tends to make me 
# feel more separate and cut off from the rest of the world"), 
# - mindfulness (e.g., "When something painful happens I try to take a 
# balanced view of the situation"),  
# - over-identification (e.g., "When I'm feeling down I tend to obsess and 
# fixate on everything that's wrong").
#
# The subscales 'self-judgment', 'isolation', and 'over-identification' 
# have been reversed. Together, they form the 'negative self-compassion'
# subscale.
# The subscales 'self-kindness', 'common humanity', and 'mindfulness'
# together form the 'positive self compassion' subscale.
# The total SCS score is given by the sum of the positive and negative
# coponents of the SCS.
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
# Results.
# PTG as the Y variable 
# The total effect is different from zero in the case of two personality 
# traits: Extraversion and Conscientiousness CHECK!!
# 
# PTSD as the Y variable 
# The total effect is different from zero in the case of X personality 
# traits: Neuroticism, 
#
# Corrado Caudek
# This file was last modified on "Wed Oct 16 09:44:27 2019"
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
library("mice")
library("corrplot")
library("bayesplot")
theme_set(theme_classic())


options(max.print=99999999)


source(here("libraries", "self_compassion_fnc.R"))



# Read data ---------------------------------------------------------------


temp <- read.csv(
  file=here("data", "rescue_workers.csv"), 
  header=TRUE, 
  sep=";"
)

# remove uninformative variable
temp$is_rescue_worker <- NULL

# remove missing data on all questionnaires data (for convenience, I use the
# variable 'friends')
mydata <- temp[!is.na(temp$friends), ]
# summary(mydata)



# Multiple imputation -----------------------------------------------------

# for age!

# select only the numeric columns of 'temp1'
nums <- unlist(lapply(mydata, is.numeric))  
dat_num <- mydata[, nums]

# one value of 'years_experience' is miscoded: 2011;
# it will be recoded as NA and the multiply imputed and
# added again to the 'mydata' data.frame
dat_num$years_experience <- ifelse(
  dat_num$years_experience == 2011, NA, dat_num$years_experience
)


imp <- mice(
  dat_num, 
  method = "midastouch", 
  m = 1, 
  maxit = 1,
  seed = 12345
)

d_imp <- complete(imp)
# summary(dat_num$age)
# summary(d_imp$age)

# add imputed 'age' to mydata
mydata$age_imp <- d_imp$age
mydata$years_experience <- d_imp$years_experience

# mydata$life_appreciation <- d_imp$life_appreciation
# mydata$new_possibilities <- d_imp$new_possibilities
# mydata$personal_strength <- d_imp$personal_strength
# mydata$spirituality_changes <- d_imp$spirituality_changes
# mydata$interpersonal_relationships <- d_imp$interpersonal_relationships


ggplot(data=mydata, aes(new_possibilities)) + 
  geom_histogram(
    aes(y =..density..), 
    #breaks=seq(0, 85, by = 2), 
    col="red", 
    fill="green", 
    alpha=.2) + 
  geom_density(col=2) 


# mydata$ptg <- mydata$life_appreciation + mydata$new_possibilities +
#   mydata$personal_strength + mydata$spirituality_changes + 
#   mydata$interpersonal_relationships


rm(d_imp, dat_num, temp)



# Data wrangling ----------------------------------------------------------


# Compute total score for MSPSS 
mydata$mspss <- mydata$family + mydata$friends + mydata$significant_other

# Recode rate of activity as a numeric variable 
mydata$activity_rate <- as.numeric(
  as.character(fct_recode(
  mydata$rate_of_activity, 
  "5" = "Più di 1 turno a settimana", 
  "4" = "1 turno a settimana",
  "3" = "1 turno ogni due settimane",
  "2" = "1 turno al mese",
  "1" = "Meno di 1 turno al mese"))
)

# Recode education as a numeric variable
mydata$edu <- as.numeric(
  as.character(fct_recode(
    mydata$education, 
    "1" = "Scuola media primaria", 
    "2" = "Diploma",
    "3" = "Laurea breve",
    "4" = "Laurea magistrale",
    "5" = "Dottorato"))
)

# Recode last_training as a numeric variable
mydata$training_time <- as.numeric(
  as.character(fct_recode(
    mydata$last_training, 
    "1" = "Meno di 2 mesi fa", 
    "2" = "Più di 2 mesi fa",
    "3" = "Meno di 6 mesi fa",
    "4" = "Più di 6 mesi fa",
    "5" = "Meno di 1 anno fa",
    "6" = "Più di 1 anno fa")
    )
)

mydata$job_qualification <-  fct_recode(
  mydata$job_qualification, 
  "Soccorritore" = "Infermiere", 
  "Soccorritore" = "Medico"
)


# compute Self-Compassion Scale total score
mydata$self_compassion <- 
  mydata$neg_self_compassion + mydata$pos_self_compassion



# Create data.frame for mediation modeling --------------------------------



# Rename variables 
d <- mydata %>% 
  dplyr::mutate(
    X1 = neuroticism,
    X2 = extraversion,
    X3 = openness,
    X4 = agreeableness,
    X5 = conscientiousness,
    Y = NA, 
    M_psc = pos_self_compassion,
    M_nsc = neg_self_compassion,
    M_sc = self_compassion,
    M_cop = cope_nvi,
    M_sos = mspss
  ) 



# Select Y variable (PTG or PTSD) -----------------------------------------

d$Y <- d$ptg

# d$Y <- d$ies # ~post-traumatic stress disorder




# Standardize variables ---------------------------------------------------


d <- d %>% 
  mutate(
    Yz = standardize(Y), 
    X1z = standardize(X1), 
    X2z = standardize(X2), 
    X3z = standardize(X3), 
    X4z = standardize(X4), 
    X5z = standardize(X5), 
    M_pscz = standardize(M_psc),
    M_nscz = standardize(M_nsc),
    M_scz = standardize(M_sc),
    M_copz = standardize(M_cop),
    M_sosz = standardize(M_sos)
  )



# Compute correlations ----------------------------------------------------

# # better to use the bayesian robust correlation estimates!
# dat <- d[, (63-10):63]
# dat$Xz <- NULL
# 
# res <- Hmisc::rcorr(as.matrix(dat))
# 
# round(res$r, 2)
# 
# corrplot(res$r, type="upper", order="hclust", 
#          p.mat = res$P, sig.level = 0.01, 
#          insig = "blank", diag = FALSE)



# Select X variable (personality trait) -----------------------------------


# Neuroticism
# d$Xz <- d$X1z

# Extroversion
# d$Xz <- d$X2z

# Openness
# d$Xz <- d$X3z

# Agreeableness
# d$Xz <- d$X4z

# Conscientiousness
d$Xz <- d$X4z



# Descriptive statistics --------------------------------------------------


psych::describe(d)

# Kaiser (1975) suggested that KMO > .9 were marvelous, 
# in the .80s, mertitourious, in the .70s, middling, 
# in the .60s, medicore, in the .50s, miserable, and 
# less than .5, unacceptable.
names(d[, 63:72])
psych::KMO(d[, 63:72])


# Run the four mediators' model (standardized coefficients) ---------------


fit_model_z(d)


y_model  <- bf(Yz ~ 1 + Xz + M_copz + M_sosz + M_pscz + M_nscz, family = skew_normal())
m1_model <- bf(M_copz ~ 1 + Xz) # coping
m2_model <- bf(M_sosz ~ 1 + Xz, family = skew_normal()) # social support
m3_model <- bf(M_pscz ~ 1 + Xz) # positive SC
m4_model <- bf(M_nscz ~ 1 + Xz, family = skew_normal()) # negative SC

fit <-
  brm(data = d, 
      y_model + m1_model + m2_model + m3_model + m4_model + set_rescor(FALSE),
      chains = 4, 
      cores = 4,
      inits = 0
  )


post <- posterior_samples(fit)

# Computing the indirect effect with multiplication
post <-
  post %>% 
  mutate(
    direct_eff = b_Yz_Xz,
    ind_eff_M_sos = b_Msosz_Xz * b_Yz_M_sosz,
    ind_eff_M_cop = b_Mcopz_Xz * b_Yz_M_copz,
    ind_eff_M_psc = b_Mpscz_Xz * b_Yz_M_pscz,
    ind_eff_M_nsc = b_Mnscz_Xz * b_Yz_M_nscz,
    indirect_eff = ind_eff_M_sos + ind_eff_M_cop + ind_eff_M_psc + ind_eff_M_nsc,
    total_eff = direct_eff + indirect_eff
  )

output <- post %>% 
  select(
    direct_eff, 
    ind_eff_M_sos, 
    ind_eff_M_cop, 
    ind_eff_M_psc, 
    ind_eff_M_nsc, 
    indirect_eff,
    total_eff
  ) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(
    mean = mean(value), 
    ll = quantile(value, probs = .025),
    ul = quantile(value, probs = .975)
  ) %>% 
  mutate_if(is_double, round, digits = 3)

output

fixef(fit) %>% round(digits = 3)

pp_check(fit, resp = "Yz")
pp_check(fit, resp = "Mcopz")
pp_check(fit, resp = "Msosz")
pp_check(fit, resp = "Mpscz")
pp_check(fit, resp = "Mnscz")

print(fit, digits = 3)

bayes_R2(fit)
(loo1 <- loo(fit))
plot(loo1)
















mediation_model <- ' 
    # direct effect
    Yz ~ bx*Xz + b_cop*M_copz + b_sos*M_sosz + b_psc*M_pscz + b_nsc*M_nscz
    # mediator
    M_copz ~ bx_cop*Xz
    M_sosz ~ bx_sos*Xz
    M_pscz ~ bx_pcs*Xz
    M_nscz ~ bx_nsc*Xz
    # indirect effects 
    ind_ef     := bx_cop * b_cop + bx_sos * b_sos + bx_psc * b_psc + bx_nsc * b_nsc
    ind_ef_cop := bx_cop * b_cop
    ind_ef_sos := bx_sos * b_sos
    ind_ef_psc := bx_psc * b_psc
    ind_ef_nsc := bx_nsc * b_nsc
    # total effect
    tot_ef := bx + (bx_cop * b_cop + bx_sos * b_sos + bx_psc * b_psc + bx_nsc * b_nsc)
  '




m1 <- lavaan::sem(mediation_model, data = d)
lavaan::summary(m1, fit.measures = TRUE)


fm <- lm(Yz ~ Xz + M_pscz + M_nscz, data = d)
summary(fm)
plot(fm)

d$res <- rstandard(fm)
temp <- d[abs(d$res) < 2.2, ]


mod_1 <- ' 
    # direct effect
    Yz ~ bx*Xz + b_psc*M_pscz + b_nsc*M_nscz
    # mediator
    M_pscz ~ bx_pcs*Xz
    M_nscz ~ bx_nsc*Xz
  '



mod <- ' 
    # direct effect
    ptg ~ pos_self_compassion + cope_nvi + social_support 
    # mediator
    cope_nvi ~ pos_self_compassion 
    social_support ~ pos_self_compassion 
  '

mod <- ' 
    # direct effect
    ies ~ neuroticism + extraversion + cope_nvi + social_support 
    cope_nvi ~ neuroticism + extraversion
    social_support ~ neuroticism + extraversion
  '




nsc ~ self_judgment + isolation + over_identification
psc ~ self_kindness + common_humanity + mindfulness

# 



m1 <- lavaan::sem(mod, data = mydata)
lavaan::summary(m1, fit.measures = TRUE)



mod_2 <- ' 
    # direct effect
    Yz ~ bx*Xz + b_cop*M_copz + b_sos*M_sosz 
    # mediator
    M_copz ~ bx_cop*Xz
    M_sosz ~ bx_sos*Xz
  '

m2 <- lavaan::sem(mod_2, data = d)
lavaan::summary(m2, fit.measures = TRUE)



# Standardize variables ---------------------------------------------------


# select only the numeric columns of 'temp1'
nums <- unlist(lapply(mydata, is.numeric))  

# categorical variables
dat_nonum <- mydata[, !nums]
# numerical variables
dat_num <- mydata[, nums]
# standardizes all variables
datz_num <- lapply(dat_num, scale) 

dz <- cbind(dat_nonum, datz_num)




# Regression model for PTG ------------------------------------------------



mod1 <- bf(
  ptg ~ 1 + where + training_time + years_experience + 
    activity_rate + is_team_invariant + 
    # job_qualification + 
    # is_job_qualification_invariant + 
    age + gender + edu + 
    neuroticism + extraversion + openness + 
    agreeableness + conscientiousness + 
    cope_nvi + 
    pos_self_compassion + neg_self_compassion
)


fit_mod1 <- brm(
  data = dz,
  family = skew_normal(),
  mod1,
  chains = 4,
  cores = 4,
  inits = 0
)


pp_check(fit_mod1) # + xlim(0, 5)
print(fit_mod1) 
bayes_R2(fit_mod1)
(loo1 <- loo(fit_mod1))
plot(loo1)

launch_shinystan(fit_mod1)





#  E  N  D


temp <- d[-c(708, 435, 374, 154, 704), ]


fm <- lm(
  ptg ~ 1 + where + training_time + years_experience + 
    activity_rate + training_time + 
    job_qualification + is_job_qualification_invariant + 
    is_team_invariant + 
    age + gender + edu + 
    neuroticism + extraversion + openness + agreeableness + conscientiousness + 
    cope_nvi + 
    #self_compassion,
    pos_self_compassion + neg_self_compassion,
  data = dz
)

summary(fm)


# data for computing semistandardized partial coefficients: we rescale
# X by dividing by its SD, but we leave Y untouched. The semistandardized 
# partial coefficients are interpreted as follows: "An increase of X
# by 1 SD produce an expected variation of E(Y) equal to b_X".
dd <- data.frame(
  ptg = mydata$ptg,
  ies = mydata$ies,
  where = mydata$where,
  training_time = mydata$training_time / sd(mydata$training_time),
  years_experience = mydata$years_experience / sd(mydata$years_experience, na.rm = TRUE),
  activity_rate = mydata$activity_rate / sd(mydata$activity_rate),
  training_time = mydata$training_time / sd(mydata$training_time),
  job_qualification = mydata$job_qualification,
  is_job_qualification_invariant = mydata$is_job_qualification_invariant,
  is_team_invariant = mydata$is_team_invariant,
  age = mydata$age_imp / sd(mydata$age_imp),
  gender = mydata$gender,
  edu = mydata$edu / sd(mydata$edu),
  neuroticism = mydata$neuroticism / sd(mydata$neuroticism),
  extraversion = mydata$extraversion / sd(mydata$extraversion),
  openness = mydata$openness / sd(mydata$openness),
  agreeableness = mydata$agreeableness / sd(mydata$agreeableness), 
  conscientiousness = mydata$conscientiousness / sd(mydata$conscientiousness),
  cope_nvi = mydata$cope_nvi / sd(mydata$cope_nvi),
  neg_self_compassion = mydata$neg_self_compassion / sd(mydata$neg_self_compassion), 
  pos_self_compassion = mydata$pos_self_compassion / sd(mydata$pos_self_compassion)
)


ggplot(data=dd, aes(dd$ies)) + 
  geom_histogram(
    aes(y =..density..), 
    #breaks=seq(0, 85, by = 2), 
    col="red", 
    fill="green", 
    alpha=.2) + 
  geom_density(col=2) 
  # + labs(title=..., x=..., y=...)


fm1 <- lm(
  ptg ~ 1 + where + training_time + years_experience + 
    activity_rate + training_time + 
    job_qualification + is_job_qualification_invariant + 
    is_team_invariant + 
    age + gender + edu,
  data = dd
)
summary(fm1)

mydata$ptg_clean <- fm1$res



mod1 <- bf(
  ptg ~ 1 + where + training_time + years_experience + 
    activity_rate + training_time + 
    job_qualification + is_job_qualification_invariant + 
    is_team_invariant + 
    age + gender + edu + 
    neuroticism + extraversion + openness + 
    agreeableness + 
    conscientiousness + 
    cope_nvi + 
    pos_self_compassion + neg_self_compassion
)


temp <- dd
temp$ptg <- dd$ptg + 1
temp$ptg <- temp$ptg / sd(temp$ptg)
temp$ies <- dd$ies + 1
temp$ies <- temp$ies / sd(temp$ies)


fit_mod1 <- brm(
    data = dz,
    family = skew_normal(),
    mod1,
    chains = 4,
    cores = 4,
    inits = 0
  )


pp_check(fit_mod1) + xlim(0, 5)
print(fit_mod1) 
bayes_R2(fit_mod1)
(loo1 <- loo(fit_mod1))
plot(loo1)



mod2 <- bf(
  ies ~ 1 + where + training_time + years_experience + 
    activity_rate + training_time + 
    job_qualification + is_job_qualification_invariant + 
    is_team_invariant + 
    age + gender + edu + 
    neuroticism + extraversion + openness + 
    agreeableness + 
    conscientiousness + 
    cope_nvi + 
    pos_self_compassion + neg_self_compassion
)

fit_mod2 <- brm(
  data = dz,
  family = skew_normal(),
  mod2,
  chains = 4,
  cores = 4,
  inits = 0
)


pp_check(fit_mod2) + xlim(0, 5)
print(fit_mod2) 
bayes_R2(fit_mod2)
(loo2 <- loo(fit_mod2))
plot(loo2)








dz1 <- dz
dz1$ies <- dz1$ies + 1.14

fit_mod2 <- brm(
  data = dz1,
  #family = skew_normal(),
  family = shifted_lognormal(),
  mod2,
  niter
  chains = 4,
  cores = 4,
  inits = 0
)


pp_check(fit_mod2) + xlim(0, 10)
print(fit_mod2) 
bayes_R2(fit_mod2)
(loo2 <- loo(fit_mod2))
plot(loo2)





m1 <- brm(
  mvbind(
    life_appreciation, new_possibilities, personal_strength, 
    spirituality_changes, interpersonal_relationships ) ~ 
    pos_self_compassion + neg_self_compassion +
     (1|p|id) + (1|q|where),
  data = mydata, 
  chains = 2, 
  cores = 4
)



ggplot(data=mydata, aes(life_appreciation)) + 
  geom_histogram(
    aes(y =..density..), 
    #breaks=seq(0, 85, by = 2), 
    col="red", 
    fill="green", 
    alpha=.2) + 
  geom_density(col=2) 






# Neuroticism
d$X <- d$X1

hist(d$Y)
hist(d$M_cop)
hist(d$M_sos)
hist(d$M_psc)
hist(d$M_nsc)

y_model  <- bf(Y   ~ 1 + X + M_cop + M_sos + M_psc + M_nsc, family = skew_normal())
m1_model <- bf(M_cop ~ 1 + X) # coping
m2_model <- bf(M_sos ~ 1 + X, family = skew_normal()) # social support
m3_model <- bf(M_psc ~ 1 + X) # positive SC
m4_model <- bf(M_nsc ~ 1 + X, family = skew_normal()) # negative SC



fit <-
  brm(
    data = d, 
    y_model + m1_model + m2_model + m3_model + m4_model + set_rescor(FALSE),
    chains = 4, 
    cores = 4, 
    inits = 0
  )



# putting the posterior draws into a data frame
post <- posterior_samples(fit)

SD_y <- sd(d$Y)

# Computing the indirect effect with multiplication and the total effect
post <-
  post %>% 
  mutate(
    direct_eff = b_Y_X,
    ind_eff_M_sos = (b_Msos_X * b_Y_M_sos),
    ind_eff_M_cop = (b_Mcop_X * b_Y_M_cop),
    ind_eff_M_psc = (b_Mpsc_X * b_Y_M_psc),
    ind_eff_M_nsc = (b_Mnsc_X * b_Y_M_nsc),
    total_eff = direct_eff + ind_eff_M_sos + ind_eff_M_cop + ind_eff_M_psc + ind_eff_M_nsc
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

# # the total effect
# post <-
#   post %>%
#   mutate(
#     total_eff = b_Y_X + ind_eff_M_sos + ind_eff_M_cop + ind_eff_M_psc + ind_eff_M_nsc
#   )
# 
# quantile(post$total_eff, probs = c(.5, .025, .975)) %>% 
#   round(digits = 3)



post %>% 
  # mutate(
  #   direct_eff = b_Y_X
  # ) %>% 
  # mutate(total_eff = 
  #          direct_eff + ind_eff_M_sos + ind_eff_M_cop + ind_eff_M_psc + ind_eff_M_nsc) %>% 
  select(direct_eff, ind_eff_M_sos, ind_eff_M_cop, ind_eff_M_psc, ind_eff_M_nsc, total_eff) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)



# Posterior plots
post %>% 
  ggplot(aes(x = ind_eff_M_sos)) +
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

pp_check(fit, resp = "Y")
pp_check(fit, resp = "Mcop")
pp_check(fit, resp = "Msos")
pp_check(fit, resp = "Mpsc")
pp_check(fit, resp = "Mnsc")

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














