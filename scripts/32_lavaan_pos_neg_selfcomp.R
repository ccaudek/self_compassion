#' ------------------------------------------------------------------
#' Self-compassion project
#'
#' Question: does the positive component of the 
#' self-compassion scale add anything to what is provided by 
#' the negative component of self-compassion?
#'
#' 32_lavaan_pos_neg_selfcomp.R
#' Corrado Caudek
#' Last modified on: "Fri Oct 25 13:58:26 2019"
# -------------------------------------------------------------------


library("here")
suppressPackageStartupMessages(library("lavaan"))
suppressPackageStartupMessages(library("brms"))
suppressPackageStartupMessages(library("tidyverse"))
library("ggthemes")
suppressPackageStartupMessages(library("viridis"))
suppressPackageStartupMessages(library("tidyverse"))
library("tidyr")
suppressPackageStartupMessages(library("rstanarm"))
suppressPackageStartupMessages(library("projpred"))
suppressPackageStartupMessages(library("mice"))
suppressPackageStartupMessages(library("corrplot"))
suppressPackageStartupMessages(library("bayesplot"))
theme_set(theme_classic())


options(max.print = 99999999)

source(here("libraries", "self_compassion_fnc.R"))


# Read data ----

temp <- read.csv(
  file = here("data", "processed", "rescue_workers.csv"),
  header = TRUE,
  sep = ";"
)

# remove uninformative variable
temp$is_rescue_worker <- NULL

# remove missing data on all questionnaires data (for convenience,
# I use the variable 'friends')
mydata <- temp[!is.na(temp$friends), ]
# summary(mydata)


# Multiple imputation ----

# MI for age.

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

ggplot(data = mydata, aes(new_possibilities)) +
  geom_histogram(
    aes(y = ..density..),
    # breaks=seq(0, 85, by = 2),
    col = "red",
    fill = "green",
    alpha = .2
  ) +
  geom_density(col = 2)

# mydata$ptg <- mydata$life_appreciation + mydata$new_possibilities +
#   mydata$personal_strength + mydata$spirituality_changes +
#   mydata$interpersonal_relationships

rm(d_imp, dat_num, temp)


# Data wrangling ----

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
    "1" = "Meno di 1 turno al mese"
  ))
)

# Recode education as a numeric variable
mydata$edu <- as.numeric(
  as.character(fct_recode(
    mydata$education,
    "1" = "Scuola media primaria",
    "2" = "Diploma",
    "3" = "Laurea breve",
    "4" = "Laurea magistrale",
    "5" = "Dottorato"
  ))
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
    "6" = "Più di 1 anno fa"
  ))
)

mydata$job_qualification <- fct_recode(
  mydata$job_qualification,
  "Soccorritore" = "Infermiere",
  "Soccorritore" = "Medico"
)

df <- mydata %>%
  drop_na(ptg)

df <- df %>%
  mutate(
    spirituality_changes = spirituality_changes * 3
  )


length(unique(df$id))


# Models ----


# * Model 1 ----

model1 <- "
  # measurement model
  
  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
  
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ social_support + avoiding_strategies + 
          positive_attitude + problem_orientation + 
          transcendent_orientation
  
  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  sc =~ self_judgment + isolation + over_identification +
        self_kindness + common_humanity + mindfulness
  
  # regressions
  ptg  ~ cope + soc + sc 
  ptss ~ cope + soc + sc 
  
  self_judgment ~~ self_kindness
  # spirituality_changes ~~ transcendent_orientation
  
  "

# * Model 1a ----

model1a <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
  
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 
   
  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  sc =~ self_judgment + isolation + over_identification +
        self_kindness + common_humanity + mindfulness
  
  # regressions
  ptg  ~ cope + soc + sc 
  ptss ~ cope + soc + sc 
  
  self_judgment ~~ self_kindness

  "


# * Model 2 ----

model2 <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
  
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 
   # + transcendent_orientation + avoiding_strategies + social_support 
  
  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # regressions
  ptg  ~ cope + soc + nsc + psc 
  ptss ~ cope + soc + nsc + psc 
  
  psc ~~ nsc

  self_judgment ~~ self_kindness
  # spirituality_changes ~~ transcendent_orientation
  
  "


# * Model 3 ----

model3 <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
         
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # regressions
  ptg  ~ cope + soc + psc 
  ptss ~ cope + soc + psc 
  
  psc ~~ nsc

  self_judgment ~~ self_kindness

  "


# * Model 3a ----

model3a <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
         
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # regressions
  ptg  ~ cope + soc + nsc 
  ptss ~ cope + soc + nsc 
  
  psc ~~ nsc

  self_judgment ~~ self_kindness

  "


# * Model 4 ----

model4 <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
         
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # ffi
  ffi =~ neuroticism + extraversion + conscientiousness +
         openness + agreeableness 

  # regressions
  ptg  ~ cope + soc + nsc + psc + ffi
  ptss ~ cope + soc + nsc + psc + ffi
  # ffi ~ ptg + ptss
  
  psc ~~ nsc

  self_judgment ~~ self_kindness
  # spirituality_changes ~~ transcendent_orientation
  
  "


# * Model 5 ----

model5 <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
         
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # neuroticism
  negative_affect =~ i1_na + i2_na + i3_na + i4_na + i5_na 
  self_reproach =~ i1_sc + i2_sc + i3_sc + i4_sc + i5_sc + 
                   i6_sc + i7_sc 
  neuroticism =~ negative_affect + self_reproach

  # regressions
  ptg  ~ cope + soc + nsc + psc + neuroticism
  ptss ~ cope + soc + nsc + psc + neuroticism
  # ffi ~ ptg + ptss
  
  psc ~~ nsc

  self_judgment ~~ self_kindness

  "


# * Model 6 ----

model6 <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
         
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # neuroticism
  neuroticism =~ negative_affect + self_reproach

  # regressions
  ptg  ~ cope + soc + nsc + psc + neuroticism
  ptss ~ cope + soc + nsc + psc + neuroticism
  
  psc ~~ nsc

  self_judgment ~~ self_kindness

  "


# * Model 7 ----

model7 <- "

  # post-traumatic growth
  ptg =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
         
  # ptss
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 
  
  # perceived social support
  soc =~ family + friends + significant_other
  
  # neuroticism
  neuroticism =~ negative_affect + self_reproach
  
  # regressions
  ptg  ~ cope + soc + neuroticism
  ptss ~ cope + soc + neuroticism

  "


# Fits ----

# * Fit of model 1 ----
# M1 does not distinguish between the two components of 
# self-compassion

fit1 <- lavaan::sem(
  model1,
  data = df,
  estimator = "MLM",
  likelihood = "wishart"
)

summary(
  fit1,
  standardized = TRUE,
  fit.measures = TRUE,
  rsquare = TRUE
)
fitMeasures(fit1)

# Interpretation: the fit is bad


# * Fit of model 1a ----
# M1a does not distinguish between the two components of 
# self-compassion and consider only a subset of dimensions of
# coping.

fit1a <- lavaan::sem(
  model1a,
  data = df,
  estimator = "MLM",
  likelihood = "wishart"
)

summary(
  fit1a,
  standardized = TRUE,
  fit.measures = TRUE,
  rsquare = TRUE
)
fitMeasures(fit1a)

# Interpretation: the fit is still bad.


# * Fit of model 2 ----
# M2 distinguishes between the two components of self-compassion 
# and uses only a subset of the coping dimensions.

fit2 <- sem(
  model2,
  data = df,
  estimator = "MLM",
  likelihood = "wishart",
  meanstructure = TRUE
)

varTable(fit2)

summary(
  fit2,
  standardized = TRUE,
  fit.measures = TRUE,
  rsquare = TRUE
)
fitMeasures(fit2)
parTable(fit2)
standardizedSolution(fit2, type = "std.all")

# Interpretation: the fit is good.

semPlot::semPaths(
  fit2,
  what = "std",
  nCharNodes = 6,
  sizeMan = 10,
  edge.label.cex = 1.25,
  curvePivot = TRUE,
  fade = FALSE
)

modificationindices(
  fit2,
  minimum.value = 20
) # only print MIs > 20

inspect(fit2, what = "cor.all")
lavCor(fit2)

# plot_matrix <- function(matrix_toplot){
#   corrplot::corrplot(matrix_toplot, is.corr = FALSE,
#                      type = 'lower',
#                      order = "original",
#                      tl.col='black', tl.cex=.75)
# }
#
# plot_matrix(residuals(fit2, type="cor")$cor)


# * Fit of model 3 ----
# Remove the effect of the negative component of self-compassion.

fit3 <- sem(
  model3,
  data = df,
  estimator = "MLM",
  likelihood = "wishart",
  meanstructure = TRUE
)

summary(
  fit3, 
  standardized = TRUE, 
  fit.measures = TRUE, 
  rsquare = TRUE
)
fitMeasures(fit3)
anova(fit2, fit3)


# * Fit of model 3a ----
# Remove the effect of the positive component of self-compassion.

fit3a <- sem(
  model3a,
  data = df,
  estimator = "MLM",
  likelihood = "wishart",
  meanstructure = TRUE
)

summary(
  fit3a, 
  standardized = TRUE, 
  fit.measures = TRUE, 
  rsquare = TRUE
)
fitMeasures(fit3a)

anova(fit2, fit3)
anova(fit2, fit3a)
# If the positive component of self-compassion is removed,
# the model's fit does not change.
# If the negative component of self-compassion is removed,
# the model's fit decreases.


# * Fit of model 4 ----

fit4 <- sem(
  model4,
  data = df,
  estimator = "MLM",
  likelihood = "wishart",
  meanstructure = TRUE
)

summary(
  fit4, 
  standardized = TRUE, 
  fit.measures = TRUE, 
  rsquare = TRUE
)
fitMeasures(fit4)

# Models 2 and 4 are not nested, therefore a chi-square difference 
# test is not meaningful.

# Also after removing openness + agreeableness, which have low 
# loadingson ffi, the regression coefficients associated to ffi 
# are not statistically significant. I conclude that there is no 
# obvious effect of personality factors in the relation between 
# self compassion and ptg and ptss.


# * Fit of model 5 ----

fit5 <- sem(
  model5,
  data = df,
  estimator = "MLM",
  likelihood = "wishart",
  meanstructure = TRUE
)

summary(
  fit5, 
  standardized = TRUE, 
  fit.measures = TRUE, 
  rsquare = TRUE
)
fitMeasures(fit5)


# * Fit of model 6 ----

fit6 <- sem(
  model6,
  data = df,
  estimator = "MLM",
  likelihood = "wishart",
  meanstructure = TRUE
)

summary(
  fit6, 
  standardized = TRUE, 
  fit.measures = TRUE, 
  rsquare = TRUE
)
fitMeasures(fit6)


# * Fit of model 7 ----

fit7 <- sem(
  model7,
  data = df,
  estimator = "MLM",
  likelihood = "wishart",
  meanstructure = TRUE
)

summary(
  fit7, 
  standardized = TRUE, 
  fit.measures = TRUE, 
  rsquare = TRUE
)
fitMeasures(fit7)

anova(fit6, fit7)





# Question: how do differ individuals who experience a PTG and 
# who did not experience any PTG?

df$has_ptg <- ifelse(df$ptg > 0, 1, 0)
df$years_experience <- as.numeric(df$years_experience)
df$is_unemployed <- ifelse(df$employment == "Disoccupato", 1, 0)


fm <- glm(
  has_ptg ~ gender + age_imp + edu + is_unemployed +
    training_time + activity_rate,
  family = binomial,
  data = df
)

summary(fm)

exp(-1.56)



fm <- lm(
  ptg ~ gender + age + edu + employment +
    training_time + activity_rate +
    neuroticism + extraversion + conscientiousness +
    openness + agreeableness,
  data = df
)

summary(fm)
