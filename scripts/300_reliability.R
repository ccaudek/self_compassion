# Script name: 300_reliability.R
# Project: self-compassion
# Script purpose: compute reliability of the scales with the present sample
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun  1 15:27:54 2021
# Last Modified Date: Tue Jun  1 15:27:54 2021
# 
# Notes: 


# In the self-compassion project, the following scales have been 
# administered.

# - NEO FFI 60: 
#   neuroticism extraversion openness agreeableness conscientiousness
# 
# - COPE:
#   social_support avoiding_strategies positive_attitude 
#   problem_orientation transcendent_orientation 
# coping <- social_support + positive_attitude + problem_orientation + 
#           transcendent_orientation
# 
# - PTG:
#   interpersonal_relationships new_possibilities personal_strength 
#   life_appreciation spirituality_changes 
# ptg <- life_appreciation + new_possibilities + personal_strength + 
#        spirituality_changes + interpersonal_relationships
# 
# - IES-R (The Impact of Event Scale-Revised):
#   avoiding intrusivity iperarousal (components of PTSD)
# ies_r_score <- avoiding + intrusivity + iperarousal
# 
# - SCS:
#   self_kindness self_judgment common_humanity isolation mindfulness 
#   over_identification
# neg_self_compassion <- self_judgment + isolation + over_identification
# pos_self_compassion <- self_kindness + common_humanity + mindfulness
# 
# - MSPSS:
#   family friends significant_other 


# Set up
library("here")
suppressPackageStartupMessages(library("tidyverse")) 
library("forcats")
library("readxl")

# Read data
df_toscana <- read_excel(here("data", "raw", "toscana_1.xlsx"))
df_toscana$where <- "toscana"
df_lombardia <- read_excel(here("data", "raw", "lombardia_1.xlsx"))
df_lombardia$where <- "lombardia"

# Merge the two data.frames 
df <- rbind(df_toscana, df_lombardia)

# remove NAs.
df <- df[!is.na(df$X17), ]

# Rename variables
df <- df %>% 
  rename(
    date = `Informazioni cronologiche`,
    permission = `Trattamento dei dati personali`,
    id = `Inserisci il codice identificativo come sopra indicato:`,
    gender = Sesso,
    education = `Livello istruzione`,
    employment =  Occupazione,
    is_rescue_worker = `Sei un Soccorritore in Emergenza?`,
    red_cross_commeetee_location =  `Comitato Croce Rossa Italiana di appartenenza (per es. Comitato di Firenze)`,
    rescue_worker_qualification =  `Possiedi la qualifica TSSA/PTSI?`, 
    last_training = `Quando hai effettuato l'ultimo re-training, corso, inerente la qualifica TSSA/FULL-D?`,
    rate_of_activity =  `Con quale frequenza fai turno in Ambulanza?`,
    job_qualification =  `Generalmente quale ruolo ricopri nella squadra?`,
    is_job_qualification_invariant= `Tendi a ricoprire sempre lo stesso ruolo?`,
    is_team_invariant = `Generalmente fai turno sempre con la stessa squadra?`,
    age =  `Quanti anni hai?`
  ) 


# The data.frame contains the following scales:

# - NEO_FFI_60: 60 items, from X17 to X76
# - COPE_NVI: 60 items, from X77 to X136
# - PTGI: 29 items, X137 to X144, X145 to X165
# - IES_R: 28 items, items X166 to X171, X172 to X193
# - SCS: 26 items, items X194 to 219
# - MSPSS: 12 items, items X220 to 231

gender <- df %>%
  dplyr::select(gender)

education <- df %>%
  dplyr::select(education)

employment <- df %>%
  dplyr::select(employment)

is_rescue_worker <- df %>%
  dplyr::select(is_rescue_worker)

red_cross_commeetee_location <- df %>%
  dplyr::select(red_cross_commeetee_location)

rescue_worker_qualification <- df %>%
  dplyr::select(rescue_worker_qualification)

last_training <- df %>%
  dplyr::select(last_training)

rate_of_activity <- df %>%
  dplyr::select(rate_of_activity)

job_qualification <- df %>%
  dplyr::select(job_qualification)

is_job_qualification_invariant <- df %>%
  dplyr::select(is_job_qualification_invariant)

is_team_invariant <- df %>%
  dplyr::select(is_team_invariant)

age <- df %>%
  dplyr::select(age)

glimpse(df)


# Impact of Event Scale - Revised (IES-R) ----

# The IES-R is a 22-item self-report measure (for DSM-IV) that assesses 
# subjective distress caused by traumatic events.
# The IES-R yields a total score (ranging from 0 to 88) and subscale scores 
# can also be calculated for the Intrusion, Avoidance, and Hyperarousal subscales.
# Il massimo punteggio medio di ognuna delle 3 subscale è 4, quindi il 
# punteggio medio totale massimo della scala IES-R è 12. 
# Bassi punteggi sono migliori. 
# Un punteggio totale alla IES-R di 33 o superiore su un punteggio massimo di 88 
# significa la probabile presenza di un PTSD.

traumatic_event <- df %>% 
  select(X166)
time_traumatic_event <- df %>% 
  select(X167)
defusing_ies_r <- df %>% 
  select(X168)
when_defusing_ies_r <- df %>% 
  select(X169)
debriefing_ies_r <- df %>% 
  select(X170)
when_debriefing_ies_r <- df %>% 
  select(X171)

# Let start with the coding of ies_r. 
# First, we select the right columns:
ies_r <- df %>% 
  select(X172:X193)

# Then we change the column names to facilitate coding
item_names <- paste0("ies_", 1:22)
names(ies_r) <- item_names

# Avoidance
avoiding <- 
  ies_r$ies_5 + ies_r$ies_7 + ies_r$ies_8 + ies_r$ies_11 + 
  ies_r$ies_12 + ies_r$ies_13 + ies_r$ies_17 + ies_r$ies_22
# hist(avoiding)

# Intrusion
intrusivity <- 
  ies_r$ies_1 + ies_r$ies_2 + ies_r$ies_3 + ies_r$ies_6 + 
  ies_r$ies_9 + ies_r$ies_14 + ies_r$ies_16 + ies_r$ies_20
# hist(intrusivity)

# Hyperarousal
iperarousal <-  
  ies_r$ies_4 + ies_r$ies_10 + ies_r$ies_15 + ies_r$ies_18 + 
  ies_r$ies_19 + ies_r$ies_21
# hist(iperarousal)

# Total score
ies_r_score <- avoiding + intrusivity + iperarousal
# hist(ies_r_score)

mod_ies <- '
  AV =~ NA*ies_5 + ies_7 + ies_8 + ies_11 + ies_12 + ies_13 + ies_17 + ies_22
  IN =~ NA*ies_1 + ies_2 + ies_3 + ies_6 + ies_9 + ies_14 + ies_16 + ies_20
  HY =~ NA*ies_4 + ies_10 + ies_15 + ies_18 + ies_19 + ies_21
'

fit_ies <- lavaan::cfa(
  mod_ies,
  data = ies_r,
  estimator = "WLSMV",
  ordered = names(ies_r),
  std.lv = TRUE,
  orthogonal = FALSE
)

summary(
  fit_ies, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitMeasures(
  fit_ies, 
  c("chisq", "df", "cfi", "cfi.scaled", "tli", 
    "rmsea", "srmr")
)

round(semTools::reliability(fit_ies, return.total = TRUE), 2)


# Post Traumatic Growth Inventory (PTGI) ----

# PTGI is an instrument for assessing positive outcomes reported 
# by persons who have experienced traumatic events.
# 
# We rename column and variables of PTG event as follows
life_event <- df %>% 
  select(X137)
time_life_event <- df %>% 
  select(X138)
rescue_event <- df %>% 
  select(X139)
time_rescue_event <- df %>% 
  select(X140)
defusing_ptg <- df %>% 
  select(X141)
when_defusiong_ptg <- df %>% 
  select(X142)
debriefing_ptg <- df %>% 
  select(X143)
when_debriefing_ptg <- df %>% 
  select(X144)

# Let start with the coding of ptgi. First, we select the 
# right columns:
ptgi <- df %>% 
  select(X145:X165)

# Then we change the column names to facilitate coding
item_names <- paste0("ptgi_", 1:21)
names(ptgi) <- item_names

# Relating to Others
interpersonal_relationships <- ptgi$ptgi_15 + ptgi$ptgi_20 + ptgi$ptgi_9 + 
  ptgi$ptgi_21 + ptgi$ptgi_8 + ptgi$ptgi_16 + ptgi$ptgi_6
# hist(interpersonal_relationships)

# New Possibilities
new_possibilities <- ptgi$ptgi_11 + ptgi$ptgi_7 + ptgi$ptgi_3 + ptgi$ptgi_17 + 
  ptgi$ptgi_14
# hist(new_possibilities)

# Personal Strength
personal_strength <- ptgi$ptgi_10 + ptgi$ptgi_19 + ptgi$ptgi_4 + ptgi$ptgi_12
# hist(personal_strength)

# Appreciation
life_appreciation <- ptgi$ptgi_13 + ptgi$ptgi_2 + ptgi$ptgi_1
# hist(life_appreciation)

# Spiritual Enhancement
spirituality_changes <- ptgi$ptgi_5 + ptgi$ptgi_18
# hist(spirituality_changes)

ptg <- life_appreciation + new_possibilities + personal_strength + 
  spirituality_changes + interpersonal_relationships

mod_ptgi <- '
  RO =~ NA*ptgi_15 + ptgi_20 + ptgi_9 + ptgi_21 + ptgi_8 + ptgi_16 + ptgi_6
  NP =~ NA*ptgi_11 + ptgi_7 + ptgi_3 + ptgi_17 + ptgi_14
  PS =~ NA*ptgi_10 + ptgi_19 + ptgi_4 + ptgi_12
  AP =~ NA*ptgi_13 + ptgi_2 + ptgi_1
  SE =~ NA*ptgi_5 + ptgi_18
'

fit_ptgi <- lavaan::cfa(
  mod_ptgi,
  data = ptgi,
  estimator = "WLSMV",
  ordered = names(ptgi),
  std.lv = TRUE,
  orthogonal = FALSE
)

summary(
  fit_ptgi, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitMeasures(
  fit_ptgi, 
  c("chisq", "df", "cfi", "tli", "rmsea", "srmr")
)

round(semTools::reliability(fit_ptgi, return.total = TRUE), 2)


# Multidimensional Scale of Perceived Social Support (MSPSS) ----

# The MSPSS is a 12-item scale designed to measure perceived social 
# support from three sources:
# - Family, 
# - Friends, 
# - a Significant other

# Let start with the coding of mspss. First, we select the right 
# columns:
mspss <- df %>% 
  select(X220:X231)

# Then we change the column names to facilitate coding
item_names <- paste0("mspss_", 1:12)
names(mspss) <- item_names

# Family
family <- mspss$mspss_3 + mspss$mspss_4 + mspss$mspss_8 + mspss$mspss_11
# hist(family)

# Friends
friends <- mspss$mspss_6 + mspss$mspss_7 + mspss$mspss_9 + mspss$mspss_12
# hist(friends)

# A significant other
significant_other <- mspss$mspss_1 + mspss$mspss_2 + mspss$mspss_5 + mspss$mspss_10
# hist(significant_other)

mod_mspss <- '
  FA =~ NA*mspss_3 + mspss_4 + mspss_8 + mspss_11
  FR =~ NA*mspss_6 + mspss_7 + mspss_9 + mspss_12
  SO =~ NA*mspss_1 + mspss_2 + mspss_5 + mspss_10
'

fit_mspss <- lavaan::cfa(
  mod_mspss,
  data = mspss,
  estimator = "WLSMV",
  ordered = names(mspss),
  std.lv = TRUE,
  orthogonal = FALSE
)

summary(
  fit_mspss, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitMeasures(
  fit_mspss, 
  c("chisq", "df", "cfi", "tli", "rmsea", "srmr")
)

round(semTools::reliability(fit_mspss, return.total = TRUE), 2)



# NEO-FIve Factor Inventory (NEO-FFI) ----

# NEO-FFI measures five core personality traits: 
# neuroticism, openness, agreeableness, conscientiousness.

# First, let's select the right columns:
neo_ffi_60 <- df %>% 
  dplyr::select(X17:X76)

# Then we change the column names to facilitate coding
item_names <- paste0("neoffi_", 1:60)
names(neo_ffi_60) <- item_names

# Some items are reversed. In such cases, the correct values are given 
# by: abs(x - max).

# Neuroticism (1)

# The first factor of neo_ffi_60 is called *neuroticism* and is composed 
# by two subscales. The scores on such scales are the following:

# Negative affect:
negative_affect <- 
  abs(neo_ffi_60$neoffi_1 - 4) + neo_ffi_60$neoffi_11 + abs(neo_ffi_60$neoffi_16 - 4) + 
  abs(neo_ffi_60$neoffi_31 - 4) + abs(neo_ffi_60$neoffi_46 -4)

# Self reproach:
self_reproach <-
  neo_ffi_60$neoffi_6 + neo_ffi_60$neoffi_21 + neo_ffi_60$neoffi_26 + neo_ffi_60$neoffi_36 + 
  neo_ffi_60$neoffi_41 + neo_ffi_60$neoffi_51 + neo_ffi_60$neoffi_56

# neuroticism scores
neuroticism <- negative_affect + self_reproach

# Just to take a look to the data, an histogram of neuroticism is
# hist(neuroticism)

# Extraversion (2)
# The second factor of neo_ffi_60 is called *extraversion* and is composed by 
# three subscales. The scores on such scales are the following:
positive_affect <- 
  neo_ffi_60$neoffi_7 + abs(neo_ffi_60$neoffi_12 - 4) + neo_ffi_60$neoffi_37 + 
  abs(neo_ffi_60$neoffi_42 -4)

sociability <- neo_ffi_60$neoffi_2 + neo_ffi_60$neoffi_17 + 
  abs(neo_ffi_60$neoffi_27 - 4) + abs(neo_ffi_60$neoffi_57 - 4)

activity <- neo_ffi_60$neoffi_22 + neo_ffi_60$neoffi_32 + neo_ffi_60$neoffi_47 + 
  neo_ffi_60$neoffi_52

extraversion <- positive_affect + sociability + activity

# Just to take a look to the data, an histogram of estraversion is
# hist(extraversion)

# Openness (3)
# The third factor of neo_ffi_60 is called *openness* and is composed 
# by three subscales. The scores on such scales are the following.

# Aesthetic interests:
aesthetic_interests <- 
  neo_ffi_60$neoffi_13 + abs(neo_ffi_60$neoffi_23 - 4) + neo_ffi_60$neoffi_43

# Intellectual interests:
intellectual_interests <- 
  abs(neo_ffi_60$neoffi_48 - 4) + neo_ffi_60$neoffi_53 + neo_ffi_60$neoffi_58

# Unconventionality:
unconventionality <- 
  abs(neo_ffi_60$neoffi_3 - 4) + abs(neo_ffi_60$neoffi_8 - 4) + 
  abs(neo_ffi_60$neoffi_18 - 4) + abs(neo_ffi_60$neoffi_38 - 4) + neo_ffi_60$neoffi_28 + 
  abs(neo_ffi_60$neoffi_33 - 4)

# Openness scores
openness <- aesthetic_interests + intellectual_interests + 
            unconventionality
# hist(openness)

# Agreeableness (4)
# The fourth factor of neo_ffi_60 is called *agreeableness* and 
# is composed by two subscales. 

# Nonantagonistic orientation:
nonantagonistic_orientation <- 
  abs(neo_ffi_60$neoffi_9 - 4) + abs(neo_ffi_60$neoffi_14 - 4) + 
  neo_ffi_60$neoffi_19 + abs(neo_ffi_60$neoffi_24 - 4) + 
  abs(neo_ffi_60$neoffi_29 - 4) + abs(neo_ffi_60$neoffi_44 - 4) + 
  abs(neo_ffi_60$neoffi_54 - 4) + abs(neo_ffi_60$neoffi_59 - 4)

# Prosocial orientation:
prosocial_orientation <- 
  neo_ffi_60$neoffi_4 + neo_ffi_60$neoffi_34 + abs(neo_ffi_60$neoffi_39 - 4) + 
  neo_ffi_60$neoffi_49

# agreeableness scores
agreeableness <- nonantagonistic_orientation + prosocial_orientation
# hist(agreeableness)

# Conscientiousness (5)
# The fifth factor of neo_ffi_60 is called *conscientiousness* and 
# is composed by three subscales. 

# Orderliness:
orderliness <- 
  neo_ffi_60$neoffi_5 + neo_ffi_60$neoffi_10 + abs(neo_ffi_60$neoffi_15 - 4) + 
  abs(neo_ffi_60$neoffi_30 - 4) + abs(neo_ffi_60$neoffi_55 - 4)

# Goal striving:
goal_striving <- neo_ffi_60$neoffi_25 + neo_ffi_60$neoffi_35 + neo_ffi_60$neoffi_60

# Dependability:
dependability <- neo_ffi_60$neoffi_20 + neo_ffi_60$neoffi_40 + 
  abs(neo_ffi_60$neoffi_45 - 4) + neo_ffi_60$neoffi_50

# conscientiousness scores
conscientiousness <- orderliness + goal_striving + dependability
# hist(conscientiousness)

neoffi_data <- data.frame(
  negative_affect, self_reproach,
  positive_affect, sociability, activity,
  aesthetic_interests, intellectual_interests, unconventionality,
  nonantagonistic_orientation, prosocial_orientation,
  orderliness, goal_striving, dependability
  )

mod_neoffi <- '
  NE =~ NA*negative_affect + self_reproach
  EX =~ NA*positive_affect + sociability + activity
  OP =~ NA*aesthetic_interests + intellectual_interests + unconventionality
  AG =~ NA*nonantagonistic_orientation + prosocial_orientation
  CO =~ NA*orderliness + goal_striving + dependability
'

fit_neoffi <- lavaan::cfa(
  mod_neoffi,
  data = neoffi_data,
  estimator = "MLM",
  # ordered = names(neoffi_data),
  std.lv = TRUE,
  orthogonal = FALSE
)


fitMeasures(
  fit_neoffi, 
  c("chisq", "df", "cfi", "tli", "rmsea", "srmr")
)

round(semTools::reliability(fit_neoffi, return.total = TRUE), 2)


# Negative affect:
negative_affect_df <- data.frame(
  i1 = abs(neo_ffi_60$neoffi_1 - 4), i11 = neo_ffi_60$neoffi_11,
  i16 = abs(neo_ffi_60$neoffi_16 - 4), i31 = abs(neo_ffi_60$neoffi_31 - 4),
  i46 = abs(neo_ffi_60$neoffi_46 -4)
)
  
# Self reproach:
self_reproach_df <- data.frame(
  i6 = neo_ffi_60$neoffi_6, i21 = neo_ffi_60$neoffi_21, i26 = neo_ffi_60$neoffi_26,
  i36 = neo_ffi_60$neoffi_36, i41 = neo_ffi_60$neoffi_41, i51 = neo_ffi_60$neoffi_51,
  i56 = neo_ffi_60$neoffi_56
)

# extraversion
positive_affect_df <- data.frame(
  i7 = neo_ffi_60$neoffi_7, i12 = abs(neo_ffi_60$neoffi_12 - 4), 
  i37 = neo_ffi_60$neoffi_37, i42 = abs(neo_ffi_60$neoffi_42 -4)
)

sociability_df <- data.frame(
  i2 = neo_ffi_60$neoffi_2, i17 = neo_ffi_60$neoffi_17,
  i27 = abs(neo_ffi_60$neoffi_27 - 4), i57 = abs(neo_ffi_60$neoffi_57 - 4)
)

activity_df <- data.frame(
  i22 = neo_ffi_60$neoffi_22, i32 = neo_ffi_60$neoffi_32, 
  i47 = neo_ffi_60$neoffi_47, i52 =neo_ffi_60$neoffi_52
)

# openness

aesthetic_interests_df <- data.frame(
  i13 = neo_ffi_60$neoffi_13, i23 = abs(neo_ffi_60$neoffi_23 - 4), 
  i43 = neo_ffi_60$neoffi_43
)
  
intellectual_interests_df <- data.frame(
  i48 = abs(neo_ffi_60$neoffi_48 - 4), i53 = neo_ffi_60$neoffi_53, 
  i58 = neo_ffi_60$neoffi_58
)
  
unconventionality_df <- data.frame(
  i3 = abs(neo_ffi_60$neoffi_3 - 4), i8 = abs(neo_ffi_60$neoffi_8 - 4), 
  i18 = abs(neo_ffi_60$neoffi_18 - 4), i38 = abs(neo_ffi_60$neoffi_38 - 4),
  i28 = neo_ffi_60$neoffi_28, i33 = abs(neo_ffi_60$neoffi_33 - 4)
)
  
# agreableness

nonantagonistic_orientation_df <- data.frame(
  i9 = abs(neo_ffi_60$neoffi_9 - 4), i14 = abs(neo_ffi_60$neoffi_14 - 4),
  i19 = neo_ffi_60$neoffi_19, i24 = abs(neo_ffi_60$neoffi_24 - 4), 
  i29 = abs(neo_ffi_60$neoffi_29 - 4), i44 = abs(neo_ffi_60$neoffi_44 - 4), 
  i54 = abs(neo_ffi_60$neoffi_54 - 4), i59 = abs(neo_ffi_60$neoffi_59 - 4)
)
  
prosocial_orientation_df <- data.frame(
  i4 = neo_ffi_60$neoffi_4, i34 = neo_ffi_60$neoffi_34, 
  i39 = abs(neo_ffi_60$neoffi_39 - 4),
  i49 = neo_ffi_60$neoffi_49
)
  
# Consciensciousness
orderliness_df <- data.frame(
  i5 = neo_ffi_60$neoffi_5, i10 = neo_ffi_60$neoffi_10, 
  i15 = abs(neo_ffi_60$neoffi_15 - 4),
  i30 = abs(neo_ffi_60$neoffi_30 - 4), i55 = abs(neo_ffi_60$neoffi_55 - 4)
)

goal_striving_df <- data.frame(
  i25 = neo_ffi_60$neoffi_25, i35 = neo_ffi_60$neoffi_35, i60 = neo_ffi_60$neoffi_60
)

dependability_df <- data.frame(
  i20 = neo_ffi_60$neoffi_20, i40 = neo_ffi_60$neoffi_40,
  i45 = abs(neo_ffi_60$neoffi_45 - 4), i50 = neo_ffi_60$neoffi_50
)

neof_df <- cbind(
  negative_affect_df, self_reproach_df,
  positive_affect_df, sociability_df, activity_df,
  aesthetic_interests_df, intellectual_interests_df, unconventionality_df,
  nonantagonistic_orientation_df, prosocial_orientation_df,
  orderliness_df, goal_striving_df, dependability_df
)

mod_neof <- '
  NE =~ NA*i1 + i11 + i16 + i31 + i46 + i6 + i21 + i26 + i36 + i41 + i51 + i56
  EX =~ NA*i2 + i7 + i12 + i22 + i27 + i32 + i37 + i42 + i47 + i52 + i57
  OP =~ NA*i3 + i8 + i13 + i18 + i23 + i28 + i33 + i38 + i43 + i48 + i53 + i58
  AG =~ NA*i4 + i9 + i14 + i19 + i24 + i29 + i34 + i39 + i44 + i49 + i54 + i59
  CO =~ NA*i5 + i10 + i15 + i20 + i25 + i30 + i35 + i40 + i45 + i50 + i55 + i20
'

fit_neof <- lavaan::cfa(
  mod_neof,
  data = neof_df,
  ordered = names(neof_df),
  std.lv = TRUE,
  orthogonal = FALSE
)

fitMeasures(
  fit_neof, 
  c("chisq", "df", "cfi", "tli", "rmsea", "srmr")
)

round(semTools::reliability(fit_neof, return.total = TRUE), 2)
#          NE   EX   OP   AG   CO total
# alpha  0.89 0.83 0.72 0.73 0.88  0.77
# omega  0.51 0.62 0.13 0.23 0.61  0.46
# omega2 0.51 0.62 0.13 0.23 0.61  0.46
# omega3 0.50 0.63 0.11 0.20 0.61  0.40
# avevar 0.43 0.38 0.22 0.18 0.43  0.32




# Coping Orientation to Problems Experienced (COPE-NVI) ----

# COPE-NVI was devised to measure coping strategies on five dimensions: 
# - problem-oriented coping strategies, 
# - avoidance strategies, 
# - social-support strategies,
# - positive attitude,
# - transcendent orientation.
#
# Supporto sociale (Ricerca di comprensione + Ricerca di informazioni + 
# Sfogo emotivo), Strategie di evitamento (Negazione + Umorismo + 
# Uso di droga + Distacco comportamentale +  Distacco mentale), 
# Attitudine positiva (Accettazione + Contenimento + 
# Reinterpretazione positiva), Orientamento al problema (Soppressione + 
# Pianifi cazione + Attività), Religione (Sica, Novara, Dorz e Sanavio, 1997a)
# 
# Let start with the coding of cope_nvi. 
# First, we select the right columns:
cope_nvi <- df %>% 
  select(X77:X136)

# Then we change the column names to facilitate coding
item_names <- paste0("cope_", 1:60)
names(cope_nvi) <- item_names

# Some items are reversed. In that cases, we can compute the right 
# values by using the following formula: abs(x - max).

# Social support (1)
social_support_df <- data.frame(
  cope_4 = cope_nvi$cope_4, cope_14 = cope_nvi$cope_14, 
  cope_30 = cope_nvi$cope_30, cope_45 = cope_nvi$cope_45, 
  cope_11 = cope_nvi$cope_11, cope_23 = cope_nvi$cope_23, 
  cope_34 = cope_nvi$cope_34, cope_52 = cope_nvi$cope_52, 
  cope_3 = cope_nvi$cope_3, cope_17 = cope_nvi$cope_17, 
  cope_28 = cope_nvi$cope_28, cope_46 = cope_nvi$cope_46
)

# Avoiding strategies (2)
avoiding_strategies_df <- data.frame(
  cope_6 = cope_nvi$cope_6, cope_27 = cope_nvi$cope_27, 
  cope_40 = cope_nvi$cope_40, cope_57 = cope_nvi$cope_57, 
  cope_9 = cope_nvi$cope_9, cope_24 = cope_nvi$cope_24, 
  cope_37 = cope_nvi$cope_37, cope_51 = cope_nvi$cope_51, 
  cope_2 = cope_nvi$cope_2, cope_16 = cope_nvi$cope_16, 
  cope_31 = cope_nvi$cope_31, cope_43 = cope_nvi$cope_43,  
  cope_12 = cope_nvi$cope_12, cope_26 = cope_nvi$cope_26, 
  cope_35 = cope_nvi$cope_35, cope_53 = cope_nvi$cope_53
)
  
# Positive attitude (3)
positive_attitude_df <- data.frame(
  cope_10 = cope_nvi$cope_10, cope_22 = cope_nvi$cope_22, 
  cope_41 = cope_nvi$cope_41, cope_49 = cope_nvi$cope_49, 
  cope_1 = cope_nvi$cope_1, cope_29 = cope_nvi$cope_29, 
  cope_38 = cope_nvi$cope_38, cope_59 = cope_nvi$cope_59, 
  cope_13 = cope_nvi$cope_13, cope_21 = cope_nvi$cope_21, 
  cope_44 = cope_nvi$cope_44, cope_54 = cope_nvi$cope_54
)
  
# Problem solving (4)
problem_solving_df <- data.frame(
  cope_5  = cope_nvi$cope_5, cope_25  = cope_nvi$cope_25, 
  cope_47 = cope_nvi$cope_47, cope_58 = cope_nvi$cope_58, 
  cope_19 = cope_nvi$cope_19, cope_32 = cope_nvi$cope_32, 
  cope_39 = cope_nvi$cope_39, cope_56 = cope_nvi$cope_56, 
  cope_15 = cope_nvi$cope_15, cope_33 = cope_nvi$cope_33, 
  cope_42 = cope_nvi$cope_42, cope_55 = cope_nvi$cope_55
)
  
# Transcendent orientation (5)
transcendent_orientation_df <- data.frame(
  cope_8 = abs(cope_nvi$cope_8  - 5), cope_20 = abs(cope_nvi$cope_20 - 5),
  cope_36 = abs(cope_nvi$cope_36 - 5), cope_50 = abs(cope_nvi$cope_50 - 5),
  cope_7 = cope_nvi$cope_7,  cope_18 = cope_nvi$cope_18, 
  cope_48 = cope_nvi$cope_48, cope_60 = cope_nvi$cope_60
)

cope_df <- cbind(
  social_support_df, avoiding_strategies_df, positive_attitude_df,
  problem_solving_df, transcendent_orientation_df
)


md <- mahalanobis(
  cope_df, 
  center = colMeans(cope_df), 
  cov = cov(cope_df)
)

alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(cope_df)))
names_outliers_MH <- which(md > cutoff)
excluded_mh <- names_outliers_MH
d_clean <- cope_df[-excluded_mh, ]
dim(d_clean)


mod_cope <- '
  SS =~ NA*cope_4 + cope_14 + cope_30 + cope_45 + cope_11 + cope_23 + cope_34 + cope_52 + cope_3 + cope_17 + cope_28 + cope_46 
  AS =~ NA*cope_6 + cope_27 + cope_40 + cope_57 + cope_9 + cope_24 + cope_37 + cope_51 + cope_2 + cope_16 + cope_31 + cope_43 + cope_12 + cope_26 + cope_35 + cope_53 
  PA =~ NA*cope_10 + cope_22 + cope_41 + cope_49 + cope_1 + cope_29 + cope_38 + cope_59 + cope_13 + cope_21 + cope_44 + cope_54 
  PS =~ NA*cope_5 + cope_25 + cope_47 + cope_58 + cope_19 + cope_32 + cope_39 + cope_56 + cope_15 + cope_33 + cope_42 + cope_55 
  TO =~ NA*cope_8 + cope_20 + cope_36 + cope_50 + cope_7 + cope_18 + cope_48 + cope_60 
'

fit_cope <- lavaan::cfa(
  mod_cope,
  data = cope_df,
  ordered = names(cope_df),
  std.lv = TRUE,
  orthogonal = FALSE
)

fitMeasures(
  fit_cope, 
  c("chisq", "df", "cfi", "tli", "rmsea", "srmr")
)

round(semTools::reliability(fit_cope, return.total = TRUE), 2)
# all data
#          SS   AS   PA   PS   TO total
# alpha  0.91 0.87 0.79 0.87 0.82  0.86
# omega  0.71 0.48 0.57 0.72 0.39  0.75
# omega2 0.71 0.48 0.57 0.72 0.39  0.75
# omega3 0.70 0.50 0.63 0.74 0.08  0.71
# avevar 0.49 0.43 0.28 0.39 0.69  0.44

# no outliers
#          SS   AS   PA   PS   TO total
# alpha  0.91 0.86 0.79 0.87 0.83  0.87
# omega  0.72 0.41 0.56 0.74 0.92  0.75
# omega2 0.72 0.41 0.56 0.74 0.92  0.75
# omega3 0.71 0.41 0.61 0.76 1.24  0.71
# avevar 0.50 0.44 0.29 0.40 0.72  0.45


# avoinding strategies Reversed:
avoiding_strategies_R <- 
  abs(cope_nvi$ii6 - 5) + abs(cope_nvi$ii27 - 5) + 
  abs(cope_nvi$ii40 - 5) + abs(cope_nvi$ii57 - 5) + 
  abs(cope_nvi$ii9 - 5) + abs(cope_nvi$ii24 - 5) + 
  abs(cope_nvi$ii37 - 5) + abs(cope_nvi$ii51 - 5) + 
  abs(cope_nvi$ii2 - 5) + abs(cope_nvi$ii16 - 5) + 
  abs(cope_nvi$ii31 - 5) + abs(cope_nvi$ii43 - 5) +  
  abs(cope_nvi$ii12 - 5) + abs(cope_nvi$ii26 - 5) + 
  abs(cope_nvi$ii43 - 5) + abs(cope_nvi$ii35 - 5) + 
  abs(cope_nvi$ii53 - 5)

# trascendent orientation Reversed
transcendent_orientation_R <- cope_nvi$ii8 + 
  cope_nvi$ii20 + cope_nvi$ii36 + cope_nvi$ii50 + 
  abs(cope_nvi$ii7 - 5) + abs(cope_nvi$ii18 - 5) + 
  abs(cope_nvi$ii48 - 5) + abs(cope_nvi$ii60 - 5)

# total score
cope_nvi <- social_support + avoiding_strategies + 
  positive_attitude + problem_orientation + 
  transcendent_orientation




# Self-Compassion Scale (SCS) ----

# The Self-Compassion Scale (SCS) is used to assess how people 
# treated themselves in difficult times (Neff 2003a). 
# It includes 26 items across six subscales: 
# - Self-Kindness, 
# - Self-Judgment, 
# - Common Humanity, 
# - Isolation, 
# - Mindfulness, 
# - Overidentification
# I punteggi delle sottoscale del scs sono ottenuti calcolando la 
# media delle risposte nei rispettivi item.
# Quindi calcolare la media dei punteggi medi delle sei sottoscale. 
# COME SI FA?
# In this approach any mean scale score ranging from 1 to 2.9 could 
# be considered low support; a score of 3 to 5 could be considered 
# moderate support; a score from 5.1  to 7 could be considered high 
# support. This approach would seem to have more validity, but if you 
# have very few respondents in any of the groups, it could be problematic.

# Let start with the coding of scs. First, we select the right columns:
scs <- df %>% 
  select(X194:X219)

# Then we change the column names to facilitate coding
item_names <- paste0("iiiii", 1:26)
names(scs) <- item_names

# Self-Kindness
self_kindness <- 
  scs$iiiii5 + scs$iiiii12 + scs$iiiii19 + scs$iiiii23 + 
  scs$iiiii26
# hist(self_kindness)

# Self-Judgment
self_judgment <- 
  abs(scs$iiiii1 - 6) + abs(scs$iiiii8 - 6) + 
  abs(scs$iiiii11 - 6) + abs(scs$iiiii16 - 6) + 
  abs(scs$iiiii21 - 6)
# hist(self_judgment)

# Common Humanity
common_humanity <- 
  scs$iiiii3 + scs$iiiii7 + scs$iiiii10 + scs$iiiii15
# hist(common_humanity)

# Isolation
isolation <- 
  abs(scs$iiiii4 - 6) + abs(scs$iiiii13 - 6) + 
  abs(scs$iiiii18 - 6) + abs(scs$iiiii25 - 6)
# hist(isolation)

# Mindfulness
mindfulness <- 
  scs$iiiii9 + scs$iiiii14 + scs$iiiii17 + scs$iiiii22
# hist(mindfulness)

# Overidentification
over_identification <- 
  abs(scs$iiiii2 - 6) + abs(scs$iiiii6 - 6) + 
  abs(scs$iiiii20 - 6) + abs(scs$iiiii24 - 6)
# hist(over_identification)

neg_self_compassion <- self_judgment + isolation + 
  over_identification
pos_self_compassion <- self_kindness + common_humanity + 
  mindfulness



# Data wrangling ----

df$gender <- fct_recode(
  df$gender, 
  "female" = "Femmina", 
  "male" = "Maschio"
)

df$employment <- fct_recode(
  df$employment, 
  "Dipendente" = "Operaio",
  "Dipendente" = "Impiegato",
  "Libero professionista" = "Imprenditore",
  "Dipendente" = "Dipendente CRI",
  "Dipendente" = "Infermiere",
  "Dipendente" = "Militare",
  "Dipendente" = "Vigile del Fuoco",
  "Pensionato" = "pensionato",
  "Dipendente" = "Funzionario Pubblico",
  "Dipendente" = "Forze dell'Ordine",
  "Dipendente" = "Ricercatore",
  "Libero professionista" = "Commerciante", 
  "Dipendente" = "Educatore",
  "Dipendente" = "Soccorritore",
  "Dipendente" = "Polizia locale",
  "Dipendente" = "Polizia Locale",
  "Studente" = "Dottoranda",
  "Dipendente" = "Medico",
  "Pensionato" = "Pensione",
  "Dipendente" = "Insegnante"
)

df$is_rescue_worker <- fct_recode(
  df$is_rescue_worker,
  "Yes" = "Sì",
  "Yes" = "Si"
)

df <- df %>% 
  dplyr::rename(
    years_experience = `Anni di esperienza TSSA/PTSI (inserire il numero)`
  )

df$last_training <- factor(
  df$last_training,
  levels = c(
    "Più di 1 anno fa", "Meno di 1 anno fa", "Più di 6 mesi fa", "Meno di 6 mesi fa", 
    "Più di 2 mesi fa", "Meno di 2 mesi fa" 
  )
)
unique(df$last_training)

df$rate_of_activity <- factor(
  df$rate_of_activity,
  levels = c(
    "1 turno al mese", "Meno di 1 turno al mese", "1 turno ogni due settimane", 
    "1 turno a settimana", "Più di 1 turno a settimana"
  )
)
unique(df$rate_of_activity)

unique(df$job_qualification)

df$is_job_qualification_invariant <- fct_recode(
  df$is_job_qualification_invariant, 
  "Yes" = "Sì",
  "Yes" = "Si"
)
table(df$is_job_qualification_invariant)

df$is_team_invariant <- fct_recode(
  df$is_team_invariant, 
  "Yes" = "Sì",
  "Yes" = "Si"
)
table(df$is_team_invariant)




# final data frame:
df1 <- data.frame(
  where = factor(df$where),
  id = factor(df$id),
  gender = factor(df$gender),
  age = df$age,
  education = factor(df$education),
  employment = factor(df$employment),
  is_rescue_worker = factor(df$is_rescue_worker),
  red_cross_commeetee_location = 
    factor(df$red_cross_commeetee_location),
  rescue_worker_qualification = 
    factor(df$rescue_worker_qualification),
  years_experience = factor(df$years_experience),
  last_training = factor(df$last_training),
  rate_of_activity = factor(df$rate_of_activity),
  job_qualification = factor(df$job_qualification),
  is_job_qualification_invariant = 
    factor(df$is_job_qualification_invariant),
  is_team_invariant = factor(df$is_team_invariant),
  neuroticism,
  negative_affect,
  i1_na = abs(neo_ffi_60$i1 - 4),
  i2_na = neo_ffi_60$i11,
  i3_na = abs(neo_ffi_60$i16 - 4),
  i4_na = abs(neo_ffi_60$i31 - 4),
  i5_na = abs(neo_ffi_60$i46 -4),
  i1_sr = neo_ffi_60$i6,
  i2_sr = neo_ffi_60$i21,
  i3_sr = neo_ffi_60$i26,
  i4_sr = neo_ffi_60$i36,
  i5_sr = neo_ffi_60$i41,
  i6_sr = neo_ffi_60$i51,
  i7_sr = neo_ffi_60$i56,
  self_reproach,
  extraversion,
  openness,
  agreeableness,
  nonantagonistic_orientation, 
  prosocial_orientation,
  conscientiousness,
  cope_nvi,
  social_support,
  avoiding_strategies,
  positive_attitude,
  problem_orientation,
  transcendent_orientation,
  avoiding_strategies_R, # reversed
  transcendent_orientation_R, # reversed
  ptg, 
  life_appreciation,
  new_possibilities,
  personal_strength,
  spirituality_changes,
  interpersonal_relationships,
  ies = ies_r_score,
  avoiding,
  intrusivity,
  iperarousal,
  # self-compassion subscales:
  self_kindness,
  self_judgment, # reversed
  common_humanity, 
  isolation, # reversed
  mindfulness,
  over_identification, # reversed
  neg_self_compassion,
  pos_self_compassion,
  # MSPSS
  family, 
  friends, 
  significant_other
)

# select only respondents who are rescue workers:
thedat <- df1 %>% 
  dplyr::filter(is_rescue_worker == "Yes")

# thedat <- thedat2[complete.cases(thedat2), ]

# Write data files to the "processed" directory:
saveRDS(thedat, file = here("data", "processed", "rescue_workers.Rds"))

write.table(
  thedat, 
  file = here("data", "processed", "rescue_workers.csv"),
  sep = ";", 
  row.names = FALSE
)






# e  n  d  ---




