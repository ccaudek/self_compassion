#--------------------------------------------------------------------
# Self-compassion project
#
# The Five-Factor Model (FFM) is a well-established paradigm for the 
# conceptualisation of human personality, described in terms of 
# Neuroticism, Extraversion, Openness to Experience, Agreeableness and 
# Conscientiousness. The NEO Personality Inventories (NEO-PI) (240-item 
# questionnaires) are among the most widely used instruments to assess 
# the Big Five personality traits. The short form of the NEO-PI, the 
# NEO Five-Factor Inventory (NEO-FFI), was designed to capture the 5 
# main factors in a more economical way. To maximise convergent and 
# discriminant validity of the NEO-FFI, its 60 items were selected from 
# the NEO-PI based on examinations of factor structure and internal 
# consistency.
# 
# Question: to examine the psychometric properties of the NEO-FFI 60 
# in the present sample.
# 
# 40_neoffi_factor_an.R
# Corrado Caudek
# Last modified on: "Fri Oct 25 14:23:00 2019"
#--------------------------------------------------------------------


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
suppressPackageStartupMessages(library("readxl"))

options(max.print=99999999)


source(here("libraries", "self_compassion_fnc.R"))



# Read data ---------------------------------------------------------------

df_toscana <- read_excel(here("data", "raw", "toscana_1.xlsx"))
df_toscana$where <- "toscana"
df_lombardia <- read_excel(here("data", "raw", "lombardia_1.xlsx"))
df_lombardia$where <- "lombardia"

# Merge the two data.frames 
df <- rbind(df_toscana, df_lombardia)


# Combine items ----

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

# Remove 1 extreme observation.
df <- df[-c(680), ]

# The data.frame contains the following scales:

# - NEO_FFI_60: 60 items, from X17 to X76
# - COPE_NVI: 60 items, from X77 to X136
# - PTGI: 29 items, X137 to X144, X145 to X165
# - IES_R: 28 items, items X166 to X171, X172 to X193
# - SCS: 26 items, items X194 to 219
# - MSPSS: 12 items, items X220 to 231

gender <- df %>% 
  select(gender)

education <- df %>% 
  select(education)

employment <- df %>% 
  select(employment)

is_rescue_worker <- df %>% 
  select(is_rescue_worker)

red_cross_commeetee_location <- df %>% 
  select(red_cross_commeetee_location)

rescue_worker_qualification <- df %>% 
  select(rescue_worker_qualification)

last_training <- df %>% 
  select(last_training)

rate_of_activity <- df %>% 
  select(rate_of_activity)

job_qualification <- df %>% 
  select(job_qualification)

is_job_qualification_invariant <- df %>% 
  select(is_job_qualification_invariant)

is_team_invariant <- df %>% 
  select(is_team_invariant)

age <- df %>% 
  select(age)

glimpse(df)


# NEO-FIve Factor Inventory (NEO-FFI) -------------------------------------


# NEO-FFI measures five core personality traits: 
# neuroticism, openness, agreeableness, and conscientiousness.

# First, let's select the right columns:
neo_ffi_60 <- df %>% 
  select(X17:X76)

# Then we change the column names to facilitate coding
item_names <- paste0("i", 1:60)
names(neo_ffi_60) <- item_names

# Some items are reversed. In that cases, we can compute the right value 
#by using the following formula: abs(x - max).

# Neuroticism

# The first factor of neo_ffi_60 is called *neuroticism* and is composed by two subscales. 
# The scores on such scales are the following:

# Negative affect:
negative_affect <- 
  abs(neo_ffi_60$i1 - 4) + neo_ffi_60$i11 + abs(neo_ffi_60$i16 - 4) + 
  abs(neo_ffi_60$i31 - 4) + abs(neo_ffi_60$i46 -4)

# Self reproach:
self_reproach <-
  neo_ffi_60$i6 + neo_ffi_60$i21 + neo_ffi_60$i26 + neo_ffi_60$i36 + 
  neo_ffi_60$i41 + neo_ffi_60$i51 + neo_ffi_60$i56

# neuroticism scores
neuroticism <- negative_affect + self_reproach

# negative_affect
i1_na <- abs(neo_ffi_60$i1 - 4) 
i2_na <- neo_ffi_60$i11 
i3_na <- abs(neo_ffi_60$i16 - 4)  
i4_na <- abs(neo_ffi_60$i31 - 4) 
i5_na <- abs(neo_ffi_60$i46 - 4)

# self_reproach
i1_sr <- neo_ffi_60$i6 
i2_sr <- neo_ffi_60$i21 
i3_sr <- neo_ffi_60$i26 
i4_sr <- neo_ffi_60$i36  
i5_sr <- neo_ffi_60$i41 
i6_sr <- neo_ffi_60$i51 
i7_sr <- neo_ffi_60$i56

# Just to take a look to the data, an histogram of neuroticism is
# hist(neuroticism)

# Extroversion
# The second factor of neo_ffi_60 is called *extraversion* and is composed by 
# three subscales. The scores on such scales are the following:
positive_affect <- 
  neo_ffi_60$i7 + abs(neo_ffi_60$i12 - 4) + neo_ffi_60$i37 + abs(neo_ffi_60$i42 -4)

sociability <- 
  neo_ffi_60$i2 + neo_ffi_60$i17 + abs(neo_ffi_60$i27 - 4) + abs(neo_ffi_60$i57 - 4)

activity <- 
  neo_ffi_60$i22 + neo_ffi_60$i32 + neo_ffi_60$i47 + neo_ffi_60$i52

extraversion <- 
  positive_affect + sociability + activity

# positive affect
i1_pa <- neo_ffi_60$i7 
i2_pa <- abs(neo_ffi_60$i12 - 4) 
i3_pa <- neo_ffi_60$i37 
i4_pa <- abs(neo_ffi_60$i42 - 4)

# sociability
i1_so <- neo_ffi_60$i2 
i2_so <- neo_ffi_60$i17 
i3_so <- abs(neo_ffi_60$i27 - 4) 
i4_so <- abs(neo_ffi_60$i57 - 4)

# activity
i1_ac <- neo_ffi_60$i22 
i2_ac <- neo_ffi_60$i32 
i3_ac <- neo_ffi_60$i47 
i4_ac <- neo_ffi_60$i52


# Just to take a look to the data, an histogram of estraversion is
# hist(extraversion)

# Openness
# The third factor of neo_ffi_60 is called *openness* and is composed by three subscales. 
# The scores on such scales are the following.

# Aesthetic interests:
aesthetic_interests <- 
  neo_ffi_60$i13 + abs(neo_ffi_60$i23 - 4) + neo_ffi_60$i43

# Intellectual interests:
intellectual_interests <- 
  abs(neo_ffi_60$i48 - 4) + neo_ffi_60$i53 + neo_ffi_60$i58

# Unconventionality:
unconventionality <- 
  abs(neo_ffi_60$i3 - 4) + abs(neo_ffi_60$i8 - 4) + abs(neo_ffi_60$i18 - 4) + abs(neo_ffi_60$i38 - 4) +  
  neo_ffi_60$i28 + abs(neo_ffi_60$i33 - 4)

# Openness scores
openness <- aesthetic_interests + intellectual_interests + unconventionality
# hist(openness)

# aesthetic_interests 
i1_ai <- neo_ffi_60$i13
i2_ai <- abs(neo_ffi_60$i23 - 4) 
i3_ai <- neo_ffi_60$i43

# intellectual_interests
i1_ii <- abs(neo_ffi_60$i48 - 4) 
i2_ii <- neo_ffi_60$i53 
i3_ii <- neo_ffi_60$i58

# unconventionality  
i1_un <- abs(neo_ffi_60$i3 - 4) 
i2_un <- abs(neo_ffi_60$i8 - 4) 
i3_un <- abs(neo_ffi_60$i18 - 4) 
i4_un <- abs(neo_ffi_60$i38 - 4)
i5_un <- neo_ffi_60$i28
i6_un <- abs(neo_ffi_60$i33 - 4)

# Agreeableness
# The fourth factor of neo_ffi_60 is called *agreeableness* and is composed by two subscales. 
# Nonantagonistic orientation:
nonantagonistic_orientation <- 
  abs(neo_ffi_60$i9 - 4) + abs(neo_ffi_60$i14 - 4) + neo_ffi_60$i19 +  
  abs(neo_ffi_60$i24 - 4) + abs(neo_ffi_60$i29 - 4) + abs(neo_ffi_60$i44 - 4) + 
  abs(neo_ffi_60$i54 - 4) + abs(neo_ffi_60$i59 - 4)

# Prosocial orientation:
prosocial_orientation <- 
  neo_ffi_60$i4 + neo_ffi_60$i34 + abs(neo_ffi_60$i39 - 4) + neo_ffi_60$i49

# agreeableness scores
agreeableness <- nonantagonistic_orientation + prosocial_orientation
# hist(agreeableness)

# nonantagonistic_orientation 
i1_no <- abs(neo_ffi_60$i9 - 4) 
i2_no <- abs(neo_ffi_60$i14 - 4) 
i3_no <- neo_ffi_60$i19 
i4_no <- abs(neo_ffi_60$i24 - 4) 
i5_no <- abs(neo_ffi_60$i29 - 4) 
i6_no <- abs(neo_ffi_60$i44 - 4) 
i7_no <- abs(neo_ffi_60$i54 - 4) 
i8_no <- abs(neo_ffi_60$i59 - 4)

# prosocial_orientation 
i1_po <- neo_ffi_60$i4 
i2_po <- neo_ffi_60$i34 
i3_po <- abs(neo_ffi_60$i39 - 4) 
i4_po <- neo_ffi_60$i49


# Conscientiousness
# The fifth factor of neo_ffi_60 is called *conscientiousness* and is composed 
# by three subscales. 
# The scores on such scales are the following.

# Orderliness:
orderliness <- 
  neo_ffi_60$i5 + neo_ffi_60$i10 + abs(neo_ffi_60$i15 - 4) + 
  abs(neo_ffi_60$i30 - 4) + abs(neo_ffi_60$i55 - 4)

# Goal striving:
goal_striving <- neo_ffi_60$i25 + neo_ffi_60$i35 + neo_ffi_60$i60

# Dependability:
dependability <- 
  neo_ffi_60$i20 + neo_ffi_60$i40 + abs(neo_ffi_60$i45 - 4) + neo_ffi_60$i50

# conscientiousness scores
conscientiousness <- orderliness + goal_striving + dependability
# hist(conscientiousness)


# orderliness  
i1_or <- neo_ffi_60$i5 
i2_or <- neo_ffi_60$i10 
i3_or <- abs(neo_ffi_60$i15 - 4)  
i4_or <- abs(neo_ffi_60$i30 - 4) 
i5_or <- abs(neo_ffi_60$i55 - 4)

# goal_striving 
i1_gs <- neo_ffi_60$i25 
i2_gs <- neo_ffi_60$i35 
i3_gs <- neo_ffi_60$i60

# dependability 
i1_de <- neo_ffi_60$i20 
i2_de <- neo_ffi_60$i40 
i3_de <- abs(neo_ffi_60$i45 - 4) 
i4_de <- neo_ffi_60$i50


df <- data.frame(
  i1_na, i2_na, i3_na, i4_na, i5_na, 
  i1_sr, i2_sr, i3_sr, i4_sr, i5_sr, i6_sr, i7_sr,
  i1_pa, i2_pa, i3_pa, i4_pa, 
  i1_so, i2_so, i3_so, i4_so,
  i1_ac, i2_ac, i3_ac, i4_ac, 
  i1_ai, i2_ai, i3_ai,
  i1_ii, i2_ii, i3_ii,
  i1_un, i2_un, i3_un, i4_un, i5_un, i6_un,
  i1_no, i2_no, i3_no, i4_no, i5_no, i6_no, i7_no, i8_no, 
  i1_po, i2_po, i3_po, i4_po,
  i1_or, i2_or, i3_or, i4_or, i5_or, 
  i1_gs, i2_gs, i3_gs, 
  i1_de, i2_de, i3_de, i4_de,
  negative_affect, self_reproach,
  positive_affect, sociability, activity,
  aesthetic_interests, intellectual_interests, unconventionality,
  nonantagonistic_orientation, prosocial_orientation,
  orderliness, goal_striving, dependability
)


# Confirmatory factor analysis --------------------------------------------


model1 <- '
  # measurement model
  
  # neuroticism
  neg_af  =~ i1_na + i2_na + i3_na + i4_na + i5_na 
  self_rp =~ i1_sr + i2_sr + i3_sr + i4_sr + i5_sr + i6_sr + i7_sr 
  
  # extraversion
  pos_af =~ i1_pa + i2_pa + i3_pa + i4_pa 
  sociab =~ i1_so + i2_so + i3_so + i4_so
  activ  =~ i1_ac + i2_ac + i3_ac + i4_ac 
  
  # openness
  aest_in =~ i1_ai + i2_ai + i3_ai
  inte_in =~ i1_ii + i2_ii + i3_ii 
  unconv  =~ i1_un + i2_un + i3_un + i4_un 
  
  # agreeableness
  nonant_or =~ i1_no + i2_no + i3_no + i4_no + i5_no + i6_no + i7_no + i8_no 
  prosoc_or =~ i1_po + i2_po + i3_po + i4_po 
  
  # conscientiousness
  order   =~ i1_or + i2_or + i3_or + i4_or + i5_or 
  goal_st =~ i1_gs + i2_gs + i3_gs 
  depend  =~ i1_de + i2_de + i3_de + i4_de 
  
  neuroticism  =~ neg_af + self_rp
  extraversion =~ pos_af + sociab + activ
  openness     =~ aest_in + inte_in + unconv
  agreeabl     =~ nonant_or + prosoc_or
  conscient    =~ order + goal_st + depend
'


model2 <- '
  # measurement model
  
  n =~ i1_na + i2_na + i3_na + i4_na + i5_na + i1_sr + i2_sr + i3_sr + i4_sr + i5_sr + i6_sr + i7_sr 
  e =~ i1_pa + i2_pa + i3_pa + i4_pa + i1_so + i2_so + i3_so + i4_so + i1_ac + i2_ac + i3_ac + i4_ac 
  o =~ i1_ai + i2_ai + i3_ai + i1_ii + i2_ii + i3_ii + i1_un + i2_un + i3_un + i4_un + i5_un + i6_un
  a =~ i1_no + i2_no + i3_no + i4_no + i5_no + i6_no + i7_no + i8_no + i1_po + i2_po + i3_po + i4_po 
  c =~ i1_or + i2_or + i3_or + i4_or + i5_or + i1_gs + i2_gs + i3_gs + i1_de + i2_de + i3_de + i4_de 
  '


model3 <- '
  # measurement model
  n =~ negative_affect + self_reproach 
  e =~ positive_affect + sociability + activity 
  o =~ aesthetic_interests + intellectual_interests + unconventionality 
  a =~ nonantagonistic_orientation + prosocial_orientation 
  c =~ orderliness + goal_striving + dependability 
  '




fit2 <- sem(
  model2, 
  data = df,
  std.lv = TRUE
)

summary(fit2, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit2)



fit3 <- sem(
  model3, 
  data = df, 
  std.lv = TRUE
)

summary(fit3, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
fitMeasures(fit3)

modificationindices(
  fit3,
  minimum.value = 20
) # only print MIs > 20


# Item response theory and validity of the NEO-FFI in adolescents
# Ruth Spence Matthew Owens Ian Goodyer
# Personality and Individual Differences
# Volume 53, Issue 6, October 2012, Pages 801-807


# Neuroticism ----

neuro1 <- '
  neuroticism  =~ NA*i1_na + i2_na + i3_na + i4_na + i5_na + i1_sr + i2_sr + i3_sr + i4_sr + i5_sr + i6_sr + i7_sr 
  neuroticism ~~ 1*neuroticism
'

# bifactor model for Neuroticism, with two factors plus a general factor
neuro2 <- '

  neg_af  =~ NA*i1_na + i2_na + i3_na + i4_na + i5_na 
  self_rp =~ NA*i1_sr + i2_sr + i3_sr + i4_sr + i5_sr + i6_sr + i7_sr   
  g       =~ NA*i1_na + i2_na + i3_na + i4_na + i5_na + i1_sr + i2_sr + i3_sr + i4_sr + i5_sr + i6_sr + i7_sr
  neuro   =~ neg_af + self_rp
  
  neg_af  ~~ 1*neg_af
  self_rp ~~ 1*self_rp
  g       ~~ 1*g
  neuro   ~~ 1*neuro
  
  neg_af ~~ 0*self_rp
  g      ~~ 0*self_rp
  g      ~~ 0*neg_af
  
  neuro ~~ 0*neg_af
  neuro ~~ 0*self_rp
  neuro ~~ 0*g
  
'

fit_neuro <- sem(
  neuro2, 
  data = df
)

summary(fit_neuro, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


# Extraversion ----

# bifactor model for Extraversion, with three factors plus a general factor

extrav <- '
  
  pos_af =~ NA*i1_pa + i2_pa + i3_pa + i4_pa 
  sociab =~ NA*i1_so + i2_so + i3_so + i4_so
  activ  =~ NA*i1_ac + i2_ac + i3_ac + i4_ac 
  extrav =~ pos_af + sociab + activ
  g      =~ NA*i1_pa + i2_pa + i3_pa + i4_pa + i1_so + i2_so + i3_so + i4_so + i1_ac + i2_ac + i3_ac + i4_ac
  
  pos_af ~~ 1*pos_af
  sociab ~~ 1*sociab
  activ  ~~ 1*activ
  g      ~~ 1*g
  extrav ~~ 1*extrav
  
  pos_af ~~ 0*sociab
  pos_af ~~ 0*activ
  pos_af ~~ 0*g
  pos_af ~~ 0*extrav
  sociab ~~ 0*activ
  sociab ~~ 0*g
  sociab ~~ 0*extrav
  activ  ~~ 0*g
  activ  ~~ 0*extrav
  g      ~~ 0*extrav
'


fit_extrav <- sem(
  extrav, 
  data = df
)

summary(fit_extrav, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)



# Openness ----

# Problems with fit
openness <- '
  
  aest_in =~ NA*i1_ai + i2_ai + i3_ai
  inte_in =~ NA*i1_ii + i2_ii + i3_ii 
  unconv  =~ NA*i1_un + i2_un + i3_un + i4_un 
  open    =~ aest_in + inte_in + unconv
  g       =~ i1_ai + i2_ai + i3_ai + i1_ii + i2_ii + i3_ii + i1_un + i2_un + i3_un + i4_un 
  
  aest_in ~~ 1*aest_in
  inte_in ~~ 1*inte_in
  unconv  ~~ 1*unconv
  open    ~~ 1*open
  g       ~~ 1*g
  
  aest_in ~~ 0*inte_in
  aest_in ~~ 0*unconv
  aest_in ~~ 0*open
  aest_in ~~ 0*g
  inte_in ~~ 0*unconv
  inte_in ~~ 0*open
  inte_in ~~ 0*g
  unconv  ~~ 0*open
  unconv  ~~ 0*g
  open    ~~ 0*g
  
'


fit_open <- sem(
  openness, 
  data = df
)

summary(fit_open, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


# Agreeableness ----

agreeableness <- '

  nonant_or     =~ NA*i1_no + i2_no + i3_no + i4_no + i5_no + i6_no + i7_no + i8_no 
  prosoc_or     =~ NA*i1_po + i2_po + i3_po + i4_po 
  agreeableness =~ nonant_or + prosoc_or
  g             =~ NA*i1_no + i2_no + i3_no + i4_no + i5_no + i6_no + i7_no + i8_no + i1_po + i2_po + i3_po + i4_po
  
  nonant_or     ~~ 1*nonant_or
  prosoc_or     ~~ 1*prosoc_or
  agreeableness ~~ 1*agreeableness
  g             ~~ 1*g
  
  nonant_or     ~~ 0*prosoc_or
  nonant_or     ~~ 0*agreeableness
  nonant_or     ~~ 0*g
  prosoc_or     ~~ 0*agreeableness
  prosoc_or     ~~ 0*g
  agreeableness ~~ 0*g
 
'


fit_agree <- sem(
  agreeableness, 
  data = df
)

summary(fit_agree, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)



# Conscientiousness

conscientiousness <- '

  order   =~ NA*i1_or + i2_or + i3_or + i4_or + i5_or 
  goal_st =~ NA*i1_gs + i2_gs + i3_gs 
  depend  =~ NA*i1_de + i2_de + i3_de + i4_de 
  consc   =~ order + goal_st + depend
  g       =~ NA*i1_or + i2_or + i3_or + i4_or + i5_or + i1_gs + i2_gs + i3_gs + i1_de + i2_de + i3_de + i4_de
  
  order   ~~ 1*order
  goal_st ~~ 1*goal_st
  depend  ~~ 1*depend
  consc   ~~ 1*consc
  g       ~~ 1*g
  
  order   ~~ 0*goal_st
  order   ~~ 0*depend
  order   ~~ 0*consc
  order   ~~ 0*g
  goal_st ~~ 0*depend
  goal_st ~~ 0*consc
  goal_st ~~ 0*g
  depend  ~~ 0*consc
  depend  ~~ 0*g
  consc  ~~ 0*g
  
'


fit_consc <- sem(
  conscientiousness, 
  data = df
)

summary(fit_consc, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)




