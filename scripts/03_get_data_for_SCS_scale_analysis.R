## ------------------------------------------------------------------
## 03_get_data_for_SCS_scale_analysis.R
## 
## Project: self-compassion and rescue workers
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

library("here")
suppressPackageStartupMessages(library("tidyverse")) 
suppressPackageStartupMessages(library("tidyr"))
suppressPackageStartupMessages(library("mice"))
suppressPackageStartupMessages(library("corrplot"))
suppressPackageStartupMessages(library("readxl"))
suppressPackageStartupMessages(library("mvoutlier"))
suppressPackageStartupMessages(library("janitor"))

options(max.print=99999999)

source(here("libraries", "self_compassion_fnc.R"))


# Read data ----

# temp <- readRDS(here("data", "processed", "rescue_workers_cleaned_data.rds"))

df_toscana <- read_excel(here("data", "raw", "toscana_1.xlsx"))
df_toscana$where <- "toscana"
df_lombardia <- read_excel(here("data", "raw", "lombardia_1.xlsx"))
df_lombardia$where <- "lombardia"


# Merge the two data.frames 
df <- rbind(df_toscana, df_lombardia)

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
# df <- df[-c(680), ]

if (0) df <- df[df$id %in% good_ids, ]


# If I select only the subjects used in the other analyses, the models 
# do not converge.  So I use here all the subjects.
# subjects <- readRDS(here("data", "processed", "participants.Rds"))
# df <- df[df$id %in% subjects$id, ]

# # Campione di non soccorritori
# df_nonsoccorritori <- read_excel(here("data", "raw", "non_soccorritori.xlsx"))
# df_nonsoccorritori$where <- "non_soccorritori"
# 
# scs_non_socc <- df_nonsoccorritori[, 176:(176+26-1)]
# 
# item_names <- paste0("i", 1:26)
# names(scs_non_socc) <- item_names


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


# Use only subjects of the following analyses.
# df <- df[df$id %in% subjects$id, ]



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
item_names <- paste0("i", 1:26)
names(scs) <- item_names

# scs <- rbind(scs, scs_non_socc)

# Self-Kindness
self_kindness <- scs$i5 + scs$i12 + scs$i19 + scs$i23 + scs$i26
u5  <- scs$i5 
u12 <- scs$i12 
u19 <- scs$i19 
u23 <- scs$i23 
u26 <- scs$i26

# Self-Judgment
self_judgment <- 
  abs(scs$i1 - 6) + abs(scs$i8 - 6) + abs(scs$i11 - 6) + abs(scs$i16 - 6) + abs(scs$i21 - 6)
u1  <- abs(scs$i1 - 6) 
u8  <- abs(scs$i8 - 6) 
u11 <- abs(scs$i11 - 6) 
u16 <- abs(scs$i16 - 6) 
u21 <- abs(scs$i21 - 6)

# Common Humanity
common_humanity <- scs$i3 + scs$i7 + scs$i10 + scs$i15
u3  <- scs$i3 
u7  <- scs$i7 
u10 <- scs$i10 
u15 <- scs$i15

# Isolation
isolation <- abs(scs$i4 - 6) + abs(scs$i13 - 6) + abs(scs$i18 - 6) + abs(scs$i25 - 6)
u4  <- abs(scs$i4 - 6) 
u13 <- abs(scs$i13 - 6) 
u18 <- abs(scs$i18 - 6) 
u25 <- abs(scs$i25 - 6)

# Mindfulness
mindfulness <- scs$i9 + scs$i14 + scs$i17 + scs$i22
u9  <- scs$i9 
u14 <- scs$i14 
u17 <- scs$i17 
u22 <- scs$i22

# Overidentification
over_identification <- abs(scs$i2 - 6) + abs(scs$i6 - 6) + abs(scs$i20 - 6) + abs(scs$i24 - 6)
u2  <- abs(scs$i2 - 6) 
u6  <- abs(scs$i6 - 6) 
u20 <- abs(scs$i20 - 6) 
u24 <- abs(scs$i24 - 6)

neg_self_compassion <- self_judgment + isolation + over_identification
pos_self_compassion <- self_kindness + common_humanity + mindfulness


mydata <- data.frame(
  u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15,
  u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26
)

# mydata$gender <- df$gender

nrow(mydata)

items <- c(
  "u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", 
  "u11", "u12", "u13", "u14", "u15", "u16", "u17", "u18", "u19", 
  "u20", "u21", "u22", "u23", "u24", "u25", "u26"
)


mydata <- mydata[complete.cases(mydata), ]

# bad_ids <- assumptions(mydata)$Mah_significant[, 1]
# d <- mydata[-bad_ids, ]

md <- mahalanobis(
  mydata,
  center = colMeans(mydata),
  cov = cov(mydata)
)

alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(mydata)))
names_outliers_MH <- which(md > cutoff)
excluded_mh <- names_outliers_MH
data_clean_mh <- mydata[-excluded_mh, ]
dim(mydata[excluded_mh, ])
d <- data_clean_mh

foo <- d %>% 
  distinct()

foo %>% 
  get_dupes


saveRDS(d, here("data", "processed", "SCS_26item_data.Rds"))


