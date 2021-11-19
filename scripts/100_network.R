# Self-compassion Scale
# 
# Question: Network analysis of the Self Compassion Scale.
# 
# 100_network.R
# Corrado Caudek
# Last modified on: "Sat Dec 19 10:05:04 2020"


## Prelims ----

# Load packages 

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
suppressPackageStartupMessages(library("mvoutlier"))

library("psych")
library("tidyr")
library("bootnet")
library("igraph")
# Please note: The EGA library is not the same as the ega library in CRAN and need to be installed
# library("devtools")
# devtools::install_github('hfgolino/EGA')
library("EGAnet")

library("qgraph")

options(max.print=99999999)

source(here("libraries", "self_compassion_fnc.R"))

# Read data 
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
df <- df[-c(680), ]

# Only participants used in the SEM models' comparisons.
if (0) {
  participants <- readRDS(here("data", "processed", "participants.Rds"))
  df <- df[df$id %in% participants$id, ]
}

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

# Self-Compassion Scale (SCS) 

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

mydata <- mydata[complete.cases(mydata), ] 
nrow(mydata)

items <- c(
  "u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", "u11", "u12", "u13", "u14", "u15",
  "u16", "u17", "u18", "u19", "u20", "u21", "u22", "u23", "u24", "u25", "u26"
)

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


# Network analysis ----

corMat <- cor_auto(d)

Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring")

Graph_pcor <- qgraph(
  corMat, graph = "pcor", layout = "spring", threshold = "bonferroni",
                     sampleSize = nrow(d), alpha = 0.05)


Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
                      sampleSize = nrow(d))

centRes <- centrality(Graph_lasso)
centRes$OutDegree # Or InDegree, it's the same in unweighted networks
centRes$Closeness
centRes$Betweenness

centralityPlot(Graph_lasso)

Groups <- c(rep("sj",4),rep("is",5),rep("Extraversion",5),rep("Neuroticism",5),rep("Opennness",5))


Groups <- c("sj", "oi", "cu", "is", "sk", "oi", "cu", "sj",
            "mi", "cu", "sj", "sk", "is", "mi", "cu", "sj",
            "mi", "is", "sk", "oi", "sj", "mi", "sk", "oi",
            "is", "sk")
qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(d), groups = Groups, palette = "colorblind")


ega.rand <- EGA(data = d, model = "glasso", palette = "colorblind")

set.seed(0)
boot.bell <- bootEGA(data = d, model = "glasso",
                     iter = 1000, type = "resampling",
                     plot.typicalStructure = FALSE)

boot.bell$summary.table


tab1 <- boot.bell$summary.table

knitr::kable(tab1, booktabs = TRUE, caption = "bootEGA Summary Table") %>%
  kable_styling(latex_options = c("HOLD_position"),
                position = "center")


set.seed(9324)
ids <- sample(nrow(d), 500)
random.sample <- d[ids,]

ega.rand <- EGA(data = random.sample, model = "glasso")
boot.rand <- bootEGA(data = random.sample, iter = 1000,
                     model = "glasso", type = "resampling")

boot.rand$summary.table
boot.rand$likelihood

res <- itemStability(boot.rand, orig.wc = ega.rand$wc)
res$plot.itemStability + 
  ggplot2::scale_color_manual(values = rainbow(max(res$uniq.num)))

res <- dimStability(boot.bell, orig.wc = ega.rand$wc, item.stability = TRUE)
res$items$plot.itemStability + 
  ggplot2::scale_color_manual(values = rainbow(max(res$items$uniq.num)))





mod <- '

  sj =~ NA*u1 + u8 + u11 + u16 + u21
  is =~ NA*u4 + u13 + u18 + u25
  oi =~ NA*u2 + u6 + u20 + u24
  
  sk =~ NA*u5 + u12 + u19 + u23 + u26
  cu =~ NA*u3 + u7 + u10 + u15
  mi =~ NA*u9 + u14 + u17 + u22
  
  neg =~ NA*sj + is + oi
  pos =~ NA*sk + cu + mi
  
  g   =~ NA*u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10 + 
         u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + 
         u20 + u21 + u22 + u23 + u24 + u25 + u26
  
  sj  ~~ 1* sj
  is  ~~ 1* is
  oi  ~~ 1* oi
  sk  ~~ 1* sk
  cu  ~~ 1* cu
  mi  ~~ 1* mi

  neg ~~ 1* neg
  pos ~~ 1* pos

  g   ~~ 1* g
  
  pos ~~ 0*neg
  g   ~~ 0* neg
  g   ~~ 0* pos
  
'


fit <- lavaan::cfa(
  mod,
  data = mydata,
  # data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)


summary(
  fit, 
  fit.measures = TRUE, 
  standardized = TRUE
)

semTools::reliability(fit, return.total = TRUE)
semTools::reliabilityL2(fit, "pos")
semTools::reliabilityL2(fit, "neg")




mod_2bi <- '

  sj =~ NA*u1 + u8 + u11 + u16 + u21
  is =~ NA*u4 + u13 + u18 + u25
  oi =~ NA*u2 + u6 + u20 + u24
  
  sk =~ NA*u5 + u12 + u19 + u23 + u26
  cu =~ NA*u3 + u7 + u10 + u15
  mi =~ NA*u9 + u14 + u17 + u22
  
  gn   =~ NA*u1 + u8 + u11 + u16 + u21 +
          u4 + u13 + u18 + u25 +
          u2 + u6 + u20 + u24
  
  gp   =~ NA*u5 + u12 + u19 + u23 + u26 +
          u3 + u7 + u10 + u15 +
          u9 + u14 + u17 + u22
  
  sj  ~~ 1* sj
  is  ~~ 1* is
  oi  ~~ 1* oi
  sk  ~~ 1* sk
  cu  ~~ 1* cu
  mi  ~~ 1* mi

  gp ~~ 1* gp
  gn ~~ 1* gn
  
  gp ~~ NA*gn
  
  gp ~~ 0*sj
  gp ~~ 0*is
  gp ~~ 0*oi
  gp ~~ 0*sk
  gp ~~ 0*cu
  gp ~~ 0*mi
  
  gn ~~ 0*sj
  gn ~~ 0*is
  gn ~~ 0*oi
  gn ~~ 0*sk
  gn ~~ 0*cu
  gn ~~ 0*mi
  
  sj ~~ 0* is      
    sj ~~ 0* oi      
    sj ~~ 0* sk            
    sj ~~ 0* cu           
    sj ~~ 0* mi             
  is ~~ 0* oi              
    is ~~ 0*sk              
    is ~~ 0*cu              
    is ~~ 0*mi               
  oi ~~ 0*sk              
    oi ~~ 0*cu              
    oi ~~ 0*mi               
  sk ~~ 0*cu          
    sk ~~ 0*mi      
  cu ~~ 0*mi         
'


fit2 <- lavaan::cfa(
  mod_2bi,
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)


summary(
  fit2, 
  fit.measures = TRUE, 
  standardized = TRUE
)

semTools::reliability(fit_bifactor_hier, return.total = TRUE)












# Two-bifactor model ----

mod_twobifactor <- '

  sj =~ NA*u1 + u8 + u11 + u16 + u21
  is =~ NA*u4 + u13 + u18 + u25
  oi =~ NA*u2 + u6 + u20 + u24
  
  sk =~ NA*u5 + u12 + u19 + u23 + u26
  cu =~ NA*u3 + u7 + u10 + u15
  mi =~ NA*u9 + u14 + u17 + u22

  gn =~ NA*u1 + u8 + u11 + u16 + u21 + 
        u4 + u13 + u18 + u25 +
        u2 + u6 + u20 + u24
        
  gp =~ NA*u5 + u12 + u19 + u23 + u26 +
        u3 + u7 + u10 + u15 +
        u9 + u14 + u17 + u22
  
  sj  ~~ 1* sj
  is  ~~ 1* is
  oi  ~~ 1* oi
  sk  ~~ 1* sk
  cu  ~~ 1* cu
  mi  ~~ 1* mi
  gn  ~~ 1* gn
  gp  ~~ 1* gp
  
  sj  ~~ 0* is
  sj  ~~ 0* oi
  sj  ~~ 0* sk
  sj  ~~ 0* cu
  sj  ~~ 0* mi
  sj  ~~ 0* gn
  sj  ~~ 0* gp
  is  ~~ 0* oi
  is  ~~ 0* sk
  is  ~~ 0* cu
  is  ~~ 0* mi
  is  ~~ 0* gn
  is  ~~ 0* gp
  oi  ~~ 0* sk
  oi  ~~ 0* cu
  oi  ~~ 0* mi
  oi  ~~ 0* gn
  oi  ~~ 0* gp
  sk  ~~ 0* cu
  sk  ~~ 0* mi
  sk  ~~ 0* gn
  sk  ~~ 0* gp
  cu  ~~ 0* mi
  cu  ~~ 0* gn
  cu  ~~ 0* gp
  mi  ~~ 0* gn
  mi  ~~ 0* gp
  gn  ~~ NA* gp
'

fit_twobifactor <- lavaan::cfa(
  mod_twobifactor,
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)

summary(
  fit_twobifactor, 
  fit.measures = TRUE, 
  standardized = TRUE
)

fitMeasures(fit_twobifactor)

c(
  fitmeasures(fit_bifactor_hier)[c(4,3,5,25,26,40:42)], 
  pcfi_wlsmv.lavaan(fit_bifactor_hier)
)

semTools::reliability(fit_sc1)


# Bifactor hierarchical model ----

mod_bifactor_hier <- '

  sj =~ NA*u1 + u8 + u11 + u16 + u21
  is =~ NA*u4 + u13 + u18 + u25
  oi =~ NA*u2 + u6 + u20 + u24
  
  sk =~ NA*u5 + u12 + u19 + u23 + u26
  cu =~ NA*u3 + u7 + u10 + u15
  mi =~ NA*u9 + u14 + u17 + u22
  
  neg =~ NA*sj + is + oi
  pos =~ NA*sk + cu + mi
  g   =~ NA*u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10 + 
         u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + 
         u20 + u21 + u22 + u23 + u24 + u25 + u26
  
  c =~ 1*neg + 1*pos
  
  sj  ~~ 1* sj
  is  ~~ 1* is
  oi  ~~ 1* oi
  sk  ~~ 1* sk
  cu  ~~ 1* cu
  mi  ~~ 1* mi
  neg ~~ NA* neg
  pos ~~ NA* pos
  g   ~~ 1* g
  c   ~~ 1*c
  
  sj  ~~ 0* is
  sj  ~~ 0* oi
  sj  ~~ 0* sk
  sj  ~~ 0* cu
  sj  ~~ 0* mi
  sj  ~~ 0* neg
  sj  ~~ 0* pos
  sj  ~~ 0* g
  is  ~~ 0* oi
  is  ~~ 0* sk
  is  ~~ 0* cu
  is  ~~ 0* mi
  is  ~~ 0* neg
  is  ~~ 0* pos
  is  ~~ 0* g
  oi  ~~ 0* sk
  oi  ~~ 0* cu
  oi  ~~ 0* mi
  oi  ~~ 0* neg
  oi  ~~ 0* pos
  oi  ~~ 0* g
  sk  ~~ 0* cu
  sk  ~~ 0* mi
  sk  ~~ 0* neg
  sk  ~~ 0* pos
  sk  ~~ 0* g
  cu  ~~ 0* mi
  cu  ~~ 0* neg
  cu  ~~ 0* pos
  cu  ~~ 0* g
  mi  ~~ 0* neg
  mi  ~~ 0* pos
  mi  ~~ 0* g
  neg ~~ 0* pos
  neg ~~ 0* g
  pos ~~ 0* g
  
  sj ~~ 0*c
  is ~~ 0*c
  oi ~~ 0*c
  sk ~~ 0*c
  cu ~~ 0*c
  mi ~~ 0*c
  neg ~~ 0*c
  pos ~~ 0*c
  g ~~ 0*c
'

fit_bifactor_hier <- lavaan::cfa(
  mod_bifactor_hier,
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)

summary(
  fit_bifactor_hier, 
  fit.measures = TRUE, 
  standardized = TRUE
)

fitMeasures(fit_bifactor_hier)

c(
  fitmeasures(fit_bifactor_hier)[c(4,3,5,25,26,40:42)], 
  pcfi_wlsmv.lavaan(fit_bifactor_hier)
)

semTools::reliability(fit_bifactor_hier, return.total = TRUE)



# Bifactor model ----

mod_bifactor <- '

  sj =~ NA*u1 + u8 + u11 + u16 + u21
  is =~ NA*u4 + u13 + u18 + u25
  oi =~ NA*u2 + u6 + u20 + u24
  
  sk =~ NA*u5 + u12 + u19 + u23 + u26
  cu =~ NA*u3 + u7 + u10 + u15
  mi =~ NA*u9 + u14 + u17 + u22
  
  g   =~ NA*u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10 + 
         u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + 
         u20 + u21 + u22 + u23 + u24 + u25 + u26
  
  sj  ~~ 1* sj
  is  ~~ 1* is
  oi  ~~ 1* oi
  sk  ~~ 1* sk
  cu  ~~ 1* cu
  mi  ~~ 1* mi
  g   ~~ 1* g
  
  sj  ~~ NA* is
  sj  ~~ NA* oi
  sj  ~~ NA* sk
  sj  ~~ NA* cu
  sj  ~~ NA* mi
  sj  ~~ 0* g
  is  ~~ NA* oi
  is  ~~ NA* sk
  is  ~~ NA* cu
  is  ~~ NA* mi
  is  ~~ 0* g
  oi  ~~ NA* sk
  oi  ~~ NA* cu
  oi  ~~ NA* mi
  oi  ~~ 0* g
  sk  ~~ NA* cu
  sk  ~~ NA* mi
  sk  ~~ 0* g
  cu  ~~ NA* mi
  cu  ~~ 0* g
  mi  ~~ 0* g
'

fit_bifactor <- lavaan::cfa(
  mod_bifactor,
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)

anova(fit_bifactor_hier, fit_bifactor)

reliability(fit_bifactor, return.total = TRUE)


summary(
  fit_bifactor, 
  fit.measures = TRUE, 
  standardized = TRUE
)

fitMeasures(fit_bifactor)

semTools::reliabilityL2(fit_bifactor, "P")
semTools::reliabilityL2(fit_bifactor, "N")
semTools::reliabilityL2(fit_bifactor, "g")


# 6-factor model ----

mod_6factors <- '

  sj =~ NA*u1 + u8 + u11 + u16 + u21
  is =~ NA*u4 + u13 + u18 + u25
  oi =~ NA*u2 + u6 + u20 + u24
  
  sk =~ NA*u5 + u12 + u19 + u23 + u26
  cu =~ NA*u3 + u7 + u10 + u15
  mi =~ NA*u9 + u14 + u17 + u22
  
  sj  ~~ 1* sj
  is  ~~ 1* is
  oi  ~~ 1* oi
  sk  ~~ 1* sk
  cu  ~~ 1* cu
  mi  ~~ 1* mi

  sj  ~~ NA* is
  sj  ~~ NA* oi
  sj  ~~ NA* sk
  sj  ~~ NA* cu
  sj  ~~ NA* mi
  is  ~~ NA* oi
  is  ~~ NA* sk
  is  ~~ NA* cu
  is  ~~ NA* mi
  oi  ~~ NA* sk
  oi  ~~ NA* cu
  oi  ~~ NA* mi
  sk  ~~ NA* cu
  sk  ~~ NA* mi
  cu  ~~ NA* mi
'

fit_6factors <- lavaan::cfa(
  mod_6factors,
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)

anova(fit_6factors, fit_bifactor)


summary(
  fit_6factors, 
  fit.measures = TRUE, 
  standardized = TRUE
)

fitMeasures(fit_6factors)






summary(fit_sc3, fit.measures = TRUE, standardized = TRUE)
c(
  fitmeasures(fit_sc3)[c(3,4,5,25,26,40:42)],
  pcfi_wlsmv.lavaan(fit.s1_scs_5a.WMLSMV)
)

out_fit_sc3 <- semTools::reliability(fit_sc3, return.total = TRUE)
omega_total <- round(out_fit_sc3[2, 7], 2)
omega_total

semTools::reliabilityL2(fit_sc3, "P")
semTools::reliabilityL2(fit_sc3, "N")
semTools::reliabilityL2(fit_sc3, "SC")


# parameterEstimates(fit_sc3) %>%
#   as_tibble() %>%
#   filter(op=="~") %>%
#   mutate(Term=paste(lhs, op, rhs)) %>%
#   rename(estimate=est,
#          p=pvalue) %>%
#   select(Term, estimate, z, p) %>%
#   pander::pander(caption="Regression parameters from `mediation.fit`")


## two components CFA model ####
mod_2c <- '

        SK =~ u5+ u12+ u19+ u23+ u26
        CH =~ u3+ u7+ u10+ u15
        MI =~ u9+ u14+ u17+ u22

        SJ =~ u1+ u8+ u11+ u16+ u21
        IS =~ u4+ u13+ u18+ u25
        OI =~ u2+ u6+ u20+ u24
              
        P =~ SK + CH + MI
        N =~ SJ + IS + OI
        
        P ~~ N

'

anova(fit_sc3, fit_sc4)


fit_sc4 <- lavaan::cfa(
  mod_2c,
  data = mydata,
  estimator = "WLSMV",
  #std.lv = TRUE
  ordered = items
)

fitMeasures(fit_sc4)


semTools::reliability(fit_sc4, return.total = TRUE)

# Positive self-compassion.
# the proportion of variance of a composite score calculated from 
# the observed indicators (e.g., a total score or scale mean) that 
# is attributable to the second-order factor, i.e. coefficient 
# omega at Level 1:
reliability_P <- as.numeric(round(semTools::reliabilityL2(fit_sc4, "P")[1], 2))
reliability_P

# Negative self-compassion.
reliability_N <- as.numeric(round(semTools::reliabilityL2(fit_sc4, "N")[1], 2))
reliability_N












fs <- lavPredict(fit_bifactor)

hist(fs[, 7])
t.test(fs[, 7] ~ data_clean_mh$gender)


mod_2factors <- '

  ne =~ NA*u1 + u8 + u11 + u16 + u21 +
        u4 + u13 + u18 + u25 +
        u2 + u6 + u20 + u24
  
  po =~ NA*u5 + u12 + u19 + u23 + u26 +
        u3 + u7 + u10 + u15 +
        u9 + u14 + u17 + u22
  
  g   =~ NA*u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10 + 
         u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + 
         u20 + u21 + u22 + u23 + u24 + u25 + u26
  
  ne  ~~ 1* ne
  po  ~~ 1* po
  g   ~~ 1* g
  
  po  ~~ NA* ne
  po  ~~ 0* g
  ne  ~~ 0* g
'



fit_2f <- lavaan::cfa(
  mod_2factors,
  data = data_clean_mh,
  estimator = "WLSMV",
  #std.lv = TRUE
  ordered = items
)

fitMeasures(fit_2f)

fs2 <- lavPredict(fit_2f)

t.test(fs2[, 1] ~ data_clean_mh$gender)
t.test(fs2[, 2] ~ data_clean_mh$gender)








tot_1 <- rowSums(fs[, 1:3])
tot_2 <- rowSums(fs[, 4:6])

t.test(fs[, 1] ~ data_clean_mh$gender)
t.test(fs[, 2] ~ data_clean_mh$gender)
t.test(fs[, 3] ~ data_clean_mh$gender)
t.test(fs[, 4] ~ data_clean_mh$gender)
t.test(fs[, 5] ~ data_clean_mh$gender)
t.test(fs[, 6] ~ data_clean_mh$gender)





# ---------------------------- end


pcfi_ml.lavaan <- function(x){
  pcfi.ml <- (fitMeasures(x,"all")[[4]]/fitMeasures(x,"all")[[7]])*fitMeasures(x,"all")[[9]]
  names(pcfi.ml) <- "PCFI"
  pcfi.ml
}

pcfi_wlsmv.lavaan <- function(x){
  pcfi.wlsmv <- (fitMeasures(x,"all")[[4]]/fitMeasures(x,"all")[[11]])*fitMeasures(x,"all")[[25]]
  names(pcfi.wlsmv) <- "PCFI"
  pcfi.wlsmv
}


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




