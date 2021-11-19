#--------------------------------------------------------------------
# Self-compassion Scale
# 
# Question: to examine the psychometric properties of the 
# Self Compassion Scale in the present sample.
# 
# 50_self_compassion_scale.R
# Corrado Caudek
# Last modified on: "Mon Feb  3 13:47:10 2020"
#--------------------------------------------------------------------


assumptions <- function(x) { #x is a data frame includes item responses #creating a summary data frame
  #descriptive statistics were obtain via pastects package
  descr <- as.data.frame(matrix(NA, nrow = 8, ncol = ncol(x))) 
  rownames(descr) <- c("Number_of_Observations",
                       "Number_of_missing_values", 
                       "min_value", "max_value", 
                       "mode_value", "median_value", 
                       "_skewness_", "_kurtosis_")
  descriptives <- pastecs::stat.desc(x) #calculate descriptive statistics.
  
  #This function taken from https://www.r-bloggers.com/computing-the-mode-in-r/
  Mode = function(x) { 
    ta = table(x)
    tam = max(ta)
    if (all(ta == tam))
      mod = NA
    else if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam]) 
    else
      mod = names(ta)[ta == tam] 
    return(mod)
  }
  
  #calculate modes
  mods <- as.data.frame(apply(as.matrix(x), 2, Mode)) 
  descr[1:4, ] <- descriptives[c(1, 3, 4, 5), ] 
  descr[5, ] <- mods[1:ncol(x), ]
  descr[6, ] <- descriptives[8, ]
  descr[7, ] <- moments::skewness(x, na.rm = T) 
  descr[8, ] <- moments::kurtosis(x, na.rm = T)-3
  
  #Calculate VIF and Tolerance values
  #To obtain IF and TV values describe the model 
  x_new <- x
  x_new$rn <- 1:nrow(x)
  model_for_collinearity <- lm(
    as.formula(paste(colnames(x_new)[ncol(x_new)], "~", 
                     paste(colnames(x_new)[1:(ncol(x_new)-1)], collapse = "+"),
                     sep = "" 
                     )), data = x_new)
  mc_VIF_TOL <- as.data.frame(mctest::mctest(model_for_collinearity,
                                             type = "i")$idiags[,1:2]) #calculate VIF and Tollerance values
  #Calculate Condition Index
  mc_CI <- mctest::eigprop(mod = model_for_collinearity)$ci
  
  #A data frame for summary of multicollinearity
  mc_control <- data.frame(min_VIF = min(mc_VIF_TOL$VIF), 
                           max_VIF = max(mc_VIF_TOL$VIF), 
                           min_TOL = min(mc_VIF_TOL$TOL), 
                           max_TOL = max(mc_VIF_TOL$TOL),
                           min_CI = min(mc_CI),
                           max_CI = max(mc_CI) #giving a summary of multicollinearity
                           ) 

#Mahalanobis Distance Calculation
#To calculate mahalanobis distance, missing values are not accepted. 
distance <- as.matrix(mahalanobis(x, colMeans(x), cov = cov(x)))
#Those with Mahalanobis Distance p values bigger than 0.001 were considered as outliers.
Mah_significant <- x %>% transmute(row_number = 1:nrow(x),
                                   Mahalanobis_distance = distance,
                                   Mah_p_value = pchisq(distance, df = ncol(x), lower.tail = F)
                                   ) %>% 
  filter(Mah_p_value <= 0.001)

#Calculate Mardia's kurtosis value for multivariate normality
mardia_kurt <- psych::mardia(x, plot = FALSE)
#Return a list consist of descriptive statistics, multicollinearity, multivariate normality and Mahalanobis distance for multivariate outliers
return(list(descriptives = round(descr, 2), 
            multicollineartiy = round(mc_control, 2),
            Mah_significant = Mah_significant,
            n_outlier = nrow(Mah_significant), 
            Mardia_Kurtosis = mardia_kurt$kurtosis, 
            Mardia_Kurtosis_p_value = mardia_kurt$p.kurt)
       )
}



# We report robust goodness of fit indicators given the use of ‘WLSM.’
# We use the ‘WLSM’ estimator, which allows for categorical variables.

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

library("GPArotation")
library("psych")

options(max.print=99999999)


source(here("libraries", "self_compassion_fnc.R"))

#### additional functions ####
## calculate pcfi ####
# PGFI = PRATIO*GFI
# PRATIO = PGFI/GFI
# PCFI = PRATIO*CFI
# requires x to be a lavaan fit input

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


# Read data ----

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

mydata <- mydata[complete.cases(mydata), ] 
nrow(mydata)

items <- c(
  "u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", "u11", "u12", "u13", "u14", "u15",
  "u16", "u17", "u18", "u19", "u20", "u21", "u22", "u23", "u24", "u25", "u26"
)


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



fa.un <- efaUnrotate(data_clean_mh, nf = 6) 
summary(fa.un, std = TRUE)
fitMeasures(fa.un, c("chisq", "df", "pvalue", "cfi", "tli", "srmr", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic"))

d <- data_clean_mh
new_data <- d

lowerCor(d, use = "pairwise.complete.obs")
corr.test(d, use = "pairwise.complete.obs")$ci

##################################################### 
# Interpratation of KMO Value (Kaiser & Rice, 1974) # 
# KMO > 0.90 ==> Marvelous,                         # 
# 0.80 < KMO < 0.90 ==> Meritorious,                # 
# 0.70 < KMO < 0.80 ==> Middling,                   # 
# 0.60 < KMO < 0.70 ==> Mediocre,                   #
# 0.50 < KMO < 0.60 ==> Miserable,                  #
# KMO < 0.50 ==> Unacceptable.                      # 
#####################################################
C <- psych::polychoric(d)

# Bartlett’s (1950) test of sphericity is an inferential statistic 
# used to assess the factorability of R.  This statistic tests the 
# null hypothesis that the population correlation matrix is equal to an 
# identity matrix.  However, this test is known to be extremely sensitive 
# and should be used with only relatively small samples (Tabachnick & 
# Fidell, 2001).  For this reason, Dziuban and Shirkey (1974) 
# propose using this statistic as a conservative indicator of factorability.  
# While a significant test is still dubious, a correlation matrix with a 
# non‐significant test should certainly not be subjected to factor analysis
bart <- psych::cortest.bartlett(C$rho, n = nrow(d), diag = TRUE)

# Kaiser (1970, 1981) viewed factorability as a psychometric issue, 
# opting to use the term sampling adequacy to reflect the importance 
# of sampling the right set of variables in order to detect 
# any meaningful underlying structure.  He based his work in this area 
# on that of Guttman, who had shown that the closer the off‐diagonal 
# elements of an anti‐image correlation matrix were to zero, 
# the stronger the evidence that the data have a common‐factor structure 
# (Cerny & Kaiser, 1977; Kaiser, 1970).  The anti‐image of a variable is 
# the residual portion of that variable remaining after 
# removing the variance that can be associated with the other variables in 
# the set (Gorsuch, 1983). The correlation between the anti‐images of two 
# observed variables (from a set of p) can conveniently be defined as the 
# opposite value of the corresponding (p - 2)-th-order partial 
# correlation (Rummel, 1970).  Consequently, if there is in fact at least 
# one common factor underlying a set of observed variables, then the 
# anti‐image correlations will be relatively small in absolute magnitude 
# as compared to the zero‐order correlations.  Using this property, Kaiser 
# sought to create a non‐inferential, psychometrically based indicator of 
# factorability.  What resulted from this line of thought is now better 
# known as the Kaiser‐Meyer‐Olkin (KMO) measure of sampling adequacy.
kaiser <- psych::KMO(C$rho)

interpretation_KMO <- dplyr::case_when(
  kaiser$MSA >= 0.90 ~ "Marvelous",
  kaiser$MSA >= 0.80 & kaiser$MSA < 0.90 ~ "Mertitourious",
  kaiser$MSA >= 0.70 & kaiser$MSA < 0.80 ~ "Middling", 
  kaiser$MSA >= 0.60 & kaiser$MSA < 0.70 ~ "Medicore", 
  kaiser$MSA >= 0.50 & kaiser$MSA < 0.60 ~ "Miserable", 
  kaiser$MSA <0.50 ~ "Unacceptable"
)

Bart_KMO <- data.frame(
  KMO = round(kaiser$MSA, 3), 
  Interpretation_KMO = interpretation_KMO,
  Bartlett_Chi = bart$chisq,
  Bartlett_df = bart$df,
  Bartlett_sig = sprintf("%.3f",bart$p.value)
)
Bart_KMO

#Scree Plot for determine number of factors
eigenvalues <- nFactors::eigenComputes(x = new_data) 
eigen_for_graph <- data.frame(item_number = 1:ncol(new_data), eigenvalues) 

scree_plot <- ggplot(data = eigen_for_graph) +
  geom_point(aes(x = item_number, y = eigenvalues )) + 
  geom_line(aes(x = item_number, y = eigenvalues )) + 
  xlab("Factor Number")+
  ylab ("Eigenvalues")+
  papaja::theme_apa()+
  scale_x_continuous(breaks = seq(from = 1, to = ncol(new_data), by = 1))
#MAP analysis for examine number of dimensions
map_analysis <- psych::vss(new_data, n = (ncol(new_data) - 1)) 
map_factors <- which(map_analysis$map == min(map_analysis$map))
#Parallel analysis for examine number of dimensions
#Conduct Parallel analysis with Pearson Correlation Matrix 
PA_pearson <- psych::fa.parallel(new_data, fa = "both", cor = "cor")
#Conduct Parallel Analysis with Polychoric Correlation Matrix
PA_poly <- psych::fa.parallel(new_data, fa = "both", cor = "poly")
results_factor_retentation <- list(MAP_Result = map_factors, Parallel_Analysis_Pearson = PA_pearson$nfact,
                                   Parallel_Analysis_Polychoric = PA_poly$nfact,
                                   Scree_Plot = "Look at the Plots Section for Scree Plot", 
                                   scree_plot)

results_factor_retentation

alpha(d)
splitHalf(d)

scree(d, factors = FALSE)

tot_score <- rowSums(d)
plot(density(tot_score, na.rm = TRUE), 
     main = "Total score")
describe(d)
error.dots(d)
error.bars(d)


model <- '

  sj =~ u1 + u8 + u11 + u16 + u21
  is =~ u4 + u13 + u18 + u25
  oi =~ u2 + u6 + u20 + u24
  
  sk =~ u5 + u12 + u19 + u23 + u26
  cu =~ u3 + u7 + u10 + u15
  mi =~ u9 + u14 + u17 + u22

'
  

# https://tkaiser.science/esemR.html


# M1

m1_model <- '

  fu =~ u1 + u8 + u11 + u16 + u21 +
        u4 + u13 + u18 + u25 +
        u2 + u6 + u20 + u24
  
  fc =~ u5 + u12 + u19 + u23 + u26 + 
        u3 + u7 + u10 + u15 +
        u9 + u14 + u17 + u22
  
'

fit_1 <- lavaan::cfa(
  m1_model, 
  data = d, 
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE,
  orthogonal = TRUE
)

summary(
  fit_1, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitmeasures(fit_1, c("cfi","tli","rmsea","srmr"))

f2_efa <- fa(d, nfact = 2, rotate = "geominQ", fm = "ml")
fa.diagram(f1_efa)

f_loadmat <- zapsmall(matrix(round(f2_efa$loadings, 2), nrow = 26, ncol = 2))
rownames(f_loadmat) <- colnames(d[, 1:26])

# This will turn the loading matrix into a lavaan-compatible equation set. 
terms <- vector()
for (i in 1:2) {
  terms[i] <-
    paste0("F",i,"=~ ", 
           paste0(c(f_loadmat[,i]), "*", 
                  names(f_loadmat[,1]), collapse = "+")
           )
}

# terms[7] <- "A1 ~~ C2+E3+N3\n C2 ~~ E3+N3\n E3 ~~ N3"

m_esem <- paste(terms, collapse = "\n")
m_esem

fit_2 <- lavaan::cfa(
  m_esem, 
  data = d, 
  verbose = FALSE, 
  estimator = "WLSMV",
  ordered = items
  )


fitmeasures(fit_2, c("cfi","tli","rmsea","srmr"))

summary(
  fit_2, 
  standardized=TRUE, 
  fit.measures=TRUE
)


# Bifactor ESEM



m2_model <- '

fu =~ u1 + u8 + u11 + u16 + u21 +
      u4 + u13 + u18 + u25 +
      u2 + u6 + u20 + u24

fc =~ u5 + u12 + u19 + u23 + u26 + 
      u3 + u7 + u10 + u15 +
      u9 + u14 + u17 + u22

f  =~ u1 + u8 + u11 + u16 + u21 +
      u4 + u13 + u18 + u25 +
      u2 + u6 + u20 + u24 + 
      u5 + u12 + u19 + u23 + u26 + 
      u3 + u7 + u10 + u15 +
      u9 + u14 + u17 + u22

f ~~ 0*fu
f ~~ 0*fc
'

fit_2 <- lavaan::cfa(
  m2_model, 
  data = d, 
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE
)

summary(
  fit_2, 
  standardized=TRUE, 
  fit.measures=TRUE
)

fitmeasures(fit_1, c("cfi","tli","rmsea","srmr"))


f1_efa <- fa(d, nfact = 2, rotate = "geominQ", fm = "ml")
fa.diagram(f1_efa)


f2_loadmat <- zapsmall(matrix(round(f2_efa$loadings, 2), nrow = 26, ncol = 2))
rownames(f2_loadmat) <- colnames(d[, 1:26])

# This will turn the loading matrix into a lavaan-compatible equation set. 
terms <- vector()
for (i in 1:2) {
  terms[i] <-
    paste0("F",i,"=~ ", 
           paste0(c(f2_loadmat[,i]), "*", 
                  names(f2_loadmat[,1]), collapse = "+")
    )
}

# terms[7] <- "A1 ~~ C2+E3+N3\n C2 ~~ E3+N3\n E3 ~~ N3"

m_esem <- paste(terms, collapse = "\n")
m_esem

# m_esem <- "F1=~ 0.499*u1+0.815*u2+-0.204*u3+0.8*u4+-0.155*u5+0.802*u6+-0.334*u7+0.694*u8+-0.053*u9+-0.227*u10+0.684*u11+0.011*u12+0.737*u13+0.192*u14+-0.022*u15+0.722*u16+0.209*u17+0.796*u18+0.025*u19+0.667*u20+0.549*u21+0.068*u22+0.234*u23+0.653*u24+0.769*u25+0.063*u26\nF2=~ -0.005*u1+0.024*u2+0.386*u3+0.026*u4+0.703*u5+0.007*u6+0.497*u7+0.043*u8+0.425*u9+0.525*u10+0.023*u11+0.731*u12+-0.004*u13+0.422*u14+0.634*u15+0.051*u16+0.541*u17+-0.058*u18+0.759*u19+-0.128*u20+0.038*u21+0.624*u22+0.617*u23+-0.021*u24+0.029*u25+0.685*u26\nF3=~ -0.314*u1+0.027*u2+0.303*u3+0.046*u4+-0.106*u5+-0.035*u6+0.04*u7+-0.326*u8+0.476*u9+0.171*u10+-0.288*u11+-0.075*u12+0.114*u13+0.498*u14+0.235*u15+-0.228*u16+0.39*u17+0.068*u18+-0.1*u19+0.259*u20+-0.286*u21+0.189*u22+-0.095*u23+0.177*u24+0.027*u25+-0.093*u26\nF3~~0*F1\nF3~~0*F2"



fit_3 <- lavaan::cfa(
  m_esem, 
  data = d, 
  verbose = FALSE, 
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE
)


fitmeasures(fit_2, c("cfi","tli","rmsea","srmr"))

summary(
  fit_3, 
  standardized=TRUE, 
  fit.measures=TRUE
)

























summary(
  b6_cfa2, 
  standardized=TRUE, 
  fit.measures=TRUE
)


fit <- cfa(
  mod, 
  std.lv = TRUE, 
  orthogonal = TRUE, 
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)

summary(
  fit, 
  standardized=TRUE, 
  fit.measures=TRUE
)




mod1 <- '

  sj =~ u1 + u8 + u11 + u16 + u21
  is =~ u4 + u13 + u18 + u25
  oi =~ u2 + u6 + u20 + u24
  
  sk =~ u5 + u12 + u19 + u23 + u26
  cu =~ u3 + u7 + u10 + u15
  mi =~ u9 + u14 + u17 + u22
  
  g   =~ u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10 + 
         u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + 
         u20 + u21 + u22 + u23 + u24 + u25 + u26
  
'


fit1 <- cfa(
  mod1, 
  std.lv = TRUE, 
  orthogonal = TRUE, 
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)

summary(
  fit1, 
  standardized=TRUE, 
  fit.measures=TRUE
)



mod2 <- '

  sj =~ u1 + u8 + u11 + u16 + u21
  is =~ u4 + u13 + u18 + u25
  oi =~ u2 + u6 + u20 + u24
  
  sk =~ u5 + u12 + u19 + u23 + u26
  cu =~ u3 + u7 + u10 + u15
  mi =~ u9 + u14 + u17 + u22
  
  gn =~ u1 + u8 + u11 + u16 + u21 +
        u4 + u13 + u18 + u25 +
        u2 + u6 + u20 + u24
  
  gp =~ u5 + u12 + u19 + u23 + u26 +
        u3 + u7 + u10 + u15 +
        u9 + u14 + u17 + u22
  
'


fit2 <- cfa(
  mod2, 
  std.lv = TRUE, 
  orthogonal = TRUE, 
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)

summary(
  fit2, 
  standardized=TRUE, 
  fit.measures=TRUE
)












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




