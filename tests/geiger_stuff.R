## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

###################################### ###########
##### Reanalysis on Neff et al.'s Commentary #####
########## Mattis Geiger (May 2018) ##############
############################################ #####

# By: Mattis Geiger (04/2018-05/2018)
## Preface: ####
# - this is the complete syntax for the rebuttal on Neff, Tóth-Király, & Colosimo (under review): Self-compassion is best measured as a global construct and is overlapping with but distinct from neuroticism: Aresponse to Pfattheicher, Geiger, Hartung, Weiss, and Schindler
# - Neff et al.'s manuscript is a reply on Pfattheicher, Geiger, Hartung, Weiss, & Schindler (2017). Old Wine in New Bottles? The Case of Self-compassion and Neuroticism. European Journal of Personality, 31, 160-169. 
# - study numbers refer to the numbers reported in Neff et al.
# - Study 1 is the data from Pfattheicher et al. (2017)
# - Pfattheicher et al. ran all analysis in R, Neff et al. ran all analysis in Mplus 8. To increase accessibility and overview, this syntax will use MplusAutomation when replicating models of Neff et al. in Mplus.

#### packages ####
library(MplusAutomation)
library(psych)
library(lavaan)
library(semTools)
library(haven)
library(texreg)


#### data ####
# Study 1 ####
S1_data <- read_sav("~3 - Results/Study 1 - clean data.sav")
# Study 2 ####
S2_data <- read_sav("~3 - Results/Study 2 - scs neuro ders.sav")
colnames(S2_data) <- c("scs_01r","scs_02r","scs_03","scs_04r","scs_05","scs_06r","scs_07","scs_08r","scs_09",          
                       "scs_10","scs_11r","scs_12","scs_13r","scs_14","scs_15","scs_16r","scs_17",          
                       "scs_18r","scs_19","scs_20r","scs_21r","scs_22","scs_23","scs_24r","scs_25r",          
                       "scs_26",
                       "neo1","neo2",
                       "neo3","neo4","neo5","neo6","neo7","neo8","neo9","neo10","neo11","neo12","neo13","neo14","neo15","neo16",
                       "neo17","neo18","neo19","neo20","neo21","neo22","neo23","neo24","neo25","neo26","neo27","neo28","neo29","neo30",  
                       "neo31","neo32","neo33","neo34","neo35","neo36","neo37","neo38","neo39","neo40","neo41","neo42","neo43","neo44",  
                       "neo45","neo46","neo47","neo48","er1", "er2", "er3", "er4", "er5", "er6", "er7", "er8", "er9", "er10",
                       "er11","er12","er13","er14","er15","er16","er17","er18","er19","er20","er21","er22","er23","er24",
                       "er25","er26","er27","er28","er29","er30","er31","er32","er33","er34","er35","er36","TotSC","CS", 
                       "RUS", "SK","SJRC","CH","ISRC","MI","OIRC","noen1anx","neon2","neon3dep","neon4","neon5","neon6","TotN",
                       "DER")
names(S2_data)
# Study 3 ####
S3_data <- read_sav("~3 - Results/Study 3 - scs neuro outcomes.sav")
colnames(S3_data) <- c("s#","age","grade","sex","ethnic",
                       "scs_01r","scs_02r","scs_03","scs_04r","scs_05","scs_06r","scs_07","scs_08r","scs_09",          
                       "scs_10","scs_11r","scs_12","scs_13r","scs_14","scs_15","scs_16r","scs_17",          
                       "scs_18r","scs_19","scs_20r","scs_21r","scs_22","scs_23","scs_24r","scs_25r",          
                       "scs_26",
                       "wis1","wis2","wis3","wis4","wis5","wis6","wis7","wis8","wis9",     
                       "wis10","wis11","wis12","wis13","wis14","wis15","wis16","wis17","wis18","wis19",    
                       "wis20","wis21","wis22","wis23","wis24","wis25","wis26","wis27","wis28","wis29",    
                       "wis30","wis31","wis32","wis33","wis34","wis35","wis36","wis37","wis38","wis39",    
                       "panas1","panas2","panas3","panas4","panas5","panas6","panas7","panas8","panas9","panas10",  
                       "panas11","panas12","panas13","panas14","panas15","panas16","panas17","panas18","panas19","panas20",  
                       "se1", "se2", "se3", "se4", "se5", "se6", "se7", "se8", "se9", "se10",
                       "lot1","lot2","lot3","lot4","lot5","lot6","lot7","lot8","lot9","lot10",
                       "cei1","cei2","cei3","cei4","cei5","cei6","cei7","well1","well2","well3",
                       "well4","well5","well6","well7","well8","well9","well10","well11","well12","well13",
                       "well14","well15","well16","well17","well18","well19","well20","well21","well22","well23",
                       "well24","well25","well26","well27","well28","well29","well30","well31","well32","well33",
                       "well34","well35","well36","well37","well38","well39","well40","well41","well42","well43",
                       "well44","well45","well46","well47","well48","well49","well50","well51","well52","well53",
                       "well54","happy1","happy2","happy3","happy4","pg1", "pg2", "pg3", "pg4", "pg5",
                       "pg6", "pg7", "pg8", "pg9", "neo1","neo2","neo3","neo4","neo5","neo6",
                       "neo7","neo8","neo9","neo10","neo11","neo12","neo13","neo14","neo15","neo16",
                       "neo17","neo18","neo19","neo20","neo21","neo22","neo23","neo24","neo25","neo26",
                       "neo27","neo28","neo29","neo30","neo31","neo32","neo33","neo34","neo35","neo36",
                       "neo37","neo38","neo39","neo40","neo41","neo42","neo43","neo44","neo45","neo46",
                       "neo47","neo48","neo49","neo50","neo51","neo52","neo53","neo54","neo55","neo56",
                       "neo57","neo58","neo59","neo60","selfkind","selfjudg","comhum","isolate","mindful","overiden", 
                       "TotSC","CS","RUS","wisrefl","wisaffect","wiscogn","posaffct","negaffct","selfestm","Optimism", 
                       "Curiosity","PWB","happines","pgrowint","neurotic","extrover","neoopen","neoagree","neoconsc")
names(S3_data)


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

# requires x to be a MplusAutomation readModels$summaries input
pcfi_ml.mplusAuto <- function(x){
  pcfi.ml <- (x[[13]]/x[[16]])*x[[20]]
  names(pcfi.ml) <- "PCFI"
  pcfi.ml
}

pcfi_wlsmv.mplusAuto <- function(x){
  pcfi.wlsmv <- (x[[13]]/x[[16]])*x[[18]]
  names(pcfi.wlsmv) <- "PCFI"
  pcfi.wlsmv
}


######################## #####
##### Reanalysis Neff et al. (2018) #####
######################## #####
## Agenda: ####
# 1. Inclusion of Higher Order Models to Neff et al.'s Table 1
# 1.1. Study 1 - Factor Structure of the SCS
# 1.2. Study 2 - Factor Structure of the SCS (only Models 4,5 & 7)
# 1.3. Study 3 - Factor Structure of the SCS (only Models 4,5 & 7)
# 2. Remodelling SCS
# 3. Big Five Models and discriminant validity
# 3.1. Big Five NEO-PI-R in Study 1 and disriminant validity of the simplified bifactor SCS model
# 3.2. Big Five NEO-PI-R in Study 2 and disriminant validity of the simplified bifactor SCS model
# 3.3. Big Five NEO-FFI in Study 3 and disriminant validity of the simplified bifactor SCS model
# 4. Incremental Predictive Validity of SCS over (facets of) neuroticism and extraversion
# 4.1. All facets of neuroticism and extaversion predicting life satisfaction; next step + SCS-Factors
# 4.3. Factors neuroticism and extraversion predicting happiness; next step + SCS factors
# 5. Additional exploratory analysis: SCS-factors as facets of neuroticism
# 5.1. SCS in neuroticism in Study 1
# 5.2. SCS in neuroticism in Study 2
#### MplusAutomation working directory ####
cd("~3 - Results/Study 1 - Mplus Files")


d <- scs[complete.cases(scs), ]

d <- dplyr::rename(d,
    scs_01 = i1,
    scs_02 = i2,
    scs_03 = i3,
    scs_04 = i4,
    scs_05 = i5,
    scs_06 = i6,
    scs_07 = i7,
    scs_08 = i8,
    scs_09 = i9,
    scs_10 = i10,
    scs_11 = i11,
    scs_12 = i12,
    scs_13 = i13,
    scs_14 = i14,
    scs_15 = i15,
    scs_16 = i16,
    scs_17 = i17,
    scs_18 = i18,
    scs_19 = i19,
    scs_20 = i20,
    scs_21 = i21,
    scs_22 = i22,
    scs_23 = i23,
    scs_24 = i24,
    scs_25 = i25,
    scs_26 = i26
  )




##### 1. Inclusion of Higher Order Models to Neff et al.'s Table 1 #####
#### 1.1. Study 1 - Factor Structure of the SCS ####
## 1.1.1a g-Factor CFA model ####
s1_scs_1a <- 'SC =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
                    scs_03+ scs_07+ scs_10+ scs_15+
                    scs_09+ scs_14+ scs_17+ scs_22+
                    scs_01 + scs_08 + scs_11 + scs_16 + scs_21 +
                    scs_04 + scs_13 + scs_18 + scs_25 +
                    scs_02 + scs_06 + scs_20 + scs_24 '

fit.s1_scs_1a.WMLSMV <- cfa(
  s1_scs_1a,
  d,
  estimator="WLSMV",
  ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26",
            "scs_03","scs_07","scs_10","scs_15","scs_09",
            "scs_14","scs_17","scs_22","scs_01","scs_08",
            "scs_11","scs_16","scs_21","scs_04",
            "scs_13","scs_18","scs_25","scs_02",
            "scs_06","scs_20","scs_24")
  )


fit.s1_scs_1a.ML <- cfa(s1_scs_1a,d,estimator="ML")

c(fitmeasures(fit.s1_scs_1a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_1a.WMLSMV))
c(fitmeasures(fit.s1_scs_1a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_1a.ML))

c(c(fitmeasures(fit.s1_scs_1a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_1a.WMLSMV)),
  c(fitmeasures(fit.s1_scs_1a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_1a.ML)))


## 1.1.2a 2-Factor CFA model ####
s1_scs_2a <- '
             CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
                    scs_03+ scs_07+ scs_10+ scs_15+
                    scs_09+ scs_14+ scs_17+ scs_22

              RUS =~ scs_01+ scs_08+ scs_11+ scs_16+ scs_21+
                     scs_04+ scs_13+ scs_18+ scs_25+
                     scs_02+ scs_06+ scs_20+ scs_24
'

fit.s1_scs_2a.WMLSMV <- cfa(
  s1_scs_2a,
  d,
  estimator="WLSMV",
  ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26",
            "scs_03","scs_07","scs_10","scs_15","scs_09",
            "scs_14","scs_17","scs_22","scs_01","scs_08",
            "scs_11","scs_16","scs_21","scs_04",
            "scs_13","scs_18","scs_25","scs_02",
            "scs_06","scs_20","scs_24")
  )

fit.s1_scs_2a.ML <- cfa(s1_scs_2a,d,estimator="ML")
c(fitmeasures(fit.s1_scs_2a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_2a.WMLSMV))
c(fitmeasures(fit.s1_scs_2a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_2a.ML))



## 1.1.3a 6-Factor CFA model ####
s1_scs_3a <- 'SK =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26
              CH =~ scs_03+ scs_07+ scs_10+ scs_15
              MI =~ scs_09+ scs_14+ scs_17+ scs_22

              SJ =~ scs_01 + scs_08 + scs_11 + scs_16 + scs_21
              IS =~ scs_04 + scs_13 + scs_18 + scs_25 
              OI =~ scs_02 + scs_06 + scs_20 + scs_24 
'

fit.s1_scs_3a.WMLSMV <- cfa(
  s1_scs_3a,
  d,
  estimator="WLSMV",
  ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26",
            "scs_03","scs_07","scs_10","scs_15","scs_09",
            "scs_14","scs_17","scs_22","scs_01","scs_08",
            "scs_11","scs_16","scs_21","scs_04",
            "scs_13","scs_18","scs_25","scs_02",
            "scs_06","scs_20","scs_24")
)

fit.s1_scs_3a.ML <- cfa(s1_scs_3a,d,estimator="ML")
c(fitmeasures(fit.s1_scs_3a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_3a.WMLSMV))
c(fitmeasures(fit.s1_scs_3a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_3a.ML))



## 1.1.4a bifactor g CFA model ####
s1_scs_4a <- 'SC =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
                    scs_03+ scs_07+ scs_10+ scs_15+
                    scs_09+ scs_14+ scs_17+ scs_22+
                    scs_01+ scs_08+ scs_11+ scs_16+ scs_21+
                    scs_04+ scs_13+ scs_18+ scs_25+
                    scs_02+ scs_06+ scs_20+ scs_24

              SK =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26
              CH =~ scs_03+ scs_07+ scs_10+ scs_15
              MI =~ scs_09+ scs_14+ scs_17+ scs_22

              SJ =~ scs_01+ scs_08+ scs_11+ scs_16+ scs_21
              IS =~ scs_04+ scs_13+ scs_18+ scs_25
              OI =~ scs_02+ scs_06+ scs_20+ scs_24

'

fit.s1_scs_4a.WMLSMV <- cfa(
  s1_scs_4a,
  d,
  estimator="WLSMV",
  orthogonal=TRUE,
  ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26",
            "scs_03","scs_07","scs_10","scs_15","scs_09",
            "scs_14","scs_17","scs_22","scs_01","scs_08",
            "scs_11","scs_16","scs_21","scs_04",
            "scs_13","scs_18","scs_25","scs_02",
            "scs_06","scs_20","scs_24")
)

fit.s1_scs_4a.ML <- cfa(
  s1_scs_4a,
  d,
  estimator="ML",
  orthogonal=TRUE)

c(fitmeasures(fit.s1_scs_4a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_4a.ML))
c(fitmeasures(fit.s1_scs_4a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_4a.WMLSMV))


## 1.1.5a bifactor 2F CFA model ####
s1_scs_5a <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
                    scs_03+ scs_07+ scs_10+ scs_15+
                    scs_09+ scs_14+ scs_17+ scs_22

              RUS =~ scs_01+ scs_08+ scs_11+ scs_16+ scs_21+
                     scs_04+ scs_13+ scs_18+ scs_25+
                     scs_02+ scs_06+ scs_20+ scs_24
              
              CS~~RUS

              SK =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26
              CH =~ scs_03+ scs_07+ scs_10+ scs_15
              MI =~ scs_09+ scs_14+ scs_17+ scs_22

              SJ =~ scs_01+ scs_08+ scs_11+ scs_16+ scs_21
              IS =~ scs_04+ scs_13+ scs_18+ scs_25
              OI =~ scs_02+ scs_06+ scs_20+ scs_24
'

fit.s1_scs_5a.WMLSMV <- cfa(
  s1_scs_5a,
  d,
  estimator="WLSMV",
  orthogonal=TRUE,
  std.lv=TRUE,
  ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26",
            "scs_03","scs_07","scs_10","scs_15","scs_09",
            "scs_14","scs_17","scs_22","scs_01","scs_08",
            "scs_11","scs_16","scs_21","scs_04",
            "scs_13","scs_18","scs_25","scs_02",
            "scs_06","scs_20","scs_24")
)

fit.s1_scs_5a.ML <- cfa(
  s1_scs_5a,
  d,
  estimator="ML",
  orthogonal=TRUE,
  std.lv=TRUE
)

summary(fit.s1_scs_5a.WMLSMV,standardized=TRUE,fit.measures=TRUE)
summary(fit.s1_scs_5a.ML,standardized=TRUE,fit.measures=TRUE)
c(fitmeasures(fit.s1_scs_5a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_5a.ML))
c(fitmeasures(fit.s1_scs_5a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_5a.WMLSMV))

fit.s1_scs_5a.WMLSMV_r1 <- cfa(
  s1_scs_5a,
  d,
  estimator="WLSMV",
  orthogonal=TRUE,
  std.lv=TRUE,
  ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26",
            "scs_03","scs_07","scs_10","scs_15","scs_09",
            "scs_14","scs_17","scs_22","scs_01","scs_08",
            "scs_11","scs_16","scs_21","scs_04",
            "scs_13","scs_18","scs_25","scs_02",
            "scs_06","scs_20","scs_24")
)

fit.s1_scs_5a.ML_r1 <- cfa(
  s1_scs_5a,
  d,
  estimator="ML",
  orthogonal=TRUE,
  std.lv=TRUE,
  constraints = "r1==1")

anova(fit.s1_scs_5a.WMLSMV_r1,fit.s1_scs_5a.WMLSMV)
anova(fit.s1_scs_5a.ML_r1,fit.s1_scs_5a.ML)
c(fitmeasures(fit.s1_scs_5a.ML_r1)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_5a.ML))


## 1.1.6a higher order g CFA model ####
s1_scs_6a <- 'SC =~ SK + CH + MI + SJ + IS + OI

SK =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26
CH =~ scs_03+ scs_07+ scs_10+ scs_15
MI =~ scs_09+ scs_14+ scs_17+ scs_22

SJ =~ scs_01+ scs_08+ scs_11+ scs_16+ scs_21
IS =~ scs_04+ scs_13+ scs_18+ scs_25
OI =~ scs_02+ scs_06+ scs_20+ scs_24

'

fit.s1_scs_6a.WMLSMV <- cfa(
  s1_scs_6a,
  d,
  estimator="WLSMV",
  ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26",
            "scs_03","scs_07","scs_10","scs_15","scs_09",
            "scs_14","scs_17","scs_22","scs_01","scs_08",
            "scs_11","scs_16","scs_21","scs_04",
            "scs_13","scs_18","scs_25","scs_02",
            "scs_06","scs_20","scs_24")
)

fit.s1_scs_6a.ML <- cfa(s1_scs_6a,d,estimator="ML")

c(fitmeasures(fit.s1_scs_6a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_6a.ML))
c(fitmeasures(fit.s1_scs_6a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_6a.WMLSMV))

## 1.1.7a higher order 2F CFA model ####
s1_scs_7a <- 'CS =~ cs_sk*SK + cs_ch*CH + cs_mi*MI 
              RUS =~ rus_sj*SJ + rus_is*IS + rus_oi*OI

SK =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26
CH =~ scs_03+ scs_07+ scs_10+ scs_15
MI =~ scs_09+ scs_14+ scs_17+ scs_22

SJ =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r
IS =~ scs_04r+ scs_13r+ scs_18r+ scs_25r
OI =~ scs_02r+ scs_06r+ scs_20r+ scs_24r

'
s1_scs_7a_i <- 'CS =~ cs_sk*SK + cs_ch*CH + cs_mi*MI 
              RUS =~ rus_sj*SJ + rus_is*IS + rus_oi*OI

SK ~ iSK*1
CH ~ iCH*1
MI ~ iMI*1
SJ ~ iSJ*1
IS ~ iIS*1
OI ~ iOI*1

SK =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26
CH =~ scs_03+ scs_07+ scs_10+ scs_15
MI =~ scs_09+ scs_14+ scs_17+ scs_22

SJ =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r
IS =~ scs_04r+ scs_13r+ scs_18r+ scs_25r
OI =~ scs_02r+ scs_06r+ scs_20r+ scs_24r

'
fit.s1_scs_7a.WMLSMV <- cfa(s1_scs_7a,S1_data,estimator="WLSMV",ordered=c("scs_05","scs_12","scs_19","scs_23","scs_26","scs_03","scs_07","scs_10","scs_15","scs_09","scs_14","scs_17","scs_22","scs_01r","scs_08r","scs_11r","scs_16r","scs_21r","scs_04r","scs_13r","scs_18r","scs_25r","scs_02r","scs_06r","scs_20r","scs_24r"))
fit.s1_scs_7a.ML <- cfa(s1_scs_7a,S1_data,estimator="ML",std.lv=TRUE)
summary(fit.s1_scs_7a.ML,standardized=TRUE,fit.measures=TRUE,rsquare=TRUE)
fit.s1_scs_7a_i.ML <- cfa(s1_scs_7a_i,S1_data,estimator="ML",std.lv=TRUE)
summary(fit.s1_scs_7a_i.ML,standardized=TRUE,fit.measures=TRUE)
c(fitmeasures(fit.s1_scs_7a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_7a.ML))
c(fitmeasures(fit.s1_scs_7a.WMLSMV)[c(4,3,5,25,26,40:42)],pcfi_wlsmv.lavaan(fit.s1_scs_7a.WMLSMV))

# DIFFTESTS ####
# WLSMV: see Mplus files ####
# ML ####
anova(fit.s1_scs_5a.ML,fit.s1_scs_4a.ML)
compareModels(readModels("s1_scs_5b.ML.out"),readModels("s1_scs_4b.ML.out"),show="summaries",diffTest=TRUE)
anova(fit.s1_scs_5a.ML,fit.s1_scs_7a.ML)
anova(fit.s1_scs_4a.ML,fit.s1_scs_6a.ML)
#### 1.2. Study 2 - Factor Structure of the SCS (only Models 4,5 & 7) ####
#### MplusAutomation working directory ####
cd("~3 - Results/Study 2 - Mplus Files")

## 1.2.4b ####
s2_scs_4b.ML <- mplusObject(
  TITLE = "MplusAutomation Study 1 Model 4b: Bifactor 1+6-Factor ESEM ML;",
  ANALYSIS = "
  estimator = ml; 
  rotation = target(orthogonal);
  iteration = 10000;",
  MODEL = "
  sc by scs_05 scs_12 scs_19 scs_23 scs_26
  scs_01r scs_08r scs_11r   scs_16r scs_21r 
  scs_03 scs_07 scs_10 scs_15      
  scs_04r scs_13r scs_18r scs_25r 
  scs_09 scs_14 scs_17 scs_22 
  scs_02r scs_06r scs_20r scs_24r (*1); 
  
  sk BY scs_05 scs_12 scs_19 scs_23 scs_26
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  sj BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r scs_08r scs_11r  scs_16r scs_21r 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  ch BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03 scs_07 scs_10 scs_15      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  is BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r scs_13r scs_18r scs_25r 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  mi BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09 scs_14 scs_17 scs_22 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  oi BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r scs_06r scs_20r scs_24r (*1);",
  OUTPUT = "stdyx;",
  rdata = S2_data,
  usevariables = c("scs_05","scs_12","scs_19","scs_23","scs_26","scs_03","scs_07","scs_10","scs_15","scs_09","scs_14","scs_17","scs_22","scs_01r","scs_08r","scs_11r","scs_16r","scs_21r","scs_04r","scs_13r","scs_18r","scs_25r","scs_02r","scs_06r","scs_20r","scs_24r")
)
s2_scs_4b.WLSMV <- update(s2_scs_4b.ML,
                          TITLE = ~"MplusAutomation Study 2 Model 4b: Bifactor 1+6-Factor ESEM WLSMV;",
                          ANALYSIS = ~"estimator = wlsmv; 
                          rotation = target(orthogonal);
                          iteration = 10000;",
                          VARIABLE = ~+"CATEGORICAL ARE all;
                          USEVARIABLES = scs_05 scs_12 scs_19 scs_23 
                          scs_26 scs_03 scs_07 scs_10 scs_15 scs_09 
                          scs_14 scs_17 scs_22 scs_01r scs_08r scs_11r 
                          scs_16r scs_21r scs_04r scs_13r scs_18r 
                          scs_25r scs_02r scs_06r scs_20r scs_24r"
)
fit.s2_scs_4b.WMLSMV <- mplusModeler(s2_scs_4b.WLSMV,"s2_4b_data.dat","s2_scs_4b.WLSMV.inp",run=1)

## 1.2.5a ####
fit.s2_scs_5a.ML <- cfa(s1_scs_5a,S2_data,estimator="ML",std.lv=TRUE,orthogonal=TRUE,missing="fiml")
summary(fit.s2_scs_5a.ML,standardized=TRUE,fit.measures=TRUE)
c(fitmeasures(fit.s2_scs_5a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s2_scs_5a.ML))

## 1.2.5b ####
s2_scs_5b.ML <- mplusObject(
  TITLE = "MplusAutomation Study 1 Model 5b: Bifactor 2corr+6-Factor ESEM ML;",
  ANALYSIS = "
  estimator = ml; 
  rotation = target(orthogonal);
  iteration = 10000;",
  MODEL = "
  cs BY scs_05* scs_12 scs_19 scs_23 scs_26
  scs_03 scs_07 scs_10 scs_15      
  scs_09 scs_14 scs_17 scs_22; 
  rus BY scs_01r* scs_08r scs_11r scs_16r scs_21r
  scs_04r scs_13r scs_18r scs_25r
  scs_02r scs_06r scs_20r scs_24r; 
  cs@1; rus@1;
  
  sk BY scs_05 scs_12 scs_19 scs_23 scs_26
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  sj BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r scs_08r scs_11r  scs_16r scs_21r 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  ch BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03 scs_07 scs_10 scs_15      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  is BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r scs_13r scs_18r scs_25r 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  mi BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09 scs_14 scs_17 scs_22 
  scs_02r~0 scs_06r~0 scs_20r~0 scs_24r~0 (*1); 
  oi BY scs_05~0 scs_12~0 scs_19~0 scs_23~0 scs_26~0
  scs_01r~0 scs_08r~0 scs_11r~0   scs_16r~0 scs_21r~0 
  scs_03~0 scs_07~0 scs_10~0 scs_15~0      
  scs_04r~0 scs_13r~0 scs_18r~0 scs_25r~0 
  scs_09~0 scs_14~0 scs_17~0 scs_22~0 
  scs_02r scs_06r scs_20r scs_24r (*1);
  
  cs WITH sk-oi@0; 
  rus WITH sk-oi@0;",
  OUTPUT = "stdyx;",
  rdata = S2_data,
  usevariables = c("scs_05","scs_12","scs_19","scs_23","scs_26","scs_03","scs_07","scs_10","scs_15","scs_09","scs_14","scs_17","scs_22","scs_01r","scs_08r","scs_11r","scs_16r","scs_21r","scs_04r","scs_13r","scs_18r","scs_25r","scs_02r","scs_06r","scs_20r","scs_24r")
)
s2_scs_5b.WLSMV <- update(s2_scs_5b.ML,
                          TITLE = ~"MplusAutomation Study 1 Model 4b: Bifactor 2corr+6-Factor ESEM WLSMV;",
                          ANALYSIS = ~"estimator = wlsmv; 
                          rotation = target(orthogonal);
                          iteration = 10000;",
                          VARIABLE = ~+"CATEGORICAL ARE all;
                          USEVARIABLES = scs_05 scs_12 scs_19 scs_23 
                          scs_26 scs_03 scs_07 scs_10 scs_15 scs_09 
                          scs_14 scs_17 scs_22 scs_01r scs_08r scs_11r 
                          scs_16r scs_21r scs_04r scs_13r scs_18r 
                          scs_25r scs_02r scs_06r scs_20r scs_24r"
)
fit.s2_scs_5b.WMLSMV <- mplusModeler(s2_scs_5b.WLSMV,"s2_5b_data.dat","s2_scs_5b.WLSMV.inp",run=1)

## 1.2.7a ####
fit.s2_scs_7a.ML <- cfa(s1_scs_7a,S2_data,estimator="ML",std.lv=TRUE,missing="fiml")
summary(fit.s2_scs_7a.ML,standardized=TRUE,rsquare=TRUE)
c(fitmeasures(fit.s2_scs_7a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s2_scs_7a.ML))
anova(fit.s2_scs_5a.ML,fit.s2_scs_7a.ML)

#### 1.3. Study 3 - Factor Structure of the SCS (only Models 5 & 7) ####
## 1.3.5a ####
fit.s3_scs_5a.ML <- cfa(s1_scs_5a,S3_data,estimator="ML",std.lv=TRUE,orthogonal=TRUE,missing="fiml")
summary(fit.s3_scs_5a.ML,standardized=TRUE,fit.measures=TRUE)
c(fitmeasures(fit.s3_scs_5a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s3_scs_5a.ML))

## 1.3.7a ####
fit.s3_scs_7a.ML <- cfa(s1_scs_7a,S3_data,estimator="ML",std.lv=TRUE,missing="fiml")
summary(fit.s3_scs_7a.ML,standardized=TRUE,rsquare=TRUE)
c(fitmeasures(fit.s3_scs_7a.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s3_scs_7a.ML))
anova(fit.s3_scs_5a.ML,fit.s3_scs_7a.ML)

##### 2. Remodelling SCS #####
## 2.8. CFA 2-factor + bifactor with only CH ####
scs_8 <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CS~~RUS


CH =~ scs_03+ scs_07+ scs_10+ scs_15
'
fit.s1_scs_8.ML <- cfa(scs_8,S1_data,estimator="ML",orthogonal=TRUE,std.lv=TRUE)
summary(fit.s1_scs_8.ML,standardized=TRUE,fit.measures=TRUE)
c(fitmeasures(fit.s1_scs_8.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s1_scs_8.ML))
fit.s2_scs_8.ML <- cfa(scs_8,S2_data,estimator="ML",orthogonal=TRUE,std.lv=TRUE)
summary(fit.s2_scs_8.ML,standardized=TRUE,fit.measures=TRUE)
c(fitmeasures(fit.s2_scs_8.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s2_scs_8.ML))
fit.s3_scs_8.ML <- cfa(scs_8,S3_data,estimator="ML",orthogonal=TRUE,std.lv=TRUE)
summary(fit.s3_scs_8.ML,standardized=TRUE,fit.measures=TRUE)
c(fitmeasures(fit.s3_scs_8.ML)[c(4,3,5,9,10,23:25)],pcfi_ml.lavaan(fit.s3_scs_8.ML))
# 2.9. CFA bifactor with only CH 1 vs. 2 g-factors
scs_9 <- 'SC =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22+
scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15
'
fit.s1_scs_9.ML <- cfa(scs_9,S1_data,estimator="ML",orthogonal=TRUE,std.lv=TRUE)
fit.s2_scs_9.ML <- cfa(scs_9,S2_data,estimator="ML",orthogonal=TRUE,std.lv=TRUE)
fit.s3_scs_9.ML <- cfa(scs_9,S3_data,estimator="ML",orthogonal=TRUE,std.lv=TRUE)
anova(fit.s1_scs_9.ML,fit.s1_scs_8.ML)
anova(fit.s2_scs_9.ML,fit.s2_scs_8.ML)
anova(fit.s3_scs_9.ML,fit.s3_scs_8.ML)
##### 3. Big Five Models and discriminant validity #####
#### 3.1. Big Five NEO-PI-R in Study 1 and disriminant validity of the simplified bifactor SCS model ####
## N1=Anxiety excluding 121,181,01 ####
NEO_N1 <-
  "NEON1 =~ Neo_91+Neo_61+Neo_31+Neo_151+Neo_211"
fit_NEO_N1 <- cfa(NEO_N1 , data = S1_data)
summary(fit_NEO_N1, fit.measures = TRUE, standardized=TRUE)
#CFI= 1, RMSEA=.007

## N2=Angry Hostility ####
NEO_N2 <-
  "NEON2 =~ Neo_36+Neo_06+Neo_96+Neo_66+Neo_156+Neo_126+Neo_186+Neo_216"
fit_NEO_N2 <- cfa(NEO_N2 , data = S1_data)
summary(fit_NEO_N2, fit.measures = TRUE, standardized=TRUE)
#CFI= .87, RMSEA=.11

## N3=Depression without 11, 71 ####
NEO_N3 <-
  "NEON3   =~ Neo_41+Neo_101+Neo_131+Neo_161+Neo_191+Neo_221"
fit_NEO_N3 <- cfa(NEO_N3 , data = S1_data)
summary(fit_NEO_N3, fit.measures = TRUE, standardized=TRUE)
#CFI= .99, RMSEA=.058

## N4=Self-Consciousness without 46, 106, 226 ####
NEO_N4 <-
  "NEON4 =~ Neo_16+Neo_76+Neo_136+Neo_166+Neo_196"
fit_NEO_N4 <- cfa(NEO_N4 , data = S1_data)
summary(fit_NEO_N4, fit.measures = TRUE, standardized=TRUE)
#CFI= .977, RMSEA=.077

## N5=Impulsivity without 81, 231, 21, 141 ####
NEO_N5 <-
  "NEON5 =~ Neo_51+Neo_111+Neo_171+Neo_201"
fit_NEO_N5 <- cfa(NEO_N5 , data = S1_data)
summary(fit_NEO_N5, fit.measures = TRUE, standardized=TRUE)
#CFI= .985, RMSEA=.088

## N6=Vulnerability ####
NEO_N6 <-
  "NEON6 =~ Neo_56+Neo_26+Neo_116+Neo_86+Neo_176+Neo_146+Neo_206+Neo_236"
fit_NEO_N6 <- cfa(NEO_N6 , data = S1_data)
summary(fit_NEO_N6, fit.measures = TRUE, standardized=TRUE)
#CFI= .804, RMSEA=.147

## 3.1.2. Anx and Dep predicting all SCS ####
s1_scs_8_neoN1_3 <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON1 =~ Neo_91+Neo_61+Neo_31+Neo_151+Neo_211
NEON3 =~ Neo_41+Neo_101+Neo_131+Neo_161+Neo_191+Neo_221

CS ~ NEON1+NEON3
RUS ~ NEON1+NEON3
CH ~ NEON1+NEON3
' 
fit.s1_scs_8_neoN1_3 <- sem(s1_scs_8_neoN1_3,S1_data)
summary(fit.s1_scs_8_neoN1_3,standardized=TRUE,rsquare=TRUE)
fitmeasures(fit.s1_scs_8_neoN1_3)[c(4,3,5,9,10,23:25)]

#### 3.2. Big Five NEO-PI-R in Study 2 and disriminant validity of the simplified bifactor SCS model ####
# NEON1 model S2####
s2.neon <- '
NEON1 =~ neo2+neo3+neo4+neo6+neo8
NEON2 =~ neo9+neo10+neo11+neo12+neo13+neo14+neo15+neo16
NEON3 =~ neo18+neo20+neo21+neo22+neo23+neo24
NEON4 =~ neo25+neo27+neo30+neo31+neo32
NEON5 =~ neo34+neo36+neo38+neo39
NEON6 =~ neo41+neo42+neo43+neo44+neo45+neo46+neo47+neo48'
fit.s2.neon <- cfa(s2.neon, S2_data,missing="fiml")
summary(fit.s2.neon,standardized=TRUE)
s2.neoN <- '
NEON =~ neo2+neo3+neo4+neo6+neo8+
        neo9+neo10+neo11+neo12+neo13+neo14+neo15+neo16+
neo18+neo20+neo21+neo22+neo23+neo24+
neo25+neo27+neo30+neo31+neo32+
neo34+neo36+neo38+neo39+
neo41+neo42+neo43+neo44+neo45+neo46+neo47+neo48'
fit.s2.neoN <- cfa(s2.neoN, S2_data,missing="fiml")
summary(fit.s2.neoN,standardized=TRUE,fit.measures=TRUE)

# NOTE: here, I assume that the item codes in Neff et al.'s online data are structured by facets. Specifically, that NEON1 is neo1:neo8, NEON2 is neo9:neo16, etc. ####
# The problematically large loadings might indicate that factors are actually loading on 1 item per facet. 
# NEON2 model S2####
s2.neon2 <- '
NEON1 =~ neo7+neo13+neo19+neo31+neo43
NEON2 =~ neo2+neo8+neo14+neo20+neo26+neo32+neo38+neo44
NEON3 =~ neo9+neo21+neo27+neo33+neo39+neo45
NEON4 =~ neo4+neo16+neo34+neo40+neo46
NEON5 =~ neo11+neo23+neo35+neo41
NEON6 =~ neo6+neo12+neo18+neo24+neo30+neo36+neo42+neo48
'
fit.s2.neon2 <- cfa(s2.neon2, S2_data,missing="fiml")
summary(fit.s2.neon2,standardized=TRUE)

s2.neon2_ho <- '
NEON1 =~ neo7+neo13+neo19+neo31+neo43
NEON2 =~ neo2+neo8+neo14+neo20+neo26+neo32+neo38+neo44
NEON3 =~ neo9+neo21+neo27+neo33+neo39+neo45
NEON4 =~ neo4+neo16+neo34+neo40+neo46
NEON5 =~ neo11+neo23+neo35+neo41
NEON6 =~ neo6+neo12+neo18+neo24+neo30+neo36+neo42+neo48

NEON =~ NEON1+NEON2+NEON3+NEON4+NEON5+NEON6
'
fit.s2.neon2_ho <- cfa(s2.neon2_ho, S2_data,missing="fiml")
summary(fit.s2.neon2_ho,standardized=TRUE)

# NOTE: here, I assume that the item codes in Neff et al.'s online data are structured as in the NEO-PI_R. Specifically, that NEON1 is neo1,neo7,neo13, etc., NEON2 is neo2,neo14,neo20, etc., and so on ####
# It is less than clear, which structure is true, although these models seem correct compared to the NEON1 models. Must be discussed with editor Kandler ####
## 3.2.2. Anx and Dep predicting all SCS ####
s2_scs_8_neoN1_3 <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON1 =~ neo7+neo13+neo19+neo31+neo43
NEON3 =~ neo9+neo21+neo27+neo33+neo39+neo45

CS ~ NEON1+NEON3
RUS ~ NEON1+NEON3
CH ~ NEON1+NEON3
' 
fit.s2_scs_8_neoN1_3 <- sem(s2_scs_8_neoN1_3,S2_data,missing="fiml")
summary(fit.s2_scs_8_neoN1_3,standardized=TRUE,rsquare=TRUE)

##### 4. Incremental Predictive Validity of SCS over (facets of) neuroticism and extraversion ####
#### 4.1. All facets of neuroticism and extaversion predicting life satisfaction; next step + SCS-Factors ####
## 4.1.1 Study 1 neone -> swl ####
s1.iv_neoNE_swl <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON1 =~ Neo_91+Neo_61+Neo_31+Neo_151+Neo_211
NEON3 =~ Neo_41+Neo_101+Neo_131+Neo_161+Neo_191+Neo_221
NEOE1 =~ Neo_02+Neo_62+Neo_122+Neo_152+Neo_182+Neo_212
NEOE2 =~ Neo_07+Neo_37+Neo_67+Neo_127+Neo_217+Neo_187
NEOE6 =~ Neo_57+Neo_117+Neo_177+Neo_237

SWL =~ swls_1+swls_2+swls_3+swls_4+swls_5

SWL~~0*RUS+0*CS+0*CH

SWL ~ NEON1+NEON3+NEOE1+NEOE2+NEOE6
' 
fit.s1.iv_neoNE_swl <- sem(s1.iv_neoNE_swl,S1_data)
summary(fit.s1.iv_neoNE_swl,standardized=TRUE,rsquare=TRUE)


## 4.1.2 Study 1 neone+rus -> swl ####
s1.iv_neoNE_rus_swl <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON1 =~ Neo_91+Neo_61+Neo_31+Neo_151+Neo_211
NEON3 =~ Neo_41+Neo_101+Neo_131+Neo_161+Neo_191+Neo_221
NEOE1 =~ Neo_02+Neo_62+Neo_122+Neo_152+Neo_182+Neo_212
NEOE2 =~ Neo_07+Neo_37+Neo_67+Neo_127+Neo_217+Neo_187
NEOE6 =~ Neo_57+Neo_117+Neo_177+Neo_237

SWL =~ swls_1+swls_2+swls_3+swls_4+swls_5

SWL~~0*CS+0*CH

SWL ~ NEON1+NEON3+NEOE1+NEOE2+NEOE6+RUS
' 
fit.s1.iv_neoNE_rus_swl <- sem(s1.iv_neoNE_rus_swl,S1_data)
summary(fit.s1.iv_neoNE_rus_swl,standardized=TRUE,rsquare=TRUE)

## 4.1.3 Study 1 neone+rus+cs -> swl ####
s1.iv_neoNE_ruscs_swl <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON1 =~ Neo_91+Neo_61+Neo_31+Neo_151+Neo_211
NEON3 =~ Neo_41+Neo_101+Neo_131+Neo_161+Neo_191+Neo_221
NEOE1 =~ Neo_02+Neo_62+Neo_122+Neo_152+Neo_182+Neo_212
NEOE2 =~ Neo_07+Neo_37+Neo_67+Neo_127+Neo_217+Neo_187
NEOE6 =~ Neo_57+Neo_117+Neo_177+Neo_237

SWL =~ swls_1+swls_2+swls_3+swls_4+swls_5

SWL~~0*CH

SWL ~ NEON1+NEON3+NEOE1+NEOE2+NEOE6+RUS+CS
' 
fit.s1.iv_neoNE_ruscs_swl <- sem(s1.iv_neoNE_ruscs_swl,S1_data)
summary(fit.s1.iv_neoNE_ruscs_swl,standardized=TRUE,rsquare=TRUE)

## 4.1.4 Study 1 neone+rus+cs+ch -> swl ####
s1.iv_neoNE_ruscsch_swl <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON1 =~ Neo_91+Neo_61+Neo_31+Neo_151+Neo_211
NEON3 =~ Neo_41+Neo_101+Neo_131+Neo_161+Neo_191+Neo_221
NEOE1 =~ Neo_02+Neo_62+Neo_122+Neo_152+Neo_182+Neo_212
NEOE2 =~ Neo_07+Neo_37+Neo_67+Neo_127+Neo_217+Neo_187
NEOE6 =~ Neo_57+Neo_117+Neo_177+Neo_237

SWL =~ swls_1+swls_2+swls_3+swls_4+swls_5



SWL ~ NEON1+NEON3+NEOE1+NEOE2+NEOE6+RUS+CS+CH
' 
fit.s1.iv_neoNE_ruscsch_swl <- sem(s1.iv_neoNE_ruscsch_swl,S1_data)
summary(fit.s1.iv_neoNE_ruscsch_swl,standardized=TRUE,rsquare=TRUE)

#### 4.3. Factors neuroticism and extraversion predicting happiness; next step + SCS factors ####
## 4.3.1.1 Study 3 neone -> happy ####
s3.iv_neoNE_happy <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON =~ neo1+neo6+neo11+neo16+neo21+neo26+neo31+neo36+neo41+neo46+neo51+neo56
NEOE =~ neo2+neo7+neo12+neo17+neo22+neo27+neo32+neo37+neo42+neo47+neo52+neo57

Happy =~ happy1+happy2+happy3+happy4

Happy~~0*RUS+0*CS+0*CH


Happy ~ NEON+NEOE
' 
fit.s3.iv_neoNE_happy <- sem(s3.iv_neoNE_happy,S3_data,std.lv=TRUE,missing="fiml")

summary(fit.s3.iv_neoNE_happy,standardized=TRUE,rsquare=TRUE)
## 4.3.1.2 Study 3 neone+rus -> happy ####
s3.iv_neoNE_rus_happy <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON =~ neo1+neo6+neo11+neo16+neo21+neo26+neo31+neo36+neo41+neo46+neo51+neo56
NEOE =~ neo2+neo7+neo12+neo17+neo22+neo27+neo32+neo37+neo42+neo47+neo52+neo57

Happy =~ happy1+happy2+happy3+happy4

Happy~~0*CS+0*CH


Happy ~ NEON+NEOE+RUS

' 
fit.s3.iv_neoNE_rus_happy <- sem(s3.iv_neoNE_rus_happy,S3_data,std.lv=TRUE,missing="fiml")

summary(fit.s3.iv_neoNE_rus_happy,standardized=TRUE,rsquare=TRUE)

## 4.3.1.3 Study 3 neone+rus+cs -> happy ####
s3.iv_neoNE_ruscs_happy <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON =~ neo1+neo6+neo11+neo16+neo21+neo26+neo31+neo36+neo41+neo46+neo51+neo56
NEOE =~ neo2+neo7+neo12+neo17+neo22+neo27+neo32+neo37+neo42+neo47+neo52+neo57

Happy =~ happy1+happy2+happy3+happy4

Happy~~0*CH


Happy ~ NEON+NEOE+RUS+CS

' 
fit.s3.iv_neoNE_ruscs_happy <- sem(s3.iv_neoNE_ruscs_happy,S3_data,std.lv=TRUE,missing="fiml")

summary(fit.s3.iv_neoNE_ruscs_happy,standardized=TRUE,rsquare=TRUE)
## 4.3.1.4 Study 3 neon+rus+cs+ch -> happy ####
s3.iv_neoNE_ruscsch_happy <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15

CS~~0*CH
RUS~~0*CH

NEON =~ neo1+neo6+neo11+neo16+neo21+neo26+neo31+neo36+neo41+neo46+neo51+neo56
NEOE =~ neo2+neo7+neo12+neo17+neo22+neo27+neo32+neo37+neo42+neo47+neo52+neo57

Happy =~ happy1+happy2+happy3+happy4


Happy ~ NEON+NEOE+RUS+CS+CH

' 
fit.s3.iv_neoNE_ruscsch_happy <- sem(s3.iv_neoNE_ruscsch_happy,S3_data,std.lv=TRUE,missing="fiml")

summary(fit.s3.iv_neoNE_ruscsch_happy,standardized=TRUE,rsquare=TRUE)

##### 5. Additional exploratory analysis: SCS-factors as facets of neuroticism ####
#### 5.1.Study 1 SCS as a facet of neuroticism ####
s1_scs_asneo <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15
CH ~~ 0*RUS+0*CS+0*NEON1+0*NEON2+0*NEON3+0*NEON4+0*NEON5+0*NEON6+0*NEON

NEON1 =~ Neo_91+Neo_61+Neo_31+Neo_151+Neo_211
NEON2 =~ Neo_36+Neo_06+Neo_96+Neo_66+Neo_156+Neo_126+Neo_186+Neo_216
NEON3 =~ Neo_41+Neo_101+Neo_131+Neo_161+Neo_191+Neo_221
NEON4 =~ Neo_16+Neo_76+Neo_136+Neo_166+Neo_196
NEON5 =~ Neo_51+Neo_111+Neo_171+Neo_201
NEON6 =~ Neo_56+Neo_26+Neo_116+Neo_86+Neo_176+Neo_146+Neo_206+Neo_236

NEON =~ NEON1+NEON2+NEON3+NEON4+NEON5+NEON6+RUS+CS

' 
fit.s1_scs_asneo <- cfa(s1_scs_asneo,S1_data,std.lv=TRUE)
summary(fit.s1_scs_asneo,standardized=TRUE,rsquare=TRUE)
fitmeasures(fit.s1_scs_asneo)[c(4,3,5,9,10,23:25)]


#### 5.2.Study 2 SCS as a facet of neuroticism ####
s2_scs_asneo <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15
CH ~~ 0*RUS+0*CS+0*NEON

NEON1 =~ neo7+neo13+neo19+neo31+neo43
NEON2 =~ neo2+neo8+neo14+neo20+neo26+neo32+neo38+neo44
NEON3 =~ neo9+neo21+neo27+neo33+neo39+neo45
NEON4 =~ neo4+neo16+neo34+neo40+neo46
NEON5 =~ neo11+neo23+neo35+neo41
NEON6 =~ neo6+neo12+neo18+neo24+neo30+neo36+neo42+neo48

NEON =~ NEON1+NEON2+NEON3+NEON4+NEON5+NEON6+RUS+CS
' 
fit.s2_scs_asneo <- cfa(s2_scs_asneo,S2_data,std.lv=TRUE)
summary(fit.s2_scs_asneo,standardized=TRUE)

s2_scs_asneo2 <- 'CS =~ scs_05+ scs_12+ scs_19+ scs_23+ scs_26+
scs_03+ scs_07+ scs_10+ scs_15+
scs_09+ scs_14+ scs_17+ scs_22

RUS =~ scs_01r+ scs_08r+ scs_11r+ scs_16r+ scs_21r+
scs_04r+ scs_13r+ scs_18r+ scs_25r+
scs_02r+ scs_06r+ scs_20r+ scs_24r

CH =~ scs_03+ scs_07+ scs_10+ scs_15
CH ~~ 0*RUS+0*CS+0*NEON

NEON1 =~ neo7+neo13+neo19+neo31+neo43
NEON2 =~ neo2+neo8+neo14+neo20+neo26+neo32+neo38+neo44
NEON3 =~ neo9+neo21+neo27+neo33+neo39+neo45
NEON4 =~ neo4+neo16+neo34+neo40+neo46
NEON5 =~ neo11+neo23+neo35+neo41
NEON6 =~ neo6+neo12+neo18+neo24+neo30+neo36+neo42+neo48

NEON =~ NEON1+NEON2+NEON3+NEON4+NEON5+NEON6
+RUS+CS

' 
fit.s2_scs_asneo2 <- cfa(s2_scs_asneo2,S2_data,std.lv=TRUE,missing="fiml")
summary(fit.s2_scs_asneo2,standardized=TRUE)

