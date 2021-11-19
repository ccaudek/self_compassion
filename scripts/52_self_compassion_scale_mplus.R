## ------------------------------------------------------------------
## 51_self_compassion_scale_mplus.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: "Tue Feb 23 15:01:50 2021"
## ------------------------------------------------------------------

# The acceptable range for the SRMR index is between 0 and 0.08, 
# see Hu and Bentler (1999). Since most of the terms in the SRMR 
# definition are simply MSE of estimated and observed correlations, 
# the value of 0.08 can be interpreted as follows. If all correlations 
# are equally misfitted, the model is approximately well fitting if 
# the estimated and the observed correlations are less than 0.08 apart.


# "a cutoff value close to .95 for TLI, BL89, CFI, RNI, and Gamma Hat; 
# a cutoff value close to .90 for Mc; a cutoff value close to .08 for SRMR; 
# and a cutoff value close to .06 for RMSEA are needed before we can conclude 
# that there is a relatively good fit between the hypothesized model and the 
# observed data"

# Hu, L. & Bentler, P. (1999). Cutoff criteria for fit indices in 
# covariance structure analysis: conventional criteria versus new 
# alternatives. Structural Equation Modeling, 6, 1-55.

library("here")
suppressPackageStartupMessages(library("tidyverse")) 
suppressPackageStartupMessages(library("tidyr"))
suppressPackageStartupMessages(library("ggthemes"))
suppressPackageStartupMessages(library("papaja"))
suppressPackageStartupMessages(library("lavaan"))
suppressPackageStartupMessages(library("mice"))
suppressPackageStartupMessages(library("corrplot"))
suppressPackageStartupMessages(library("readxl"))
suppressPackageStartupMessages(library("mvoutlier"))
suppressPackageStartupMessages(library("GPArotation"))
suppressPackageStartupMessages(library("psych"))
suppressPackageStartupMessages(library("janitor"))
suppressPackageStartupMessages(library("bootnet"))
suppressPackageStartupMessages(library("igraph"))
# Please note: The EGA library is not the same as the ega library in CRAN and need to be installed
# library("devtools")
# devtools::install_github('hfgolino/EGA')
suppressPackageStartupMessages(library("EGAnet"))
suppressPackageStartupMessages(library("qgraph"))

options(max.print=99999999)

source(here("libraries", "self_compassion_fnc.R"))


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

# Read the 26 items of the SCS ----

d <- readRDS(here("data", "processed", "SCS_26item_data.Rds"))

# Descriptives ----

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


rio::export(d, "self_compassion.csv")
rio::export(d, "self_compassion.txt")

items <- c("u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9",
           "u10", "u11", "u12", "u13", "u14", "u15", "u16",
           "u17", "u18", "u19", "u20", "u21", "u22", "u23", 
           "u24", "u25", "u26")

# 6-factor model ----

mod_6factor <- '
  sj =~ u1 + u8 + u11 + u16 + u21
  is =~ u4 + u13 + u18 + u25
  oi =~ u2 + u6 + u20 + u24
  
  sk =~ u5 + u12 + u19 + u23 + u26
  cu =~ u3 + u7 + u10 + u15
  mi =~ u9 + u14 + u17 + u22
'


fit_6factor <- lavaan::cfa(
  mod_6factor,
  data = d,
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE,
  orthogonal = FALSE
)

summary(
  fit_6factor, 
  standardized=TRUE, 
  fit.measures=TRUE
)


# 2-factor model ----

mod_2factor <- '
  unc =~ u1 + u8 + u11 + u16 + u21 +
         u4 + u13 + u18 + u25 +
         u2 + u6 + u20 + u24
  
  com =~ u5 + u12 + u19 + u23 + u26 +
         u3 + u7 + u10 + u15 +
         u9 + u14 + u17 + u22
'

fit_2factor <- lavaan::cfa(
  mod_2factor,
  data = d,
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE,
  orthogonal = FALSE
)

summary(
  fit_2factor, 
  standardized=TRUE, 
  fit.measures=TRUE
)



# Bifactor model ----

mod_bifactor <- '
  sj =~ u1 + u8 + u11 + u16 + u21
  is =~ u4 + u13 + u18 + u25
  oi =~ u2 + u6 + u20 + u24
  
  sk =~ u5 + u12 + u19 + u23 + u26
  cu =~ u3 + u7 + u10 + u15
  mi =~ u9 + u14 + u17 + u22
  
  g   =~ u1 + u8 + u11 + u16 + u21 + 
         u4 + u13 + u18 + u25 + 
         u2 + u6 + u20 + u24 + 
         u5 + u12 + u19 + u23 + u26 + 
         u3 + u7 + u10 + u15 + 
         u9 + u14 + u17 + u22
'


fit_bifactor <- lavaan::cfa(
  mod_bifactor,
  data = d,
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE,
  orthogonal = TRUE
)

summary(
  fit_bifactor, 
  standardized=TRUE, 
  fit.measures=TRUE
)



# 2-factor ESEM ----

d1 <- read.table(here("scripts", "_mplus", "scsallsubjects.dat"))

new_item_names <- c("u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
                    "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
                    "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
                    "c22", "c23", "u24", "u25", "c26")

colnames(d) <- new_item_names
# colnames(d) <- items

d_s <- scale(d)

f2_efa <- fa(d_s, nfact = 2, rotate = "geominQ", fm = "ml")
fa.diagram(f2_efa)

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

m_esem <- paste(terms, collapse = "\n")
m_esem

fit_ESEM <- lavaan::cfa(
  m_esem, 
  data = d, 
  verbose = FALSE, 
  estimator = "WLSMV",
  ordered = new_item_names
)

summary(
  fit_ESEM, 
  standardized=TRUE, 
  fit.measures=TRUE
)

mi <- modindices(fit_ESEM)
mi[mi$op == "=~",]

# Adding residual correlations based on modification indices.
# modification indices and analyzed correlations between items’ residuals in order to improve the goodness of fit.
m_esem1 <- 
  c("F1=~ 0.51*u1+0.81*u2+-0.22*c3+0.8*u4+-0.13*c5+0.8*u6+-0.33*c7+0.7*u8+-0.08*c9+-0.23*c10+0.69*u11+0.03*c12+0.73*u13+0.16*c14+-0.03*c15+0.73*u16+0.19*c17+0.79*u18+0.05*c19+0.64*u20+0.56*u21+0.06*c22+0.25*c23+0.63*u24+0.77*u25+0.08*c26\nF2=~ -0.05*u1+0.04*u2+0.44*c3+0.04*u4+0.66*c5+0.02*u6+0.51*c7+-0.01*u8+0.49*c9+0.56*c10+-0.02*u11+0.7*c12+0.03*u13+0.49*c14+0.67*c15+0.02*u16+0.6*c17+-0.03*u18+0.72*c19+-0.06*u20+-0.01*u21+0.65*c22+0.59*c23+0.02*u24+0.05*u25+0.66*c26\nc9~~NA*c14\nc7~~NA*c10\nu13~~NA*u18")

fit_ESEM <- lavaan::cfa(
  m_esem1, 
  data = d, 
  verbose = FALSE, 
  estimator = "WLSMV",
  ordered = new_item_names
)

summary(
  fit_ESEM, 
  standardized=TRUE, 
  fit.measures=TRUE
)





# # Bifactor version 2 ----
# 
# SCS_corr_mat <- lavaan::lavCor(d, ordered = names(d))
# 
# ## Conduct tranditional Schmid-Leiman procedure, do not round output
# SLOutput <- 
#   SchmidLeiman(R          = SCS_corr_mat,   ## Corr matrix
#                numFactors = c(2, 1),        ## Number of factors per level
#                facMethod  = "fals",         ## Fac extraction method
#                rotate     = "geominQ",      ## Rotational criterion
#                rotateControl = 
#                  list(numberStarts = 100,   ## Random start configs
#                       delta        = .01,   ## Geomin tuning param
#                       standardize  = "none"))$B    
# 
# # ## Add column names to the SL Bifactor output
# # colnames(SLOutput) <- c("Performance", "Boss", "Peer", "Subordinate", "Self")
# # 
# # ## Sort the row order into 'hallow staircase' pattern
# # SortedSL <- faSort(fmat     = SLOutput,
# #                    BiFactor = TRUE)$loadings
# # 
# # ## Compute the item communalities
# # Communality <- apply(SortedSL^2, 1, sum)
# # 
# # ## Compute the item explained common variance (I-ECV)
# # IECV <- apply(SortedSL^2, 1, function(x) x[1] / sum(x))
# # 
# # ## Add columns for the communality and and I-ECV values
# # SL <- cbind(SortedSL, 
# #             Communality, 
# #             IECV) %>% round(2)
# # 
# # ## Give clear row and column names
# # dimnames(SL) <- 
# #   list(rep(c("Technical", "Interpersonal", "Leadership"), 4), 
# #        c("Performance", "Boss", "Peer", "Subordinate", "Self", 
# #          "$h^2$", "I-ECV"))
# 
# 
# f_loadmat <- SLOutput
# rownames(f_loadmat) <- colnames(d[, 1:26])
# 
# # This will turn the loading matrix into a lavaan-compatible equation set. 
# terms <- vector()
# for (i in 1:3) {
#   terms[i] <-
#     paste0("F",i,"=~ ", 
#            paste0(c(f_loadmat[,i]), "*", 
#                   names(f_loadmat[,1]), collapse = "+")
#     )
# }
# 
# m_esem <- paste(terms, collapse = "\n")
# m_esem
# 
# 
# # m_esem <- "F1=~ 0.237489281801502*u1+0.429887259500751*u2+0.125541390817628*u3+0.425429144546642*u4+0.258130199595277*u5+0.409125142187821*u6+0.0808427342147022*u7+0.356082044998831*u8+0.217548495279002*u9+0.16541897318553*u10+0.341800878982743*u11+0.360741223339561*u12+0.379204200545866*u13+0.343121430473647*u14+0.332359301659812*u15+0.380014704177943*u16+0.410325824368322*u17+0.38226462047563*u18+0.378939476044336*u19+0.297448970007021*u20+0.277595395056909*u21+0.363413056718487*u22+0.427395668678096*u23+0.350260328410562*u24+0.412127780691352*u25+0.373392400410804*u26\nF2=~ 0.490249882328467*u1+0.737663354482553*u2+-0.21200723535891*u3+0.726176813922544*u4+-0.119889136321777*u5+0.726178413247306*u6+-0.342876666743195*u7+0.665523107407188*u8+-0.0693593054240087*u9+-0.241623443104029*u10+0.655509021751017*u11+0.032897501331412*u12+0.654515303711587*u13+0.154415759871224*u14+-0.0313597448195801*u15+0.674323686490117*u16+0.181107410509167*u17+0.710064711711802*u18+0.0550508065984046*u19+0.58159747356244*u20+0.529031823832191*u21+0.0579294615716469*u22+0.24949931498909*u23+0.602138907360023*u24+0.697546443489501*u25+0.0913568390859534*u26\nF3=~ -0.0592430910865683*u1+0.0425163947157969*u2+0.439845694483885*u3+0.0459121372120675*u4+0.588356042306952*u5+0.0163212625828756*u6+0.489593887852244*u7+-0.0192885553903295*u8+0.464176613297618*u9+0.541833626495867*u10+-0.0351926058836949*u11+0.62179274810147*u12+0.0336824307837687*u13+0.468297255356066*u14+0.634541139201513*u15+0.0153449883655846*u16+0.563571320181907*u17+-0.0163127822491008*u18+0.632666493339644*u19+-0.0417730017023263*u20+-0.0252385596502421*u21+0.601609757560617*u22+0.526158577233593*u23+0.0335301177951796*u24+0.0504025628403547*u25+0.586293365396466*u26\nF1~~0*F2\nF1~~0*F3\nF2~~0*F3\nc9~~NA*c14"
# 
# d_s <- scale(d)
# 
# fit_3 <- lavaan::cfa(
#   m_esem, 
#   data = d_s, 
#   verbose = FALSE, 
#   estimator = "WLSMV",
#   ordered = items
# )
# 
# 
# summary(
#   fit_3, 
#   standardized=TRUE, 
#   fit.measures=TRUE
# )



# Network analysis ----


new_item_names <- c("u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
                "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
                "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
                "c22", "c23", "u24", "u25", "c26")

names(d) <- new_item_names

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

Groups <- c("sj", "oi", "cu", "is", "sk", "oi", "cu", "sj",
            "mi", "cu", "sj", "sk", "is", "mi", "cu", "sj",
            "mi", "is", "sk", "oi", "sj", "mi", "sk", "oi",
            "is", "sk")

qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
       sampleSize = nrow(d), groups = Groups, palette = "colorblind")


ega.rand <- EGA(data = d, model = "glasso", palette = "colorblind")

set.seed(0)
boot_scs <- bootEGA(
  data = d, 
  model = "glasso",
  iter = 1000, 
  type = "resampling",
  plot.typicalStructure = FALSE
)

boot_scs$summary.table
# n.Boots median.dim    SE.dim   CI.dim Lower.CI Upper.CI Lower.Quantile Upper.Quantile
# 1    1000          4 0.5975559 1.172609 2.827391 5.172609              3              5

tab1 <- boot_scs$summary.table

knitr::kable(
  tab1, 
  digits = 2,
  booktabs = TRUE, 
  caption = "bootEGA Summary Table") %>%
  kableExtra::kable_styling(
    latex_options = c("HOLD_position"),
    position = "center"
  )



# lslx

sj =~ u1 + u8 + u11 + u16 + u21
is =~ u4 + u13 + u18 + u25
oi =~ u2 + u6 + u20 + u24

sk =~ u5 + u12 + u19 + u23 + u26
cu =~ u3 + u7 + u10 + u15
mi =~ u9 + u14 + u17 + u22




items_ord <- c(
  "u1", "u8", "u11", "u16", "u21", "u4", "u13", "u18", 
  "u25", "u2", "u6", "u20", "u24",
  "c5", "c12", "c19", "c23", "c26", "c3", "c7", "c10",
  "c15", "c9", "c14", "c17", "c22"
)


model_fa <- "

  sj :=> u1 + u8 + u11 + u16 + u21
  sj :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  is :=> u4 + u13 + u18 + u25
  is :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  oi :=> u2 + u6 + u20 + u24
  oi :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  sk :=> c5 + c12 + c19 + c23 + c26
  sk :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  cu :=> c3 + c7  + c10 + c15
  cu :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  mi :=> c9 + c14 + c17 + c22
  mi :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  sj <=> fix(1) * sj
  is <=> fix(1) * is
  oi <=> fix(1) * oi
  sk <=> fix(1) * sk
  cu <=> fix(1) * cu
  mi <=> fix(1) * mi

"

# initialize lslx object via specified model and raw data
lslx_fa <- lslx$new(
  model = model_fa, 
  data = d
)

# fit with mcp under specified penalty levels and convexity levels
lslx_fa$fit(
  penalty_method = "mcp", 
  lambda_grid = seq(.01, .60, .01), 
  delta_grid = c(1.5, 3.0, Inf)
)

# summarize fitting result under penalty level selected by 'bic'
lslx_fa$summarize(selector = "bic")


lslx_fa$plot_numerical_condition()
lslx_fa$plot_information_criterion()
lslx_fa$plot_fit_index()
lslx_fa$plot_coefficient(block = "y<-f")
lslx_fa$extract_coefficient_matrix(selector = "bic", block = "y<-f")









lslx_fa$fit(
  penalty_method = "none"
)




mi :~> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22



model_fa <- "
  unc :=> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24
  com :=> c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  unc :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24 + c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  com :~> u1 + u8 + u11 + u16 + u21 + u4 + u13 + u18 + u25 + u2 + u6 + u20 + u24 + c5 + c12 + c19 + c23 + c26 + c3 + c7 + c10 + c15 + c9 + c14 + c17 + c22
  unc <=> 1 * unc
  com <=> 1 * com
"

