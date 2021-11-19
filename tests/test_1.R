

data.frame(Items, CS, RUS) %>% 
  arrange(match(Items, new_order)) %>% 
  kable("latex", booktabs = TRUE) %>%
  #kable_paper("striped", full_width = FALSE) %>%
  pack_rows("Self-kindness", 1, 5) %>%  
  pack_rows("Mindfulness", 6, 9) %>% 
  pack_rows("Common Humanity", 10, 13) %>%
  pack_rows("Self-judgment", 14, 18) %>% 
  pack_rows("Isolation", 19, 22) %>% 
  pack_rows("Over-identification", 23, 26) %>% 
  save_kable("test.pdf")





tbl

latex_tab <- tbl %>% 
  kable("latex", booktabs = TRUE) %>%
  kable_paper("striped", full_width = FALSE) %>%
  pack_rows("Self-kindness", 1, 5) %>%  
  pack_rows("Mindfulness", 6, 9) %>% 
  pack_rows("Common Humanity", 10, 13) %>%
  pack_rows("Self-judgment", 14, 18) %>% 
  pack_rows("Isolation", 19, 22) %>% 
  pack_rows("Over-identification", 23, 26) %>% 
  save_kable("test.pdf")





tbl <- data.frame(Items, CS, RUS) %>% 
  arrange(match(Items, new_order)) 

tbl %>% 
  kable("latex", booktabs = TRUE) %>%
  kable_paper("striped", full_width = FALSE) %>%
  pack_rows("Self-kindness", 1, 5) %>%  
  pack_rows("Mindfulness", 6, 9) %>% 
  pack_rows("Common Humanity", 10, 13) %>%
  pack_rows("Self-judgment", 14, 18) %>% 
  pack_rows("Isolation", 19, 22) %>% 
  pack_rows("Over-identification", 23, 26) %>% 
  save_kable("test.pdf")


# modelParams <- readModels(paste0(the_dir, "model1.out"), what="parameters")$parameters

compareModels(m1, m2, show="all", equalityMargin=c(param=0.0001, pvalue=0.0001), 
              sort="none", showFixed=FALSE, showNS=TRUE, diffTest=FALSE)








# Two-factor ESEM v.1 ----
runModels(paste0(the_dir, "two_fact_esem_v1.inp"))
summaryStats <- readModels(paste0(the_dir, "two_fact_esem_v1.out"), what="summaries")$summaries
c(summaryStats$CFI, summaryStats$TLI, summaryStats$RMSEA_Estimate, summaryStats$SRMR)
# [1] 0.850 0.822 0.120 0.061

# Two-factor bifactor model ----
runModels(paste0(the_dir, "two_fact_bifactor.inp"))
summaryStats <- readModels(
  paste0(the_dir, "two_fact_bifactor.out"), what="summaries")$summaries
c(summaryStats$CFI, summaryStats$TLI, 
  summaryStats$RMSEA_Estimate, summaryStats$SRMR)
# [1] 0.918 0.894 0.093 0.039

# Two-correlated factors bifactor v.1 ----
runModels(paste0(the_dir, "two_cor_fact_bifactor_v1.inp"))
summaryStats <- readModels(paste0(the_dir, "two_fact_bifactor_v1.out"), what="summaries")$summaries
c(summaryStats$CFI, summaryStats$TLI, summaryStats$RMSEA_Estimate, summaryStats$SRMR)
# [1] 0.918 0.894 0.093 0.039

# Two-factor CFA ----
runModels(paste0(the_dir, "two_fact_cfa.inp"))
summaryStats <- readModels(paste0(the_dir, "two_fact_cfa.out"), what="summaries")$summaries
c(summaryStats$CFI, summaryStats$TLI, summaryStats$RMSEA_Estimate, summaryStats$SRMR)
# [1] 0.832 0.817 0.122 0.098


# 1-st order CFA (correlated 6 factors) 

runModels(paste0(the_dir, "model12.inp"))
summaryStats <- readModels(
  paste0(the_dir, "model12.out"), what="summaries")$summaries
c(
  summaryStats$CFI, summaryStats$TLI, 
  summaryStats$RMSEA_Estimate, summaryStats$SRMR
)
# [1] 0.949 0.935 0.072 0.039


# 6 correlated factors, two-bifactor model
runModels(paste0(the_dir, "six_cor_fact_two_bifact.inp"))
summaryStats <- readModels(
  paste0(the_dir, "six_cor_fact_two_bifact.out"), what="summaries")$summaries
c(
  summaryStats$CFI, summaryStats$TLI, 
  summaryStats$RMSEA_Estimate, summaryStats$SRMR
)
# [1] 0.949 0.935 0.072 0.039

# 6 orthogonal factors bifactor model
runModels(paste0(the_dir, "six_ortho_fact_bifactor.inp"))
summaryStats <- readModels(
  paste0(the_dir, "six_ortho_fact_bifactor.out"), what="summaries")$summaries
c(summaryStats$CFI, summaryStats$TLI, 
  summaryStats$RMSEA_Estimate, summaryStats$SRMR)
# [1] 0.757 0.711 0.153 0.118

# 6 correlated factors
runModels(paste0(the_dir, "six_fact.inp"))
summaryStats <- readModels(
  paste0(the_dir, "six_fact.out"), what="summaries")$summaries
c(summaryStats$CFI, summaryStats$TLI, 
  summaryStats$RMSEA_Estimate, summaryStats$SRMR)
# [1] 0.900 0.885 0.096 0.072






prepareMplusData(iris[, -5], paste0(the_dir, "iris.dat"))

runModels(paste0(the_dir, "2-iris-LPA_means_correlated_free_variances.inp"))

m3 <- readModels(paste0(the_dir, "2-iris-LPA_means_correlated_free_variances.out"))

m3$summaries$BIC

m3$parameters[[1]][-nrow(m3$parameters[[1]]), ]


#--------------------------------------------------------------


the_dir <- "/Users/corrado/Desktop/"




prepareMplusData(d, paste0(the_dir, "selfcomp.dat"))

runModels(paste0(the_dir, "six_factor_model.inp"))

m1 <- readModels(paste0(the_dir, "slid_self_compassion.out"))

m1$summaries

m1$parameters[[1]][-nrow(m1$parameters[[1]]), ]


