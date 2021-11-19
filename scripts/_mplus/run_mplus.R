
# Mplus reliability computed from here: 
# https://hectornajera83.github.io/book/Chapter-3.html

library("here")
library("tidyverse")
library("MplusAutomation")
#library("formattable")
library("gt")
library("glue")
library("kableExtra")
options(max.print=999999)

the_dir <- "/Users/corrado/Documents/papers/self_compassion/scripts/_mplus/"


d <- read.table(here("scripts", "_mplus", "scs.dat"), header = FALSE)

new_names <- c(
  "scsj1", "scoi2", "scch3", "scis4", "scsk5", "scoi6", "scch7", 
  "scsj8", "scmi9", "scch10", "scsj11", "scsk12", "scis13", "scmi14", 
  "scch15", "scsj16", "scmi17", "scis18", "scsk19", "scoi20", 
  "scsj21", "scmi22", "scsk23", "scoi24", "scis25", "scsk26"
)

colnames(d) <- new_names


# new_names <- c(
#   "n1", "n2", "p3", "n4", "p5", "n6", "p7", "n8", "p9", "p10", 
#   "n11", "p12", "n13", "p14", "p15", "n16", "p17", "n18", "p19", 
#   "n20", "n21", "p22", "p23", "n24", "n25", "p26",
#   "gender", "where"
# )
# dd <- data_clean
# colnames(dd) <- new_names

prepareMplusData(d, paste0(the_dir, "selfcompassionitems.dat"))


print_summary_stats <- function(summaryStats) {
  c(
    summaryStats$CFI, summaryStats$TLI, 
    summaryStats$RMSEA_Estimate, 
    summaryStats$RMSEA_90CI_LB, summaryStats$RMSEA_90CI_UB, 
    summaryStats$SRMR
  )
}




omega_t <- list()

# m1: 1 factor CFA --------------------------------------------------------


runModels(paste0(the_dir, "m1_one_fact_cfa.inp"))
model_results <- readModels(paste0(the_dir, "m1_one_fact_cfa.out"), 
                            quiet = TRUE)
summary(model_results)
# CFI = 0.614, TLI = 0.581, SRMR = 0.16 
# RMSEA = 0.184, 90% CI [0.181, 0.187], p < .05 = 0 

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

item_names <- lambdas[1:26, 2]


# Reliability
l <- lambdas[1:26, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[1]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[1]]) <- "m1"
omega_t[[1]]
# 0.4409275



# m2: 1 factor ESEM -------------------------------------------------------


runModels(paste0(the_dir, "m2_one_fact_esem.inp"))
model_results <- readModels(paste0(the_dir, "m2_one_fact_esem.out"), 
                            quiet = TRUE)
summary(model_results)
# CFI = 0.614, TLI = 0.581, SRMR = 0.16 
# RMSEA = 0.184, 90% CI [0.181, 0.187], p < .05 = 0 

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

# Reliability
l <- lambdas[1:26, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[2]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[2]]) <- "m2"
omega_t[[2]]
# 0.4409275



# m2a: Two-factor CFA -----------------------------------------------------


runModels(paste0(the_dir, "m2a_two_fact_cfa.inp"))
model_results <- readModels(paste0(the_dir, "m2a_two_fact_cfa.out"),
                            quiet = TRUE)
summary(model_results)
# CFI = 0.832, TLI = 0.817, SRMR = 0.098 
# RMSEA = 0.122, 90% CI [0.118, 0.125], p < .05 = 0 

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

# Reliability
l <- lambdas[1:26, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[3]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[3]]) <- "m2a"
omega_t[[3]]
# 0.9576039




# m2b: Two-factor ESEM ----------------------------------------------------


runModels(paste0(the_dir, "m2b_two_fact_esem.inp"))
model_results <- readModels(paste0(the_dir, "m2b_two_fact_esem.out"),
                            quiet = TRUE)
summary(model_results)
# CFI = 0.85, TLI = 0.822, SRMR = 0.061 
# RMSEA = 0.12, 90% CI [0.116, 0.123], p < .05 = 0 

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

item_names <- lambdas[1:26, 2]

Items <- c(
  "SCSK05",  "SCSK12", "SCSK19", "SCSK23", "SCSK26", 
  "SCSJ01",  "SCSJ08", "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCCH03",  "SCCH07", "SCCH10", "SCCH15", 
  "SCIS04",  "SCIS13", "SCIS18", "SCIS25",
  "SCMI09",  "SCMI14", "SCMI17", "SCMI22", 
  "SCOI02",  "SCOI06", "SCOI20", "SCOI24"
)

CS <- round(lambdas[1:26, 3], 2)
RUS <- round(lambdas[27:52, 3], 2)

new_order <- c(
  "SCSK05", "SCSK12", "SCSK19", "SCSK23", "SCSK26",
  "SCMI09", "SCMI14", "SCMI17", "SCMI22", 
  "SCCH03", "SCCH07", "SCCH10", "SCCH15", 
  "SCSJ01", "SCSJ08", "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCIS04", "SCIS13", "SCIS18", "SCIS25",
  "SCOI02", "SCOI06", "SCOI20", "SCOI24"
)

tbl <- data.frame(Items, CS, RUS) %>% 
  arrange(match(Items, new_order)) 

tbl$grp <- c(
  rep("Self-kindness", 5),
  rep("Mindfulness", 4),
  rep("Common Humanity", 4),
  rep("Self-judgment", 5),
  rep("Isolation", 4),
  rep("Over-identification", 4)
)

tab_2 <-
  tbl %>% 
  group_by(grp) %>% # respects grouping from dplyr
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 2,
      rows = 1:13
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 3,
      rows = 14:26
    )
  ) 
tab_2

# Saving as tex file 
tab_2 %>%
  gtsave(
    "m2b.tex",
    path = here("output")
  )

# Reliability
l <- lambdas[1:52, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[4]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[4]]) <- "m2b"
omega_t[[4]]
# 0.9602362


# m3a: Six-factor CFA -----------------------------------------------------


runModels(paste0(the_dir, "m3a_six_fact_cfa.inp"))
model_results <- readModels(paste0(the_dir, "m3a_six_fact_cfa.out"),
                            quiet = TRUE)
summary(model_results)
# CFI = 0.9, TLI = 0.885, SRMR = 0.072 
# RMSEA = 0.096, 90% CI [0.093, 0.1], p < .05 = 0 

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

# Reliability
l <- lambdas[1:26, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[5]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[5]]) <- "m3a"
omega_t[[5]]
# 0.970537



# m3b: Six-factor ESEM ----------------------------------------------------


runModels(paste0(the_dir, "m3b_six_fact_esem.inp"))
model_results <- readModels(paste0(the_dir, "m3b_six_fact_esem.out"),
                            quiet = TRUE)
summary(model_results)
# CFI = 0.98, TLI = 0.965, SRMR = 0.019 
# RMSEA = 0.053, 90% CI [0.049, 0.058], p < .05 = 0.121 

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

# Reliability
l <- lambdas[1:156, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[6]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[6]]) <- "m3b"
omega_t[[6]]
# 0.9789576



# m4a: Bifactor-CFA (1 G- and 6 S-factors) --------------------------------


runModels(paste0(the_dir, "m4a_bifactor_six_s_fact.inp"))
model_results <- readModels(paste0(the_dir, "m4a_bifactor_six_s_fact.out"),
                            quiet = TRUE)
summary(model_results)
# [1] 0.757 0.711 0.153 0.118

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

# Reliability
l <- lambdas[1:52, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[7]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[7]]) <- "m4a"
omega_t[[7]]
# 0.969037



# m4b: Bifactor-ESEM (1 G- and 6 S-factors) -------------------------------


runModels(paste0(the_dir, "m4b_bifactor_esem.inp"))
model_results <- readModels(paste0(the_dir, "m4b_bifactor_esem.out"),
                            quiet = TRUE)
summary(model_results)
# 0.980 0.965 0.053 0.019

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

# Reliability
l <- lambdas[1:156, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[8]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[8]]) <- "m4b"
omega_t[[8]]
# 0.9866562



# m5a: Two-bifactor (two-tier) CFA model (2 G- and 6 S-factors) -----------


runModels(paste0(the_dir, "m5a_two_bifactor_cfa.inp"))
model_results <- readModels(paste0(the_dir, "m5a_two_bifactor_cfa.out"),
                            quiet = TRUE)
summary(model_results)
# 0.899 0.879 0.099 0.081

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )


# Reliability
l <- lambdas[1:52, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[9]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[9]]) <- "m5a"
omega_t[[9]]
# 0.9854103



# m5b: Two-bifactor (two-tier) ESEM model (2 G- and 6 S-factors) ----------


runModels(paste0(the_dir, "m5b_two_bifactor_esem.inp"))
model_results <- readModels(paste0(the_dir, "m5b_two_bifactor_esem.out"),
                            quiet = TRUE)
summary(model_results)
# [1] 0.989 0.977 0.043 0.014

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

# Reliability

l <- lambdas[1:182, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[10]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[10]]) <- "m5b"
omega_t[[10]]
# 0.990415


# m5c: Two-bifactor (two-tier) ESEM model (2 G- and 6 S-factors) MLR ----

runModels(paste0(the_dir, "m5c_two_bifactor_esem.inp"))
summaryStats <- readModels(
  paste0(the_dir, "m5c_two_bifactor_esem.out"))
summary(model_results)
# [1] 0.980 0.958 0.036 0.015
# BIC 55918.434 Adjusted BIC 55219.823 AIC 54892.544 


# m6a: Two-correlated factors bifactor v.2 ---------------------------

runModels(paste0(the_dir, "m6a_bifactor_two_fact.inp"))
model_results <- readModels(
  paste0(the_dir, "m6a_bifactor_two_fact.out"))
summary(model_results)
# 0.918 0.894 0.093 0.039

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

item_names <- lambdas[1:26, 2]

Items <- c(
  "SCSK05",  "SCSK12",  "SCSK19", "SCSK23", "SCSK26", 
  "SCSJ01",  "SCSJ08",  "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCCH03",  "SCCH07",  "SCCH10", "SCCH15", 
  "SCIS04",  "SCIS13",  "SCIS18", "SCIS25",
  "SCMI09",  "SCMI14",  "SCMI17", "SCMI22", 
  "SCOI02",  "SCOI06",  "SCOI20", "SCOI24"
)

GEN <- round(lambdas[ 1:26, 3], 2)
CS  <- round(lambdas[27:52, 3], 2)
RUS <- round(lambdas[53:78, 3], 2)

new_order <- c(
  "SCSK05", "SCSK12", "SCSK19", "SCSK23", "SCSK26",
  "SCMI09", "SCMI14", "SCMI17", "SCMI22", 
  "SCCH03", "SCCH07", "SCCH10", "SCCH15", 
  "SCSJ01", "SCSJ08", "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCIS04", "SCIS13", "SCIS18", "SCIS25",
  "SCOI02", "SCOI06", "SCOI20", "SCOI24"
)

tbl <- data.frame(Items, GEN, CS, RUS) %>% 
  arrange(match(Items, new_order)) 

tbl$grp <- c(
  rep("Self-kindness", 5),
  rep("Mindfulness", 4),
  rep("Common Humanity", 4),
  rep("Self-judgment", 5),
  rep("Isolation", 4),
  rep("Over-identification", 4)
)

tab_4 <-
  tbl %>% 
  group_by(grp) %>% # respects grouping from dplyr
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 2,
      rows = 1:13
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 3,
      rows = 14:26
    )
  ) 
tab_4

# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_4 %>%
  gtsave(
    "m6a.tex",
    path = here("output")
  )

# Reliability
l <- lambdas[1:78, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[11]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[11]]) <- "m6a"
omega_t[[11]]
# 0.9641396


# m6b: Two-correlated factors bifactor v.2 --------------------------------

runModels(paste0(the_dir, "m6b_bifactor_two_fact_cor_res.inp"))
model_results <- readModels(
  paste0(the_dir, "m6b_bifactor_two_fact_cor_res.out"))
summary(model_results)
# 0.946 0.930 0.075 0.033

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )

item_names <- lambdas[1:26, 2]

Items <- c(
  "SCSK05",  "SCSK12",  "SCSK19", "SCSK23", "SCSK26", 
  "SCSJ01",  "SCSJ08",  "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCCH03",  "SCCH07",  "SCCH10", "SCCH15", 
  "SCIS04",  "SCIS13",  "SCIS18", "SCIS25",
  "SCMI09",  "SCMI14",  "SCMI17", "SCMI22", 
  "SCOI02",  "SCOI06",  "SCOI20", "SCOI24"
)

GEN <- round(lambdas[ 1:26, 3], 2)
CS  <- round(lambdas[27:52, 3], 2)
RUS <- round(lambdas[53:78, 3], 2)

new_order <- c(
  "SCSK05", "SCSK12", "SCSK19", "SCSK23", "SCSK26",
  "SCMI09", "SCMI14", "SCMI17", "SCMI22", 
  "SCCH03", "SCCH07", "SCCH10", "SCCH15", 
  "SCSJ01", "SCSJ08", "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCIS04", "SCIS13", "SCIS18", "SCIS25",
  "SCOI02", "SCOI06", "SCOI20", "SCOI24"
)

tbl <- data.frame(Items, GEN, CS, RUS) %>% 
  arrange(match(Items, new_order)) 

tbl$grp <- c(
  rep("Self-kindness", 5),
  rep("Mindfulness", 4),
  rep("Common Humanity", 4),
  rep("Self-judgment", 5),
  rep("Isolation", 4),
  rep("Over-identification", 4)
)

tab_3 <-
  tbl %>% 
  group_by(grp) %>% # respects grouping from dplyr
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 2,
      rows = 1:13
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 3,
      rows = 14:26
    )
  ) 
tab_3

# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_3 %>%
  gtsave(
    "m6b.tex",
    path = here("output")
  )

# Reliability
l <- lambdas[1:78, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[12]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[12]]) <- "m6b"
omega_t[[12]]
# 0.9633323


# m6c: Two-correlated factors bifactor v.2 MLR ----------------------------


runModels(paste0(the_dir, "m6c_bifacor_two_fact_cor_res.inp"))
summaryStats <- readModels(
  paste0(the_dir, "m6c_bifacor_two_fact_cor_res.out"))
summary(model_results)
# 0.946 0.93 0.057 0.033
# BIC 56330.110 Adjusted BIC 55926.822 AIC 55737.893



# m7: Two-factor ESEM v.2 -------------------------------------------------


runModels(paste0(the_dir, "m7_two_factor_esem_v2.inp"))
model_results <- readModels(paste0(the_dir, "m7_two_factor_esem_v2.out"),
                            quiet = TRUE)
summary(model_results)

lambdas <- model_results$parameters$stdyx.standardized %>% 
  dplyr::filter(
    paramHeader != 'Thresholds',
    paramHeader != 'Variances'
  )
item_names <- lambdas[1:26, 2]

Items <- c("SCSJ01",  "SCOI02",  "SCCH03",  "SCIS04",  "SCSK05",  
  "SCOI06", "SCCH07",  "SCSJ08", "SCMI09",  "SCCH10", 
  "SCSJ11", "SCSK12", "SCIS13", "SCMI14", "SCCH15", 
  "SCSJ16", "SCMI17", "SCIS18", "SCSK19", "SCOI20", "SCSJ21", 
  "SCMI22", "SCSK23", "SCOI24", "SCIS25", "SCSK26")

CS <- lambdas[1:26, 3]
RUS <- lambdas[27:52, 3]

new_order <- c(
  "SCSK05", "SCSK12", "SCSK19", "SCSK23", "SCSK26",
  "SCMI09", "SCMI14", "SCMI17", "SCMI22", 
  "SCCH03", "SCCH07", "SCCH10", "SCCH15", 
  "SCSJ01", "SCSJ08", "SCSJ11", "SCSJ16", "SCSJ21", 
  "SCIS04", "SCIS13", "SCIS18", "SCIS25",
  "SCOI02", "SCOI06", "SCOI20", "SCOI24"
)

tbl <- data.frame(Items, CS, RUS) %>% 
  arrange(match(Items, new_order)) 

tbl$grp <- c(
  rep("Self-kindness", 5),
  rep("Mindfulness", 4),
  rep("Common Humanity", 4),
  rep("Self-judgment", 5),
  rep("Isolation", 4),
  rep("Over-identification", 4)
)

tab_1 <-
  tbl %>% 
  group_by(grp) %>% # respects grouping from dplyr
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 2,
      rows = 1:13
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 3,
      rows = 14:26
    )
  ) 
tab_1

# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_1 %>%
  gtsave(
    "m7.tex",
    path = here("output")
  )


# Reliability
l <- lambdas[1:52, 3]
e <- model_results$parameters$r2[, 6]
lambda_2 <- sum(l)^2
error <- sum(e)
omega_t[[13]] <- lambda_2 / (lambda_2 + error)
names(omega_t[[13]]) <- "m7"
omega_t[[13]]
# 0.949033


# m7a: Two-factor ESEM v.2 MLR --------------------------------------------


runModels(paste0(the_dir, "m7a_two_factor_esem_v2.inp"))
model_results <- readModels(
  paste0(the_dir, "m7a_two_factor_esem_v2.out"), what="summaries")
summary(model_results)


saveRDS(omega_t, here("data", "processed", "scs_omega_total.Rds"))



# e  n  d   ---------------------------------------------------------------





