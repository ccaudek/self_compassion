
library("here")
library("tidyverse")
library("MplusAutomation")
#library("formattable")
library("gt")
library("glue")
library("kableExtra")
options(max.print=999999)


the_dir <- "/Users/corrado/Documents/papers/self_compassion/scripts/_mplus/"

nrw <- get_scs_items_not_rescue_workers()
dim(nrw)

rw <- get_scs_items_rescue_workers()
dim(rw)

d <- bind_rows(rw, nrw)

# d <- read.table(here("scripts", "_mplus", "scs.dat"), header = FALSE)

new_names <- c(
  "group", 
  "scsj1", "scoi2", "scch3", "scis4", "scsk5", "scoi6", "scch7", 
  "scsj8", "scmi9", "scch10", "scsj11", "scsk12", "scis13", "scmi14", 
  "scch15", "scsj16", "scmi17", "scis18", "scsk19", "scoi20", 
  "scsj21", "scmi22", "scsk23", "scoi24", "scis25", "scsk26",
  "where", "sex", "age" 
)

colnames(d) <- new_names

d$group <- factor(d$group)
d$where <- factor(d$where)
d$sex <- factor(d$sex)

MplusAutomation::prepareMplusData(
  d, 
  paste0(the_dir, "selfcompassionitemsgroup.dat")
)
# TITLE: Your title goes here
# DATA: FILE = "/Users/corrado/Documents/papers/self_compassion/scripts/_mplus/selfcompassionitemsgroup.dat";
# VARIABLE: 
#   NAMES = group scsj1 scoi2 scch3 scis4 scsk5 scoi6 scch7 scsj8 scmi9 scch10 scsj11
# scsk12 scis13 scmi14 scch15 scsj16 scmi17 scis18 scsk19 scoi20 scsj21 scmi22
# scsk23 scoi24 scis25 scsk26 where sex age; 
# MISSING=.;


print_summary_stats <- function(summaryStats) {
  c(
    summaryStats$CFI, summaryStats$TLI, 
    summaryStats$RMSEA_Estimate, 
    summaryStats$RMSEA_90CI_LB, summaryStats$RMSEA_90CI_UB, 
    summaryStats$SRMR
  )
}

temp <- read.table(here("scripts", "_mplus", "selfcompassionitemsgroup.dat"))
glimpse(temp)

omega_t <- list()


runModels(paste0(the_dir, "fact_inv_1.inp"))

model_results <- readModels(paste0(the_dir, "fact_inv_1.out"), quiet = TRUE)

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
