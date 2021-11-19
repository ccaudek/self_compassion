## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

library("tidyverse")


df <- iris[, 1:2] 
names(df) <- c("x", "y")

dfs <- df %>%       
  mutate_at(c("x", "y"), ~(scale(.) %>% as.vector))
head(dfs)   

summary(dfs)
cov(dfs)

lavaan::sem("y ~ x", dfs) %>%
  tidySEM::graph_sem(spacing_x = 2.5, fix_coord = TRUE)

fm <- lm(y ~ x, dfs)
summary(fm)


lavaan::sem("y ~ x", dfs) %>%
  standardizedSolution() 



d <- dplyr::select_if(clean_dat, is.numeric)

dfs <- d %>%       
  mutate_at(names(d), ~(scale(.) %>% as.vector))
head(dfs) 



model10 <- "
  # post-traumatic growth
  ptgr =~ life_appreciation + new_possibilities + 
         personal_strength + spirituality_changes + 
         interpersonal_relationships
         
  # ptsd
  ptss =~ avoiding + intrusivity + iperarousal
  
  # coping
  cope =~ positive_attitude + problem_orientation 

  # perceived social support
  soc =~ family + friends + significant_other
  
  # self-compassion
  nsc =~ self_judgment + isolation + over_identification
  psc =~ self_kindness + common_humanity + mindfulness
  
  # neuroticism
  neuro =~ negative_affect + self_reproach

  # regressions
  ptgr ~ dg_cope*cope + dg_soc*soc + dg_neuro*neuro
  ptss ~ ds_cope*cope + ds_soc*soc + ds_neuro*neuro

  nsc ~ nsc_cope*cope + nsc_soc*soc + nsc_neuro*neuro
  psc ~ psc_cope*cope + psc_soc*soc + psc_neuro*neuro
  
  ptgr ~ ig_nsc*nsc + ig_psc*psc 
  ptss ~ is_nsc*nsc + is_psc*psc
  
  # covariances
  self_judgment ~~ self_kindness
"



fit10 <- sem(
  model10,
  data = d,
  estimator = "MLM",
  std.lv = TRUE
)


summary(
  fit10,
  standardized = TRUE,
  fit.measures = TRUE,
  rsquare = TRUE
)


dim(data.frame(tidySEM::edges(graph_data)))



std_solution <- lavaan::standardizedSolution(fit10) %>% 
  mutate_if("is.numeric","round",3) %>% 
  filter(op == "~" | op == "~~")

lay <- get_layout(
  "", "cope", "soc",  "neuro", "",
  "nsc", "", "", "", "psc",
  "", "ptss", "", "ptgr", "",
  rows = 3)

graph_data <- prepare_graph(model = fit10, layout = lay)
foo <- edges(graph_data) 
foo1 <- foo[c(2, 7, 9:10, 12, 14, 15, 24:27), ]

graph_data$edges <- foo1

graph_data[[1]]$label <- 
  round(std_solution[c(2, 7, 9, 10, 12, 14, 15, 24:27), 4], 2)

plot(graph_data)



fit6a <- sem(
  model6,
  data = clean_dat,
  std.lv = TRUE
)


fit10a <- sem(
  model10,
  data = clean_dat,
  std.lv = TRUE
)


vuong_out <- vuongtest(fit6a, fit10a)

icci_out <- icci(fit10a, fit6a)
