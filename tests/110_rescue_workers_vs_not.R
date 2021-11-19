library("here")

library("effectsize")
library("brms")
library("cmdstanr")
set_cmdstan_path("/Users/corrado/cmdstan")
library("posterior")
library("bayesplot")
library("recipes")
library("MCMCpack")
library("bayest")
library("bayestestR")
library("insight")


df_lombardia <- read_excel(here("data", "raw", "lombardia_1.xlsx"))

temp <- df_lombardia[, 17:231]

df_ns <- read_excel(here("data", "raw", "non_soccorritori_20210404.xlsx"))
df_ns$where <- "toscana"


ns <- df_ns[, 9:213]
dim(ns)


#176 - 201 inclusi
temp <- ns[, 176:201]

scs <- temp[complete.cases(temp), ]

# Then we change the column names to facilitate coding
item_names <- paste0("iiiii", 1:26)
names(scs) <- item_names

# Self-Kindness
self_kindness <- 
  scs$iiiii5 + scs$iiiii12 + scs$iiiii19 + scs$iiiii23 + 
  scs$iiiii26
# hist(self_kindness)

# Self-Judgment - REVERSED
self_judgment <- 
  abs(scs$iiiii1 - 6) + abs(scs$iiiii8 - 6) + 
  abs(scs$iiiii11 - 6) + abs(scs$iiiii16 - 6) + 
  abs(scs$iiiii21 - 6)
# hist(self_judgment)

# Common Humanity
common_humanity <- 
  scs$iiiii3 + scs$iiiii7 + scs$iiiii10 + scs$iiiii15
# hist(common_humanity)

# Isolation - REVERSED
isolation <- 
  abs(scs$iiiii4 - 6) + abs(scs$iiiii13 - 6) + 
  abs(scs$iiiii18 - 6) + abs(scs$iiiii25 - 6)
# hist(isolation)

# Mindfulness
mindfulness <- 
  scs$iiiii9 + scs$iiiii14 + scs$iiiii17 + scs$iiiii22
# hist(mindfulness)

# Overidentification - REVERSED
over_identification <- 
  abs(scs$iiiii2 - 6) + abs(scs$iiiii6 - 6) + 
  abs(scs$iiiii20 - 6) + abs(scs$iiiii24 - 6)
# hist(over_identification)

neg_self_compassion <- self_judgment + isolation + 
  over_identification
pos_self_compassion <- self_kindness + common_humanity + 
  mindfulness


not_rescue_workers <- data.frame(
  is_rescue_worker = 0,
  neg_self_compassion,
  pos_self_compassion, 
  where
)


rescue_workers <- data.frame(
  is_rescue_worker = 1,
  neg_self_compassion,
  pos_self_compassion,
  where
)


mydata <- bind_rows(
  not_rescue_workers, rescue_workers
) %>% 
  mutate(
    scs_total_score = neg_self_compassion + pos_self_compassion
  )

hist(mydata$scs_total_score)

t.test(scs_total_score ~ is_rescue_worker, data = mydata)
t.test(neg_self_compassion ~ is_rescue_worker, data = mydata)
t.test(pos_self_compassion ~ is_rescue_worker, data = mydata)

mydata %>% 
  group_by(is_rescue_worker) %>% 
  summarise(
    avg_scs = mean(scs_total_score),
    sd_scs = sd(scs_total_score),
    avg_rus = mean(neg_self_compassion),
    sd_rus = sd(neg_self_compassion),
    avg_cs = mean(pos_self_compassion),
    sd_cs = sd(pos_self_compassion)
  )

hedges_g(scs_total_score ~ is_rescue_worker, data = mydata)
hedges_g(neg_self_compassion ~ is_rescue_worker, data = mydata)
hedges_g(pos_self_compassion ~ is_rescue_worker, data = mydata)

saveRDS(mydata, here("data", "processed", "rescue_workers_and_not.rds"))

mydata <- readRDS(here("data", "processed", "rescue_workers_and_not.rds"))

mydata %>% 
  group_by(group) %>% 
  summarise(
    n = n()
  )
# group                 n
# <chr>             <int>
# 1 not_rescue_worker   153
# 2 rescue_worker       783


# Standardize numeric variables -------------------------------------------

mydata$group <- ifelse(
  mydata$is_rescue_worker == 1, "rescue_worker", "not_rescue_worker"
) 

mydata$scs <- as.numeric(scale(mydata$scs_total_score))
mydata$rus <- as.numeric(scale(mydata$neg_self_compassion))
mydata$cs <- as.numeric(scale(mydata$pos_self_compassion))

mydata_s <- mydata %>% 
  dplyr::select(
    c(-is_rescue_worker, -neg_self_compassion, -pos_self_compassion, 
      -scs_total_score, group, scs, rus, cs)
    )



# SCS total score ---------------------------------------------------------

uneq_var_scs <- bf(
  scs ~ group + (1 | where), 
  sigma ~ group + (1 | where)
)

get_prior(
  uneq_var_scs,
  data = mydata_s,
  family = student
)

mod_scs <- brm(
  uneq_var_scs,
  family = student,
  prior = c(prior(student_t(5, 0, 1), class = Intercept),
            prior(student_t(5, 0, 1), class = b),
            prior(cauchy(0, 2), dpar = sigma)),
  data = mydata_s,
  cores = 4,
  backend = "cmdstan"
)

mod_scs <- add_criterion(mod_scs, "loo")
loo(mod_scs)

pp_check(mod_scs, resp = "scs_total_score")

round(bayes_R2(mod_scs), 2)
#    Estimate Est.Error Q2.5 Q97.5
# R2        0         0    0  0.01

summary(mod_scs)
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                 79.35      0.39    78.57    80.13 1.00     5037     3199
# sigma_Intercept            1.58      0.06     1.47     1.69 1.00     3485     2990
# is_rescue_worker           2.25      0.70     0.84     3.61 1.00     2360     2479
# sigma_is_rescue_worker     1.20      0.06     1.07     1.31 1.00     3387     2517


# effect size ----

eq_var_scs <- bf(
  scs ~ group
)

mod1_scs <- brm(
  eq_var_scs,
  family = student,
  prior = c(prior(student_t(5, 0, 1), class = Intercept),
            prior(student_t(5, 0, 1), class = b),
            prior(cauchy(0, 2), class = sigma)),
  data = mydata_s,
  cores = 4,
  backend = "cmdstan"
)

effectsize::standardize_parameters(mod1_scs, method = "refit", ci = 0.95)
# Parameter            | Median (std.) |        95% CI
# ----------------------------------------------------
# b_Intercept          |         -0.13 | [-0.26, 0.02]
# b_grouprescue_worker |          0.18 | [ 0.03, 0.34]

draws <- as.data.frame(mod1_scs)
draws$cohens_d <- draws$b_grouprescue_worker / draws$sigma
median(draws$cohens_d)
#[1] 0.2068031


# Reduced Uncompassionate Responding --------------------------------------

uneq_var_rus <- bf(
  rus ~ group, 
  sigma ~ group
)

mod_rus <- brm(
  uneq_var_rus,
  family = student,
  prior = c(prior(student_t(5, 0, 1), class = Intercept),
            prior(student_t(5, 0, 1), class = b),
            prior(cauchy(0, 2), dpar = sigma)),
  data = mydata_s,
  cores = 4,
  backend = "cmdstan", 
)

mod_rus <- add_criterion(mod_rus, "loo")
loo(mod_rus)

pp_check(mod_rus, resp = "neg_self_compassion")

round(bayes_R2(mod_rus), 2)
#    Estimate Est.Error Q2.5 Q97.5
# R2     0.12      0.01 0.09  0.14

summary(mod_rus)
#                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                   -0.77      0.04    -0.84    -0.69 1.00     4953     3167
# sigma_Intercept             -0.76      0.06    -0.87    -0.63 1.00     4692     2920
# grouprescue_worker           0.92      0.05     0.81     1.02 1.00     4215     3160
# sigma_grouprescue_worker     0.75      0.07     0.62     0.88 1.00     4783     2948


# effect size ----

eq_var_rus <- bf(
  rus ~ group
)

mod1_rus <- brm(
  eq_var_rus,
  family = student,
  prior = c(prior(student_t(5, 0, 1), class = Intercept),
            prior(student_t(5, 0, 1), class = b),
            prior(cauchy(0, 2), class = sigma)),
  data = mydata_s,
  cores = 4,
  backend = "cmdstan"
)

effectsize::standardize_parameters(mod1_rus, method = "refit", ci = 0.95)
# Parameter            | Median (std.) |         95% CI
# -----------------------------------------------------
# b_Intercept          |         -0.76 | [-0.91, -0.61]
# b_grouprescue_worker |          0.92 | [ 0.76,  1.10]

draws <- as.data.frame(mod1_rus)
draws$cohens_d <- draws$b_grouprescue_worker / draws$sigma
median(draws$cohens_d)
# [1] 0.9868137



# Compassionate responding ------------------------------------------------


uneq_var_cs <- bf(
  cs ~ group, 
  sigma ~ group
)

mod_cs <- brm(
  uneq_var_cs,
  family = student,
  prior = c(prior(student_t(5, 0, 1), class = Intercept),
            prior(student_t(5, 0, 1), class = b),
            prior(cauchy(0, 2), dpar = sigma)),
  data = mydata,
  cores = 4,
  backend = "cmdstan", 
)

mod_cs <- add_criterion(mod_cs, "loo")
loo(mod_cs)

pp_check(mod_cs, resp = "pos_self_compassion")

round(bayes_R2(mod_cs), 2)
#    Estimate Est.Error Q2.5 Q97.5
# R2     0.12      0.01 0.09  0.15

summary(mod_cs)
#                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                    0.78      0.05     0.69     0.88 1.00     4582     2749
# sigma_Intercept             -0.51      0.07    -0.64    -0.38 1.00     3644     2957
# grouprescue_worker          -0.93      0.06    -1.05    -0.81 1.00     3809     2891
# sigma_grouprescue_worker     0.46      0.07     0.32     0.59 1.00     3982     3080


# effect size ----

eq_var_cs <- bf(
  cs ~ group
)

mod1_cs <- brm(
  eq_var_cs,
  family = student,
  prior = c(prior(student_t(5, 0, 1), class = Intercept),
            prior(student_t(5, 0, 1), class = b),
            prior(cauchy(0, 2), class = sigma)),
  data = mydata_s,
  cores = 4,
  backend = "cmdstan"
)

effectsize::standardize_parameters(mod1_cs, method = "refit", ci = 0.95)
# Parameter            | Median (std.) |         95% CI
# -----------------------------------------------------
# b_Intercept          |          0.78 | [ 0.63,  0.91]
# b_grouprescue_worker |         -0.93 | [-1.08, -0.77]

draws <- as.data.frame(mod1_cs)
draws$cohens_d <- draws$b_grouprescue_worker / draws$sigma
median(draws$cohens_d)
# [1] -1.026689

