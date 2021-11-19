clean_dat <- readRDS(here("data", "processed", "rescue_workers_cleaned_data.rds"))


clean_dat$ptg <- with(
  clean_dat,
  life_appreciation + new_possibilities + personal_strength + 
    spirituality_changes + interpersonal_relationships
)

clean_dat$pts <- with(
  clean_dat,
  avoiding + intrusivity + iperarousal
)

clean_dat$rsu <- with(
  clean_dat,
  avoiding + intrusivity + iperarousal
)

clean_dat$cs <- with(
  clean_dat,
  self_kindness + common_humanity + mindfulness
)

clean_dat$rsu <- with(
  clean_dat,
  self_judgment + isolation + over_identification
)

clean_dat$coping <- with(
  clean_dat,
  social_support + positive_attitude + problem_orientation + 
    transcendent_orientation
)


ds <- clean_dat %>% 
  dplyr::select(
    ptg, pts, gender, age_imp, edu, employment, 
    years_experience, training_time, activity_rate, 
    job_qualification, is_job_qualification_invariant,
    is_team_invariant, cs, rsu, where, 
    red_cross_commeetee_location
  ) %>% 
  mutate_if(is.numeric, scale) %>% 
  dplyr::rename(
    age = age_imp,
    city = red_cross_commeetee_location,
    region = where
  )

with(ds, cor(cs, rsu))


hist(ds$pts)

m1 <- brms::brm(rsu ~ gender + age + edu + years_experience + 
                  training_time + activity_rate + 
                  (1 | region/city), 
                family = skew_normal(),
                backend = "cmdstan", 
                data = ds)
pp_check(m1)
bayes_R2(m1)
summary(m1)

conditional_effects(m1, effects = "gender")


hist(ds$cs)

m2 <- brms::brm(cs ~ gender + age + edu + years_experience + 
                  training_time + activity_rate + 
                  (1 | region/city), 
                family = skew_normal(),
                backend = "cmdstan", 
                data = ds)
pp_check(m2)
bayes_R2(m2)
summary(m2)

conditional_effects(m2, effects = "age")

ds$self_comp <- ds$rsu + ds$cs


y1 <- ds[ds$gender == "female", ]$self_comp
y2 <- ds[ds$gender == "male", ]$self_comp

BESTout <- BEST::BESTmcmc(y1, y2, parallel=TRUE)
summary(BESTout)


m3 <- brms::brm(self_comp ~ gender +  
                  (1 | region/city), 
                family = skew_normal(),
                backend = "cmdstan", 
                data = ds)
pp_check(m3)
bayes_R2(m3)
summary(m3)

conditional_effects(m3, effects = "age")



m3 <- brms::brm(pts ~ gender + age + edu + years_experience + 
                  training_time + activity_rate + 
                  (1 | region/city), 
                family = skew_normal(),
                backend = "cmdstan", 
                data = ds)
pp_check(m3)
bayes_R2(m3)
summary(m3)

conditional_effects(m3, effects = "age")





# Personality -------------------------------------------------------------

df$ptg <- with(
  df,
  life_appreciation + new_possibilities + personal_strength + 
    spirituality_changes + interpersonal_relationships
)

df$pts <- with(
  df,
  avoiding + intrusivity + iperarousal
)

df$rsu <- with(
  df,
  avoiding + intrusivity + iperarousal
)

df$cs <- with(
  df,
  self_kindness + common_humanity + mindfulness
)

clean_dat$rsu <- with(
  clean_dat,
  self_judgment + isolation + over_identification
)

df$coping <- with(
  df,
  social_support + positive_attitude + problem_orientation + 
    transcendent_orientation
)

