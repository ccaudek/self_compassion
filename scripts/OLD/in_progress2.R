

# Mediational analysis (1)

# We consider the path analysis model of Mattson, James, & Engdahl 
# (2018) described in their Figure 2.
# X: openness
# Y: ptg
# M: coping

d <- data.frame(
  X  = mydata$conscientiousness - mean(mydata$conscientiousness, na.rm = TRUE), 
  M1 = mydata$pos_self_compassion - mean(mydata$pos_self_compassion, na.rm = TRUE),
  M2 = mydata$neg_self_compassion - mean(mydata$neg_self_compassion, na.rm = TRUE),
  M3 = mydata$cope_nvi - mean(mydata$cope_nvi, na.rm = TRUE),
  Y = mydata$ptg - mean(mydata$ptg, na.rm = TRUE)
)

d <- d[complete.cases(d), ]
nrow(d)

hist(d$Y)
hist(d$M1)
hist(d$M2)
hist(d$M3)


# model  <- bf(Y  ~ 1 + X * (M1 + M2 + M3))
# 
# fit <-
#   brm(
#     data = d,
#     model,
#     family = skew_normal(),
#     chains = 4,
#     cores = 4,
#     inits = 0
#   )
# summary(fit)
# 
# pp_check(fit)


y_model  <- bf(Y  ~ 1 + X + M1 + M2 + M3, family = skew_normal())
m1_model <- bf(M1 ~ 1 + X, family = gaussian())
m2_model <- bf(M2 ~ 1 + X, family = skew_normal())
m3_model <- bf(M3 ~ 1 + X, family = gaussian())


m1 <-
  brm(
    data = d,
    y_model + m1_model + m2_model + m3_model + set_rescor(FALSE),
    chains = 4,
    cores = 4,
    inits = 0
  )

summary(m1)

# putting the posterior draws into a data frame
post <- posterior_samples(m1)


# Computing the indirect effect with multiplication
post <-
  post %>% 
  mutate(indirect_eff1 = b_M1_X * b_Y_M1)
# getting the posterior median and 95% intervals with `quantile()`
quantile(post$indirect_eff1, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)

post <-
  post %>% 
  mutate(indirect_eff2 = b_M2_X * b_Y_M2)
quantile(post$indirect_eff2, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)


post <-
  post %>% 
  mutate(indirect_eff3 = b_M3_X * b_Y_M3)
quantile(post$indirect_eff3, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)



# the direct effect
posterior_summary(model1)["b_Y_X", ] %>% 
  round(digits = 3)

# the total effect
post <-
  post %>% 
  mutate(total_eff = b_Y_X + indirect_eff1 + indirect_eff2 + indirect_eff3)
quantile(post$total_eff, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)



post %>% 
  mutate(direct_eff   = b_Y_X,
         indirect_eff = indirect_eff) %>% 
  mutate(total_eff = direct_eff + indirect_eff) %>% 
  select(direct_eff, indirect_eff, total_eff) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)



# Posterior plots
post %>% 
  ggplot(aes(x = indirect_eff)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(3)[3]) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .5, .975)), 
             color = "white", linetype = c(2, 1, 2), size = c(.5, .8, .5)) +
  # scale_x_continuous(breaks = quantile(post$ab, probs = c(.025, .5, .975)),
  #                    labels = quantile(post$ab, probs = c(.025, .5, .975)) %>% 
  #                      round(2) %>% 
  #                      as.character()) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("The indirect effect, the ", b[mx] * b[ym], " pathway")),
       x = NULL) +
  theme_classic()


post %>% 
  ggplot(aes(x = b_Y_X)) +
  geom_density(color = "transparent", 
               fill = colorblind_pal()(4)[4]) +
  geom_vline(xintercept = 0, color = "white", linetype = 2) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("0 is not a credible value for the direct effect ", b[yx], ".")),
       x = NULL) +
  theme_classic()


ll <- quantile(post$total_eff, probs = .025) %>% round(digits = 3)
ul <- quantile(post$total_eff, probs = .975) %>% round(digits = 3)

post %>% 
  ggplot(aes(x = total_eff)) +
  geom_histogram(color = "white", size = .25, 
                 fill = colorblind_pal()(5)[5],
                 binwidth = .025, boundary = 0) +
  geom_vline(xintercept = quantile(post$ab, probs = c(.025, .975)),
             linetype = 3, color = colorblind_pal()(6)[6]) +
  labs(x = expression(paste("Total effect (", b[yx] + b[mx] * b[ym], ")")),
       y = "Frequency in 4,000 HMC posterior draws",
       subtitle = paste("95% of the posterior draws are between", ll, "and", ul)) +
  theme_classic()


# Evaluate the model's fit

pp_check(m1, resp = "Y")
pp_check(m1, resp = "M1")
pp_check(m1, resp = "M2")
pp_check(m1, resp = "M3")


print(model1, digits = 3)

bayes_R2(m1)
(loo1 <- loo(m1))
  


# Partially-standardized coefficients

SD_y <- sd(d$Y)

post %>% 
  mutate(direct_eff_ps    = b_Y_X / SD_y,
         indirect_eff1_ps = indirect_eff1 / SD_y,
         indirect_eff2_ps = indirect_eff2 / SD_y,
         indirect_eff3_ps = indirect_eff3 / SD_y
         ) %>% 
  mutate(total_eff_ps = direct_eff_ps + 
           indirect_eff1_ps + indirect_eff2_ps + indirect_eff3_ps) %>% 
  select(direct_eff_ps, indirect_eff1_ps, indirect_eff2_ps, indirect_eff3_ps, 
         total_eff_ps) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)


post %>% 
  mutate(direct_eff_ps   = b_Y_X / SD_y,
         indirect_eff1_ps = indirect_eff1 / SD_y,
         indirect_eff2_ps = indirect_eff2 / SD_y,
         indirect_eff3_ps = indirect_eff3 / SD_y
         ) %>% 
  mutate(total_eff_ps = direct_eff_ps + 
         indirect_eff1_ps + indirect_eff2_ps + indirect_eff3_ps) %>% 
  select(direct_eff_ps, indirect_eff1_ps, indirect_eff2_ps, indirect_eff3_ps, 
         total_eff_ps) %>% 
  gather() %>%
  ggplot(aes(x = value, fill = key)) +
  geom_density(alpha = .85, color = "transparent") +
  scale_fill_viridis(discrete = T, option = "D") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Partially-standardized coefficients",
       x = NULL) +
  theme_black() +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  facet_wrap(~key, ncol = 3)


# Standardized coefficients

standardize <- function(x){
  (x - mean(x))/sd(x)
}


d <-
  d %>% 
  mutate(
    Yz = standardize(Y), 
    Xz = standardize(X), 
    M1z = standardize(M1),
    M2z = standardize(M2),
    M3z = standardize(M3)
  )

y_model  <- bf(Yz  ~ 1 + Xz + M1z + M2z + M3z, family = skew_normal())
m1_model <- bf(M1z ~ 1 + Xz, family = gaussian())
m2_model <- bf(M2z ~ 1 + Xz, family = skew_normal())
m3_model <- bf(M3z ~ 1 + Xz, family = gaussian())


m2 <-
  brm(data = d, 
      y_model + m1_model + m2_model + m3_model + set_rescor(FALSE),
      chains = 4, 
      cores = 4,
      inits = 0
  )

fixef(m2) %>% round(digits = 3)

post <- posterior_samples(m2)

post %>% 
  mutate(
    indirect_eff1_s = b_M1z_Xz * b_Yz_M1z,
    indirect_eff2_s = b_M2z_Xz * b_Yz_M2z,
    indirect_eff3_s = b_M3z_Xz * b_Yz_M3z,
    direct_eff_s = b_Yz_Xz
    ) %>%
  mutate(total_eff_s = indirect_eff1_s + indirect_eff2_s + 
           indirect_eff3_s + direct_eff_s) %>% 
  select(direct_eff_s, indirect_eff1_s, indirect_eff2_s, 
         indirect_eff3_s, total_eff_s) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 4)





