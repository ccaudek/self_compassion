
df1 <- data.frame(neuroticism, ies_r_score, avoiding_strategies)
df1 <- df1[complete.cases(df1), ]
nrow(df1)


df1 <- df1 %>% 
  dplyr::mutate(
    o = neuroticism - mean(neuroticism, na.rm = TRUE),
    p = ies_r_score - mean(ies_r_score, na.rm = TRUE),
    c = avoiding_strategies - mean(avoiding_strategies, na.rm = TRUE)
  ) %>% 
  dplyr::select(o, p, c) %>% 
  dplyr::rename(
    X = o,
    Y = p,
    M = c
  )


model0 <- 
  brm(data = df1, 
      family = gaussian,
      cbind(X, Y, M) ~ 1,
      chains = 4, 
      cores = 4
  )

print(model0, digits = 3)

posterior_samples(model0) %>% 
  select(rescor__X__Y, rescor__X__M, rescor__Y__M) %>% 
  gather() %>% 
  ggplot(aes(x = value, fill = key)) +
  geom_density(alpha = .85, color = "transparent") +
  scale_fill_viridis(discrete = T, option = "D", direction = -1,
                     labels = c(expression(paste(rho["X, M"])),
                                expression(paste(rho["X, Y"])),
                                expression(paste(rho["Y, M"]))),
                     guide = guide_legend(label.hjust = 0,
                                          label.theme = element_text(size = 15, angle = 0, color = "white"),
                                          title.theme = element_blank())) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(title = "Our correlation density plot",
       x = NULL) +
  theme_black() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())


hist(df1$Y)


# Model 1 -----------------------------------------------------------------


y_model <- bf(Y ~ 1 + X + M, family = skew_normal())
m_model <- bf(M ~ 1 + X, family = skew_normal())

model1 <-
  brm(
    data = df1,
    y_model + m_model + set_rescor(FALSE),
    chains = 4,
    cores = 4
  )

summary(model1)


# putting the posterior draws into a data frame
post <- posterior_samples(model1)

# Computing the indirect effect with multiplication
post <-
  post %>% 
  mutate(indirect_eff = b_M_X * b_Y_M)

# getting the posterior median and 95% intervals with `quantile()`
quantile(post$indirect_eff, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)

# the direct effect
posterior_summary(model1)["b_Y_X", ] %>% 
  round(digits = 3)

# the total effect
post <-
  post %>% 
  mutate(total_eff = b_Y_X + indirect_eff)

quantile(post$total_eff, probs = c(.5, .025, .975)) %>% 
  round(digits = 3)



post %>% 
  mutate(total_eff = b_Y_X + indirect_eff) %>% # total effect
  rename(direct_eff = b_Y_X) %>% # direct effect
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
  labs(title = expression(paste("0 is a credible value for the direct effect ", b[yx], ".")),
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

pp_check(model1, resp = "Y")
pp_check(model1, resp = "M")

print(model1, digits = 3)

bayes_R2(model1)



# Partially-standardized coefficients

SD_y <- sd(df1$Y)

post %>% 
  mutate(direct_eff_ps   = b_Y_X / SD_y,
         indirect_eff_ps = indirect_eff / SD_y) %>% 
  mutate(total_eff_ps = direct_eff_ps + indirect_eff_ps) %>% 
  select(direct_eff_ps, indirect_eff_ps, total_eff_ps) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 3)


post %>% 
  mutate(direct_eff_ps   = b_Y_X / SD_y,
         indirect_eff_ps = indirect_eff / SD_y) %>% 
  mutate(total_eff_ps = direct_eff_ps + indirect_eff_ps) %>% 
  select(direct_eff_ps, indirect_eff_ps, total_eff_ps) %>% 
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


df1 <-
  df1 %>% 
  mutate(
    Yz = standardize(Y), 
    Xz = standardize(X), 
    Mz = standardize(M)
  )

y_model <- bf(Yz ~ 1 + Xz + Mz, family = skew_normal())
m_model <- bf(Mz ~ 1 + Xz, family = gaussian())

model2 <-
  brm(data = df1, 
      y_model + m_model + set_rescor(FALSE),
      chains = 4, 
      cores = 4
  )

fixef(model2) %>% round(digits = 3)

post <- posterior_samples(model2)

post %>% 
  mutate(indirect_eff_s = b_Mz_Xz * b_Yz_Mz,
         direct_eff_s = b_Yz_Xz) %>%
  mutate(total_eff_s = indirect_eff_s + direct_eff_s) %>% 
  select(direct_eff_s, indirect_eff_s, total_eff_s) %>% 
  gather() %>%
  group_by(key) %>% 
  summarize(mean = mean(value), 
            median = median(value),
            ll = quantile(value, probs = .025),
            ul = quantile(value, probs = .975)) %>% 
  mutate_if(is_double, round, digits = 4)



