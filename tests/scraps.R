## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

# Regressions

Not particularly interesting.

```{r}
clean_dat$ptg <- with(clean_dat, life_appreciation + new_possibilities + 
                        personal_strength + spirituality_changes + 
                        interpersonal_relationships)
hist(clean_dat$ptg)
```


```{r}
fm <- lm(ptg ~ gender + edu + age_imp + activity_rate +
           employment + 
           years_experience + last_training + rate_of_activity +
           job_qualification + is_job_qualification_invariant +
           is_team_invariant,
         data = clean_dat)
car::Anova(fm)
```


```{r}
summary(fm)
```


```{r}
car::outlierTest(fm)
```


```{r}
car::influenceIndexPlot(fm)
```


```{r}
ggResidpanel::resid_panel(fm)
```


```{r}
boxplot(clean_dat$ptg ~ clean_dat$gender)
```

```{r}
clean_dat$PTSD <- with(clean_dat, 1 + avoiding + intrusivity + iperarousal)
hist(log(clean_dat$PTSD + 1))
```


```{r}
fm1 <- lm(log(PTSD) ~ gender + edu + age_imp + employment + 
            years_experience + last_training + rate_of_activity +
            job_qualification + is_job_qualification_invariant +
            is_team_invariant,
          data = clean_dat)
car::Anova(fm1)
```

```{r}
summary(fm1)
```

```{r}
boxplot(log(clean_dat$PTSD) ~ clean_dat$gender)
```

```{r}
car::outlierTest(fm1)
```

```{r}
car::influenceIndexPlot(fm1)
```

```{r}
ggResidpanel::resid_panel(fm1)
```


# References

Geiger, M., Pfattheicher, S., Hartung, J., Weiss, S., Schindler, S., & Wilhelm, O. (2018). Self‐Compassion as a Facet of Neuroticism? A Reply to the Comments of Neff, Tóth‐Király, and Colosimo (2018). *European Journal of Personality*, *32*(4), 393-404.

Muris, P., Otgaar, H., & Pfattheicher, S. (2019). Stripping the forest from the rotten trees: compassionate self-responding is a way of coping, but reduced uncompassionate self-responding mainly reflects psychopathology. *Mindfulness*, *10*(1), 196-199.


# Original Computing Environment {-}

```{r}
sessionInfo()
```



# Measurement invariance


```{r}
model_invariance <- "

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
  ptgr ~ cope + soc + nsc + psc + neuro
  ptss ~ cope + soc + nsc + psc + neuro
  
"
```


```{r}
step1_comb_mean <- lavaan::sem(
  data = clean_dat, 
  model = model_invariance,
  estimator = "MLM",
  group = "where")
```


```{r}
step2_comb_mean <- lavaan::sem(
  data=clean_dat, 
  model=model_invariance,
  group="where", estimator="MLM",
  group.equal=c("loadings")
)
```

```{r}
step3_comb_mean <- lavaan::sem(
  data=clean_dat, 
  model=model_invariance,
  group="where", 
  estimator="MLM",
  group.equal=c("loadings", "intercepts")
)
```

```{r}
step4_comb_mean <- lavaan::sem(
  data=clean_dat, 
  model=model_invariance,
  group="where", estimator="MLM",
  group.equal=c("loadings", "intercepts", "residuals")
)
```

```{r}
compareFit(step1_comb_mean, step2_comb_mean, step3_comb_mean, step4_comb_mean)
```

```{r}
anova(step1_comb_mean, step2_comb_mean, step3_comb_mean, step4_comb_mean)
```



```{r}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

v <- ggplot(clean_dat, aes(nsc, psc, fill = ptss))
v + geom_tile()
```

