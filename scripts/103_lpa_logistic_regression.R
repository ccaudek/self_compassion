# Author: Andrew Gelman
# With minor modifications for reading in the data more universally
## 1.  Set up
library("rstan")
library("rstanarm")
library("arm")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## 2.  Cetirizine data
ej_1  <- data.frame(
  defects_yes=c(0, 8), 
  defects_no=c(13, 5), 
  cetirizine = c(1,0)
)
ej_1


## Simple comparison

y <- ej_1$defects_yes
n <- ej_1$defects_no + ej_1$defects_yes
p_hat <- y/n
diff <- p_hat[2] - p_hat[1]
se_diff <- sqrt(p_hat[1]*(1 - p_hat[1])/n[1] + p_hat[2]*(1 - p_hat[2])/n[2])
pfround(c(p_hat, diff, se_diff), 2)

## Simple model

fit_1 <- stan_glm(
  cbind(defects_yes, defects_no) ~ cetirizine, 
  family=binomial(link="logit"), 
  data=ej_1
)
print(fit_1, digits=2)




## 3.  Amygdalar data

ej_2 <- read.csv("https://osf.io/ev52x/download")
ej_2$amygdalar <- ej_2[, 2]
ej_2$stress <- ej_2[, 1]

plot(log(ej_2$amygdalar), log(ej_2$stress))

## Simple model

fit_2 <- stan_glm(log(stress) ~ log(amygdalar), data=ej_2)
print(fit_2)

## 4.  Understand the median absolute deviation calculation used to compute standard errors
help("mad")

