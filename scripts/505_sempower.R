library("semPower")

ap <- semPower.aPriori(
  effect = .05, 
  effect.measure = 'RMSEA', 
  alpha = .05, 
  power = .80, 
  df = 168
)

summary(ap)
# semPower: A-priori power analysis
# 
# F0                        0.420000
# RMSEA                     0.050000
# Mc                        0.810584
# 
# df                        168     
# Required Num Observations 123     
# 
# Critical Chi-Square       199.2441
# NCP                       51.24000
# Alpha                     0.050000
# Beta                      0.197163
# Power (1-beta)            0.802837
# Implied Alpha/Beta Ratio  0.253597

# We used the function semPower.aPriori of the R package semPower to determine 
# the required sample size to detect misspecifications of a model (involving 
# df = 168 degrees of freedom) -- our
# final SEM model -- corresponding to RMSEA = .05 with a power of 80% on an 
# alpha error of .05. The results show that a sample size of N = 123 yields a 
# power of approximately 80% to reject a wrong model (with df = 168) with an 
# amount of misspecification corresponding to RMSEA = .05 on alpha = .05.

ph <- semPower.postHoc(
  effect = .06, 
  effect.measure = 'RMSEA', 
  alpha = .05, 
  N = 783, 
  df = 168)

summary(ph)
# semPower: Post-hoc power analysis
# 
# F0                       0.604800    
# RMSEA                    0.060000    
# Mc                       0.739042    
# 
# df                       168         
# Num Observations         783         
# NCP                      472.9536    
# 
# Critical Chi-Square      199.2441    
# Alpha                    0.050000    
# Beta                     1.848897e-35
# Power (1-beta)           > 0.9999    
# Implied Alpha/Beta Ratio 2.704314e+33

# We also used the semPower.postHoc function to determine the actually 
# achieved power with the sample size of the present sample. The results 
# show that a sample size of N = 783 is associated with a power larger 
# than > 99.99% to reject a wrong model (with df = 168) with an amount of 
# misspecification corresponding to RMSEA = .06 on alpha = .05.


