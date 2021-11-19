## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------


mod <- '

  sj =~ NA*u1 + u8 + u11 + u16 + u21
  is =~ NA*u4 + u13 + u18 + u25
  oi =~ NA*u2 + u6 + u20 + u24
  
  sk =~ NA*u5 + u12 + u19 + u23 + u26
  cu =~ NA*u3 + u7 + u10 + u15
  mi =~ NA*u9 + u14 + u17 + u22
  
  neg =~ NA*sj + is + oi
  pos =~ NA*sk + cu + mi
  
  g   =~ NA*u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10 + 
         u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + 
         u20 + u21 + u22 + u23 + u24 + u25 + u26
  
  sj  ~~ 1* sj
  is  ~~ 1* is
  oi  ~~ 1* oi
  sk  ~~ 1* sk
  cu  ~~ 1* cu
  mi  ~~ 1* mi

  neg ~~ 1* neg
  pos ~~ 1* pos

  g   ~~ 1* g
  
  pos ~~ 0*neg
  g   ~~ 0* neg
  g   ~~ 0* pos
  
'


fit <- lavaan::cfa(
  mod,
  data = data_clean_mh,
  estimator = "WLSMV",
  ordered = items
)


summary(
  fit, 
  fit.measures = TRUE, 
  standardized = TRUE
)






