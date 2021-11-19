## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

library(blavaan)
future::plan("multiprocess")

HS.model <- 
  "
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9 
  "

fit <- bcfa(
  HS.model, 
  data = HolzingerSwineford1939, 
  std.lv=TRUE, 
  mcmcfile ="default", 
  target="stan"
  )

summary(fit)








setwd("") #insert directory

#install.packages('lavaan')
#install.packages('psych')
#install.packages('blavaan')
library(lavaan)
library(psych)
library(blavaan)

A <- read.csv("Data.csv",header=T,sep=";"); headTail(A)

itemnames <- names(A)

#Models: Design od 3(0|1|2 method factors) x 2(with/without Observe) x 2(Hierarchical/Bifactor)
model1Baer <-'M=~ O+D+AW+NJ+NR
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33'

model2 <-'M=~ D+AW+NJ+NR
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33'

model3 <-'M=~ O+D+AW+NJ+NR
Bias=~  FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

Bias~~0*M
Bias~~0*O
Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR'

model4 <-'M=~ D+AW+NJ+NR
Bias=~  FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

Bias~~0*M
Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR'

model5 <-'M=~ O+D+AW+NJ+NR
Pos=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ12 + FFMQ16 + FFMQ22
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

Pos~~0*M
Pos~~0*O
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Pos~~0*Neg

Neg~~0*M
Neg~~0*O
Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR'

model6 <-'M=~ D+AW+NJ+NR
Pos=~ FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ12 + FFMQ16 + FFMQ22
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

Pos~~0*M
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Pos~~0*Neg

Neg~~0*M
Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR'

model7 <- 'M=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

M~~0*O
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

O~~0*D
O~~0*AW
O~~0*NJ
O~~0*NR
D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model8 <- 'M=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model9 <- 
  'M =~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Bias=~  FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

M~~0*Bias
M~~0*O
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Bias~~0*O
Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR

O~~0*D
O~~0*AW
O~~0*NJ
O~~0*NR
D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model10 <- 
  'M =~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Bias=~  FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

M~~0*Bias
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR

D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model11Aguado <- 'M=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Pos=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ12 + FFMQ16 + FFMQ22
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

M~~0*Pos
M~~0*Neg
M~~0*O
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Pos~~0*Neg
Pos~~0*O
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Neg~~0*O
Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR

O~~0*D
O~~0*AW
O~~0*NJ
O~~0*NR
D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model12 <- 'M=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Pos=~ FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ12 + FFMQ16 + FFMQ22
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33

M~~0*Pos
M~~0*Neg
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Pos~~0*Neg
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR

D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'


#possible MIs
'FFMQ5~~FFMQ13
FFMQ34~~FFMQ38'


#Estimate CFAs
fit1Baer <- cfa(model1Baer, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit2 <- cfa(model2, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit3 <- cfa(model3, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit4 <- cfa(model4, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit5 <- cfa(model5, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit6 <- cfa(model6, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit7 <- cfa(model7, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit8 <- cfa(model8, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit9 <- cfa(model9, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit10 <- cfa(model10, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit11Aguado <- cfa(model11Aguado, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")
fit12 <- cfa(model12, data = A,ordered=itemnames,std.lv=TRUE,estimator="WLSMV",test="Satorra.Bentler")

fits <- list(fit1Baer,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11Aguado,fit12)

# some models do not converge

#obtain fit indices
fitss <- lapply(fits,fitMeasures,fit.measures=c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr"))
array(unlist(fitss),dim=c(12,7),dimnames=list(paste("model",1:12,sep=""),list("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr")))

#obtain fit and parameters
summary(fit1Baer,fit.measures=TRUE,standardized=TRUE)
summary(fit2,fit.measures=TRUE,standardized=TRUE)
summary(fit3,fit.measures=TRUE,standardized=TRUE)
summary(fit4,fit.measures=TRUE,standardized=TRUE)
summary(fit5,fit.measures=TRUE,standardized=TRUE)
summary(fit6,fit.measures=TRUE,standardized=TRUE) #good fit
summary(fit7,fit.measures=TRUE,standardized=TRUE)
summary(fit8,fit.measures=TRUE,standardized=TRUE)
summary(fit9,fit.measures=TRUE,standardized=TRUE)
summary(fit10,fit.measures=TRUE,standardized=TRUE)
summary(fit11Aguado,fit.measures=TRUE,standardized=TRUE)
summary(fit12,fit.measures=TRUE,standardized=TRUE) #borderline fit

#only standardized estimates
standardizedsolution(fit1Baer)
standardizedsolution(fit2)
standardizedsolution(fit3)
standardizedsolution(fit4)
standardizedsolution(fit5)
standardizedsolution(fit6)
standardizedsolution(fit7)
standardizedsolution(fit8)
standardizedsolution(fit9)
standardizedsolution(fit10)
standardizedsolution(fit11Aguado)
standardizedsolution(fit12)

#examine modification indices
fitm1 <- modindices(fit1Baer)
#MIs>10
mi2 <- subset(fit1mi,mi.scaled>50); mi2
mi3 <- subset(fit1mi,mi.scaled>100); mi3

fitm2 <- modindices(fit2)
#MIs>10
mi2 <- subset(fit2mi,mi.scaled>50); mi2
mi3 <- subset(fit2mi,mi.scaled>100); mi3

fitm3 <- modindices(fit3)
#MIs>10
mi2 <- subset(fit3mi,mi.scaled>50); mi2
mi3 <- subset(fit3mi,mi.scaled>100); mi3

fitm4 <- modindices(fit4)
#MIs>10
mi2 <- subset(fit4mi,mi.scaled>50); mi2
mi3 <- subset(fit4mi,mi.scaled>100); mi3

fitm5 <- modindices(fit5)
#MIs>10
mi2 <- subset(fit5mi,mi.scaled>50); mi2
mi3 <- subset(fit5mi,mi.scaled>100); mi3

fitm6 <- modindices(fit6)
#MIs>10
mi2 <- subset(fit6mi,mi.scaled>50); mi2
mi3 <- subset(fit6mi,mi.scaled>100); mi3

fitm7 <- modindices(fit7)
#MIs>10
mi2 <- subset(fit7mi,mi.scaled>50); mi2
mi3 <- subset(fit7mi,mi.scaled>100); mi3

fitm8 <- modindices(fit8)
#MIs>10
mi2 <- subset(fit8mi,mi.scaled>50); mi2
mi3 <- subset(fit8mi,mi.scaled>100); mi3

fitm9 <- modindices(fit9)
#MIs>10
mi2 <- subset(fit9mi,mi.scaled>50); mi2
mi3 <- subset(fit9mi,mi.scaled>100); mi3

fitm10 <- modindices(fit10)
#MIs>10
mi2 <- subset(fit10mi,mi.scaled>50); mi2
mi3 <- subset(fit10mi,mi.scaled>100); mi3

fitm11 <- modindices(fit11Aguado)
#MIs>10
mi2 <- subset(fit11mi,mi.scaled>50); mi2
mi3 <- subset(fit11mi,mi.scaled>100); mi3

fitm12 <- modindices(fit12)
#MIs>10
mi2 <- subset(fit12mi,mi.scaled>50); mi2
mi3 <- subset(fit12mi,mi.scaled>100); mi3

#all MIs
fitm <- rbind(fitm1,fitm2,fitm3,fitm4,fitm5,fitm6,fitm7,fitm8,fitm9,fitm10,fitm11,fitm12); fitm

##############        BSEM with blavaan     ###########################

#Using the prior() operator within the model syntax always override prior distributions set using dpriors()

dpriors() #see default priors

#dpriors(nu = "dnorm(0,1e-3)", alpha = "dnorm(0,1e-2)",
#        lambda = "dnorm(0,1e-2)", beta = "dnorm(0,1e-2)",
#        itheta = "dgamma(1,.5)", ipsi = "dgamma(1,.5)",
#        rho = "dbeta(1,1)", ibpsi = "dwish(iden,3)",
#        tau="dnorm(0,.1)", delta="dgamma(1,.5)")

# nu Prior distribution for nu (observed variable intercept) parameters.
# alpha Prior distribution for alpha (latent variable intercept) parameters.
# lambda Prior distribution for lambda (loading) parameters.
# beta Prior distribution for regression parameters.
# itheta Prior distribution for observed variable precision parameters. Adding [sd] to it goes for the sd rather than precision
# ipsi Prior distribution for latent variable precision parameters. Adding [sd] to it goes for the sd rather than precision: ipsi = "dgamma(1,.5)[sd]"
# rho Prior distribution for correlation parameters (only used under srs approach).
# ibpsi Prior distribution for inverse covariance matrix of blocks of latent variables.
# tau Prior distribution for threshold parameters (ordinal data only).
# delta Prior distribution for delta parameters (ordinal data only).

# test test test
findjags(look_in = 'C:/Program Files/JAGS')
testjags('C:/Program Files/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe')

#test test one two test
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '

fit <- bcfa(HS.model, data=HolzingerSwineford1939,n.chains=3,sample=1000,burnin=1000,adapt=1000,
            jagcontrol=list(method="rjparallel"))
summary(fit)
# end of test

#implmented priors
priors <- dpriors(#nu = "dunif(3,3.5)[sd]",
  #lambda = "dunif(0,1)[sd]", 
  lambda = "dnorm(.5,10)", #loadings
  
  nu = "dnorm(3,1)T(1,5)", #item intercepts
  itheta = "dlnorm(1, .1)[sd]", #item precision parameter. [sd] added to make it as SD
  #itheta = "dgamma(1, 4)[sd]",
  
  alpha = "dnorm(0,1)", #factor intercepts
  ipsi = "dlnorm(1, 5)[sd]", #factors precision parameters. [sd] added to make it as SD
  #ipsi = "dlnorm(.5, .1)[sd]",
  
  ibpsi = "dwish(iden,5)" #inverse covariance matrix of blocks of latent variables
  #ibpsi = "dunif(-1, 1)",
  #ibpsi = "dlnorm(1, .5)[sd]",
  #rho = "dbeta(2,2)") #correlations
  #rho = "dnorm(.9,0.01)"),
)

#BSEM does not recognize polychoric or estimate to ordinal data; estimator will be ignored
#BSEM produces HUGE files (>100MB, even 1 or 2 GB): Maybe your PC is unable to save all models in one workspace. Consider saving in multiple .RData files


#BSEM for normal models

bfit1Baer <- bcfa(model1Baer, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit2 <- bcfa(model2, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit3 <- bcfa(model3, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit4 <- bcfa(model4, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit5 <- bcfa(model5, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit6 <- bcfa(model6, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit7 <- bcfa(model7, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit8 <- bcfa(model8, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit9 <- bcfa(model9, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit10 <- bcfa(model10, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit11Aguado <- bcfa(model11Aguado, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
bfit12 <- bcfa(model12, data = A,dp=priors,cp="srs",std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(2); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing

#fit
bfitm1 <- fitMeasures(bfit1Baer)
bfitm2 <- fitMeasures(bfit2)
bfitm3 <- fitMeasures(bfit3)
bfitm4 <- fitMeasures(bfit4)
bfitm5 <- fitMeasures(bfit5)
bfitm6 <- fitMeasures(bfit6)
bfitm7 <- fitMeasures(bfit7)
bfitm8 <- fitMeasures(bfit8)
bfitm9 <- fitMeasures(bfit9)
bfitm10 <- fitMeasures(bfit10)
bfitm11 <- fitMeasures(bfit11Aguado)
bfitm12 <- fitMeasures(bfit12)

bfit <- rbind(bfitm1,bfitm2,bfitm3,bfitm4,bfitm5,bfitm6,bfitm7,bfitm8,bfitm9,bfitm10,bfitm11,bfitm12); bfit

#Models with cross-loadings
model1Baerc <-'M=~ O+D+AW+NJ+NR
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 +prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 +  prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39'

model2c <-'M=~ D+AW+NJ+NR
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39'

model3c <-'M=~ O+D+AW+NJ+NR
Bias=~  FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 +prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

Bias~~0*M
Bias~~0*O
Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR'

model4c <-'M=~ D+AW+NJ+NR
Bias=~  FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

Bias~~0*M
Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR'

model5c <-'M=~ O+D+AW+NJ+NR
Pos=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ12 + FFMQ16 + FFMQ22 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 +prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

Pos~~0*M
Pos~~0*O
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Pos~~0*Neg

Neg~~0*M
Neg~~0*O
Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR'

model6c <-'M=~ D+AW+NJ+NR
Pos=~ FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ12 + FFMQ16 + FFMQ22 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + 
prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + 
prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + 
prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + 
prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + 
prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

Pos~~0*M
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Pos~~0*Neg

Neg~~0*M
Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR'

model7c <- 'M=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + 
prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + 
prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 +
prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + 
prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + 
prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

M~~0*O
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

O~~0*D
O~~0*AW
O~~0*NJ
O~~0*NR
D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model8c <- 'M=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model9c <- 
  'M =~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Bias=~  FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + 
prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 +
prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + 
prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + 
prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

M~~0*Bias
M~~0*O
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Bias~~0*O
Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR

O~~0*D
O~~0*AW
O~~0*NJ
O~~0*NR
D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model10c <- 
  'M =~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Bias=~  FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + 
prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + 
prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + 
prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

M~~0*Bias
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Bias~~0*D
Bias~~0*AW
Bias~~0*NJ
Bias~~0*NR

D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model11Aguadoc <- 'M=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Pos=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + 
FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + 
FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + 
FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + 
FFMQ12 + FFMQ16 + FFMQ22 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
O=~ FFMQ1 + FFMQ6 + FFMQ11 + FFMQ15 + FFMQ20 + FFMQ26 + FFMQ31 + FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 +  prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ1 + prior("dnorm(0,.5)")*FFMQ6 + prior("dnorm(0,.5)")*FFMQ11 + prior("dnorm(0,.5)")*FFMQ15 + prior("dnorm(0,.5)")*FFMQ20 + prior("dnorm(0,.5)")*FFMQ26 + prior("dnorm(0,.5)")*FFMQ31 + prior("dnorm(0,.5)")*FFMQ36 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

M~~0*Pos
M~~0*Neg
M~~0*O
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Pos~~0*Neg
Pos~~0*O
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Neg~~0*O
Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR

O~~0*D
O~~0*AW
O~~0*NJ
O~~0*NR
D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'

model12c <- 'M=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33
Pos=~ FFMQ2 + FFMQ7 + FFMQ27 + FFMQ32 + FFMQ37 + FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22
Neg=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + FFMQ12 + FFMQ16 + FFMQ22 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
D=~ FFMQ2 + FFMQ7 + FFMQ12 + FFMQ16 + FFMQ22 + FFMQ27 + FFMQ32 + FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
AW=~ FFMQ5 + FFMQ8 + FFMQ13 + FFMQ18 + FFMQ23 + FFMQ28 + FFMQ34 + FFMQ38 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NJ=~ FFMQ3 + FFMQ10 + FFMQ14 + FFMQ17 + FFMQ25 + FFMQ30 + FFMQ35 + FFMQ39 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ4 + prior("dnorm(0,.5)")*FFMQ9 + prior("dnorm(0,.5)")*FFMQ19 + prior("dnorm(0,.5)")*FFMQ21 + prior("dnorm(0,.5)")*FFMQ24 + prior("dnorm(0,.5)")*FFMQ29 + prior("dnorm(0,.5)")*FFMQ33
NR=~ FFMQ4 + FFMQ9 + FFMQ19 + FFMQ21 + FFMQ24 + FFMQ29 + FFMQ33 + prior("dnorm(0,.5)")*FFMQ2 + prior("dnorm(0,.5)")*FFMQ7 + prior("dnorm(0,.5)")*FFMQ12 + prior("dnorm(0,.5)")*FFMQ16 + prior("dnorm(0,.5)")*FFMQ22 + prior("dnorm(0,.5)")*FFMQ27 + prior("dnorm(0,.5)")*FFMQ32 + prior("dnorm(0,.5)")*FFMQ37 + prior("dnorm(0,.5)")*FFMQ5 + prior("dnorm(0,.5)")*FFMQ8 + prior("dnorm(0,.5)")*FFMQ13 + prior("dnorm(0,.5)")*FFMQ18 + prior("dnorm(0,.5)")*FFMQ23 + prior("dnorm(0,.5)")*FFMQ28 + prior("dnorm(0,.5)")*FFMQ34 + prior("dnorm(0,.5)")*FFMQ38 + prior("dnorm(0,.5)")*FFMQ3 + prior("dnorm(0,.5)")*FFMQ10 + prior("dnorm(0,.5)")*FFMQ14 + prior("dnorm(0,.5)")*FFMQ17 + prior("dnorm(0,.5)")*FFMQ25 + prior("dnorm(0,.5)")*FFMQ30 + prior("dnorm(0,.5)")*FFMQ35 + prior("dnorm(0,.5)")*FFMQ39

M~~0*Pos
M~~0*Neg
M~~0*D
M~~0*AW
M~~0*NJ
M~~0*NR

Pos~~0*Neg
Pos~~0*D
Pos~~0*AW
Pos~~0*NJ
Pos~~0*NR

Neg~~0*D
Neg~~0*AW
Neg~~0*NJ
Neg~~0*NR

D~~0*AW
D~~0*NJ
D~~0*NR
AW~~0*NJ
AW~~0*NR
NJ~~0*NR'


#estimate
bfit1Baerc <- bcfa(model1Baerc, data = A,dp=priors,cp="srs",
                   std.lv=TRUE,auto.var=TRUE,n.chains=3,sample=20000,burnin=50000,adapt=50000,auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit2c <- bcfa(model2c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit3c <- bcfa(model3c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit4c <- bcfa(model4c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit5c <- bcfa(model5c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit6c <- bcfa(model6c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit7c <- bcfa(model7c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit8c <- bcfa(model8c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit9c <- bcfa(model9c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit10c <- bcfa(model10c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit11Aguadoc <- bcfa(model11Aguadoc, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(1); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing
bfit12c <- bcfa(model12c, data = A,dp=priors,cp="srs",n.chains=3,sample=20000,burnin=50000,adapt=50000,std.lv=TRUE,auto.var=TRUE, auto.fix.first=TRUE,auto.cov.lv.x=TRUE,jagcontrol=list(method="rjparallel")); beepr::beep(2); gc()
save.image("C:/.../.RData") #save work image due to huge time for computing

#fit
bfitm1c <- fitMeasures(bfit1Baerc)
bfitm2c <- fitMeasures(bfit2c)
bfitm3c <- fitMeasures(bfit3c)
bfitm4c <- fitMeasures(bfit4c)
bfitm5c <- fitMeasures(bfit5c)
bfitm6c <- fitMeasures(bfit6c)
bfitm7c <- fitMeasures(bfit7c)
bfitm8c <- fitMeasures(bfit8c)
bfitm9c <- fitMeasures(bfit9c)
bfitm10c <- fitMeasures(bfit10c)
bfitm11c <- fitMeasures(bfit11Aguadoc)
bfitm12c <- fitMeasures(bfit12c)

bfitc <- rbind(bfitm1c,bfitm2c,bfitm3c,bfitm4c,bfitm5c,bfitm6c,bfitm7c,bfitm8c,bfitm9c,bfitm10c,bfitm11c,bfitm12c); bfitc

save.image("C:/.../.RData") #save work image 

#save fits in csv
write.table(bfit,file="bfit.csv",dec=",",sep="\t")
write.table(bfitc,file="bfitc.csv",dec=",",sep="\t")

#search for significant loadings
hpd1 <- blavInspect(bfit1Baer,"hpd")
hpd2 <- blavInspect(bfit2,"hpd")
hpd3 <- blavInspect(bfit3,"hpd")
hpd4 <- blavInspect(bfit4,"hpd")
hpd5 <- blavInspect(bfit5,"hpd")
hpd6 <- blavInspect(bfit6,"hpd")
hpd7 <- blavInspect(bfit7,"hpd")
hpd8 <- blavInspect(bfit8,"hpd")
hpd9 <- blavInspect(bfit9,"hpd")
hpd10 <- blavInspect(bfit10,"hpd")
hpd11 <- blavInspect(bfit11Aguado,"hpd")
hpd12 <- blavInspect(bfit12,"hpd")

hpd1
hpd2
hpd3
hpd4
hpd5
hpd6
hpd7
hpd8
hpd9
hpd10
hpd11
hpd12

#models with cross
hpd1c <- blavInspect(bfit1Baerc,"hpd")
hpd2c <- blavInspect(bfit2c,"hpd")
hpd3c <- blavInspect(bfit3c,"hpd")
hpd4c <- blavInspect(bfit4c,"hpd")
hpd5c <- blavInspect(bfit5c,"hpd")
hpd6c <- blavInspect(bfit6c,"hpd")
hpd7c <- blavInspect(bfit7c,"hpd")
hpd8c <- blavInspect(bfit8c,"hpd")
hpd9c <- blavInspect(bfit9c,"hpd")
hpd10c <- blavInspect(bfit10c,"hpd")
hpd11c <- blavInspect(bfit11Aguadoc,"hpd")
hpd12c <- blavInspect(bfit12c,"hpd")

hpd1c
hpd2c
hpd3c
hpd4c
hpd5c
hpd6c
hpd7c
hpd8c
hpd9c
hpd10c
hpd11c
hpd12c

#fit and parameters
summary(bfit1Baer,postmedian=TRUE,standardized=TRUE)
summary(bfit2,postmedian=TRUE,standardized=TRUE)
summary(bfit3,postmedian=TRUE,standardized=TRUE)
summary(bfit4,postmedian=TRUE,standardized=TRUE)
summary(bfit5,postmedian=TRUE,standardized=TRUE)
summary(bfit6,postmedian=TRUE,standardized=TRUE)
summary(bfit7,postmedian=TRUE,standardized=TRUE)
summary(bfit8,postmedian=TRUE,standardized=TRUE)
summary(bfit9,postmedian=TRUE,standardized=TRUE)
summary(bfit10,postmedian=TRUE,standardized=TRUE)
summary(bfit11Aguado,postmedian=TRUE,standardized=TRUE)
summary(bfit12,postmedian=TRUE,standardized=TRUE)

#models with cross
summary(bfit1Baerc,postmedian=TRUE,standardized=TRUE)
summary(bfit2c,postmedian=TRUE,standardized=TRUE)
summary(bfit3c,postmedian=TRUE,standardized=TRUE)
summary(bfit4c,postmedian=TRUE,standardized=TRUE)
summary(bfit5c,postmedian=TRUE,standardized=TRUE)
summary(bfit6c,postmedian=TRUE,standardized=TRUE)
summary(bfit7c,postmedian=TRUE,standardized=TRUE)
summary(bfit8c,postmedian=TRUE,standardized=TRUE)
summary(bfit9c,postmedian=TRUE,standardized=TRUE)
summary(bfit10c,postmedian=TRUE,standardized=TRUE)
summary(bfit11Aguadoc,postmedian=TRUE,standardized=TRUE)
summary(bfit12c,postmedian=TRUE,standardized=TRUE)


#informative plots for any model
#plot(bfit12c,148,"trace") #trace plots
#plot(bfit12c,148,"density") #density plots
#plot(bfit12c,148,"key") #key plots
#plot(bfit12c,148,"ecdf") #ecdf plots
#plot(bfit12c,148,"histogram") #histogram plots
#plot(bfit12c,148,"autocorr") #autocorr plots
#plot(bfit12c,148,"crosscorr") #crosscorr plots
#plot(bfit12c,1484,"all") #all plots

#Check for convergence issues by parameters: Search for psrf>1,2 in each object
converbfit1c <- subset(cbind(parameterEstimates(bfit1Baerc),bfit1Baerc@ParTable$se,bfit1Baerc@ParTable$psrf),bfit1Baerc@ParTable$psrf>1.2); converbfit1c
converbfit2c <- subset(cbind(parameterEstimates(bfit2c),bfit2c@ParTable$se,bfit2c@ParTable$psrf),bfit2c@ParTable$psrf>1.2); converbfit2c
converbfit3c <- subset(cbind(parameterEstimates(bfit3c),bfit3c@ParTable$se,bfit3c@ParTable$psrf),bfit3c@ParTable$psrf>1.2); converbfit3c
converbfit4c <- subset(cbind(parameterEstimates(bfit4c),bfit4c@ParTable$se,bfit4c@ParTable$psrf),bfit4c@ParTable$psrf>1.2); converbfit4c
converbfit5c <- subset(cbind(parameterEstimates(bfit5c),bfit5c@ParTable$se,bfit5c@ParTable$psrf),bfit5c@ParTable$psrf>1.2); converbfit5c
converbfit6c <- subset(cbind(parameterEstimates(bfit6c),bfit6c@ParTable$se,bfit6c@ParTable$psrf),bfit6c@ParTable$psrf>1.2); converbfit6c
converbfit7c <- subset(cbind(parameterEstimates(bfit7c),bfit7c@ParTable$se,bfit7c@ParTable$psrf),bfit7c@ParTable$psrf>1.2); converbfit7c
converbfit8c <- subset(cbind(parameterEstimates(bfit8c),bfit8c@ParTable$se,bfit8c@ParTable$psrf),bfit8c@ParTable$psrf>1.2); converbfit8c
converbfit9c <- subset(cbind(parameterEstimates(bfit9c),bfit9c@ParTable$se,bfit9c@ParTable$psrf),bfit9c@ParTable$psrf>1.2); converbfit9c
converbfit10c <- subset(cbind(parameterEstimates(bfit10c),bfit10c@ParTable$se,bfit10c@ParTable$psrf),bfit10c@ParTable$psrf>1.2); converbfit10c
converbfit11c <- subset(cbind(parameterEstimates(bfit11Aguadoc),bfit11Aguadoc@ParTable$se,bfit11Aguadoc@ParTable$psrf),bfit11Aguadoc@ParTable$psrf>1.2); converbfit11c
converbfit12c <- subset(cbind(parameterEstimates(bfit12c),bfit12c@ParTable$se,bfit12c@ParTable$psrf),bfit12c@ParTable$psrf>1.2); converbfit12c

#errata finder: if label puts "error blablabla" get the number in partable to find which:
bfit1Baerc@ParTable$label
bfit2c@ParTable$label
bfit3c@ParTable$label
bfit4c@ParTable$label
bfit5c@ParTable$label
bfit6c@ParTable$label
bfit7c@ParTable$label
bfit8c@ParTable$label
bfit9c@ParTable$label
bfit10c@ParTable$label
bfit11Aguadoc@ParTable$label
bfit12c@ParTable$label

#errors encountered
cbind(bfit4c@ParTable$lhs[90],bfit4c@ParTable$op[90],bfit4c@ParTable$rhs[90])
cbind(bfit5c@ParTable$lhs[193],bfit5c@ParTable$op[193],bfit5c@ParTable$rhs[193])
cbind(bfit12c@ParTable$lhs[148],bfit12c@ParTable$op[148],bfit12c@ParTable$rhs[148])


#MODEL COMPARISON

#compare models - use the LOO statistic + se
#systematic comparison between each factors

#NO CROSS-LOADINGS MODELS
#Observe included vs excluded
deltafit1 <- blavCompare(bfit1Baer,bfit2); gc()
deltafit2 <- blavCompare(bfit3,bfit4); gc()
deltafit3 <- blavCompare(bfit5,bfit6); gc()
deltafit4 <- blavCompare(bfit7,bfit8); gc()
deltafit5 <- blavCompare(bfit9,bfit10); gc()
deltafit6 <- blavCompare(bfi11Aguado,bfit12); gc()

#hierarchical vs bifactor
deltafit1 <- blavCompare(bfit1Baer,bfit7); gc()
deltafit2 <- blavCompare(bfit2,bfit8); gc()
deltafit3 <- blavCompare(bfit3,bfit7); gc()
deltafit4 <- blavCompare(bfit4,bfit10); gc()
deltafit5 <- blavCompare(bfit5,bfit11Aguado); gc()

#0 vs 1 vs 2 method factors
#0 vs 1
deltafit1 <- blavCompare(bfit1Baer,bfit3); gc()
deltafit2 <- blavCompare(bfit2,bfit4); gc()
deltafit3 <- blavCompare(bfit7,bfit9); gc()
deltafit4 <- blavCompare(bfit8,bfit10); gc()
#1 vs 2
deltafit6 <- blavCompare(bfit3,bfit5); gc()
deltafit7 <- blavCompare(bfit4,bfit6); gc()
deltafit8 <- blavCompare(bfit9,bfit11Aguado); gc()
deltafit9 <- blavCompare(bfit10,bfit12); gc()
#0 vs 2
deltafit10 <- blavCompare(bfit1,bfit5); gc()
deltafit11 <- blavCompare(bfit1,bfit6); gc()
deltafit12 <- blavCompare(bfit7,bfit11Aguado); gc()
deltafit13 <- blavCompare(bfit8,bfit12); gc()

#CROSS-LOADINGS MODELS

#Observe included vs excluded
deltafit1cross <- blavCompare(bfit1Baerc,bfit2c); gc()
deltafit2cross <- blavCompare(bfit3c,bfit4c); gc()
deltafit3cross <- blavCompare(bfit5c,bfit6c); gc()
deltafit4cross <- blavCompare(bfit7c,bfit8c); gc()
deltafit5cross <- blavCompare(bfit9c,bfit10c); gc()
deltafit6cross <- blavCompare(bfit11Aguadoc,bfit12c); gc()

#hierarchical vs bifactor
deltafit1cross <- blavCompare(bfit1Baerc,bfit7c); gc()
deltafit2cross <- blavCompare(bfit2c,bfit8c); gc()
deltafit3cross <- blavCompare(bfit3c,bfit7c); gc()
deltafit4cross <- blavCompare(bfit4c,bfit10c); gc()
deltafit5cross <- blavCompare(bfit5c,bfit11Aguadoc); gc()

#0 vs 1 vs 2 method factors
#0 vs 1
deltafit1cross <- blavCompare(bfit1Baerc,bfit3c); gc()
deltafit2cross <- blavCompare(bfit2c,bfit4c); gc()
deltafit3cross <- blavCompare(bfit7c,bfit9c); gc()
deltafit4cross <- blavCompare(bfit8c,bfit10c); gc()
#1 vs 2
deltafit6cross <- blavCompare(bfit3c,bfit5c); gc()
deltafit7cross <- blavCompare(bfit4c,bfit6c); gc()
deltafit8cross <- blavCompare(bfit9c,bfit11Aguadoc); gc()
deltafit9cross <- blavCompare(bfit10c,bfit12c); gc()
#0 vs 2
deltafit10cross <- blavCompare(bfit1Baerc,bfit5c); gc()
deltafit11cross <- blavCompare(bfit1Baerc,bfit6c); gc()
deltafit12cross <- blavCompare(bfit7c,bfit11Aguadoc); gc()
deltafit13cross <- blavCompare(bfit8c,bfit12c); gc()




