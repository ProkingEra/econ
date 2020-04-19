setwd("Desktop/econ 4850")
d0 <- readRDS("workersFLID.Rda")
d1 <- subset(d0, d0$WAGP > 0, 
              select = c(SCHL, AGEP, SEX, WKHP, WAGP))
d1$WAGE <- d1$WAGP
d1$EDUC <- d1$SCHL
d1$WH <- d1$WKHP
d1$AGE <- d1$AGEP
d1$SCHL <- as.numeric(d1$SCHL)
d1$HS <- as.numeric(d1$HS)
d1$HS <- ((d1$SCHL==16)|(d1$SCHL==17))*1
d1$ASC <- ((d1$SCHL>=18)&(d1$SCHL<=20))*1
d1$BB <- (d1$SCHL>=21)*1
d1$GEND <- (d1$SEX==2)*1
d2 <- subset(d1, d0$WAGP > 0, 
             select = c(EDUC, AGE, GEND, WH, WAGE, HS, ASC, BB))
m1 <- lm(log(WAGE)~HS+ASC+BB+AGE+I(AGE^2)+GEND+log(WH), data = d2)
summary(m1)

#  Coefficients:
#               Estimate   Std. Error t value  Pr(>|t|)    
#  (Intercept)  3.004e+00  2.707e-02  110.97   <2e-16 ***
#  HS           2.438e-01  1.105e-02   22.06   <2e-16 ***
#  ASC          4.279e-01  1.067e-02   40.10   <2e-16 ***
#  BB           8.561e-01  1.074e-02   79.74   <2e-16 ***
#  AGE          8.792e-02  1.078e-03   81.53   <2e-16 ***
#  I(AGE^2)    -8.150e-04  1.182e-05  -68.97   <2e-16 ***
#  GEND        -2.135e-01  5.543e-03  -38.52   <2e-16 ***
#  log(WH)      1.320e+00  6.233e-03  211.85   <2e-16 ***
library(car)
ftest <- c(BB=ASC*.5)
linearHypothesis(m1, ,ftest)




