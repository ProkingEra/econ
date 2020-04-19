q2 <- readRDS("Workers.Rda")

q2$SCHL <- as.numeric(q2$SCHL)
q2$HS <- ((q2$SCHL>=16)&(q2$SCHL<=17))*1
q2$ASC <- ((q2$SCHL>=18)&(q2$SCHL<=20))*1
q2$OB <- ((q2$SCHL==21))*1
q2$BB <- ((q2$SCHL>=22))*1
q2$GEND <- ((q2$SEX==2))*1
q2$ID <- (q2$ST==16)*1
###
m9 <- lm(WAGP~HS+ASC+OB+BB+GEND+ID, data = q2)
## ftest
### Ho: beta3 = 2*beta2 or beta3-2*beta2=0
library(car)
aa <- c("OB=2*ASC")
linearHypothesis(m9, ,aa)

ttest <- sqrt(59.218)
### f test for Ho: 38*Beta5 + Beta6
bb <- c("38*AGEP+GEND=0")
linearHypothesis(m9, bb)

##
cc <- ("OB=2*ASC", "38*AGEP+GEND=0")
linearHypothesis(m9, cc)
help(I)

summary(m9)
