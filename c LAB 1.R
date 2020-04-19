setwd("Desktop/econ 4850" )
#makes the package "reader" available
library(readr) 
# I assigned CPS as the data name
CPS <- read_csv("CPSsample.csv")
# estimate earning model
#EARNWEEK = Bo + B1*EDUC + epsilon
mod1 <- lm(EARNWEEK~EDUC, data=CPS)
#show model in a more useful format
#(more information about the estimated model)
summary(mod1)

#calculate the residual the worker 5th
e5 <- 1346 - (-273 + 87.9*13)

eml <- resid(mod1)

e5s <- CPS$EARNWEEK[5] - (mod1$coefficients[1]+mod1$coefficients[2]*CPS$EDUC[5])

#Scatter plot to display ddata and the estimated model
plot(EARNWEEK~EDUC,data=CPS, main="My First Earning Model")
#add the estimated model
## wage.hat=-272.991+87.899*EDUC
abline(mod1,col="red",lwd=2)


#### lecture ##
### the estimated model passes through the mean of wage
### mean of y=Bo.hat + B1.hat(mean of educ)
mean(CPS$EARNWEEK)
mean(CPS$EDUC)
-272.991+87.899*mean(CPS$EDUC) # mean of the predicted salary
### gender dummy variable, reference category = male
CPS$GEND <- (CPS$SEX==2)*1
#### estimating an earning model that includes gender
mod2 <- lm(EARNWEEK~EDUC+GEND, data=CPS)
summary(mod2)
install.packages("car")
# HO: Beta1=50, Beta2=-10000
h0 <- c("EDUC=50", "GEND=-10000" )
linearHypothesis(mod2, ,h0)
