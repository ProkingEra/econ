# model 1
## wage = b0 + b1AGE + b2AGE^2 + b3EDUC + epsilon
## age is a proxy of labor experience
d1 <- CPSsample

m1 <- lm(EARNWEEK~AGE + I(AGE^2) + EDUC, data = CPSsample)
summary(m1)

#claim H0: B1<=0 ; H1: B1 > 0
#      H0: B2>=0 : H1: B2 < 0
# T value for B!<=0 = 5.867   P value on one side is close to zero
# T value for B2>=0 = -4.941  P value on one side is close to zero
# We reject H0 at any level of significance
# Therefore, the evidence supports the researcher's claim (B1 > 0 , B2 < 0)
## D) 
hist(d1$EARNWEEK)

m2 <- lm(log(EARNWEEK)~AGE + I(AGE^2) + EDUC, data = CPSsample)
summary(m2)
### the effect on education us natural logarthim would be an additional 8.8% increase in weekly salary
### on average with all else equal
d2 <- hprice2_1_
## model 
## ln(price) = B0 + B1(rooms) + B2(rooms^2) + B3(stratio) + B4(dist) + B5*ln(nox) + epsilon
m3 <- lm(log(price)~rooms+I(rooms^2)+stratio+dist+log(nox),data = d2)
summary(m3)
