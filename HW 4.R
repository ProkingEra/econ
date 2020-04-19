setwd("Desktop/econ 4850")
d0 <- constituents_financials
d0$NM <- d0$Name
d0$SC <- d0$Sector
d0$DY <- d0$`Dividend Yield`
d0$ES <- d0$`Earnings/Share`
d0$MC <- d0$`Market Cap`
d0$PB <- d0$`Price/Book`

div <- subset(d0, d0$DY > 0 & d0$PB >= 0.59, 
              select = c(NM, DY, ES, MC, PB))

div$E1 <- (div$ES >= 1.00)&(div$ES < 3.00)*1
div$E2 <- ((div$ES >= 3.00)&(div$ES < 5.00))*1
div$E3 <- ((div$ES >= 5.00))*1
div$E1 <- as.numeric(div$E1)

div$C1 <- ((div$MC >= 10000000000)&(div$MC < 20000000000))*1
div$C2 <- ((div$MC >= 20000000000)&(div$MC < 40000000000))*1
div$C3 <- (div$MC >= 40000000000)*1

div$P1 <- ((div$PB >= 1.50)&(div$PB < 3.00))*1
div$P2 <- ((div$PB >= 3.00)&(div$PB < 5.00))*1
div$P3 <- (div$PB >= 5.00)*1

m1 <- lm(DY~E1+E2+E3+C1+C2+C3+P1+P2+P3, data = div)
summary(m1)

summary(m1$DY)

# get means for variables in data frame mydata
# excluding missing values 
sapply(mydata, mean, na.rm=TRUE)

Possible functions used in sapply include mean, sd, var, min, max, median, range, and quantile.

library(pastecs)
stat.desc(div) 
# nbr.val, nbr.null, nbr.na, min max, range, sum, 
# median, mean, SE.mean, CI.mean, var, std.dev, coef.var

sapply(div, sd, na.rm=TRUE)

# Boxplot of MPG by Car Cylinders 
boxplot(DY~P3,data=div, main="Tier 3 Price to Book Ratio Effect on Dividend Yield", 
        xlab="Tier 3 Price to Book Ratio", ylab="Dividend Yield")

summary()
