setwd("C:/econometric")

d0 <- read.csv("constituents-financials.csv")

d0$NM <- d0$Name
d0$SC <- d0$Sector
d0$DY <- d0$Dividend.Yield
d0$ES <- d0$Earnings.Share
d0$MC <- d0$Market.Cap
d0$PB <- d0$Price.Book


plot(d0$PB, d0$DY)

plot(DY~ES,data = subset(d0, PB<1000&DY<=10))
plot(DY~I(MC/1000000),data = subset(d0, PB<1000))
plot(DY~PB,data = subset(d0, PB<1000&DY<=10))


m1 <- lm(DY~ES+I(MC/10000000)+PB, data = d0) ## basic model
summary(m1)

m4 <- lm(log(DY)~ES+I(MC/10000000)+PB, data = subset(d0, DY>0))
summary(m4)




m2 <- lm(DY~ES+I(MC/10000000)+PB, data =subset(d0, PB<1000))
summary(m2)

m3 <- lm(log(DY)~ES+I(ES^2)+(I(MC/10000000)), data =subset(d0, PB<1000&DY>0))
summary(m3)
a=hist(d0$DY)

plot(m3)
