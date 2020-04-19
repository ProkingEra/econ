d0 <- caschool_1_

m1 <- lm(testscr~avginc + I(avginc^2), data = testsc)
summary(m1)
