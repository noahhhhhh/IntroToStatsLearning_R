require(ISLR)
attach(Wage)
#####################################################
## polynomial and step functions ####################
#####################################################
fit <- lm(wage ~ poly(age, 4), data = Wage) # each colum is a linear combination of age, age^2, age^3, and age^4
coef(summary(fit))
# Estimate Std. Error    t value     Pr(>|t|)
# (Intercept)    111.70361  0.7287409 153.283015 0.000000e+00
# poly(age, 4)1  447.06785 39.9147851  11.200558 1.484604e-28
# poly(age, 4)2 -478.31581 39.9147851 -11.983424 2.355831e-32
# poly(age, 4)3  125.52169 39.9147851   3.144742 1.678622e-03
# poly(age, 4)4  -77.91118 39.9147851  -1.951938 5.103865e-02

fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage) # obtain age, ae^2, age^3, and age^4 directly
coef(summary(fit2))
# Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept)            -1.841542e+02 6.004038e+01 -3.067172 0.0021802539
# poly(age, 4, raw = T)1  2.124552e+01 5.886748e+00  3.609042 0.0003123618
# poly(age, 4, raw = T)2 -5.638593e-01 2.061083e-01 -2.735743 0.0062606446
# poly(age, 4, raw = T)3  6.810688e-03 3.065931e-03  2.221409 0.0263977518
# poly(age, 4, raw = T)4 -3.203830e-05 1.641359e-05 -1.951938 0.0510386498

fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage) # equivalent as above
coef(summary(fit2a))
# Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept) -1.841542e+02 6.004038e+01 -3.067172 0.0021802539
# age          2.124552e+01 5.886748e+00  3.609042 0.0003123618
# I(age^2)    -5.638593e-01 2.061083e-01 -2.735743 0.0062606446
# I(age^3)     6.810688e-03 3.065931e-03  2.221409 0.0263977518
# I(age^4)    -3.203830e-05 1.641359e-05 -1.951938 0.0510386498

fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage) # equivalent as above
coef(summary(fit2b))
# Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept)                        -1.841542e+02 6.004038e+01 -3.067172 0.0021802539
# cbind(age, age^2, age^3, age^4)age  2.124552e+01 5.886748e+00  3.609042 0.0003123618
# cbind(age, age^2, age^3, age^4)    -5.638593e-01 2.061083e-01 -2.735743 0.0062606446
# cbind(age, age^2, age^3, age^4)     6.810688e-03 3.065931e-03  2.221409 0.0263977518
# cbind(age, age^2, age^3, age^4)    -3.203830e-05 1.641359e-05 -1.951938 0.0510386498

# create a grid of values for age and predict
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = T)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# plot the data and add the fit from degree-4 polynomial
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# anova
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

fit.6 <- lm(wage ~ education + age, data = Wage)
fit.7 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.8 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.6, fit.7, fit.8)

# predcit whether an individual earns more than $250,000 per year
fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
preds <- predict(fit, newdata = list(age = age.grid), se = T)
# logit to probability
pfit <- exp(preds$fit)/(1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
preds <- predict(fit, newdata = list(age = age.grid), type = "response", se = T)
# plot
plot(age, I(wage > 250), xlim = agelims, type ="n", ylim=c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey" )
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# step function
table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

#####################################################
## splines ##########################################
#####################################################
library(splines)
# bs()
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * preds$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2 * preds$se.fit, lty = "dashed")

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

# ns() natural spline
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# smooth.splines()
# Notice that in the first call to smooth.spline(), we specified df=16. The
# function then determines which value of λ leads to 16 degrees of freedom. In
# the second call to smooth.spline(), we select the smoothness level by crossvalidation;
# this results in a value of λ that yields 6.8 degrees of freedom.
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = T)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# loess(): local regression
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)

#####################################################
## gam ##############################################
#####################################################
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = T, col = "blue")
plot.gam(gam1, se = T, col = "red")

# anova
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3)
# Analysis of Deviance Table
# 
# Model 1: wage ~ s(age, 5) + education
# Model 2: wage ~ year + s(age, 5) + education
# Model 3: wage ~ s(year, 4) + s(age, 5) + education
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1      2990    3711731                          
# 2      2989    3693842  1  17889.2 0.0001419 ***
# 3      2986    3689770  3   4071.1 0.3483897    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(gam.m3)

# predict
preds <- predict(gam.m2, newdata = Wage)
# lo() for local regression
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = .7) + education, data = Wage)
plot.gam(gam.lo, se = T, col = "green")
# using lo() to create interactions
gam.lo.i <- gam(wage ~ lo(year, age, span = .5) + education, data = Wage)

# plot
library(akima)
plot(gam.lo.i)

# logistic regression
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")
table(education, I(wage > 250))
# education            FALSE TRUE
# 1. < HS Grad         268    0
# 2. HS Grad           966    5
# 3. Some College      643    7
# 4. College Grad      663   22
# 5. Advanced Degree   381   45
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr, se = T, col = "green")


