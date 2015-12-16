require(MASS)
require(ISLR)
fix(Boston)
attach(Boston)
#####################################################
## one variable linear regression ###################
#####################################################
lm.fit1 <- lm(medv ~ lstat)
lm.fit1
summary(lm.fit1)

names(lm.fit1)
confint(lm.fit1)

predict (lm.fit1 ,data.frame(lstat =(c(5 ,10 ,15) )),
         interval ="confidence")

predict (lm.fit1 ,data.frame(lstat =(c(5 ,10 ,15) )),
         interval ="prediction")

plot(lstat ,medv)
abline (lm.fit1)

plot(predict (lm.fit1), residuals (lm.fit))
plot(predict (lm.fit1), rstudent (lm.fit))
plot(hatvalues (lm.fit1 ))

#####################################################
## multi variable linear regression #################
#####################################################
lm.fit2 <- lm(medv ~ lstat + age)
summary(lm.fit2)

lm.fit2 <- lm(medv ~., data = Boston)
summary(lm.fit2)

# r-squared
summary(lm.fit)$r.sq

# RSE
summary(lm.fit2)$sigma

# VIF for collinearity
require(car)
vif(lm.fit2)

#####################################################
## interaction terms ################################
#####################################################
lm.fit3 <- lm(medv ~ lstat * age)
summary(lm.fit3)

#####################################################
## non-linear transformation ########################
#####################################################
lm.fit4 <- lm(medv ~ lstat + I(lstat ^ 2))
summary(lm.fit4)

# use anova to quantify which model is better
# anova functino performs a hypothesis test comparing
# the two models. The null hypothesis is the two
# models fit the data equally well, and the alternative
# hypothesis is that the full model is superior.
anova(lm.fit1, lm.fit4)
plot(lm.fit4)

# poly
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)
