require(ISLR)
#####################################################
## resampling #######################################
#####################################################
# selecting a random subset of 196 records out of the 392 records
set.seed(1)
train <- sample(392, 196)

# use the subset option in lm()
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)]

# predict and calculate the MSE
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2) # 26.07098
# poly and try
# poly 2
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # 19.82259
# poly 3
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) # 19.78252

## if we use a different training set ###############
set.seed(2)
train <- sample(392, 196)

# use the subset option in lm()
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# predict and calculate the MSE
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2) # 23.29559
# poly and try
# poly 2
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # 18.90124
# poly 3
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) # 19.2574

#####################################################
## Leave-One-Out Cross-Validation ###################
#####################################################
require(boot)
# fit a model
glm.fit <- glm(mpg ~ horsepower, data = Auto)
# loocv this model
cv.err <- cv.glm(Auto, glm.fit)
cv.err
cv.err$delta # 24.23151 24.23114 
# above left is the standard estimated MSE after loocv
# above right is a bias corrected MSE after loocv

# poly and try
cv.err <- rep(0, 5)
for (i in 1:5) {
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.err

#####################################################
## k-Fold Cross-Validation ##########################
#####################################################
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1: 10) {
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10 # 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201 18.95140 19.50196

#####################################################
## bootstrap ########################################
#####################################################
# user the portfolio data and use the below function to
# estimate the the proportion of investment on X and Y
alpha.fn=function (data ,index){
    X=data$X [index]
    Y=data$Y [index]
    return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
    }
# check the proportion
alpha.fn(Portfolio, 1:100) # 0.5758321
# construct a new bootstrap data with sampling with replacement
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) # 0.5963833

# we can repeat the above many times by using below
boot(Portfolio, alpha.fn ,R = 1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#     boot(data = Portfolio, statistic = alpha.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#     original        bias    std. error
# t1* 0.5758321 -7.315422e-05  0.08861826

## estimating the accuracy of a LR model #############
# a function to calculate the coefficients
boot.fn <- function (data ,index) {
    return(coef(lm(mpg ~ horsepower, data = data, subset =index)))
}
# check the coefficients
boot.fn(Auto ,1:392)
# (Intercept)  horsepower 
# 39.9358610  -0.1578447

# use bootstrap
set.seed(1)
boot.fn(Auto ,sample(392, 392, replace = T))
# (Intercept)  horsepower 
# 38.7387134  -0.1481952 
boot.fn(Auto ,sample(392, 392, replace = T))
# (Intercept)  horsepower 
# 40.0383086  -0.1596104 

# use boot
boot(Auto, boot.fn,1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#     boot(data = Auto, statistic = boot.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#     original      bias    std. error
# t1* 39.9358610  0.02972191 0.860007896
# t2* -0.1578447 -0.00030823 0.007404467

# compare the above result (std. error) with formula
summary(lm(mpg ~ horsepower, data = Auto))$coef
# Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
# horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

# the formula one replies on some estimates (error term)
# but bootstrap does not. so bootstrap is more accurate!
