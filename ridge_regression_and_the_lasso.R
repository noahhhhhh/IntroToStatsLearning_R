require(ISLR)
require(glmnet) # ridge and the lass, and more
#####################################################
## check the data ###################################
#####################################################
fix(Hitters)
names(Hitters)
# [1] "AtBat"     "Hits"      "HmRun"     "Runs"      "RBI"       "Walks"     "Years"    
# [8] "CAtBat"    "CHits"     "CHmRun"    "CRuns"     "CRBI"      "CWalks"    "League"   
# [15] "Division"  "PutOuts"   "Assists"   "Errors"    "Salary"    "NewLeague"

# remove the entire rows if a column in the row is NA
Hitters <- na.omit(Hitters)
# check the dim of data
dim(Hitters)
# [1] 263  20
# preprocessing for fitting glmnet
# model.matrix prepare a matrix of predictors and also 
# convert qualitative variabels into dummy variable
# this is useful because glmnet only accepts numercal
# variable
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

#####################################################
## ridge regression #################################
#####################################################
# the grid for λ, the tuning parameters
grid <- 10^seq(10, -2, length = 100)
# needs to standardize when it comes to ridge and lasso
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid, standardize = T)
# coefficients matrix is 20 x 100, where 20 predictors and intercept
# 100 columns corresponding to 100 λ
dim(coef(ridge.mod))
# [1]  20 100
# checkt the 50th λ and the corresponding coefficients
ridge.mod$lambda[50] # 11497.57
coef(ridge.mod)[, 50]
# (Intercept)         AtBat          Hits         HmRun          Runs           RBI 
# 407.356050200   0.036957182   0.138180344   0.524629976   0.230701523   0.239841459 
# Walks         Years        CAtBat         CHits        CHmRun         CRuns 
# 0.289618741   1.107702929   0.003131815   0.011653637   0.087545670   0.023379882 
# CRBI        CWalks       LeagueN     DivisionW       PutOuts       Assists 
# 0.024138320   0.025015421   0.085028114  -6.215440973   0.016482577   0.002612988 
# Errors    NewLeagueN 
# -0.020502690   0.301433531 
# l_2 norm of coefficients
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) # 6.360612
# predict, λ = 50
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

## train and test for estimating test error #########
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test = y[test]

## fit a ridge regression, using λ = 4.
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2) # 101036.8

# test error
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2) # 193253.1

# check the eror if it is a least square regression model
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact = T)
mean((ridge.pred - y.test)^2) # 114783.1
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20, ]
# above two are the same

## cross validatio to choose the λ, tuning parameter
# by default, it is a k = 10 fold cv
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # 211.7416
# so λ = 211.7416 has the best accuracy
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]
# (Intercept)        AtBat         Hits        HmRun         Runs          RBI        Walks 
# 9.88487157   0.03143991   1.00882875   0.13927624   1.11320781   0.87318990   1.80410229 
# Years       CAtBat        CHits       CHmRun        CRuns         CRBI       CWalks 
# 0.13074381   0.01113978   0.06489843   0.45158546   0.12900049   0.13737712   0.02908572 
# LeagueN    DivisionW      PutOuts      Assists       Errors   NewLeagueN 
# 27.18227535 -91.63411299   0.19149252   0.04254536  -1.81244470   7.21208390 

#####################################################
## the lasso ########################################
#####################################################
# alpha = 1 means lasso
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

## cross validation to estimate test error ##########
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # 16.78016
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2) # 100743.4

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef # 12 out of 19 are 0
# (Intercept)        AtBat         Hits        HmRun         Runs          RBI        Walks 
# 18.5394844    0.0000000    1.8735390    0.0000000    0.0000000    0.0000000    2.2178444 
# Years       CAtBat        CHits       CHmRun        CRuns         CRBI       CWalks 
# 0.0000000    0.0000000    0.0000000    0.0000000    0.2071252    0.4130132    0.0000000 
# LeagueN    DivisionW      PutOuts      Assists       Errors   NewLeagueN 
# 3.2666677 -103.4845458    0.2204284    0.0000000    0.0000000    0.0000000 













