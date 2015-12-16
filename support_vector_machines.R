require(e1071) # cost is different from C. 
# When cost is large, less violations (less flexible); 
# when cost is small, more violations (more flexible);
#####################################################
## support vector classifier ########################
#####################################################
# simulate data
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] = x[y == 1, ] + 1

# plot
plot(x, col = (3 - y)) # not linearly separable

# we need to encode the repsone as a factor
dat <- data.frame(x = x, y = as.factor(y))

# model with cost = 10
svmfit <- svm(y ~., data = dat, kernal = "linear", cost = 10, scale = F)

# plot
plot(svmfit, dat) # the support vectors are plotted as crosses (X)

# support vectors
svmfit$index
# [1]  1  4  5  6  8  9 13 14 15 16

# the model summary
summary(svmfit)
# Call:
#     svm(formula = y ~ ., data = dat, kernal = "linear", cost = 10, scale = F)
# 
# 
# Parameters:
#     SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  10 
# gamma:  0.5 
# 
# Number of Support Vectors:  10
# 
# ( 6 4 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#     -1 1

# model with cost = .1
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = .1, scale = F)
svmfit$index
# [1]  1  2  3  4  5  7  9 10 12 13 14 15 16 17 18 20

# cross-validation to tune model
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(.001, .01, .1, 1, 5, 10, 100)))
summary(tune.out)
# Parameter tuning of ‘svm’:
#     
#     - sampling method: 10-fold cross validation 
# 
# - best parameters:
#     cost
# 0.1
# 
# - best performance: 0.1 
# 
# - Detailed performance results:
#     cost error dispersion
# 1 1e-03  0.70  0.4216370
# 2 1e-02  0.70  0.4216370
# 3 1e-01  0.10  0.2108185
# 4 1e+00  0.15  0.2415229
# 5 5e+00  0.15  0.2415229
# 6 1e+01  0.15  0.2415229
# 7 1e+02  0.15  0.2415229

bestmod <- tune.out$best.model
summary(bestmod)
# Call:
#     best.tune(method = svm, train.x = y ~ ., data = dat, ranges = list(cost = c(0.001, 
#                                                                                 0.01, 0.1, 1, 5, 10, 100)), kernel = "linear")
# 
# 
# Parameters:
#     SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  0.1 
# gamma:  0.5 
# 
# Number of Support Vectors:  16
# 
# ( 8 8 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#     -1 1

# predict
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = T)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)
#           truth
# predict   -1  1
#       -1  11  1
#       1   0  8

# what if cost = .01?
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = .01, scale = F)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)
#           truth
# predict   -1  1
#       -1  11  2
#       1   0  7

# plot
x[y == 1, ] <- x[y == 1, ] + .5
plot(x, col = (y + 5) / 2, pch = 19)

# try a larger cost = 1e+05
dat = data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
# Call:
#     svm(formula = y ~ ., data = dat, kernel = "linear", cost = 1e+05)
# 
# 
# Parameters:
#     SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  1e+05 
# gamma:  0.5 
# 
# Number of Support Vectors:  3
# 
# ( 1 2 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
# -1 1
plot(svmfit, dat)

#####################################################
## support vector machine ###########################
#####################################################
# use non linear kernel
# set up some data
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

# plot
plot(x, col = y)

# model with radial kernel and γ = 1
train <- sample(200, 100)
svmfit <- svm(y ~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train, ])
summary(svmfit)
# Call:
#     svm(formula = y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
# 
# 
# Parameters:
#     SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  1 
# 
# Number of Support Vectors:  37
# 
# ( 17 20 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#     1 2

# reduce training errors by reducing cost
svmfit <- svm(y ~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train, ])

# cross validation with tune to select γ and cost
set.seed(1)
tune.out <- tune(svm, y~., data = dat[train, ], kernel = "radial"
                 , ranges = list(cost = c(.1, 1, 10, 100, 1000)
                                 , gamma = c(.5, 1, 2, 3, 4)
                                 )
)
summary(tune.out)
# Parameter tuning of ‘svm’:
#     
#     - sampling method: 10-fold cross validation 
# 
# - best parameters:
#     cost gamma
# 1     2
# 
# - best performance: 0.12 
# 
# - Detailed performance results:
#     cost gamma error dispersion
# 1  1e-01   0.5  0.27 0.11595018
# 2  1e+00   0.5  0.13 0.08232726
# 3  1e+01   0.5  0.15 0.07071068
# 4  1e+02   0.5  0.17 0.08232726
# 5  1e+03   0.5  0.21 0.09944289
# 6  1e-01   1.0  0.25 0.13540064
# 7  1e+00   1.0  0.13 0.08232726
# 8  1e+01   1.0  0.16 0.06992059
# 9  1e+02   1.0  0.20 0.09428090
# 10 1e+03   1.0  0.20 0.08164966
# 11 1e-01   2.0  0.25 0.12692955
# 12 1e+00   2.0  0.12 0.09189366
# 13 1e+01   2.0  0.17 0.09486833
# 14 1e+02   2.0  0.19 0.09944289
# 15 1e+03   2.0  0.20 0.09428090
# 16 1e-01   3.0  0.27 0.11595018
# 17 1e+00   3.0  0.13 0.09486833
# 18 1e+01   3.0  0.18 0.10327956
# 19 1e+02   3.0  0.21 0.08755950
# 20 1e+03   3.0  0.22 0.10327956
# 21 1e-01   4.0  0.27 0.11595018
# 22 1e+00   4.0  0.15 0.10801234
# 23 1e+01   4.0  0.18 0.11352924
# 24 1e+02   4.0  0.21 0.08755950
# 25 1e+03   4.0  0.24 0.10749677

# best is cost = 1 and gamma = 2
# test error
table(true = dat[-train, "y"]
      , pred = predict((tune.out$best.model)
      , newx = dat[-train, ])
      )
#       pred
# true  1  2
#   1   56 21
#   2   18  5

#####################################################
## ROC ##############################################
#####################################################
require(ROCR)
rocplot <- function(pred, truth, ...){
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

# model
svmfit.opt <- svm(y ~., data = dat[train, ], kernel = "radial"
                  , gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train, ], decision.values = T))$decision.values

# plot
par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

# increasing γ to make it more flexible and further improve training accuracy
svm.flex <- svm(y ~., data = dat[train, ], kernel = "radial"
                , gamma = 50, cost = 1, decision.values = T)
fitted <- attributes(predict(svm.flex, dat[train, ], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")

# test
fitted <- attributes(predict(svmfit.opt, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svm.flex, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")

#####################################################
## SVM with Multiple Classes ########################
#####################################################
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))

# model, svm will perform one-versus-one approach
svmfit <- svm(y ~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

#####################################################
## SVM for regression ###############################
#####################################################
# change the response class from factor to numeric

#####################################################
## Application to Gene Expression Data ##############
#####################################################
require(ISLR)
names(Khan)
# [1] "xtrain" "xtest"  "ytrain" "ytest" 
dim(Khan$xtrain)
# [1]   63 2308
dim(Khan$xtest)
# [1]   20 2308
length(Khan$ytrain)
# 63
length(Khan$ytest)
# 20
table(Khan$ytrain)
# 1  2  3  4 
# 8 23 12 20 
table(Khan$ytest)
# 1 2 3 4 
# 3 6 6 5

# model with linear kernel, since so many features with radial or poly would 
# make the model too flexible
dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~., data = dat, kernel = "linear", cost = 10)
summary(out)
# Call:
#     svm(formula = y ~ ., data = dat, kernel = "linear", cost = 10)
# 
# 
# Parameters:
#     SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  10 
# gamma:  0.0004332756 
# 
# Number of Support Vectors:  58
# 
# ( 20 20 11 7 )
# 
# 
# Number of Classes:  4 
# 
# Levels: 
#     1 2 3 4
table(out$fitted, dat$y)
# 1  2  3  4
# 1  8  0  0  0
# 2  0 23  0  0
# 3  0  0 12  0
# 4  0  0  0 20

# test
dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)
# pred.te 1 2 3 4
# 1 3 0 0 0
# 2 0 6 2 0
# 3 0 0 4 0
# 4 0 0 0 5
