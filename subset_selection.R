require(ISLR)
require(leaps) # subset selection
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

#####################################################
## best subset selection ############################
#####################################################
regfit.full <- regsubsets(Salary ~., Hitters)
summary(regfit.full)
# Subset selection object
# Call: regsubsets.formula(Salary ~ ., Hitters)
# 19 Variables  (and intercept)
# Forced in Forced out
# AtBat          FALSE      FALSE
# Hits           FALSE      FALSE
# HmRun          FALSE      FALSE
# Runs           FALSE      FALSE
# RBI            FALSE      FALSE
# Walks          FALSE      FALSE
# Years          FALSE      FALSE
# CAtBat         FALSE      FALSE
# CHits          FALSE      FALSE
# CHmRun         FALSE      FALSE
# CRuns          FALSE      FALSE
# CRBI           FALSE      FALSE
# CWalks         FALSE      FALSE
# LeagueN        FALSE      FALSE
# DivisionW      FALSE      FALSE
# PutOuts        FALSE      FALSE
# Assists        FALSE      FALSE
# Errors         FALSE      FALSE
# NewLeagueN     FALSE      FALSE
# 1 subsets of each size up to 8
# Selection Algorithm: exhaustive
# AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks LeagueN
# 1  ( 1 ) " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 2  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 3  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 4  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 5  ( 1 ) "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 6  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "    " "    
# 7  ( 1 ) " "   "*"  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "    " "    
# 8  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   "*"    "*"   " "  "*"    " "    
# DivisionW PutOuts Assists Errors NewLeagueN
# 1  ( 1 ) " "       " "     " "     " "    " "       
# 2  ( 1 ) " "       " "     " "     " "    " "       
# 3  ( 1 ) " "       "*"     " "     " "    " "       
# 4  ( 1 ) "*"       "*"     " "     " "    " "       
# 5  ( 1 ) "*"       "*"     " "     " "    " "       
# 6  ( 1 ) "*"       "*"     " "     " "    " "       
# 7  ( 1 ) "*"       "*"     " "     " "    " "       
# 8  ( 1 ) "*"       "*"     " "     " "    " " 
# An asterisk indicates that a given variable is included in the corresponding
# model. For instance, this output indicates that the best two-variable model
# contains only Hits and CRBI. by default, only 8-variable model is run.
# change number of variable models to 19
regfit.full <- regsubsets(Salary ~., data = Hitters, nvmax = 19)
summary(regfit.full)
# Subset selection object
# Call: regsubsets.formula(Salary ~ ., data = Hitters, nvmax = 19)
# 19 Variables  (and intercept)
# Forced in Forced out
# AtBat          FALSE      FALSE
# Hits           FALSE      FALSE
# HmRun          FALSE      FALSE
# Runs           FALSE      FALSE
# RBI            FALSE      FALSE
# Walks          FALSE      FALSE
# Years          FALSE      FALSE
# CAtBat         FALSE      FALSE
# CHits          FALSE      FALSE
# CHmRun         FALSE      FALSE
# CRuns          FALSE      FALSE
# CRBI           FALSE      FALSE
# CWalks         FALSE      FALSE
# LeagueN        FALSE      FALSE
# DivisionW      FALSE      FALSE
# PutOuts        FALSE      FALSE
# Assists        FALSE      FALSE
# Errors         FALSE      FALSE
# NewLeagueN     FALSE      FALSE
# 1 subsets of each size up to 19
# Selection Algorithm: exhaustive
# AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks LeagueN
# 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 2  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 3  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 4  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 5  ( 1 )  "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    
# 6  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "    " "    
# 7  ( 1 )  " "   "*"  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "    " "    
# 8  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   "*"    "*"   " "  "*"    " "    
# 9  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"    " "    
# 10  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"    " "    
# 11  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"    "*"    
# 12  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"    "*"    
# 13  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"    "*"    
# 14  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    " "   " "    "*"   "*"  "*"    "*"    
# 15  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    "*"   " "    "*"   "*"  "*"    "*"    
# 16  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"   "*"  "*"    "*"    
# 17  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"   "*"  "*"    "*"    
# 18  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   " "    "*"   "*"  "*"    "*"    
# 19  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   "*"    "*"   "*"  "*"    "*"    
# DivisionW PutOuts Assists Errors NewLeagueN
# 1  ( 1 )  " "       " "     " "     " "    " "       
# 2  ( 1 )  " "       " "     " "     " "    " "       
# 3  ( 1 )  " "       "*"     " "     " "    " "       
# 4  ( 1 )  "*"       "*"     " "     " "    " "       
# 5  ( 1 )  "*"       "*"     " "     " "    " "       
# 6  ( 1 )  "*"       "*"     " "     " "    " "       
# 7  ( 1 )  "*"       "*"     " "     " "    " "       
# 8  ( 1 )  "*"       "*"     " "     " "    " "       
# 9  ( 1 )  "*"       "*"     " "     " "    " "       
# 10  ( 1 ) "*"       "*"     "*"     " "    " "       
# 11  ( 1 ) "*"       "*"     "*"     " "    " "       
# 12  ( 1 ) "*"       "*"     "*"     " "    " "       
# 13  ( 1 ) "*"       "*"     "*"     "*"    " "       
# 14  ( 1 ) "*"       "*"     "*"     "*"    " "       
# 15  ( 1 ) "*"       "*"     "*"     "*"    " "       
# 16  ( 1 ) "*"       "*"     "*"     "*"    " "       
# 17  ( 1 ) "*"       "*"     "*"     "*"    "*"       
# 18  ( 1 ) "*"       "*"     "*"     "*"    "*"       
# 19  ( 1 ) "*"       "*"     "*"     "*"    "*"  
## check some other info
names(summary(regfit.full))
# [1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"
## check R^2
summary(regfit.full)$rsq
# [1] 0.3214501 0.4252237 0.4514294 0.4754067 0.4908036 0.5087146 0.5141227 0.5285569 0.5346124
# [10] 0.5404950 0.5426153 0.5436302 0.5444570 0.5452164 0.5454692 0.5457656 0.5459518 0.5460945
# [19] 0.5461159
## plot the result
par(mfrow = c(2, 2))
plot(summary(regfit.full)$rss, type = "l", xlab = "No. of features", ylab = "RSS")
plot(summary(regfit.full)$adjr2, type = "l", xlab = "No. of features", ylab = "Adjusted R^2")
## which n-variable model is best in terms of adjr^2
which.max (summary(regfit.full)$adjr2) # 11
points (11,summary(regfit.full)$adjr2[11], col ="red",cex =2, pch =20)

plot(summary(regfit.full)$cp, type = "l", xlab = "No. of features", ylab = "Cp")
which.min(summary(regfit.full)$cp) # 10
points (10,summary(regfit.full)$cp[10], col ="red",cex =2, pch =20)

plot(summary(regfit.full)$bic, type = "l", xlab = "No. of features", ylab = "BIC")
which.min(summary(regfit.full)$bic) # 6
points (6,summary(regfit.full)$bic[6], col ="red",cex =2, pch =20)
## use the builtin plot
plot(regfit.full, scale ="r2")
plot(regfit.full, scale ="adjr2")
plot(regfit.full, scale ="Cp")
plot(regfit.full, scale ="bic")
# check coeff
coef(regfit.full, 6)
# (Intercept)        AtBat         Hits        Walks         CRBI    DivisionW      PutOuts 
# 91.5117981   -1.8685892    7.6043976    3.6976468    0.6430169 -122.9515338    0.2643076

#####################################################
## forward and backward stepwise selection ##########
#####################################################
# forward
regfit.fwd <- regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
# backward
regfit.bwd <- regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

#####################################################
## choosing among models using Validation Set Approach
## and Cross-Validation Approach ####################
#####################################################
## 1. Validatino Set Approach #######################
# training and testing set
set.seed(1)
train <- sample(c(T, F), nrow(Hitters), rep = T)
test <- !train
# apply regsubests() on the training set
regfit.best <- regsubsets(Salary~., data = Hitters[train, ], nvmax = 19)
# make a model matrix
test.mat <- model.matrix(Salary~., data = Hitters[test, ])

## looping and calculating the test MSE
val.errors <- rep(NA, 19)
for (i in 1:19){
    coefi <- coef(regfit.best, id = i)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
# [1] 220968.0 169157.1 178518.2 163426.1 168418.1 171270.6 162377.1 157909.3 154055.7 148162.1
# [11] 151156.4 151742.5 152214.5 157358.7 158541.4 158743.3 159972.7 159859.8 160105.6
# which is the smallest
which.min(val.errors) # 10
coef(regfit.best, 10)
# (Intercept)       AtBat        Hits       Walks      CAtBat       CHits      CHmRun      CWalks 
# -80.2751499  -1.4683816   7.1625314   3.6430345  -0.1855698   1.1053238   1.3844863  -0.7483170 
# LeagueN   DivisionW     PutOuts 
# 84.5576103 -53.0289658   0.2381662

## change above to a function
predict.regsubsets <- function(object, newdata, id, ...){
    form = as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}
## 2. Cross Validation Approach #####################
# k fold
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
# looping and calculating the tes MSE
# j is the test set and the rest folds are train set
for (j in 1:k){
    best.fit <- regsubsets(Salary ~., data = Hitters[folds != j, ], nvmax = 19)
    for (i in 1:19){
        pred <- predict(best.fit, Hitters[folds == j, ], id = i)
        cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
    }
}
# above gives a 10 x 19 matrix, where the (i, j)th element corresponds
# to the test MSE for ith cross-validation fold for the best j-variable
# model
# take the mean of each column
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
# 1        2        3        4        5        6        7        8        9       10 
# 160093.5 140196.8 153117.0 151159.3 146841.3 138302.6 144346.2 130207.7 129459.6 125334.7 
# 11       12       13       14       15       16       17       18       19 
# 125153.8 128273.5 133461.0 133974.6 131825.7 131882.8 132750.9 133096.2 132804.7 
# plot it
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b") # 11-variable model is the best
# see what 11-variable model looks like
reg.best <- regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(reg.best, 11)
# (Intercept)        AtBat         Hits        Walks       CAtBat        CRuns         CRBI 
# 135.7512195   -2.1277482    6.9236994    5.6202755   -0.1389914    1.4553310    0.7852528 
# CWalks      LeagueN    DivisionW      PutOuts      Assists 
# -0.8228559   43.1116152 -111.1460252    0.2894087    0.2688277 
