require(ISLR)
require(pls) # pcr and plsr
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

#####################################################
## 1. principal components regression ###############
#####################################################
# standardizing: scale = T
# compute ten fold cv error for each M
pcr.fit <- pcr(Salary ~., data = Hitters, scale = T, validation = "CV")
summary(pcr.fit)
# Data:X dimension:263 19
# Y dimension:263 1
# Fit method:svdpc
# Number of components considered:19
# 
# VALIDATION:RMSEP
# Cross - validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# CV             452    354.4    351.5    350.9    349.7    344.9    343.6    346.1    348.5
# adjCV          452    354.0    351.1    350.5    349.2    344.3    342.6    345.2    347.5
# 9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps
# CV       352.1     351.4     353.3     354.4     354.3     350.0     349.8     344.4     346.4
# adjCV    350.9     350.0     351.9     352.9     352.9     348.3     348.1     342.6     344.2
# 18 comps  19 comps
# CV        345.0     348.5
# adjCV     342.8     346.0
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
# X         38.31    60.16    70.84    79.03    84.29    88.63    92.26    94.96    96.28
# Salary    40.63    41.58    42.17    43.22    44.90    46.48    46.69    46.75    46.86
# 10 comps  11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps
# X          97.26     97.98     98.65     99.15     99.47     99.75     99.89     99.97
# Salary     47.76     47.82     47.85     48.10     50.40     50.55     53.01     53.85
# 18 comps  19 comps
# X          99.99    100.00
# Salary     54.61     54.61

# plot it
validationplot(pcr.fit, val.type = "MSEP") # 16 is the smallest

## perform PCR on train and test set
set.seed(1)
pcr.fit <- pcr(Salary ~., data = Hitters, subset = train, scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP") # 7 components are with lowest
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 7)
mean((pcr.pred - y.test)^2) # 96556.22

# fit the full data set
pcr.fit <- pcr(y~x, scale = T, ncomp = 7)
summary(pcr.fit)
# Data: 	X dimension: 263 19 
# Y dimension: 263 1
# Fit method: svdpc
# Number of components considered: 7
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
# X    38.31    60.16    70.84    79.03    84.29    88.63    92.26
# y    40.63    41.58    42.17    43.22    44.90    46.48    46.69

#####################################################
## 2. partial least squares #########################
#####################################################
set.seed(1)
pls.fit <- plsr(Salary ~., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls.fit) # M = 2 components have the smallest RMSEP
# Data: 	X dimension: 131 19 
# Y dimension: 131 1
# Fit method: kernelpls
# Number of components considered: 19
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# CV           464.6    394.2    391.5    393.1    395.0    415.0    424.0    424.5    415.8
# adjCV        464.6    393.4    390.2    391.1    392.9    411.5    418.8    418.9    411.4
# 9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps
# CV       404.6     407.1     412.0     414.4     410.3     406.2     408.6     410.5     408.8
# adjCV    400.7     402.2     407.2     409.3     405.6     401.8     403.9     405.6     404.1
# 18 comps  19 comps
# CV        407.8     410.2
# adjCV     403.2     405.5
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
# X         38.12    53.46    66.05    74.49    79.33    84.56    87.09    90.74    92.55
# Salary    33.58    38.96    41.57    42.43    44.04    45.59    47.05    47.53    48.42
# 10 comps  11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps
# X          93.94     97.23     97.88     98.35     98.85     99.11     99.43     99.78
# Salary     49.68     50.04     50.54     50.78     50.92     51.04     51.11     51.15
# 18 comps  19 comps
# X          99.99    100.00
# Salary     51.16     51.18

# evaluate on test set
pls.pred <- predict(pls.fit, x[test, ], ncomp = 2)
mean((pls.pred - y.test)^2) # 101417.5

# use it on full set
pls.fit <- plsr(Salary ~., data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)
# Data: 	X dimension: 263 19 
# Y dimension: 263 1
# Fit method: kernelpls
# Number of components considered: 2
# TRAINING: % variance explained
# 1 comps  2 comps
# X         38.08    51.03
# Salary    43.05    46.40












