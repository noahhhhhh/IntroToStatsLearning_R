require(tree)
require(ISLR)
require(MASS)
require(randomForest)
require(gbm)
#####################################################
## fitting classification trees #####################
#####################################################
attach(Carseats)
# create a variable High
High <- ifelse(Sales <= 9, "No", "Yes")
Carseats <- data.frame(Carseats, High)

# fit a classification tree
tree.carseats <- tree(High~. -Sales, Carseats)
summary(tree.carseats)
# Classification tree:
#     tree(formula = High ~ . - Sales, data = Carseats)
# Variables actually used in tree construction:
#     [1] "ShelveLoc"   "Price"       "Age"         "CompPrice"   "Advertising" "Income"     
# [7] "Education"  
# Number of terminal nodes:  22 
# Residual mean deviance:  0.3897 = 147.3 / 378 
# Misclassification error rate: 0.09 = 36 / 400 

# plot
plot(tree.carseats)
text(tree.carseats, pretty = 0)

# print
tree.carseats

# test error
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats <- tree(High ~. -Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
# High.test
# tree.pred  No Yes
# No  118  20
# Yes  20  42
(118 + 42) / 200
# [1] 0.8

# prune the tree via cv
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
# [1] "size"   "dev"    "k"      "method"
cv.carseats
# $size
# [1] 18 13 12 10  4  3  2  1 # 38 corresponds to the sizes of 12, 10, 4, we choose 4 as it is simpler
# 
# $dev
# [1] 41 41 38 38 38 39 41 52 # 38 is the lowest dev
# 
# $k
# [1]      -Inf  0.000000  1.000000  1.500000  1.666667  5.000000  6.000000 13.000000
# 
# $method
# [1] "misclass"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"

# plot
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# apply prune.misclass()
prune.carseats <- prune.misclass(tree.carseats, best = 4)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# try it on test data
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
# High.test
# tree.pred  No Yes
# No  122  29
# Yes  16  33
(122 + 33) / 200
# [1] 0.775

# try other best
prune.carseats <- prune.misclass(tree.carseats, best = 10)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
# High.test
# tree.pred  No Yes
# No  115  22
# Yes  23  40
(115 + 40)/ 200
# [1] 0.775

# try other best, again
prune.carseats <- prune.misclass(tree.carseats, best = 12)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
# High.test
# tree.pred  No Yes
# No  115  19
# Yes  23  43
(115 + 43)/ 200
# [1] 0.79

#####################################################
## fitting regression trees #########################
#####################################################
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~., Boston, subset = train)
summary(tree.boston)
# Regression tree:
#     tree(formula = medv ~ ., data = Boston, subset = train)
# Variables actually used in tree construction:
#     [1] "lstat" "rm"    "dis"  
# Number of terminal nodes:  8 
# Residual mean deviance:  12.65 = 3099 / 245 
# Distribution of residuals:
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -14.10000  -2.04200  -0.05357   0.00000   1.96000  12.60000 

# plot
plot(tree.boston)
text(tree.boston, pretty = 0)

# prune
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b") # 8
prune.boston <- prune.tree(tree.boston, best = 8)
plot(prune.boston)
text(prune.boston, pretty = 0)

# test it on test
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2)
# [1] 25.04559

#####################################################
## Bagging and Random Forest ########################
#####################################################
# bagging
set.seed(1)
bag.boston <- randomForest(medv ~., data = Boston, subset = train
                           , mtry = 13, importance = T)
bag.boston
# Call:
#     randomForest(formula = medv ~ ., data = Boston, mtry = 13, importance = T,      subset = train) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 13
# 
# Mean of squared residuals: 11.02509
# % Var explained: 86.65

# test it on test
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)
# [1] 13.47349

# random forest, only different from above is the mtry
set.seed(1)
rf.boston <- randomForest(medv~., data = Boston, subset = train
                          , mtry = 6, importance = T)
yhat.rf <- predict(rf.boston, newdata = Boston[-train ,])
mean((yhat.rf - boston.test)^2)
# [1] 11.48022

# importance
importance(rf.boston)
# %IncMSE IncNodePurity
# crim    12.547772    1094.65382
# zn       1.375489      64.40060
# indus    9.304258    1086.09103
# chas     2.518766      76.36804
# nox     12.835614    1008.73703
# rm      31.646147    6705.02638
# age      9.970243     575.13702
# dis     12.774430    1351.01978
# rad      3.911852      93.78200
# tax      7.624043     453.19472
# ptratio 12.008194     919.06760
# black    7.376024     358.96935
# lstat   27.666896    6927.98475
varImpPlot(rf.boston)

#####################################################
## boosting #########################################
#####################################################
set.seed(1)
boost.boston <- gbm(medv~., data = Boston[train, ], distribution = "gaussian"
                    , n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
# var    rel.inf
# lstat     lstat 45.9627334
# rm           rm 31.2238187
# dis         dis  6.8087398
# crim       crim  4.0743784
# nox         nox  2.5605001
# ptratio ptratio  2.2748652
# black     black  1.7971159
# age         age  1.6488532
# tax         tax  1.3595005
# indus     indus  1.2705924
# chas       chas  0.8014323
# rad         rad  0.2026619
# zn           zn  0.0148083

# plot
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# test it on test
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ]
                      , n.trees = 5000)
mean((yhat.boost - boston.test)^2)
# [1] 11.84434

# try λ = 0.2 (shrinkage value)
boost.boston <- gbm(medv ~., data = Boston[train, ], distribution = "gaussian"
                    , n.trees = 5000, interaction.depth = 4, shrinkage = .2
                    , verbose = F)
# test it on test
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ]
                      , n.trees = 5000)
mean((yhat.boost - boston.test)^2)
# [1] 11.51109













