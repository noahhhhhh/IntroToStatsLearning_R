require(MASS) # lda and qda
require(ISLR)
require(class) # knn

#####################################################
## exploratory analysis #############################
#####################################################
names(Smarket)
dim(Smarket)
summary(Smarket)

# check the correlation graphically
pairs(Smarket)

# check the correlation numerically
cor(Smarket)
cor(Smarket[, -9])

# Year and Volume are correlated
attach(Smarket)
plot(Year, Volume)


#####################################################
## logistic regression ##############################
#####################################################
# use family = binomial to train a logistics regression model
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume
               , data = Smarket
               , family = binomial)

summary(glm.fit)

# use type = "response" to predict the probability
glm.probs <- predict(glm.fit, type = "response")

# dummy variable for the response
contrasts(Direction)

# label the prediction
glm.pred <- rep("Down", dim(Smarket)[1])
glm.pred[glm.probs > .5] <- "Up"

# create a confusion table
table(glm.pred, Direction)

# training accuracy
(145 + 507) / dim(Smarket)[1] # .5216
mean(glm.pred == Direction) # .5216 randome guess

## 1. split the dataset into train and test #########
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# train on the train data
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume
               , data = Smarket
               , subset = train
               , family = binomial)

summary(glm.fit)
# predict
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
# label the prediction
glm.pred <- rep("Down", dim(Smarket.2005)[1])
glm.pred[glm.probs > .5] <- "Up"
# create a confusion matrix
table(glm.pred, Direction.2005)
# accuracy
mean(glm.pred == Direction.2005) # .4801587 even worse

## 2. improve the model #############################
summary(glm.fit) # Lag3, Lag4, Lag5, and Volume have a big p value, so remove them
# train on the train data
glm.fit <- glm(Direction ~ Lag1 + Lag2
               , data = Smarket
               , subset = train
               , family = binomial)
summary(glm.fit)
# predict
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
# label the prediction
glm.pred <- rep("Down", dim(Smarket.2005)[1])
glm.pred[glm.probs > .5] <- "Up"
# create a confusion matrix
table(glm.pred, Direction.2005)
# accuracy
mean(glm.pred == Direction.2005) # .5595238 a bit improved


#####################################################
## linear discriminant analysis (LDA) ###############
#####################################################
lda.fit <- lda(Direction ~ Lag1 + Lag2
               , data = Smarket
               , subset = train)
summary(lda.fit)
lda.fit # pies, means, and coefficients

# produces plots of the linear discriminants, obtained by computing −0.642 × Lag1 − 0.514 × Lag2 for each of the training observations
plot(lda.fit)

# predict
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred) # "class"     "posterior" "x" 
lda.class = lda.pred$class
# create a confusion matrix
table(lda.class, Direction.2005)
# accuracy
mean(lda.class == Direction.2005) # .5595238

# threshold
sum(lda.pred$posterior[, 1] >= .5) # 70
sum(lda.pred$posterior[, 1] < .5) # 182
# increase the threshold to .9
sum(lda.pred$posterior[, 1] > .9) # 0


#####################################################
## quadratic discriminant analysis (QDA) ############
#####################################################
qda.fit <- qda(Direction ~ Lag1 + Lag2
               , data = Smarket
               , subset = train)
summary(qda.fit)
qda.fit # same as lda.fit
# predict
qda.class <- predict(qda.fit ,Smarket.2005)$class
# create a confusion matrix
table(qda.class, Direction.2005)
# accuracy
mean(qda.class == Direction.2005) # .5992063 quite good though

#####################################################
## k nearest neighbour ##############################
#####################################################
# set up the parameters for the knn
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- Direction[train]

# set a seed, because if multiple points are equally the nearest, then knn will randomly choose
set.seed(1)
knn.pred <- knn(train = train.X
                , test = test.X
                , cl = train.Direction
                , k = 1)
# create a confusion matrix
table(knn.pred, Direction.2005)
# accuracy
mean(knn.pred == Direction.2005) # .5 random guess
# try k = 3
knn.pred <- knn(train = train.X
                , test = test.X
                , cl = train.Direction
                , k = 3)
mean(knn.pred == Direction.2005) # .5357143

#####################################################
## application on the caravan dataset ###############
#####################################################
# explorarty analysis
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/dim(Caravan)[1] # .05977327 only 6% of the people purchase

# knn requires scaling since knn is a distance based model
standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])

# split the dataset into train and test
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

## train a knn model with k = 1 #####################
knn.pred <- knn(train = train.X
                , test = test.X
                , cl = train.Y
                , k = 1)

# create a confusion matrix
table(knn.pred, test.Y)

# accuracy
mean(knn.pred == test.Y) # .884 good! but!!! only 6% of the customer purchase
# we should increase the threshold
# the accuracy of predicted purchase
10 / (10 + 67) # only .1298701, not good at all

# reproduce the above stesp by changing k to 3 and 5
# train a knn model with k = 3
knn.pred <- knn(train = train.X
                , test = test.X
                , cl = train.Y
                , k = 3)
# create a confusion matrix
table(knn.pred, test.Y)
# accuracy
mean(knn.pred == test.Y) # .925
# the accuracy of predicted purchase
5 / (5 + 21) # .1923077

# train a knn model with k = 5
knn.pred <- knn(train = train.X
                , test = test.X
                , cl = train.Y
                , k = 5)
# create a confusion matrix
table(knn.pred, test.Y)
# accuracy
mean(knn.pred == test.Y) # .934
# the accuracy of predicted purchase
4 / (4 + 15) # .2105263

## train a logistic regression #####################
glm.fit <- glm(Purchase ~.
               , data = Caravan
               , subset = -test
               , family = binomial)
# predict
glm.probs <- predict(glm.fit, Caravan[test, ], type = "response")
# label the prediction
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
# create confusion matrix
table(glm.pred, test.Y)
# accuracy
0 / (0 + 7) # 0, too bad

# lower the threshold and see
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
# create confusion matrix
table(glm.pred, test.Y)
# accuracy
11 / (11 + 22) # .3333333, better
