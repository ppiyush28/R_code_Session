
set.seed(1)
#create our own test dataset#

x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x;y;
x[y==1,] = x[y==1,] + 1
par(mfrow = c(1,1))
plot(x,col = (3-y))

library(e1071)
dat <- data.frame(x = x, y = factor(y))
svm.fit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = F)

#plot the SVM obtained
par(mar = c(5,4,3,2))
plot(svm.fit, dat)
summary(svm.fit)

svm.fit2 <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1 , scale = F)
plot(svm.fit2, dat)
summary(svm.fit2)

set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear", 
                 ranges = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
tune.out
summary(tune.out)
tune.out$best.model

xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1,1), 20, replace = T)
xtest[ytest == 1, ] = xtest[ytest == 1,] + 1
test.dat = data.frame(x = xtest, y = as.factor(ytest))

yhat <- predict(tune.out$best.model, test.dat)
library(caret)
confusionMatrix(yhat, test.dat$y)

#Support Vector Machine#

##Generate Some test data##
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100, ] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col = y)
plot(svm.fit, dat[train,])

##randomly split the data into train and test groups and fit a radial kernel##
train <- sample(200,100)
svm.fit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svm.fit, dat[train,])
summary(svm.fit)
yhat <- predict(svm.fit, dat[-train,])
confusionMatrix(yhat, dat[-train, 'y'])

svm.fit <- svm(y~., dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svm.fit, dat[train,]) #this might lead to overfitting, because of cost being too high

##trying cross validating parameters##
set.seed(1)
tune.out <- tune(svm, y~., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                 gamma = c(0.5,1,2,3,4)))
summary(tune.out)

yhat <- predict(tune.out$best.model, dat[-train,])
confusionMatrix(yhat, dat[-train,'y'])
plot(tune.out$best.model, dat[train,])

#ROC Curves#
library(ROCR)

##function to handle different models##
rocplot <- function(pred, truth, ...){
        predob = prediction(pred,truth)
        perf = performance(predob, 'tpr', 'fpr')
        plot(perf, ...)
}

svm.opt <- svm(y~., data = dat[train, ], kernel = "radial", gamma = 2, cost = 1,
               decision.values = T)
fitted <- attributes(predict(svm.opt, dat[train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, 'y'], main = "training data")

##Trying with CARET Package##

#ctr <- caret::trainControl(method = 'cv', number = 10, classProbs = T, 
                    summaryFunction = twoClassSummary)

#y <- as.factor(y)
#svm.c <- train(y~., dat[train, ], method = 'svmRadial', trControl = ctr, 
#               metric = "ROC")
        
#class(y)
library(ggplot2)
head(Auto)

head(mtcars)
summary(mtcars)

mtcars_svmdat <- mtcars

#Create a bindary variable that takes 1 for cars with gas milage > median

mtcars_svmdat$y <- NA
mtcars_svmdat$y[mtcars_svmdat$mpg > median(mtcars_svmdat$mpg)] <- 1
mtcars_svmdat$y[mtcars_svmdat$mpg < median(mtcars_svmdat$mpg)] <- 0
summary(mtcars_svmdat)
str(mtcars_svmdat)

mtcars_svmdat$y <- as.factor(mtcars_svmdat$y)
str(mtcars_svmdat)

is.na(mtcars_svmdat$y)
mtcars_svmdat <- mtcars_svmdat[!is.na(mtcars_svmdat$y)]
rownames(is.na(mtcars_svmdat$y))
mtcars_svmdat <- mtcars_svmdat[!is.na(mtcars_svmdat$y),]
str(mtcars_svmdat)
summary(mtcars_svmdat)

set.seed(123)
split <- createDataPartition(y = mtcars_svmdat$y, p = 0.7, list = F)
train.svm <- mtcars_svmdat[split,]
test.svm <- mtcars_svmdat[-split,]

#Remove mpg from my data#

train.svm <- train.svm[,-c(1)]
test.svm <- test.svm[,-c(1)]
summary(train.svm)
summary(test.svm)

#10 fold cross validation#

ctr.cv <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Recall as C increses the margin tends to get wider#

grid.svm <- data.frame(C = seq(0.01,5,0.5))
grid.svm

svm.fit.n <- train(y ~ ., train.svm , method = "svmLinear", preProc = c("center", "scale"),
                 trControl = ctr.cv, tuneGrid = grid.svm)
svm.fit.n

#Accuracy on training set#
confusionMatrix(predict(svm.fit.n,train), train$y)

#Accuracy on testing set#
yhat <- predict(svm.fit.n, test.svm)
confusionMatrix(yhat, test.svm$y)

#Using Polynomial Kernel#

set.seed(123)

svm.fit.p <- train(y~., train.svm, method = "svmPoly", trControl = ctr.cv)
svm.fit.p
plot(svm.fit.p)

#training error rate#
confusionMatrix(predict(svm.fit.p, train.svm), train.svm$y)

#testing error rate#
confusionMatrix(predict(svm.fit.p, test.svm), test.svm$y)
