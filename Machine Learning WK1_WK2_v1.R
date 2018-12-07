
# In sample vs out of sample errors#

install.packages("kernlab")
library(kernlab)
data(spam)
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size = 10),] #taking a random sample from containing all columns but randomly selected rows
smallSpam
spamLabel <- (smallSpam$type == "spam")*1 + 1
spamLabel
par(mfrow = c(1,1))
plot(smallSpam$capitalAve, col = spamLabel)

##apply rule 1 to smallSpam##

rule1 <- function(x){
        prediction <- rep(NA, length(x))
        prediction[x > 2.7] <- "spam"
        prediction[x < 2.40] <- "nonspam"
        prediction[x >= 2.40 & x < 2.45] <- "spam"
        prediction[x > 2.45 & x <= 2.7] <- "nonspam"
        return(prediction)
}

table(rule1(smallSpam$capitalAve),smallSpam$type)

rule2 <- function(x){
        predition <- rep(NA, length(x))
        prediction[x > 2.8] <- "spam"
        prediction[x <= 2.8] <- "nonspam"
        return(prediction)
}

table(rule2(smallSpam$capitalAve), smallSpam$type)

##apply rules to full spam data##

table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)

##look at the accuracy##

sum(rule1(spam$capitalAve) == spam$type)
sum(rule2(spam$capitalAve) == spam$type) #rule2 has better accuracy in 
#prediction spam vs non-spam due to the fact that rule1 is overfitting to 
#the single data sample that we trained it on


#SPAM Example : Data Splitting#

install.packages("caret")
install.packages("kernlab")
library(caret)
library(kernlab)
data("spam")

rm(inTrain)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
head(inTrain)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)

#SPAM Example : Fit a Model#

set.seed(32343)

modelFit <- train(type ~ ., data = training, method = "glm")
install.packages('e1071', dependencies=TRUE)
modelFit
modelFit$finalModel

##prediction##

predictions <- predict(modelFit, newdata = testing)
predictions
table(predictions)

##confusion matrix##

confusionMatrix(predictions, testing$type)

##k-fold##

set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
folds
sapply(folds, length)

folds[[1]][1:10]
folds[[1]]

str(folds)

##return test##

set.seed(32323)

folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)
sapply(folds, length)

folds[[1]][1:10]
folds[[1]]

str(folds)

##Resampling##

set.seed(32323)
folds <- createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds, length)
folds[[1]][1:10]

##timeslices##

set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$train
folds$test[[1]]
folds$test

#Training Options#

args(train)
args(trainControl)

#Plotting Predictors#

install.packages("ISLR")
library(ISLR)
library(ggplot2)
data("Wage")
summary(Wage)

##Get Training/Test sets##

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing);

##Feature Plot (Caret Package)##

featurePlot( x = training[,c("age", "education", "jobclass")], y = training$wage,
             plot = "pairs")

##qplot ggplot2 package##

qplot(age, wage, data = training)

###Qplot with color###

qplot(age, wage, colour = jobclass, data = training)

###add regression smoothers###

qq <- qplot(age, wage, colour = education, data = training)
qq + geom_smooth(method = "glm", formula = y~x)

##Cut2, making factors(Hmisc package)##

library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)

###boxplots with cut2###

p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))
p1

###boxplots with points overlayed###

p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot",
                                                                    "jitter"))
p2

install.packages("grid")
library(grid)
library(gridExtra)
library(ggplot2)
grid.arrange(p1,p2,ncol=2)

##Tables##

t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

#Density Plots#

qplot(age, colour = education, geom = "density", data = training)

#Preprocessing Data#

library(caret)
library(kernlab)
data("spam")

inTrain <- createDataPartition(y = spam$type, p = 0.7, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve, main = "", xlab = "Avg. capital run length") #this data
                                                        #requires preprocessing

mean(training$capitalAve) #checking mean
sd(training$capitalAve) #checking standard deviation

#Standardizing#

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

##Standardizing test set##

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

#Standardizing - Preprocess Function#

preObj <- preProcess(training[,-58], method = c("center","scale"))
head(preObj)
trainCapAveSP <- predict(preObj, training[,-58])$capitalAve
head(trainCapAveSP)

mean(trainCapAveSP)
sd(trainCapAveSP)

#Standardizing Test Set - Preprocess Function#

testCapAveSP <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveSP)
sd(testCapAveSP)

#########################  ERROR  = package e1071 required  ####################

#Standardizing - preProcess argument#

set.seed(32423)
install.packages("e1071")
library(e1071)
modelFit <- train(type ~., data = training, preProcess = c("center","scale"), method = "glm")
                  
#Standardizing - Box-Cox Transforamations#


rm(preObj)
install.packages("e1071")
library(e1071)
preObj <- preProcess(training[,-58], method = "BoxCox")
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow = c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

#Standardizing - Imputing Data#

##make some values NA##

data(spam)
training$capAve <-  training$capitalAve
head(training)
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.5) == 1
training$capAve[selectNA] <- NA

##Impute and Standardize##

preObj <- preProcess(training[,-58], method = "knnImpute")
install.packages("RANN")
library(RANN)
capAve <- predict(preObj, training[,-58])$capAve #Error: package RANN is required

##Standardize Truth Values##

capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth) #Error#
quantile(capAve - capAveTruth)[selectNA] #Error#
quantile(capAve - capAveTruth)[!selectNA] #Error#


##Predicting with trees##

data(iris)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y = iris$Species, p=0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Width, Sepal.Width, colour = Species, data=training)
library(caret)
modFit <- train(Species ~., method = "rpart", data = training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform = T, main = "Classfication Tree")
text(modFit$finalModel, use.n = T, all = T, cex = .8)
library(rattle)
fancyRpartPlot(modFit$finalModel)
summary(modFit)

###predicting new values###

predict(modFit, newdata = testing)

#Bagging (Botstrap Aggregating)#
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(ozone)

ozone <- ozone[order(ozone$ozone),]
head(ozone)

##Bagged loess##
ll <- matrix(NA, nrow = 10, ncol = 155)
for(i in 1:10){
        ss <- sample(1:dim(ozone)[1], replace = T)
        ozone0 <- ozone[ss,]; ozone0 <- ozone0[ozone0$ozone,]
        loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
        ll[i,] <- predict(loess0, newdata = data.frame(ozone = 1:155))
}

plot(ozone$ozone, ozone$temperature, pch = 19, cex = 0.5)
for(i in 1:10){ lines(1:155, ll[i,],col = "grey", lwd = 2)}
lines(1:155, apply(ll,2,mean), col = "red", lwd = 2)
