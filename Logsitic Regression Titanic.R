
#Logistic Regression using Titanic Dataset#

titanic.raw <- read.csv("titanic.csv", header = T, na.strings = c("")) #na.strings encode all blanl spaces to NA
head(titanic.raw)
summary(titanic.raw)
dim(titanic.raw)

##finding NAs for each variable##
sapply(titanic.raw, function(x) sum(is.na(x)))

##finding unique values for each variable##
sapply(titanic.raw, function(x) length(unique(x))) 

##using package Amelia to find missing vs observed##
install.packages("Amelia") 
library(Amelia)

##vizualize missing values for all variables##
missmap(titanic.raw, main = "Missing Values vs Observed") 

##dropping some variables##
rm(titanic.data)
titanic.data <- subset(titanic.raw, select = c(2,3,5,6,7,8,10,12))  
head(titanic.data)
summary(titanic.data)

##Imputing NAs with mean from Age##
titanic.data$Age[is.na(titanic.data$Age)] <- mean(titanic.data$Age, na.rm = T)
sum(is.na(titanic.data$Age)) #checking for any NAs in Age variable

##Checking if the variables are encoded as factors##
is.factor(titanic.data$Sex)
is.factor(titanic.data$Embarked)

##checking how R has dummified the data for regression analysis##
contrasts(titanic.data$Sex)
contrasts(titanic.data$Embarked)

##removing NAs from embarked by removing the rows
titanic.data <- titanic.data[!is.na(titanic.data$Embarked),]
rownames(titanic.data) <- NULL
head(titanic.data)
sum(is.na(titanic.data$Embarked))

##Model Training##

titanic.train <- titanic.data[1:800,] #splitting into training set
titanic.test <- titanic.data[801:889,] #splitting into testing set

titanic.model <- glm(Survived~., family = binomial(link = "logit"), 
                     data = titanic.train) #training the model using glm

summary(titanic.model)

##Running ANOVA##
anova(titanic.model, test = "Chisq")

##Calculating the r-squared equivalent for logistic regression using the McFadden R squared index##
install.packages("pscl")
library(pscl)
pR2(titanic.model)

##Accessing the predictive ability of the model##

titanic.fitted <- predict(titanic.model, newdata = subset(titanic.test, 
                                                          select = c(2,3,4,5,6,7,8)),
                          type = "response")
head(titanic.fitted)
titanic.fitted <- ifelse(titanic.fitted > 0.5, 1, 0)
titanic.mis.cla.er <- mean(titanic.fitted != titanic.test$Survived) #misclassification error
print(paste('Accuracy', 1 - titanic.mis.cla.er)) #calculating the test accuracy using missclassification error

##Plotting the ROC Curve##

install.packages("ROCR")
library(ROCR)
p <- predict(titanic.model, newdata = subset(titanic.test, select = c(2,3,4,5,6,7,8)),
             type = "response")
pr <- prediction(p, titanic.test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
