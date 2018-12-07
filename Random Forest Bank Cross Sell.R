
getwd()
install.packages("randomForest")
library(randomForest)

termCrosssell <- read.csv("termCrosssell.csv", header = T, sep = ";")
class(termCrosssell)
names(termCrosssell) #input dataset has 20 independent variables and a target variable
table(termCrosssell$y)/nrow(termCrosssell) #11% of the observations have target variable yes and 89% have target variable no
head(termCrosssell)

#Now we split the data into developement and validation samples

sample.ind <- sample(2, nrow(termCrosssell), replace = T, prob = c(0.6,0.4))
cross.sell.dev <- termCrosssell[sample.ind == 1,]
cross.sell.val <- termCrosssell[sample.ind == 2,]

table(cross.sell.dev$y)/nrow(cross.sell.dev)
table(cross.sell.val$y)/nrow(cross.sell.val)
#both the developement and validation have similar target value distribution

class(cross.sell.dev$y) #checking the class of the target variable

#Creating Formula#

varNames <- names(cross.sell.dev)

##Exclude ID and response variable##
varNames <- varNames[!varNames %in% c("y")]
varNames

##add a plus sign between exploratory variables##
varNames1 <- paste(varNames, collapse = "+")

##add response variable and convert to a formula object##
rf.form <- as.formula(paste("y", varNames1, sep = "~"))

#Building Random Forest Using R#
cross.sell.rf <- randomForest(rf.form, cross.sell.dev, ntree = 500, importance = T)
par(mfrow = c(1,1))
plot(cross.sell.rf)

#Variable Importance Plot#
varImpPlot(cross.sell.rf, sort = T, main = "Variable Importance")

#Variable Importance Table#
var.imp <- data.frame(randomForest::importance(cross.sell.rf), type = 2)

#make row names as columns#
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini, decreasing = T),]

#predict response variable using Random Forest#
cross.sell.dev$predicted.response <- predict(cross.sell.rf, cross.sell.dev)

#Confusion Matrix#
install.packages("e1071")
library(e1071)
library(caret)
confusionMatrix(data = cross.sell.dev$predicted.response, 
                reference = cross.sell.dev$y, positive = "yes") #accuracy on the developement set

##predicting response variable##
cross.sell.val$predicted.response <- predict(cross.sell.rf, cross.sell.val)
confusionMatrix(data = cross.sell.val$predicted.response, 
                reference = cross.sell.val$y, positive = "yes")

