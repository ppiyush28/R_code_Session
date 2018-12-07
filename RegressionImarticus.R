
#plotting the child/parent from galtons data
# melt so that each row is unique child - variable combinatioon  
long <- melt(galton)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram( colour = 'black', binwidth = 1)
g <- g + facet_grid(.~variable)
g

#finding the mu that minimizes the sum of squared distances between datapoints
#and mu
#install.packages("manipulate")
library(manipulate)
myHist <- function(mu){
        mse <- mean((galton$child - mu)^2)
        g <- ggplot(galton, aes(x=child)) + geom_histogram(fill = "salmon", 
                                                           colour = "black", 
                                                           binwidth = 1)
        g <- g + geom_vline(xintercept = mu, size = 3)
        g <- g + ggtitle(paste("mu = ",mu, ", MSE = ", round(mse,2), sep = ""))
        g
}
manipulate(myHist(mu), mu =  slider(62, 74, step = 0.5))

#the least squares estimate is the empirical mean
g <- ggplot(galton, aes(x=child)) + geom_histogram(fill = "salmon", 
                                                   colour = "black",
                                                   binwidth = 1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

#comparing childrens height and their parents height
ggplot(galton, aes(x = parent, y = child)) + geom_point()

freqData <- as.data.frame(table(galton$parent, galton$child))
head(freqData)
names(freqData) <- c("child","parent","freq")
par(mfrow = c(1,1))
plot(as.numeric(as.vector(freqData$parent)), as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue", cex = 0.1*freqData$freq, xlab = "parent",
     ylab = "child")

#finding the best fit line

#install.packages("shiny")
library(shiny)
library(dplyr)

myPlot <- function(beta){
        y <- galton$child - mean(galton$child)
        x <- galton$parent - mean(galton$parent)
        freqData <- as.data.frame(table(x, y))
        names(freqData) <- c("child", "parent", "freq")
        plot(
                as.numeric(as.vector(freqData$parent)), 
                as.numeric(as.vector(freqData$child)),
                pch = 21, col = "black", bg = "lightblue",
                cex = .15 * freqData$freq, 
                xlab = "parent", 
                ylab = "child"
        )
        abline(0, beta, lwd = 3)
        points(0, 0, cex = 2, pch = 19)
        mse <- mean( (y - beta * x)^2 )
        title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

#The solution using glm#
glm(I(galton$child-mean(galton$child)) ~ I(galton$parent-mean(galton$parent))-1)
glm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

#vizualzing the best fit line#
freqData <- as.data.frame(table(galton$parent, galton$child))
names(freqData) <- c("child","parent","freq")

plot(   as.numeric(as.vector(freqData$parent)), 
        as.numeric(as.vector(freqData$child)),
        pch = 21, col = "black", bg = "lightblue",
        cex = .15 * freqData$freq, 
        xlab = "parent", 
        ylab = "child"
)
lm1 <- glm(galton$child~galton$parent)
lines(galton$parent, lm1$fitted.values, col="red", lwd=3)
summary(lm1)



rm(g)
g <-ggplot(diamond,aes(c=carat,y=price))
g <- g+xlab("Mass (carat)")
g <- g+ylab("Price (carat)")

#hollow circles in ggplot2
g <- g+ geom_point(size=6, colour="black",alpha=0.2)
g <- g+ geom_point(size=4, colour="blue",alpha=0.2)
g <- g+ geom_smooth(method="glm",colour="black")
g

fit<-glm(price~carat,data=diamond)
summary(fit)

fit2 <- glm(price~I(carat-mean(carat)),data=diamond)
summary(fit2)

fit3 <- glm(price~I(carat-mean(carat*10)),data=diamond)
summary(fit3)

newx <- c(0.16,0.27,0.34)
coef(fit)[1] +coef(fit)[2]*newx
#to predict price of diamong using its mass
predict(fit,newdata=data.frame(carat=newx))



#calculate residuals
y <- diamond$price
x <- diamond$carat
n <-length(y)

#glm is generalized logistic regression will calculate regression of (x,y)
fit <-glm(y-x)

# e = redual error = summation(y-yhat)/n for all i
e <- resid(fit)
# yhat is predicted value with (x,y) as (carat,price)
yhat<- predict(fit)

#creating residual plot
plot(diamond$carat,diamond$price,xlab="MAss (carat)",ylab="Price (carat)",bg="lightblue",
     col="black",cex=1.1,pch=21,frame=FALSE)

abline(fit,lwd=2)
for(i in 1:n)
{
  lines(c(x[i],x[i]),c(y[i],yhat[i]),col="red",lwd=2)
}

# residuals vs x
plot(x,e,xlab="MAss (carat)",ylab="Price (carat)",bg="lightblue",
     col="black",cex=2,pch=21,frame=FALSE)

abline(h=0,lwd=2)
for(i in 1:n)
{
  lines(c(x[i],x[i]),c(e[i],0),col="red",lwd=2)
}
           
# heteroscedasticity 
library(manipulate)
x <- runif(100,0,6)
y <- x+rnorm(100,mean=0,sd=0.001*x)
g <- ggplot(data.frame(x=x,y=y),aes(x=x,y=y))
#hollow circles in ggplot2
g <- g+ geom_point(size=7, colour="black",alpha=0.4)
g <- g+ geom_point(size=5, colour="red",alpha=0.4)
g <- g+ geom_smooth(method="glm",colour="black")
g

#residual plot for above data
g <- ggplot(data.frame(x=x,y=resid(glm(y~x))),aes(x=x,y=y))
g <- g+ geom_hline(yintercept=0,size=2)
#hollow circles in ggplot2
g <- g+ geom_point(size=7, colour="black",alpha=0.4)
g <- g+ geom_point(size=5, colour="red",alpha=0.4)
g

#redual plot for diamond
#recidual--- difference between actual and predicted value

diamond$e <- resid(glm(price~carat,data=diamond))
g <- ggplot(diamond,aes(x=carat,y=e))
g <- g+ geom_hline(yintercept=0,size=2)
#hollow circles in ggplot2
g <- g+ geom_point(size=7, colour="black",alpha=0.5)
g <- g+ geom_point(size=5, colour="red",alpha=0.2)
g


library(grid)
library(car)
install.packages("car")II
library("car")
str(Prestige)
head(Prestige)
newdata <- Prestige[,c(1:4)]
summary(Prestige)
plot(newdata,pch=16,col="blue")
set.seed(1)
