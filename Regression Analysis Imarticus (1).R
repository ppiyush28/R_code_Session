
#plotting the child/parent from galtons data
library(UsingR)
library(reshape2)
rm(galton)
data(galton)
head(galton)
summary(galton)
str(galton)
long <- melt(galton)
str(long)
library(ggplot2)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram( colour = 'black', binwidth = 1)
g <- g + facet_grid(.~variable)
g

#finding the mu that minimizes the sum of squared distances between datapoints
#and mu
install.packages("manipulate")
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
rm(freqData)
freqData <- as.data.frame(table(galton$child, galton$parent))
head(freqData)
names(freqData) <- c("child","parent","freq")
par(mfrow = c(1,1), mar = c(5,4,3,2))
plot(as.numeric(as.vector(freqData$parent)), as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue", cex = 0.2*freqData$freq, xlab = "parent",
     ylab = "child")

#finding the best fit line

install.packages("shiny")
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
glm(I(child - mean(child))~ I(parent - mean(parent))-1, data = galton)

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
summary(lm1)
lines(galton$parent, lm1$fitted.values, col="red", lwd=3)

library(UsingR)
data(diamond)
library(ggplot2)
summary(diamond)
rm(g)
g <- ggplot(diamond, aes(x = carat, y = price))
g <- g + xlab("Mass (Carats)")
g <- g + ylab("Price (SIN $)")
g <- g + geom_point(size = 6, colour = "black", alpha = 0.2)
g <- g + geom_point(size = 5, colour = "blue", alpha = 0.2)  
g <- g + geom_smooth(method = "glm", colour = "black")
g

fit <- glm(price ~ carat, data = diamond)
summary(fit)

fit2 <- glm(price ~ I(carat - mean(carat)), data = diamond)
summary(fit2)

fit3 <- glm(price ~ I(carat*10), data = diamond)
summary(fit3)

newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
predict(fit, newdata = data.frame(carat = newx)) #predict function used to price of a diamond using its Mass

#Calculating Residuals
y <- diamond$price; x <- diamond$carat; n <- length(y)                                        
fit <- glm(y~x)
e <- resid(fit)
yhat <- predict(fit)

#Creating a residual plot
plot(diamond$carat, diamond$price, xlab = "Mass(carats)", ylab = "Price(SIN $)",
     bg = "lightblue", col = "black", cex = 1.1, pch = 21, frame = FALSE)
abline(fit, lwd=2)
for(i in 1:n){
        lines(c(x[i],x[i]), c(y[i],yhat[i]), col="red", lwd=2)
}

#Residuals versus x

plot(x,e, xlab = "Mass(carats)", ylab = "Price(SIN $)", bg = "lightblue",
     col = "black", cex = 2, pch = 21, frame = FALSE)
abline(h=0, lwd=2)
for(i in 1:n){
        lines(c(x[i],x[i]),c(e[i],0), col = "red", lwd = 2)
}

#Hetroskedasticity
 
x <- runif(100,0,6); y <- x + rnorm(100, mean=0, sd = 0.001*x)
g <- ggplot(data.frame(x=x, y=y), aes(x=x, y=y))
g <- g + geom_smooth(method = "lm", colour = "black")
g <- g + geom_point(size = 7, colour = "black", alpha = 0.4)
g <- g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

##residual plot for the above data

g <- ggplot(data.frame(x=x,y=resid(glm(y~x))), aes(x=x, y=y))
g <- g + geom_hline(yintercept = 0, size = 2)
g <- g + geom_point(size = 7, colour = "black", alpha = 0.4)
g <- g + geom_point(size = 5, colour = "red", alpha = 0.4)
g <- g + xlab("X") + ylab("Residual")
g

#Residual plot for diamond data
rm(g)
diamond$e <- resid(glm(price~carat, data = diamond))
g <- ggplot(diamond, aes(x = carat, y = e))
g <- g + xlab("Mass(Carats)")
g <- g + ylab("Residual price (SIN $)")
g <- g + geom_hline(yintercept = 0, size = 2)
g <- g + geom_point(size = 7, colour = "black", alpha = 0.5)
g <- g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g

e <- c(resid(glm(price~1, data = diamond)),resid(glm(price~carat, data = diamond)))
fit <- factor(c(rep("Itc",nrow(diamond)),rep("Itc,slope", nrow(diamond))))
g <- ggplot(data.frame(e=e, fit = fit), aes(y=e,x=fit,fill=fit))
g <- g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 10)
g <- g + xlab("Fitting Approach")
g <- g + ylab("Residual Price")
g
