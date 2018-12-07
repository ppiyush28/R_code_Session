
#plotting the child/parent from galtons data
long <- melt(galton)
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

freqData <- as.data.frame(table(galton$parent, galton$child))
names(freqData) <- c("child","parent","freq")
par(mfrow = c(1,1))
plot(as.numeric(as.vector(freqData$parent)), as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue", cex = 0.1*freqData$freq, xlab = "parent",
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


