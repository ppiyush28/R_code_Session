install.packages("car")
install.packages("corrplot")
install.packages("UsingR")
install.packages("rgl")
install.packages("scatterplot3d")

library(UsingR)
data(diamond)
library(ggplot2)
rm(g)
g <- ggplot(diamond,aes(x=cart,y=price))
g <- g+xlab("mass (carats)")
g <- g+ylab("price(SIN $)")
g <- g+geom_point(size=6,colours = "black",alpha =0.2)
g <- g+geom_point(size=5, colour="blue",alpha=0.2)
g <- g+geom_smooth(method="glm",colour="black")

fit <- glm(price ~ carat,data=diamond)
summary(fit)

fit2 <- glm(price ~ I(carat - mean(carat)),data=diamond)
summary(fit2)

fit3 <- glm(price ~ I(carat*10),data=diamond)
summary(fit3)


newx <- c(0.16,0.27,0.34)
coef(fit)[1] + coef(fit)[2] * newx
#predictor 
predict(fit,newdata=data.frame(carat=newx))
y <- diamond$price;x <- diamond;n <-lenght(y)
fit < glm(y-x)
e <- resid(fit)
yhat <- predict(fit)

#create a residual plot

plot(diamond$carat, diamond$price,xlab="Mass(carats)",ylab="price(SIN $)",
bg="lightblue",col="black",cex=1.1,pch=21,frame=false)

abline(fit,lwd=2)
for(i in 1:n)
  {
  lines(c(x[i],x[i]),c(y[i],yhat[i]),col="red",lwd=2)
}

#residuals versus
plot(x,e,xlab="Mass(carats)",ylab="price(SIN $)", bg="lightblue",
     col="black",cex=2,pch=21,frame=FALSE)
abline(h=0,lwd=2)
for(i in 1:n){
  lines(c(x[i],x[i]),c(e[i],0),col="red",lwd=2)
}

#hetroskedascity

x <- runif(100,0,6)
y <- x + rnorm(100,mean=0,sd=0.001*x)
g <- ggplot(data.frame(x=x,y=y),aes(x=x,y=y))
#hallow circles in ggplot2
g <- g + geom_point(size=7,colour="black",alpha=0.4)
g <- g + geom_point(size=5,colour="red",alpha=0.4)
g <- g + geom_smooth(method="glm",colour="black")

#residaul plot of above data

g <- ggplot(data.frame(x=x,y=resid(glm(y~x))),aes(x=x,y=y))
g <- g + geom_hline(yintercept = 0,size=2)
g <- g + geom_point(size=7,colour="black",alpha=0.4)
g <- g + geom_point(size=5,colour="red",alpha=0.4)
g <- g + xlab("x") + ylab("residual")
g

install.packages("shiny")


e <- c(resid(glm(price~1),data=diamond)),resid(glm(price~carat,data=diamond)))
fit <- factor(c(rep("Itc",nrow(diamond)),rep("Itc,slope",nrow(diamond))))
g <- ggplot(data.frame(e=e,fit=fit),aes=y=e,x=fit,fill=fit))
g <- g + geom_dotplot(binaxis = "y",size=2,stackdir="center",binwidth = 10)
g <- g + xlab
##???  


