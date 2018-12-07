#install.packages("UsingR")
#install.packages("manipulate")
#install.packages("reshape2")
#install.packages("shiny")


set.seed(12345)
datamatrix <- matrix(rnorm(400),nrow=40)
par(mfrow=c(1,2))
datamatrix
image(1:10,1:40,t(datamatrix)[,nrow(datamatrix):1])
image(datamatrix)

heatmap(datamatrix)

svd1 <- svd(scale(datamatrix))
par(nfrow=c(1,2))
plot(svd1$d, xlab="Column",ylab="row")


svd2 <- svd(scale(datamatrix))
par(mfrow=c(1,3))
image(t(datamatrix)[,nrow(datamatrix):1])
plot(svd2$v[,1],pch=19,xlab="Column",ylab="First RIGHT SINGULAR VECTOR")
plot(svd2$v[,2],pch=19,xlab="Column",ylab="First RIGHT SINGULAR VECTOR")


download.file("https://spark-public.s3.amazonaws.com/dataanalysis/face.rda",destfile ="face.rda" )

load("face.rda")
par(mfrow=c(1,1))
image(t(faceData)[,nrow(faceData):1])


#face example variance

svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singular value",ylab="Variance Explained")


#face example - create approximation

approx1 <- svd1$u[,1] %*% svd1$v[1] * svd1$d[,1]
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])

par(mfrow=c(1,4))
image(t(approx1)[,nrow(approx1):1])
image(t(approx5)[,nrow(approx5):1])
image(t(approx10)[,nrow(approx10):1])
image



library("UsingR")
library("reshape2")
rm(galton)
data(galton)
head(galton)
long <- melt(galton)
str(long)
library("ggplot2")
g <- ggplot(long,aes(x=value,fill= variable))


