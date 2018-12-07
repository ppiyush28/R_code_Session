//heatmap ??

set.seed(12345)
datamatrix <- matrix(rnorm(400),nrow=40)
par(mfrow=c(1,2))
datamatrix
image(1:10,1:40,t(datamatrix)[,nrow(datamatrix):1])
image(datamatrix)

heatmap(datamatrix)

##+++++++++++++++++++++++++++++++++++++
image(1:10,1:40),t(datamatrix)[,rnow(datamatrix):1]
heatmap(datamatrix)

#patttem in  row and col - error

hh <- hclust(dist(datamatrix))
datamatrixorder <- datamatrix[hh$order,]
par(mfrow=c(1,3))
image(i(datamatrixorder)[,nrow(datamatrixorder):1])
plot(rowMeans(datamatrixorder),40,1, xlab="row means", ylab="row", pch=19)
plot(colmeans(datamatrixorder),xlab="colum",ylab="col mean",pch=19)


#component of SVD - u and v

svd1 <- svd(scale(datamatrixorder))
par(mfrow= c(1,2))
plot(svd1$d,xlab="col mean",ylab="singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="col mean",ylab="proportion of varince explanin",pch=19)

#v and pattern of variace in rows


svd2 <- svd(scale(datamatrixorder):1)
par(mfrow=c(1,3))
image(t(datamatrixorder)[,nrow(datamatrixorder):1])
plot(svd2$v[,1],pch=19,xlab="col", ylab="first right singluar vect")
plot(svd2$v[,2],pch=19,xlab="col", ylab="second right singluar vect")


#face example

download.file("https://spark-public.s3.amazonaws.com/dataanalysis/face.rda",destfile ="face.rda" )
load("C:/Users/piyush/Downloads/face.rda")
par(mfrow= c(1,1))
image(t(faceData)[,nrow(faceData):1])

#face example varince explianined

svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch=19, xlab="signular value", ylab="varince expalined")


#face example - create approximation

approx1 <- svd1$u[,1] %*% t(svd1$u[,1] * svd1$d[1])
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5] %*% t(svd1$v[,1:5]))
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[1:10])

par(mfrow= c(1,4))
image(t(approx1)[,nrow(approx1):1])
image(t(approx5)[,nrow(approx5):1])
image(t(approx10)[,nrow(approx10):1])

                                 
                                
                                
                                
                                
                                