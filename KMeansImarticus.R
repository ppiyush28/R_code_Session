#install.packages("lattice")
library(lattice)
#install.packages("ggplot2")
library(ggplot2)
par(mfrow=c(1,1))
xyplot(Ozone ~ Wind,data=airquality )
airquality <- transform(airquality,Month=factor(Month))
xyplot(Ozone~Wind | Month, data = airquality,layout=c(5,1))


set.seed(10)
x <- rnorm(100)
f <- rep(0:1,each=50)
y <- x+f-f*x+f+rnorm(100,sd=0.5)
f<- factor(f, labels=c("Group1","Group2"))
xyplot(y~x|f, layout=c(2,1))

xyplot(y~x|f,panel=function(x,y,...){
  
  panel.xyplot(x,y,...)
  panel.abline(median(y),lty=1)
})

datasets(mpg)
ggplot2::dataset(mpg)
head(mpg)
tail(mpg)
str(mpg)

qplot(displ,hwy,data=mpg,color=drv,geom=c("point","smooth")
      )
qplot(hwy,data=mpg,facets=.~drv)


set.seed(1234)
par(mar=c(5,2,1,1))
x <- rnorm(12, mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(1,2,1,each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x=0.05,y=0.05,lables=as.character(1:12))
dataframe <- data.frame(x=x,y=y)
dist(dataframe)
distxy <- dist(dataframe)   # generate distance
hclustering <- hclust(distxy) # cluster generation
plot(hclustering) # plot clustering

set.seed(143)
dataMatrix <- as.matrix(dataframe)[sample(1:12),]
heatmap(dataMatrix)




set.seed(1234)
par(mar=c(5,2,1,1))
x <- rnorm(120, mean=rep(1:3,each=40),sd=0.2)
y <- rnorm(120,mean=rep(1,2,1,each=40),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x=0.05,y=0.05,lables=as.character(1:12))

rm(dataframe)
dataframe <- data.frame(x=x,y=y)
kmeansobj <- kmeans(dataframe, centers =3)
names(kmeansobj)
kmeansobj$cluster
par(mar=rep(0,2,4))
plot(x,y,col=kmeansobj$cluster,pch=19,cex=2)
points(kmeansobj$centers,col:1:10,pch=3,lwd=3)



