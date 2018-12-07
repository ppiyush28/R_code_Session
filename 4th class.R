#######################################
#split
x <- c(rnorm(10),runif(10),rnorm(10,1))
#F <- gl(3,10)
F <- c(TRUE,FALSE)
split(x,f)

#lapply(split(x,F),mean)
lapply(split(x,F))

x <- (1:20)
F <- c(TRUE,FALSE)
lapply(split(x,F),mean)
#######################################
split(airquality,airquality$Month)

#######################################
s <- split(airquality,airquality$Month)
lapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))
sapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")],na.rm= T))
#######################################
#error dectection
rm(x)
mean(x)
traceback()

lm(x~y)
glm(y~x)
debug(lm)
debug(glm)
#######################################
#data loading using internal Lib
library(datasets)
data("iris")
iris
class(iris)
summary(iris)
?iris

I <- subset(iris,iris$Species=="virginica")
I <- 
summary (I)
mean(I$Sepal.Length) 
mean(I$Petal.Width)

mean(i)
######################################
getwd()

setwd("C:/Users/piyush/Documents/Imarticus/R code")
if(!file.exists("cam.csv"))
{
dir.create("cam.csv")  
}
#######################################
#open balitmore

#dataset download & read
fileURL <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileURL, destfile = "C:/Users/piyush/Documents/Imarticus/R Code/data/Cam.csv")
#######################################
download.file("C:/Users/piyush/Documents/Imarticus/R Code/data/data.csv",destfile = "C:/Users/piyush/Documents/Imarticus/R Code/data/data.csv" )################################
data <- read.csv("C:/Users/piyush/Documents/Imarticus/R Code/data/data.csv")
print(data)

#install package
install.packages("data.table")
library(data.table)
DT <- data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
DT
tables()
DT[2,]
DT[DT$y=="b"]
DT(c(1,2,3,4))

DT[,list(mean(x),sum(z))]
DT[,table(y)]
DT[,w:=z^2]
DT

DT[,m:= {tmp<-(x-z);log2(tmp+5)}]

DT

DT[,a:=x>0]
DT

DT[,b:=mean(x-w),by=a]
DT

set.seed(123)
DT <- data.table(x=sample(letter[1:3],1E5,TRUE))
DT[,,N,by]


DT1 <- data.table(x=c("a","a","dt1"), y=1:4)
DT2 <- data.table(x=c("a","b","c"), z=5:7)

setkey(DT1,x)
setkey(DT2,x)
merge(DT1,DT2)