x <- as.Date('1970-01-01')
unclass(x)

x <- Sys.time()
print (x)
p <- as.POSIXct(x)
print(p)

#install.packages("swirl")

x <- as.Date("2018-04-14")
y <- as.Date("2018-04-03")

x-y
print(x-y)


str(airquality)

summary(airquality)

rnorm(10,2,2)
set.seed(1)

x <- rnorm(100)
e <- rnorm(100,0,2)
y <- 2.5 * e
plot(x,y)

for(i in [1:10])
  set.seed(1)

#elapsed time > user time
system.time(readLines("http://www.google.com"))
system.time(readLines("http://www.google.co.in"))

#elapsed time < user time

#Rprof and summary
Rprof(tmp <- tempfile())
summaryRprof(tmp)


x <- list((a=1:5,b=rnorm(10))
lapply(x,mean)
y$a

#lapply,Rapply
x <- 1:4
lapply(x,runif)
lapply(x,runif,min=0,max=10)

lapply(x,function(elt) elt[,1])


#sapply
x <- list(a=1:4,b=rnorm(10),c=rnorm(20,1))
sapply(x,mean)

#apply
x <- matrix(rnorm(6),2,3)
apply(x,2,mean)


x <- matrix(1:10,5,2)
apply(x,1,quantile,probs=c(0.25,0.75))

#apply by passing row(1), col(2)
a <- array(rnorm(2*2*2),c(2,2,2))
a
apply(a,c(1,2),mean)

#tapply
list(re(1,4), rep(2,3),rep(3,2),rep(4,1))
mapply(rep,1:4,4:1)


x <- c(rnorm(10),runif(10),rnorm(10,1))
f <- gl(3,10)
x
f

tapply(x,f,mean) 
tapply(f,x,mean)
