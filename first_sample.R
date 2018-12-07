

a <- matrix(nrow =2,ncol=3)
print(dim(a))



#########################################
a1 <- matrix(nrow =2,ncol=3, byrow=FALSE)
print(dim(a1))

a2 <- matrix(1:9,nrow =3,ncol=3)
print(dim(a2))
print(a2)

z <- list(1,"a",T,1.4)
#x <- c1

#########################################
x <- 1:3 #x decl
y <- 11:33 #y decl
z <- cbind(x,y) #cbinding 
print (z)
z1 <- rbind(x,y) #rbinding 
print ("rbinding of Z1" z1)

#########################################
xf <- factor (c("yes","no","yes","no")) #factor creation
xf #get values for xf
table(xf) #distribution of data

xNA <- c(1,2,NA,5)

#########################################
Gender <- c("male","female","male","female")
xDF <- data.frame(Gender)
print(is.factor(xDF$Gender))
print(xDF[1,1])
#########################################
m4 <- matrix(1:4,nrow=2,ncol=2) 
dimnames(m4) <- list (c("a","b"),c("c","d"))
print(dimnames)
#########################################
print(class(airquality))
print(airquality[1:6,])
head(airquality)
#########################################
#########################################
#########################################
#########################################
#########################################
#########################################
#########################################
