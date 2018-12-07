
#Multivariate Linear Regression#

install.packages("car")
library(car)
data(prestige)
head(Prestige)

library(car)
library(corrplot) # We'll use corrplot later on in this example too.
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(knitr)
library(scatterplot3d)

str(Prestige)
summary(Prestige)

# Let's subset the data to capture income, education, women and prestige.
newdata = Prestige[,c(1:4)]
summary(newdata)

# Plot matrix of all variables.
plot(newdata, pch=16, col="blue", main="Matrix Scatterplot of Income, Education, Women and Prestige")

set.seed(1)

# Center predictors.
education.c = scale(newdata$education, center=TRUE, scale=FALSE)
prestige.c = scale(newdata$prestige, center=TRUE, scale=FALSE)
women.c = scale(newdata$women, center=TRUE, scale=FALSE)

# bind these new variables into newdata and display a summary.
new.c.vars = cbind(education.c, prestige.c, women.c)
newdata = cbind(newdata, new.c.vars)
names(newdata)[5:7] = c("education.c", "prestige.c", "women.c" )
summary(newdata)

# fit a linear model and run a summary of its results.
mod1 = glm(income ~ education.c + prestige.c + women.c, data=newdata)
summary(mod1)

# Plot a correlation graph
newdatacor = cor(newdata[1:4])
corrplot(newdatacor, method = "number")

# fit a linear model excluding the variable education
mod2 = glm(income ~ prestige.c + women.c, data=newdata)
summary(mod2)

# Plot model residuals.
plot(mod2, pch=16, which=1)

#3d plot#
newdat <- expand.grid(prestige.c=seq(-35,45,by=5),women.c=seq(-25,70,by=5))
newdat$pp <- predict(mod2,newdata=newdat)
with(newdata,plot3d(prestige.c,women.c,income, col="blue", size=1, type="s", main="3D Linear Model Fit"))
with(newdat,surface3d(unique(prestige.c),unique(women.c),pp,
                      alpha=0.3,front="line", back="line"))

# fit a model excluding the variable education,  log the income variable.
mod3 = glm(log(income) ~ prestige.c + I(prestige.c^2) + women.c + I(women.c^2) , data=newdata)
summary(mod3)

# Plot model residuals.
plot(mod3, pch=16, which=1)

#3d plot 2#
newdat2 <- expand.grid(prestige.c=seq(-35,45,by=5),women.c=seq(-25,70,by=5))
newdat2$pp <- predict(mod3,newdata=newdat2)
with(newdata,plot3d(prestige.c,women.c,log(income), col="blue", size=1, type="s", main="3D Quadratic Model Fit with Log of Income"))
with(newdat2,surface3d(unique(prestige.c),unique(women.c),pp,
                       alpha=0.3,front="line", back="line"))

# fit a model excluding the variable education,  log the income variable.
mod4 = glm(log(income) ~ prestige.c + I(prestige.c^2) + women.c , data=newdata)
summary(mod4)

# Plot model residuals.
plot(mod4, pch=16, which=1)

#3d plot 3#
newdat3 <- expand.grid(prestige.c=seq(-35,45,by=5),women.c=seq(-25,70,by=5))
newdat3$pp <- predict(mod4,newdata=newdat3)
with(newdata,plot3d(prestige.c,women.c,log(income), col="blue", size=1, type="s", main="3D Quadratic Model Fit with Log of Income excl. Women^2"))
with(newdat3,surface3d(unique(prestige.c),unique(women.c),pp,
                       alpha=0.3,front="line", back="line"))

