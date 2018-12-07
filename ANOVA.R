
gapCleaned <- read.csv("gapC.csv")
head(gapCleaned)
summary(gapCleaned)
means <- round(tapply(gapCleaned$breastcancer, gapCleaned$continent,mean), 
               digits = 2)
means

library(gplots) #load the gplot package to use plotmeans
plotmeans(gapCleaned$breastcancer~gapCleaned$continent, digits = 2, ccol = "red",
          mean.labels = T, main = "Plot of the breast cancer means by continent")

boxplot(gapCleaned$breastcancer~gapCleaned$continent, main = "Breast cancer by 
        continent(black dot is the mean", xlab = "continents", 
        ylab = "new cases per 100,000", col = rainbow(7))
points(means, col = "black", pch = 18)

aov_cont <- aov(gapCleaned$breastcancer~gapCleaned$continent)
summary(aov_cont)

tuk <- TukeyHSD(aov_cont)
tuk
plot(tuk)
