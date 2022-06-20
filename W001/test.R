irisdata <- read.csv('iris.csv')


irisdata<-read.csv("iris.csv", header = T)






var1<-3:12
var2<-c(3,5,8,2,3,5,2,3,2,1)
var3<-rep(3,10)				##repeat 3 ten times
var4<-c(1,rep(2,3),rep(4,3),rep(5,3))
var5<-c(rep(1,5), rep(3,5))

mydata <- data.frame(var1,var2,var3,var4)


mynewdata <- cbind(mydata, var5)

write.csv(mynewdata,"lect1data1.csv")	


irisdata <- read.csv("iris.csv",header = T)


usarrestsdata <- read.csv('usarrests.csv', header = T)


par(mfrow = c(1,2))
boxplot(irisdata$Sepal.Length, main = 'Sepal_Length', ylab = 'cm')
boxplot(irisdata$Petal.Width, main = 'Petal_Width', ylab = 'cm', ylim = c(0,3))
boxplot(irisdata$Sepal.Width, main = 'Sepal_Width', ylab = 'cm')

boxplot(irisdata$Sepal.Length, main = 'Sepal_Length', ylab = 'cm', ylim = c(0,8))
boxplot(irisdata$Sepal.Width, main = 'Sepal_Width', ylab = 'cm', ylim = c(0,8))


par(mfrow = c(1,1))

boxplot(Petal.Width~Species, data = irisdata, col = 'green', main = 'difference', ylab = 'cm', ylim = c(0,3) )

summary(irisdata[which(irisdata$Species == 'setosa'),])
summary(irisdata[which(irisdata$Species == 'versicolor'),])
summary(irisdata[which(irisdata$Species == 'virginica'),])

summary(irisdata$Sepal.Length)
