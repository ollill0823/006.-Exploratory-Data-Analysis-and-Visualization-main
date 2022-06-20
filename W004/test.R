install.packages('readxl')
install.packages('car')
install.packages('hexbin')


library(readxl)
library(car)
library(hexbin)



avggrade <- c("A","A","C","D","F","B","B","A","D","C")
major <- c("Math","CS","Math","CS","CS","Stats","Math","CS","Math","Stats")
units <- c(48, 32, 50, 42, 48, 26, 40, 36, 40, 52)
courses <- c("4", "8", "12", "unknown", "12", "6", "10", "9", "10", "13")

samp <- data.frame(major, units, courses, avggrade)

remove(major, units, courses, avggrade)


num_major <- as.numeric(samp$major)
num_courses <- as.numeric(as.character(samp$courses))
num_avggrade_temp <- factor(samp$avggrade, levels = c("A", "B", "C", "D", "F"), labels = 4:0)
num_avggrade <- as.numeric(as.character(num_avggrade_temp))


## Common non-linear transformations include exp(), log(), sqrt(),or power (usually squared or cubed)
newdat <- read.table("rand1.txt", sep = "\t", header = T)		## sep = ";", sep = ".", sep = ","
head(newdat)
summary(newdat)
## 4.000e+00 	means 4 * 10^0	 = 4
## 3.877e+11 means 3.877 * 10^11 = 387,700,000,000
## 1.166e+22   this is the same as 1.166 * 10^22 (scientific notation)


boxplot(log(newdat$var1))
boxplot(log(newdat$var1), ylab = "Log of var1")	
hist(log(newdat$var1))

library(readxl)
library(car)
library(hexbin)


##### You CAN create new transformed variables #####
newdat$lnvar1 <- log(newdat$var1)
newdat$sqrtvar2 <- sqrt(newdat$var2)
newdat$var6mils <- newdat$var6 / 1000000
summary(newdat)


powexample <- c(1, 1.4, 1.8, 2, 2.1, 2.05, 2.08, 2.09)
boxplot(powexample)
hist(powexample)
boxplot(powexample^16)		## arbitrarily picked 16th power
hist(powexample^16)

boxplot(powexample^2)			## trade-off between keeping things simple/interpretable vs. looking "nice"
hist(powexample^2)

## age is a variable that you'll see commonly using ^2
##	(young and old are more similar -- U shape)





########## Scatter Plots and Regression ##########

##### Read in dataset #####
poverty<-read_excel("poverty.xls", sheet = 1)
poverty<-as.data.frame(poverty)			## Convert tibble to data frame
poverty$gnp<-as.numeric(poverty$gnp)		## convert gnp to numeric
poverty <- poverty[!is.na(poverty$gnp),]	## Removing countries without GNP


plot(poverty[,2:4])



## Main labels, text labels, and gridlines
plot(poverty$birthrt, poverty$deathrt, main = "Birth and Death Rates Scatterplot", 
     pch = 17, col = "red", xlab = "Birth Rate", ylab = "Death Rate")
text(25,20, labels = "Label centered at (25,20)...........", adj=0.5)
text(24, 6, labels = "Albania")
## adj = 0 is left/bottom aligned, 1 is right/top aligned, and 0.5 is centered
grid(col = "grey30")

##??? PRACTICE: Graph male life expectancy vs. female life expectancy by region
plot(poverty$lexpm[poverty$region == 1], poverty$lexpf[poverty$region == 1], main = 'Life expectancy male V.S. female',
     xlim = c(30,80), ylim = c(30,85), xlab = 'male age', ylab = 'female age', pch = 17, col = 'red')

points(poverty$lexpm[poverty$region == 2], poverty$lexpf[poverty$region == 2],
       pch = 17, col = 'orange')
points(poverty$lexpm[poverty$region == 3], poverty$lexpf[poverty$region == 3],
       pch = 17, col = 'yellow')
points(poverty$lexpm[poverty$region == 4], poverty$lexpf[poverty$region == 4],
       pch = 17, col = 'green')
points(poverty$lexpm[poverty$region == 5], poverty$lexpf[poverty$region == 5],
       pch = 17, col = 'blue')
points(poverty$lexpm[poverty$region == 6], poverty$lexpf[poverty$region == 6],
       pch = 17, col = 'purple')
legend('topleft', c('region1', 'region2', 'region3', 'region4', 'region5', 'region6'),
                    col = c('red','orange', 'yellow', 'green', 'blue', 'purple'),
       pch = 17)


model <- lm(poverty$lexpm, poverty$lexpf)
model <- lm(lexpm~lexpf, data = poverty)
model
summary(model)

model0 <- lm(lexpf~lexpm, data=poverty)
model0
summary(model0)


model1 <- lm(infmort~lexpf, data=poverty)
model1
summary(model1)
plot(model1)



modelx <- lm(poverty$deathrt ~ poverty$birthrt)
plot(poverty$birthrt, poverty$deathrt)
abline(modelx)
plot(modelx)



## You can also use plot for 1 variable only
plot(poverty$lexpm)		## Left-to-right in order of your dataset



##### Other Scatter Plot Specials #####
## plot() gives you a new plot
## 	type = "p" gives points, default
## 	"l" lines
##	"b" both lines and points, "o" similar to "b", 
## 	"c" lines with spaces,
## 	"h" histogram-like vertical lines
##	"s" stair steps
##	"n" no plotting
## points() adds points to the existing plot
## abline() adds lines to an existing plot


## This will give you the same thing as plot()
## except in two steps (axes first, then points)
plot(poverty$lexpm, type = "p")
lines(poverty$lexpm, col = "red", type = "p")	## adding points to the graph


names(poverty)

order <- poverty[order(poverty$lexpm, poverty$lexpm),]
order2 <- poverty[order(poverty$lexpm),]

plot(order2$lexpm, order2$lexpf, type = 'l')
plot(order$lexpm, order$lexpf, type = 'l')


write.csv(order, 'order.csv')
write.csv(order2, 'order2.csv')



plot(poverty$lexpf, poverty$birthrt, xlim =c(30,80),
     ylim=c(0,70))
smooth <- smooth.spline(poverty$lexpf, poverty$birthrt, spar = 0.5)
lines(smooth, col='red', lwd= 2)


model3 <- lm(birthrt~I(lexpf^2), data=poverty)
model4 <- lm(birthrt~I(lexpf^2), data = poverty)
summary(model3)
plot(model3)
plot(model4)


normalpoints <- rnorm(1000000) ## a million random points drawn from the normal distribution
plot(density(normalpoints))
polygon(density(normalpoints), col = "red")		

normalpoints <- rnorm(100000) ## a million random points drawn from the normal distribution
plot(density(normalpoints))
polygon(density(normalpoints), col = "green")		



pnorm(72, mean=81, sd= 8)
pnorm(-1.125)



##### PRACTICE!
##??? Test A had mean = 81, standard deviation = 5. On Test A, you scored 85.
##	Test B had mean = 81, standard deviation = 8. On Test B, you scored 87.
##	1. Which test did you do better on?
##	For each Test:
##	2. Draw the normal density plot and line showing where you landed.
##	3. Label on your plot how many students you did better/worse than

testA <- pnorm(85, mean = 81, sd = 5)
testB <- pnorm(87, mean = 81, sd = 8)
testA > testB

normalpoints2 <- rnorm(1000000)
plot(density(normalpoints2))
abline(v=0.7881446, lty = 2, col = 'blue' )
abline(v=0.7733726, lty = 2, col = 'red' )
text(-2,0.2, labels = '78.81% of student \nbelow me in Test A')
text(3,0.2, labels = '21.19% of student \nbelow me in Test A')
pnorm(85, mean = 81, sd = 5, lower.tail = F)


1 - pnorm(75, mean = 81, sd =5) - pnorm(85, mean = 81, sd =5, lower.tail = F)
