#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 8
##
## Agenda:
## 1. Data Cleaning: Using a Category as Response Variable
## 2. Logistic Regression Review
## 3. Group Meetings
## 4. Two-Sample T-Test
## 5. Two-Sample Proportion Test
## 6. Maps
## 7. Clustering: k-means
## 8. Clustering: Hierarchical
## 9. 3D Scatterplots
## 10. GGPlot(?)
#################################################
library(corrplot)		## install.packages("corrplot")
library(GGally)
library(scatterplot3d)

library(car)
library(rgl)

library(sf)
library(raster)
library(dplyr)		## note, dplyr gives you "mask" warnings
				## this means two librarys have the same function name
				## but the functions do different things
				## to distinguish, use dplyr:__<<function>>__()
library(spData)		##spData gives us us_states below; use ?spData to see other available maps
library(spDataLarge)   ## install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")
library(tmap)

library(cluster)
library(ggplot2)
library(readxl)
library(datasets)
library(scatterplot3d)




########## Data Cleaning: Using a Category as Response Variable ##########
wk8dat<-read.csv("randomdat2.csv", header = T)

## Easiest to work with 0/1 for categorical
## 0 =  Not Class B, 1 = Is Class B
## First create a new variable "isclassb" and make it all 0's
wk8dat$isclassb <- rep(0,300)

## Change Class B observations to 1
wk8dat$isclassb[wk8dat$cat1=="Class B"] <- 1

## Check work
table(wk8dat$cat1,wk8dat$isclassb)

plot(wk8dat[,c(3:8)])
ggscatmat(wk8dat,columns = 3:8)
	## need to be careful about including both var1 and var5 in model due to multicollinearity



m1 <- lm(isclassb~var1+var2+var3+var4+var5, data=wk8dat)
summary(m1)

m1<-lm(isclassb~var1+var2+var4+var5, data=wk8dat)		## Remove var3 because it's not significant
summary(m1)
predict(m1,list(var1=100,var2=-50,var4=0,var5=55))
				## 0 is not Class B, 1 is Class B
plot(m1)			## Could this mean that you are really really in Class B?
				## Closer to 0 means closer to not class B, and closer to 1 means closer to class B


########## Logistic Regression ##########
## Logistic Regression is used when your dependent variable is 0-1 (binary)
## P(Y): probability of being 1 (being class B)
## 1-P(Y): probability of not being 1 (of being 0, not being class B)
## odds ratio: probability of class B / probability of not class B
##			P(Y)/(1-P(Y))
##	For example:	one person in class B for every one person NOT in class B
##		1 / 1 ratio					(probability 1/2)
##	Another example: five people in class B for every 14 people NOT in class B
##		5 / 14 ratio					(probability 5/19)
## log(P(Y)/(1-P(Y))) is a log transformation of your odds ratios
##		5/1, 4/1, 3/1, 2/1, 1/1, 1/2, 1/3 ,1/4, 1/5

## Our equation in logistic regression becomes:
## log(P(Y)/(1-P(Y))) = b0 + b1*X + ...
## What the line is giving you is the log-odds of being 1 or not
## positive coefficients means increase in odds
## negative coefficients means decrease in odds


mlog1<-glm(isclassb~var1+var2+var3+var4+var5+var6, data = wk8dat, family = binomial())
summary(mlog1)
## higher values of var1 increase our odds of being class B
## higher values of var2 decrease our odds of being class B
## higher values of var3 decrease our odds of being class B
## higher values of var4 increase our odds of being class B
## ...
## higher var2 and var3 values means we're less likely to be in class B (because it's a negative coefficient) 
## higher var1, var4, var5, var6 values means we're more likely to be in class B (because it's a positive coefficient) 



mlog2<-glm(isclassb~var1+var2+var4+var5, data = wk8dat, family = binomial())
summary(mlog2)

## Took out var5 because of high collinearity with var1 (depending on your theory,
## you'll want to take out the less important of the two)
mlog3<-glm(isclassb~var1+var2+var4, data = wk8dat, family = binomial())
summary(mlog3)
predict(mlog3,list(var1=100,var2=-50,var4=0,var5=55))
predict(mlog3,list(var1=99,var2=-50,var4=0,var5=55))

## ln(odds) = 20.73834 and 20.59318
## Someone with var1 = 100, var2=-50, var4=0, var5=55 will have a increase in 
## log-odds of being in class B by 20.73834

## Someone with var1 = 99, var2=-50, var4=0, var5=55 will have a increase in 
## log-odds of being in class B by 20.59318
## The person with the higher var1 is predicted to be more likely to be in Class B.


## compare to m2, linear model
m2<-lm(isclassb~var1+var2+var4, data=wk8dat)
summary(m2)
predict(m2,list(var1=100,var2=-50,var4=0,var5=55))



##??? PRACTICE!
##	Generate a dummy variable "isclassa" where the variable equals 0 if the observation is not "Class A" and 1 if the observation 
##	is "Class A". Create a logistic regression model using var1-var5 to predict the odds of Class A. Is there anything you notice
##	about your results compared to our in-class model, m1?





##??? Try logistic regression on your own dataset -- do your results reflect what you expected?
##	Another practice option -- Generate a dummy variable for Old/Young (cat2)
##	Try to predict the likelihood of being old versus young using var1-var6
m3 <- lm(isclassa~var1+var2+var4+var5, data = wk8dat)
summary(m3)

mlog2 <- glm(isclassa~var1+var2+var4+var5, data = wk8dat, family = binomial())
summary(mlog2)

## If you have 3 or more categories, logistic regression is only for two groups
## Decide on a split	(0 for Groups A, B, C) versus (1 for Groups D and E)



########## Two Sample T-Test ##########
## (one binary categorical and one continuous)
## Dependent variable must be continuous
## Observations must be independent of one another
## Dependent variable must be approximately normally distributed (not strongly skewed)
## Dependent variable cannot have outliers
schdat <- read.csv("schooldata.csv", header = T)


##### Example 1

plot(density(schdat$pretest, na.rm=T))		## approximately normal
boxplot(schdat$pretest)					## no outliers
t.test(schdat$pretest~schdat$ontimegrad)		## ontimegrad is 0 if not on time; 1 if on time
t.test(studytime~ontimegrad, data = schdat)

t.test(schdat$studytime, schdat$ontimegrad)



## p-value is less than 0.05 (generally accepted cut-off for "Statistically significant")
## (depending on field, cut-off might be more lenient 0.1 or more strict 0.01)
## Because p-value is less than 0.05, 
## There is a statistically significant difference in pretest score between
## the student who graduate within 4 years and the students who do not.
## On average, students who graduate on time had a pretest score of about 68.59.
## Students who did not graduate on time had an average pretest score of about 59.77


## Logistic regression version
montime1 <- glm(ontimegrad~pretest, data = schdat, family = binomial())
summary(montime1)
## higher pretest predicts higher odds of graduating on time

## Logistic Regression versus T-test
## T-test is simpler, so easier to interpret
## Logistic Regression can take into account multiple independent variables at the same time
## Ex: glm(ontimegrad~pretest + hsrating + mcollege + finaid + gender + studytime, data = schdat, family = binomial())



##### Example 2

plot(density(schdat$studytime, na.rm=T))			## approximately normal
boxplot(schdat$studytime)							## no outliers
t.test(schdat$studytime~schdat$ontimegrad)
## Is there a difference between students who graduate on time and students
## who do not graduate on time in terms of their study time?
##	
## p-value is not less than 0.05, so we do not have evidence that there is a
## statistically significant difference in study time during week 4 between
## the students who graduate within 4 years and the students who do not

## Logistic regression version
montime2 <- glm(ontimegrad~studytime, data = schdat, family = binomial())
summary(montime2)


########## Two Sample Proportion Test ##########
## a pair of binary variables 	(two categorical)
## (1) gender -- female or not female
## (2) ontimegrad -- on time graduation or not on time


table(schdat$gender,schdat$ontimegrad)
prop.test(x = c(38,18), n = c(50,50))
##	x = c(38,18)		38 females graduated on time, 18 males graduated on time
##					x is successes (on time graduation is our "success")
##  	n = c(50,50)		50 females total, 50 males total
##					n is the totals
## p-value is less than 0.05, so have strong evidence that there is a
## statistically significant difference between males and females in
## 4-year graduation rates
## 0.76 (76%) of females graduated on time
## 0.36 (36%) of males graduated on time


## It's okay to define x in the other direction
## x is successes (staying for more than 4 year is the "success")
prop.test(x = c(12,32), n = c(50,50))

## Notice the same p-value
## 0.24 (24%) of females did not graduate on time
## 0.64 (64%) of males did not graduate on time


## Logistic regression version
montime3 <- glm(ontimegrad~gender, data = schdat, family = binomial())
summary(montime3)



mall <- glm(ontimegrad~gender+finaid+mcollege+studytime+hsrating+pretest, data = schdat, family = binomial())
summary(mall)




########## Maps ##########
## install.packages(c("sf","raster","spData","spDataLarge","tmap"))

## data us_states came from spData
tm_shape(us_states) + tm_fill() 
tm_shape(us_states) + tm_borders() 
tm_shape(us_states) + tm_fill() + tm_borders()
tm_shape(us_states) + tm_polygons()
tm_shape(world) + tm_fill() 
tm_shape(world) + tm_polygons()
tm_shape(world) + tm_borders() 
## tm_polygons() == tm_fill()  + tm_borders() 


head(USArrests)
dim(USArrests)	## There's only 4 columns
## The states are actually just row names -- they are not columns or "data"

us_states


## We want to combine the us_states data with USArrests
## Create a new dataset with USArrests row names as a data point
## compare USArrests with USArrests2 

USArrests2 <- cbind(USArrests, NAME = row.names(USArrests))
## call the new column "NAME" to match the us_states data


us_states2 <- merge(us_states, USArrests2, by="NAME")
## Now the two datasets are combined and matched. We can graph this

tm_shape(us_states2) + tm_polygons() +
  tm_symbols(col = "black", border.col = "red", size = "Murder")

tm_shape(us_states2) + tm_polygons() +
  tm_symbols(col = "black", border.col = "red", size = "Murder", scale = 2)

tm_shape(us_states2) + tm_polygons() +
  tm_symbols(col = "Assault", border.col = "red", size = "Murder", scale=2)


##??? PRACTICE!
## Generate a visualization of 2018 GDP for each country
## Use "world" from the spData library and merge it with world_gdp2018.csv



########## Clustering: k-Means ##########
help(kmeans)





##### From the HELP function
require(graphics)

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)




##### New part, added to help code #####
x <- cbind(x, cluster = cl$cluster)



##### Seeded example -- to get the exact same results and to put numbers to our example #####
## setting it so that all of our computers get the same random numbers
## R works on a pseudo-random number generator
## Setting the seed allows us to replicate our results while using numbers that "seem" random
set.seed(811)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)

## Interpreting our results
## If there's two groups in our data, this iteration of kmeans clustering finds one group with
## lower x and y averages (Group 1, average x = 0.03 and average y = -0.02, 49 observations)
## than the other group with higher x and y averages(Group 2, average x = 0.95 and average y = 1.06, 51 observations)
## One group with low x, low y
## One group with high x, high y




##### Let's work with our dataset
(kmeans_2 <- kmeans(wk8dat[,c("var1","var2")], 2))
plot(wk8dat[,c("var1","var2")], col = kmeans_2$cluster)
points(kmeans_2$centers, col = 1:2, pch = 8, cex = 5)

(kmeans_3 <- kmeans(wk8dat[,c("var1","var2")], 3))
plot(wk8dat[,c("var1","var2")], col = kmeans_3$cluster)
points(kmeans_3$centers, col = 1:3, pch = 8, cex = 5)
## Group 1: var1 is middle (0), var2 is low
## Group 2: var1 is high, var2 is high
## Group 3: var1 is low, var2 is high

(kfull_3 <- kmeans(wk8dat[,c("var1","var2","var3","var4","var5")], 3))
plot(wk8dat[,c("var3","var4")], col = kfull_3$cluster)
points(kfull_3$centers[,3:4], col = 1:3, pch = 8, cex = 5)
wk8dat$kfullgroup <- kmeans_3$cluster		## Store the cluster groups into the dataset

summary(wk8dat[,c("var1","var2","var3","var4","var5")])
kfull_3$centers
## compare to summary(wk8dat), where are the centers landing compared to your data?
## var3 is driving the split



##??? PRACTICE!
##	Run k-means clustering using only var1 and var2 in the dataset (we just did this). Store the clusters groups either with the
##	dataset or as a vector. Run the same likes of code another two times, each time storing the cluster groups as a new column
##	in the dataset or as a new vector. Compare you groups each time you ran k-means. Did you get the exact same cluster groups
##	and center?




##### Iris dataset
set.seed(900)
(k3iris<-kmeans(iris[,1:4],3))

## Group 1: Sepal.Length is close to the mean/median	(Versicolor with some mis-categorized Virginicas)
##		Sepal.Width is close to the min/Q1
##		Petal.Length is close to the median
##		Petal.Width is close to the median
## Group 2: Sepal.Length is close to the Q3/max		(Virginica with some mis-categorized Versiccolors)
##		Sepal.Width is close to the mean/median
##		Petal.Length is close to the Q3/max
##		Petal.Width is close to the Q3/max
## Group 3: Sepal.Length is close to the min/Q1		(Setosa)
## 		Sepal.Width is close to the Q3/max
##		Petal.Length is close to the min/Q1
##		Petal.Width is close to the min/Q1

## grouping = naming the new column's name
iris2 <- cbind(iris, grouping = k3iris$cluster)
table(iris2$Species, iris2$grouping)


plot(Sepal.Length~Sepal.Width,data=iris, col=k3iris$cluster)
plot(Petal.Length~Petal.Width,data=iris, col=k3iris$cluster)

scatterplot3d(iris$Sepal.Length, iris$Petal.Length, iris$Petal.Width, color = k3iris$cluster, type = "h", lwd = 2, box = F, cex.axis = 0.5, main = "Thick Height Bars and No Box")



########## Clustering: Hierarchical ##########
help(hclust)


hc <- hclust(dist(USArrests), "ave")
plot(hc)
plot(hc, hang = -1)
(cut1 <- cutree(hc, k = 4))


hc <- hclust(dist(USArrests)^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
## colMean == calculate the average of the specified column means
## for loop is the same as the following
## cent <- rbind(cent, colMeans(USArrests[memb == 1, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 2, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 3, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 4, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 5, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 6, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 7, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 8, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 9, , drop = FALSE]))
## cent <- rbind(cent, colMeans(USArrests[memb == 10, , drop = FALSE]))

hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)






##### dataset example #####
wk8dat<-read.csv("randomdat2.csv", header = T)
h1<-hclust(dist(wk8dat[,c("var1","var2")]),"cen")
plot(h1, hang = -1)
h1cut2<-cutree(h1,k=2)
plot(wk8dat[,c("var1","var2")], col = h1cut2)
h1<-hclust(dist(wk8dat[,c("var1","var2")]),"average")
plot(h1, hang = -1)
h1cut3<-cutree(h1,k=3)
plot(wk8dat[,c("var1","var2")], col = h1cut3)




##### Iris Dataset #####
iriss<-hclust(dist(iris[,1:4]), method = "single")
irisc<-hclust(dist(iris[,1:4]), method = "complete")
irisa<-hclust(dist(iris[,1:4]), method = "ave")
opar <- par(mfrow = c(1, 3))
plot(iriss)
plot(irisc)
plot(irisa)

## Compare these to what we know about the dataset
## single-linkage probably not a good idea for the iris dataset


##### What happens if same distance #####
samedis<-seq(1,10)
par(mfrow = c(1,3))
hc <- hclust(dist(samedis), "complete")
plot(hc, main = 'complete')
hs <- hclust(dist(samedis), "single")
plot(hs, main = 'single')
ha <- hclust(dist(samedis), "ave")
plot(ha, main = 'ave')


par(mfrow = c(1,1))

########## Other Clustering Methods ##########
library(cluster)
??cluster  ## diana, pam, mona, agnes






########## 3D Plots ##########
## We'll need the scatterplot3d library
wk8dat<-read.csv("randomdat2.csv", header = T)
scatterplot3d(wk8dat$var1, wk8dat$var2, wk8dat$var3)


scatterplot3d(wk8dat$pred1, wk8dat$pred3, wk8dat$resp2)

scaled_resp2<-wk8dat$resp2/1000	## Get resp2 in thousands
par(mfrow = c(2,2))				## split my screen into 2x2
## cex.axis changes the axis font size
scatterplot3d(wk8dat$pred1, wk8dat$pred3, scaled_resp2, main = "Default Plot")
scatterplot3d(wk8dat$pred1, wk8dat$pred3, scaled_resp2, highlight.3d = T, cex.axis = 0.5, main = "With Highlighting")
scatterplot3d(wk8dat$pred1, wk8dat$pred3, scaled_resp2, highlight.3d = T, type = "h", cex.axis = 0.5, main = "With Highlighting and Height")
scatterplot3d(wk8dat$pred1, wk8dat$pred3, scaled_resp2, highlight.3d = T, type = "h", lwd = 2, box = F, cex.axis = 0.5, main = "Thick Height Bars and No Box")





par(mfrow = c(1,1))
model3d<-scatterplot3d(wk8dat$pred1, wk8dat$pred3, wk8dat$resp2,
	pch = 16, highlight.3d = T, type = "h", box = F)
## model3d$plane(____)		##	fill in blank with regression model




## The "car" and "rgl" libraries have a similar 3d plotting function,
## with the added bonus that you are able to rotate your graph
## You need both libraries.
library(car)
library(rgl)
scatter3d(wk8dat$pred1, wk8dat$pred3, wk8dat$resp2, revolutions = 2)
scatter3d(wk8dat$pred1, wk8dat$pred3, wk8dat$resp2)
