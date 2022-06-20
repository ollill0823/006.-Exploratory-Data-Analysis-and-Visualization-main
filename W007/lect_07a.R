#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 7
##
## Agenda:
## 1. Combining Data: rbind(), cbind(), merge
## 2. Subsetting Data
## 3. Reshaping Data: long and wide data
## 4. Revisiting aggregate()
## 5. Group Project - Overview and Meetings
##	-- Time to meet in breakout rooms this week and next week
## 6. Corrgrams
## 7. Data Example with Interactions
## 8. Logistic Regression
#################################################

## install.packages(___)
library(reshape2)
library(ggplot2)
library(corrplot)
library(GGally)
library(scatterplot3d)
library(car)
library(rgl)



########## Combining Data ##########
mydata <- data.frame(var1 = c(1,2,3,4), var2 = c(5,6,7,8))
mydata
newdata <- data.frame(var2 = c(11,12,13,14), var1 = c(15,16,17,18))	## note that I swapped my columns
newdata
bothdata <- rbind(mydata,newdata)	## rbind() will bind the data by rows
bothdata					## if it is a data frame, then it will match the variables
baddata <- cbind(mydata,newdata)	## cbind() will bind the data by columns 
baddata					## this might not be what we want because we now have two var1s and two var2s
						## OR it might be what we want...
names(mydata) <- c('var1_2019', 'var2_2019')
names(newdata) <- c('var2_2020', 'var1_2020')
widedata <- cbind(mydata,newdata)
						## you can also change the names one-by-one
						## names(mydata)[1] <- 'var1_2019'
						## names(mydata)[2] <- 'var2_2019'


## BE CAREFUL IF YOU USE CBIND!! Here's an example of cbind gone wrong...
mydata2 <- data.frame(id = c(111,112,113,114,115), var1_2019 = c(1,2,3,4,5), var2_2019=c(5,6,7,8,9))
newdata2 <- data.frame(id = c(113,115,111,112,114), var1_2020 = c(77,43,65,21,54), var2_2020=c(27,23,25,36,76))
baddata2 <- cbind(mydata2,newdata2)
baddata2

## use merge() instead
?merge





## Example at bottom of ?merge
authors <- data.frame(
    ## I(*) : use character columns of names to get sensible sort order
    surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    deceased = c("yes", rep("no", 4)))
authorN <- within(authors, { name <- surname; rm(surname) })
books <- data.frame(
    name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis",
              "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA,
                     "Venables & Smith"))





(m0 <- merge(authorN, books))
(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname"))


##### Keeping unmatched observations #####
## all = TRUE, all.x = all, all.y = all,		## both x and y are true
## all = FALSE, all.x = TRUE, all.y = all,		## x only is true (keep unmatched in x only)
## all = FALSE, all.x = all, all.y = TRUE,		## y only is true (keep unmatched in y only)

(m0 <- merge(authorN, books, all = T))
(m1 <- merge(authors, books, by.x = "surname", by.y = "name", all.x = T))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname", all.x = T))



## Back to merging mydata2 and newdata2
merge(mydata2,newdata2, by = "id")		## Note, we didn't save this data anywhere




## What about data without matches?
mydata3 <- data.frame(id = c(111,112,113,114,115,116), var1_2019 = c(1,2,3,4,5,6), var2_2019=c(5,6,7,8,9,10))
newdata3 <- data.frame(id = c(113,115,111,112,114,117), var1_2020 = c(77,43,65,21,54,44), var2_2020=c(27,23,25,36,76,33))
## note arguments (1) all, (2) all.x, (3) all.y means include unmatched observations for 
## (1) both datasets, (2) just the first dataset, and (3) just the second dataset
merge(mydata3,newdata3,by="id", all = T)		## Keep all even if not matched
merge(mydata3,newdata3,by="id", all.x = T)		## Keep unmatched in mydata3, but not newdata3
merge(mydata3,newdata3,by="id", all.y = T)		## Keep unmatched in newdata3, but not mydata3




## Be careful with duplicate IDs, you'll get every possible combination
## What about data without matches?
## by id is very important, becaue it can target to each repeated row and combine them
mydata4 <- data.frame(id = c(111,112,113,114,115,115), attempt = c(1, 1, 1, 1, 1, 2), var1_2019 = c(1,2,3,4,5,6), var2_2019=c(5,6,7,8,9,10))
newdata4 <- data.frame(id = c(113,115,111,112,114,115),attempt = c(1, 1, 1, 1, 1, 2), var1_2020 = c(77,43,65,21,54,44), var2_2020=c(27,23,25,36,76,33))
merge(mydata4,newdata4,by="id", all = T)

## You can merge by multiple variables
merge(mydata4,newdata4,all = T)
## merge(mydata4,newdata4, by = c("id", "var1"), all = T)



## Let's say we finally have a dataset that we like
## And we want to save it for future use
## Use write() or write.csv()
alldata <- merge(mydata3,newdata3,by="id", all = T)
write.csv(alldata,file = "superimportantdata.csv", row.names = F)


## Let's clean up our workspace
## Remove all of the variables we just created
rm(list=ls())






########## Subsetting Data ##########
## There are different scenarios where you might want to reduce your data
## One example might be that the data is too big/confusing
## Another example is that you want a training/testing dataset
####	Example: You might want to generate a model and see how far off your models are
reshapedata <- read.csv("reshapeexample.csv",header=T)


## Suppose we want just males and just scoreA/scoreB
reshapedata[which(reshapedata$gender == "M"), c("scoreA", "scoreB")]


## Another method (just preference)
subset(reshapedata, gender == "M", select = c(scoreA, scoreB))



## Suppose we want just males and columns id through scoreA
reshapedata[which(reshapedata$gender == "M"), 1:4]		##These 2 lines will give you the same thing
subset(reshapedata, gender == "M", select = id:scoreA)


## Suppose we want just males and just scoreA/scoreB in January
reshapedata[which(reshapedata$gender == "M" & reshapedata$month == "Jan"), c("scoreA", "scoreB")]
subset(reshapedata, gender == "M" & month == "Jan", select = c(scoreA, scoreB))


## Suppose we want just males and just scoreA/scoreB in January OR February
reshapedata[which(reshapedata$gender == "M" & (reshapedata$month == "Jan" | reshapedata$month == "Feb")), c("scoreA", "scoreB")]
subset(reshapedata, gender == "M" & (month == "Jan" | month == "Feb"), select = c(scoreA, scoreB))

## Suppose we want just males and just scoreA/scoreB in any month except March
reshapedata[which(reshapedata$gender == "M" & reshapedata$month != "Mar"), c("scoreA", "scoreB")]
subset(reshapedata, gender == "M" & month != "Mar", select = c(scoreA, scoreB))





########## Reshaping Data ##########

##### Reshaping to LONG format #####
## Want 75 rows; 1 row for each score
longdata <- melt(reshapedata, id=c("id","gender","month"))
## the same as ##
(longdata <- melt(reshapedata, 1:3))

names(longdata)[4:5] <- c("test","score")
levels(longdata$test) <- c("A","B","C")


## You can select which scores to keep (in this example, we have scoreA and scoreB only, no scoreC)
longdata2 <- melt(reshapedata, id=c("id","gender","month"), 
	measure = c("scoreA","scoreB"), variable.name = "test", value.name = "score")
levels(longdata2$test) <- c("A","B")




##### Reshaping to WIDE format #####
## Want 9 rows, all scores in one row per person
wideAdata<-dcast(reshapedata, id + gender ~ month, value.var = "scoreA")
wideBdata<-dcast(reshapedata, id + gender ~ month, value.var = "scoreB")
wideCdata<-dcast(reshapedata, id + gender ~ month, value.var = "scoreC")
(wideAdata<-wideAdata[,c(1,2,4,3,5)])		## Swapping Jan and Feb (note 3 and 4 have swapped)
(wideBdata<-wideBdata[,c(1,2,4,3,5)])
(wideCdata<-wideCdata[,c(1,2,4,3,5)])


wideABdata <-merge(wideAdata,wideBdata, by = c("id", "gender"))
wideABdata		## note, you'll need to rename variables -- or it'll get confusing quickly
				## recommend renaming before you merge
names(wideABdata)<-c("id","gender","JanA", "FebA","MarA","JanB","FebB","MarB")
## alternatively, you can use...
## wideABdata <-merge(wideAdata,wideBdata, by = c("id", "gender"), suffixes = c("A", "B"))


widedata <- merge(wideABdata,wideCdata, by = c("id", "gender"))
names(widedata)[9:11] <- c("JanC", "FebC", "MarC")


widedata[,c(1,2,3,6,9,4,7,10,5,8,11)]		## Again, we did not save this anywhere
								## Remember to type __ <- to store this



## Unfortunately, you can't have multiple values for value.var at once in dcast
## You can convert to long first
## Using longdata from above...
longdata
widedata2 <- dcast(longdata, id + gender ~ month + test, value.var = "score")
widedata2 <- widedata2[,c(1,2,6:8,3:5,9:11)]		## Swap some of my columns



########## Revisiting Aggregate ##########
## Suppose we want to combine the "reshapeexample.csv" and "mergeexample.csv" datasets
## We also just want the max test score for each test from Jan/Feb/Mar
mergedata <- read.csv("mergeexample.csv",header=T)

## use longdata from above

maxdata <- aggregate(score~gender+id+test, data=longdata, FUN = max)
maxdataw <- dcast(maxdata , id + gender ~ test, value.var = "score")
combodata <- merge(maxdataw, mergedata, by = "id")




## You can also try other tricks like combining the data first and then calculating after
## rowMeans() and rowSums() are two possible functions to use
## Of course, plan out what you want your data to look like before digging in
combodata$avgq <- rowMeans(combodata[,6:10])
combodata$sumq <- rowSums(combodata[,6:10])
combodata$acratio <- combodata$A / combodata$C

					## another example: data$patients/data$bed




########## Correlations, Corrgrams ##########
## Last time we mentioned that it's important to check correlations
## In regression, independent variables should be independent.
## If the degree of correlation between variables is high, you may run 
## into errors regarding fit and interpretability. The variance of your 
## coefficients will increase, making your coefficients unstable.



randomdat <- read.csv("randomdat.csv", header = T)

## rename "GroupC" as "Group C" under var8
levels(randomdat$var8)[levels(randomdat$var8)=="GroupC"] <- "Group C"

## remove observation 123 because var5 is an outlier
randomdat<-randomdat[-123,]

randomdat$var9[which(is.na(randomdat$var9))]<-"With XYZ"


cor(randomdat[,c(1:7,11)])


##### Other Possible Correlation Graphs
## We can use the "corrplot" library

## We can visually represent our correlations (remember cor() from earlier??)
cormat <- cor(randomdat[,c(1:7,11)])

corrplot(cormat) 	## by default you get circles
corrplot(cormat, method = "color")
corrplot(cormat, method = "number")
corrplot(cormat, method = "ellipse", type = "lower")


library(GGally)

## One other type of corrgram... -- this uses GGally library
ggscatmat(randomdat,columns = c(1:7,11))



########## Data Example with Interactions ##########
schdat <- read.csv("schooldata.csv", header = T)
head(schdat)
summary(schdat)

cor(schdat[,c(5:7)])
ggscatmat(schdat, columns = c(5:7))
## Need to be careful about including both hs rating and pretest (correlation = 0.610)

m1 <- lm(examscore~gender+finaid+mcollege+studytime+hsrating+pretest, data=schdat)
summary(m1)
## Interpretation:
## Males are predicted to have a lower examscore than females by about 0.86 points.
## However, this coefficient is not significant (p = 0.135). (Note, as a rule of thumb,
## we compare the p-value to 0.05 for significance; if the slope is should be a non-zero number)
## In this case, since it's not significant, we can say that males aren't really different from females (as good as 0 difference)
## 
## A student who receives financial aid is predicted to have a lower exam score than
## a similar student (same gender, same mcollege status, same study time, same HS rating,
## same pretest score) by about 5.44 points. This coefficient is statistically significant
## (p = 2.38e-16, which is less than 0.05).
##
## A student who studies for one extra hour (compared to a similar student with the same gender
## same finaid status, same mcollege status, same HS rating, and same pretest) is predicted
## to have a higher exam score by about 0.49 points. This coefficient is statistically
## significant (p =  2.58e-11, which is less than 0.05).
##
## A student who attended a HS with a 1 point higher rating (compared to a similar student
## with the same gender, same finaid status, same mcollege status, same study hours, and 
## same pretest) is predicted to have a higher exam score by about 0.19 points. This 
## coefficient is not statistically significant (p = 0.114, which is not less than 0.05), so
## the coefficient of 0.19 is possibly due to chance--we can say that it's practically 0 (instead of 0.19).



m2 <- lm(examscore~gender*studytime, data=schdat)
## examscore = 64.79 + 1.68*male + 0.51*studytime - 0.25*male*studytime

m3 <- lm(examscore~gender*pretest, data=schdat)
## examscore = 46.31 + 16.21*male + 0.46*pretest - 0.30*male*pretest
##
## Male (male = 1)
## examscore = 46.31 + 16.21(1) + 0.46*pretest - 0.30(1)(pretest)
## examscore = 62.52 + 0.16*pretest
##
## Female (male = 0)
## examscore = 46.31 + 16.21(0) + 0.46*pretest - 0.30(0)(pretest)
## examscore = 46.31 + 0.46*pretest
##
## There is a significant association between gender and examscore.
## Being male is associated with a higher examscore by 16.21 points compared to females.
## 
## There is a significant association between pretest score and examscore
## There is a differential association between pretest score and examscore based on gender
## For females, each additional one point on the pretest is associated with a 0.46 point higher score in the exam.
## For males, ... 0.16 point... 
## Steeper effect for females!
##
##
## Low-Performing Male (male = 1, pretest = 33)
## 62.52 + 0.16*(33) = 67.8
##
## Low-Performing Female (male = 0, pretest = 33)
## 46.31 + 0.46*(33) = 61.49
##
## High-Performing Male (male = 1, pretest = 97)
## 62.52 + 0.16*(97) = 78.04
##
## High-Performing Female (male = 0, pretest = 97)
## 46.31 + 0.46*(97) = 90.93
##
## Crossover effect (females cross males)
## At some value of pretest score, females do just as well as males (and surpass for higher values)



m4 <- lm(examscore~mcollege*hsrating, data=schdat)
## examscore = 66.45 + 2.70*mcollege + 0.92*hsrating + 0.59*mcollege*hsrating
##
## MotherCollege
## examscore = 66.45 + 2.70*(1) + 0.92*hsrating + 0.59*(1)*hsrating
## examscore = 69.15 + 1.51*hsrating
##
## MotherNoCollege
## examscore = 66.45 + 2.70*(0) + 0.92*hsrating + 0.59*(0)*hsrating
## examscore = 66.45 + 0.92*hsrating
##
## Compare:
## * Low HS Rating, and Mother went to college		(plug in hsrating = 1, mcollege = 1)
## * Low HS Rating, and Mother did not go to college	(plug in hsrating = 1, mcollege = 0)
## * High HS Rating, and Mother went to college		(plug in hsrating = 10, mcollege = 1)
## * High HS Rating, and Mother did not go to college(plug in hsrating = 10, mcollege = 0)
## 
## We see widening gap between students with mothers who went to college versus students
## with mothers who did not go to college. (Range of data is important! hs rating = 1 through 10)
##
## Note, Mother's college is not statistically significant, so we might not even want to consider it in our model
## Take it out?

m5 <- lm(examscore~finaid*studytime, data=schdat)

##???PRACTICE! Interpret m5




########## Logistic Regression ##########
## Logistic Regression is used when your dependent variable is 0-1
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
mlog1 <- glm(ontimegrad~gender+finaid+mcollege+studytime+hsrating+pretest, data = schdat, family = binomial())
summary(mlog1)


## Holding other background characteristics constant (same finaid status, same mcollege,
## same study time, same HS rating, same pretest), males are LESS likely to graduate on time
## compared to females.

## We see a positive coefficient (0.077) for studytime
## Holding other background characteristics constant (same finaid status, same mcollege,
## same gender, same HS rating, same pretest), students who study more are MORE likely
## to graduate on time compared to similar students who do not study as much



## Compare the results to linear regression
mlm1 <- lm(ontimegrad~gender+finaid+mcollege+studytime+hsrating+pretest, data = schdat)
summary(mlm1)

## Linear regression is easier to interpret...
## But what's the problem?
## Male who receives financial aid. Mom did not go to college.
## Studies for 15 hours
## HS Rating of 1
## Pretest of 33
predict(mlm1, list(gender = "male", finaid = "yes", mcollege = "no", studytime = 15, hsrating = 1, pretest = 33))

## Our prediction for ontime graduation is -0.1761... does that mean he's guaranteed to not graduate on time?
## Be careful with interpretation.

## Linear regression is easy to interpret, but it violates that probability can only be between 0 and 1
## Logistic regression keeps your predictions for probability between 0 and 1, but the interpretation is not as direct




mlog2 <- glm(ontimegrad~gender*studytime, data=schdat, family = binomial())


