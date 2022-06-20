#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 2
##
## Agenda:
## 1. Review: Reading in Files
## 2. Data Cleaning: Factors
## 3. Boxplots and Fancier Boxplots
## 4. Stem-and-Leaf Plots
## 5. Histograms
## 6. Density Plots
## 7. Bar Plots
#################################################


##### Reading in .csv files (review) #####
## Read in the "temphr.csv" dataset
## Data came from https://www2.stetson.edu/~jrasp/data.htm
## taken from the Journal of Statistics Education online data archive.
## Data includes (1) body temp in degrees Fahrenheit
## (2) Gender - 1 = female, 2 = male
## (3) Heart rate, in beats per minute

temphrdata <- read.csv("temphr.csv",header=T)



## Suppose you are reading in a text file "mydata.txt" where the data is not separated by commas (maybe ; instead)
## dataset1 <- read.table("mydata.txt", sep = ";")


head(temphrdata)
names(temphrdata)
dim(temphrdata)		## row   column
summary(temphrdata)
table(temphrdata$gender)	## data$var
str(temphrdata)



##### Boxplots #####
summary(temphrdata)
## 25% of your data falls between Min and 1st Quartile
## 25% between 1st Quartile and Median
## 25% between Median and 3rd Quartile
## 25% 3rd Quartile and Max

boxplot(temphrdata$temperature, main = "Temperature", ylab = "Farenheit")
boxplot(temphrdata$temperature ~ temphrdata$gender, col = "blue",
	main = "Temperature by Gender", cex = 2, ylab = "Temperature (F)", xlab = "Gender")

## You can find a good list of colors here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf





##### Some Data Manipulation (Factors) #####
## This will generate a new column named 'genderlvls' and keep the old 'gender' column as is
temphrdata$genderlvls <- factor(temphrdata$gender, levels = c(1,2), labels = c("female","male"))
summary(temphrdata)

## IF you want to just replace "gender" with the labelled version, you can use the following line of code
## temphrdata$gender <- factor(temphrdata$gender,levels = c(1,2),labels = c("female","male"))

## You aren't limited to just two groups
## temphrdata$gender <- factor(temphrdata$gender,levels = c(1,2,3, 4, 7),labels = c("female","male", "A", "B", "C"))
## 1 is "female"
## 2 is "male"
## 3 is "A"
## 4 is "B"
## 7 is "c"


boxplot(temphrdata$temperature ~ temphrdata$genderlvls, col = "blue",
	main = "Temperature by Gender", cex = 2, ylab = "Temperature (F)", xlab = "Gender")


## SUPPOSE you received a new observation. The temperature of the person is 98.1*F
## The person is male and has a heart rate of 74 bpm
nrow(temphrdata)			## There are 130 rows currently in the dataset; new person = row 131
temphrdata[131,1] <- 98.1			## row 131, column 1, store 98.1*F
temphrdata[131,2] <- 2				## row 131, column 2, store 2 for male
temphrdata$heartrate[131] <- 74		## Alternative way of accessing  the data; 131st row of the "heartrate" column
temphrdata$genderlvls[131] <- "male"	
tail(temphrdata)					## check our work



## nrow() tells us how many rows we have
nrow(temphrdata)



## THE FOLLOWING CODE HAS AN ERROR
## SUPPOSE you received a new observation. The temperature of the person is 97.8*F
## The person is female and has a heart rate of 78 bpm
temphrdata[nrow(temphrdata)+1,1] <- 97.8
temphrdata[nrow(temphrdata)+1,2] <- 1				##R stored 1 into row 132 + 1 (a new row)
temphrdata$heartrate[nrow(temphrdata)+1] <- 78


## What happened?
tail(temphrdata)

temphrdata <- temphrdata[1:131,]				## just 1 to 131, and all of the columns
## temphrdata <- temphrdata[-(132:133),]		## or you can type this, remove 132 and 133
											## but be careful with -__, because -nothing means -everything
tail(temphrdata)





## Let's try again
n <- nrow(temphrdata)+1			## n is now always 132
temphrdata[n,1] <- 97.8
temphrdata[n,2] <- 1
temphrdata[n,3] <- 78
temphrdata[n,4] <- "female"		## Note, "Female" won't work


## Because column 4 has ONLY 2 factors/categories ("male" and "female"),
## it will not take anything else
str(temphrdata)		## By default, strings are factors




## as.character() will make things a string/character again
temphrdata$genderlvls <- as.character(temphrdata$genderlvls)
str(temphrdata)
temphrdata[n,4] <- "Female"		## Now that column 4 is a string, "Female" will work
summary(temphrdata)				## But you lose some proporties of factors
table(temphrdata$genderlvls)



## When do you use factors? When do you use characters?
## Factors: 	groups/categories (male/female, freshman/sophomore/junior/senior, 
##					single/married/divorced/...)
## Characters: open-ended survey responses (write 3 sentences about the service you received)





## Some functions in R will not treat a variable the same way
## if it were a character vs. factor.
## It's better to turn it back into a factor if that's what it should be
## String/Character would be useful for things like open-ended survey responses
temphrdata[,4] <- as.factor(temphrdata[,4])
summary(temphrdata)





## Let's get rid of observation 132
temphrdata <- temphrdata[-132,]
summary(temphrdata)		## Note, the "Female" factor is still here





## "Female" is still a category, so it still shows up
boxplot(temphrdata$temperature~temphrdata$genderlvls, col = "blue",
	main = "Temperature by Gender", cex = 0.4)





## to get rid of "Female" factor, covert to character and then back to factor
temphrdata[,4] <- as.character(temphrdata[,4])
	## If you need to make any additional changes, make the changes while it's a character (due to higher flexibility)
temphrdata[,4] <- as.factor(temphrdata[,4])




summary(temphrdata)	
boxplot(temphrdata$temperature~temphrdata$genderlvls, col = "blue",
	main = "Temperature by Gender", cex = 0.4, xlab = "Gender", ylab = "Temperature")




##### Summary of Working with Factors #####
## If I try to add a new category, and the data type is factor, I can't directly add it
temphrdata[132,4] <- "Refused"
tail(temphrdata)
str(temphrdata)


## In order to add a new category, one option is to convert to character first,
## add the new category, convert back to factor
temphrdata$genderlvls <- as.character(temphrdata$genderlvls)
str(temphrdata)
temphrdata[132,4] <- "Refused"
tail(temphrdata)
temphrdata$genderlvls <- as.factor(temphrdata$genderlvls)
str(temphrdata)
summary(temphrdata)


## To delete a category, change to character, 
## modify the category names, convert back to factor
temphrdata$genderlvls <- as.character(temphrdata$genderlvls)
temphrdata[132,4] <- "female"
temphrdata$genderlvls <- as.factor(temphrdata$genderlvls)
str(temphrdata)




##### Side Note: Switching Columns and Rows#####
temphrdata <- temphrdata[,c(1,2,4,3)]		## this gives me columns in order of col 1, 2, 4, 3
temphrdata <- temphrdata[c(2,1,3:132),]		## this swaps rows 1 and 2, but keeps 3:132
temphrdata <- temphrdata[order(temphrdata$temperature),]		##this sorts the rows in order of temperature
temphrdata <- temphrdata[order(temphrdata$temperature, decreasing = T),]		##this sorts the rows in decreasing order of temperature
temphrdata <- temphrdata[seq(132,1,-1),]	## reverse rows without using order()






##### Side Note: Limiting data to just one gender #####
temphrmales <- temphrdata[temphrdata$genderlvls == "male", ]		## Taking only the rows when genderlvls is male, keep all columns
dim(temphrmales)
summary(temphrmales)		## Note here, that "female" is still a possible category under genderlvls
remove(temphrmales, n)





## Delete person 132
temphrdata <- temphrdata[-132,]







##### Generating a new variable #####
heartratelvl <- c()		##first create a new vector

## in brackets, we're referring to element number
## less than 70 is low
## 70 to less than 80 is middle
## 80 or more is high
temphrdata$heartrate < 70								## Checking for each person if hr is less than 70
heartratelvl[temphrdata$heartrate < 70] <- "low"
heartratelvl[temphrdata$heartrate >= 70 & temphrdata$heartrate < 80] <- "middle"
	## note: heartratelvl[70 <= temphrdata$heartrate < 80] will not work
heartratelvl[temphrdata$heartrate >= 80] <- "high"
## note, this is not part of the dataset yet





## to combine it with the dataset, use the following...
## cbind(original data set, new vector)
temphrdata <- cbind(temphrdata,heartratelvl)
head(temphrdata)
tail(temphrdata)
remove(heartratelvl) 	## this removes the standalone vector heartratelvl,
				## but not the heartratelvl variable in the dataset



## Note these are out of order... (high low middle)
summary(temphrdata)
boxplot(temphrdata$temperature~temphrdata$heartratelvl, col = "blue",
	main = "Temperature by Heart Rate Level", cex = 0.4)



## The following line will guarantee that heartratelvl will display in
## low-middle-high order
temphrdata$heartratelvl <- factor(temphrdata$heartratelvl,
	levels = c("low", "middle","high"))
summary(temphrdata)
boxplot(temphrdata$temperature~temphrdata$heartratelvl, col = "blue",
	main = "Temperature by Heart Rate Level", cex = 0.4)




##??? PRACTICE!
##	Starting a new with "temphr.csv", add the following individuals to your dataset
##	(a) Add a female(1) with temperature 97.5 and heart rate 70
##	(b) Add a male(2) with heart rate 66 and unknown temperature
##	(c) Add a "other" gender (new category) with HR73 & temp 98.2
##	    For (c), the "gender" column can be either NA or 3

temphrdata <- read.csv("temphr.csv",header=T)
temphrdata$genderlvls <- factor(temphrdata$gender,levels = c(1,2),
	labels = c("female","male"))

n <- nrow(temphrdata) + 1
temphrdata[n,1] <- 97.5
temphrdata[n,2] <- 1
temphrdata[n,3] <- 70
temphrdata[n,4] <- "female"


n <- n+1		##Now, n is 132
temphrdata[n,1] <- NA	##it's okay if you skip this line
temphrdata[n,2] <- 2
temphrdata[n,3] <- 66
temphrdata[n,4] <- "male"


n<-n+1		## Onto row 133
temphrdata$genderlvls <- as.character(temphrdata$genderlvls)
temphrdata[n,4] <- "other"
temphrdata[n,3] <- 73
temphrdata[n,2] <- 3
temphrdata[n,1] <-98.2
temphrdata$genderlvls <- as.factor(temphrdata$genderlvls)

remove(n)




## possible shortcut for adding new rows
##	newobs <- data.frame(temperature = __, gender = __, h ....)
##	rbind(olddata, newobs)
##	Be careful not to use: c(97.5, 2, 70, "female") because we have mixed data types


newobs <- data.frame(temperature = 98.1, gender = 1, heartrate = 77, genderlvls = "female")
temphrdata <- rbind(temphrdata, newobs)


newobs3 <- data.frame(temperature = c(98.0, 98.3, 97.6), gender = c(1,1,2), heartrate = c(75, 76, 78), genderlvls = c("female", "female", "male"))
temphrdata <- rbind(temphrdata, newobs3)



##??? PRACTICE!
##	Generate a new variable "templvl" such that it is
##	low if temp < 97.6, middle if>=97.6 and <98.9, and high otherwise
##	You can start with a brand new "temphr.csv" dataset

temphrdata <- read.csv("temphr.csv",header=T)


## Create gender as factor (in groups instead of 1, 2) -- replace gender completely
temphrdata$gender <- factor(temphrdata$gender,levels = c(1,2),labels = c("female","male"))
heartratelvl <- c()
heartratelvl[temphrdata$heartrate<70]<-"low"
heartratelvl[temphrdata$heartrate>=70 & temphrdata$heartrate<80]<-"middle"
heartratelvl[temphrdata$heartrate>=80]<-"high"
templvl <- c()
templvl[temphrdata$temperature < 97.6] <- "low"
templvl[temphrdata$temperature>=97.6 & temphrdata$temperature<98.9] <- "middle"
templvl[temphrdata$temperature >= 98.9]<- "high"

temphrdata <- cbind(temphrdata,heartratelvl,templvl)

remove(heartratelvl,templvl)			## remove the extra copies, since we already have a copy in out dataset


summary(temphrdata)	## Note here that heartratelvl and templvl is in high-low-middle order


## re-order high-low-middle  into low-middle-high
temphrdata$heartratelvl <- factor(temphrdata$heartratelvl, levels = c("low", "middle","high"))
temphrdata$templvl <- factor(temphrdata$templvl, levels = c("low", "middle","high"))


summary(temphrdata)



## To create a numeric variable
## We're going to create a variable tempnum, where tempnum is 0 if templvl is "low"
## tempnum is 1 if templvl is "middle", and tempnum is 2 if templvl is "high"
tempnum <- c()
tempnum[temphrdata$templvl == "low"] <- 0
tempnum[temphrdata$templvl == "middle"] <- 1
tempnum[temphrdata$templvl == "high"] <- 2
temphrdata<-cbind(temphrdata,tempnum)
remove(tempnum)


## Let's say this person is low risk if heartrate < 70
## Let's say this person is high risk if heartrate > 80 and temperature > 98 (male)
## Let's say this person is high risk if heartrate > 80 and temperature > 98.5 (female)
## Let's say this person is middle risk for all others
risk <- c()
risk[temphrdata$heartrate < 70] <- "low"
risk[temphrdata$heartrate > 80 & 
	temphrdata$temperature > 98 & temphrdata$gender == "male"] <- "high"
risk[temphrdata$heartrate > 80 & 
	temphrdata$temperature > 98.5 & temphrdata$gender == "female"] <- "high"
risk[is.na(risk)] <- "middle"			## is.na will give us T/F (is the observation NA?)
temphrdata <- cbind(temphrdata, risk)
remove(risk)



## Another option for the "high" group, but not recommended since the code is getting long
## risk[(temphrdata$heartrate > 80 & 
##	temphrdata$temperature > 98 & temphrdata$gender == "male") | (temphrdata$heartrate > 80 & 
##	temphrdata$temperature > 98.5 & temphrdata$gender == "female")] <- "high"

## the vertical line "|" means "or"
## the "&" means "and"






##### Boxplots (again) #####
## Starting from the top and replacing the "gender" variable (3 columns only)
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
######### RESTART WITH A NEW DATA SET FOLLOWING THE LINES BELOW #############
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
temphrdata <- read.csv("temphr.csv",header=T)
temphrdata$gender <- factor(temphrdata$gender,levels = c(1,2),labels = c("female","male"))
heartratelvl <- c()
heartratelvl[temphrdata$heartrate<70]<-"low"
heartratelvl[temphrdata$heartrate>=70 & temphrdata$heartrate<80]<-"middle"
heartratelvl[temphrdata$heartrate>=80]<-"high"
templvl <- c()
templvl[temphrdata$temperature<97.6]<-"low"
templvl[temphrdata$temperature>=97.6 & temphrdata$heartrate<98.9]<-"middle"
templvl[temphrdata$temperature>=98.9]<-"high"
temphrdata <-cbind(temphrdata,heartratelvl,templvl)
remove(heartratelvl,templvl)
temphrdata$heartratelvl <- factor(temphrdata$heartratelvl, levels = c("low", "middle","high"))
temphrdata$templvl <- factor(temphrdata$templvl, levels = c("low", "middle","high"))
summary(temphrdata)





## use jpeg() to have your code save your graph automatically
jpeg("tempbygender.jpg", width = 400, height = 400)		## opening file
boxplot(temphrdata$temperature~temphrdata$gender,		## saving plot
	main = "Temperature by Gender", cex = 0.4,
	xlab = "Gender", ylab = "Temperature")
dev.off()	## REMEMBER TO CLOSE THE FILE AFTER YOU'RE DONE





## use pdf() to have your code save your graph automatically
pdf("tempbygender.pdf", width = 400, height = 400)
## note that this following boxplot will not pop up (it is being saved instead)
boxplot(temphrdata$temperature~temphrdata$gender,
	main = "Temperature by Gender", cex = 0.4,
	xlab = "Gender", ylab = "Temperature")
dev.off()	## REMEMBER TO CLOSE THE FILE AFTER YOU'RE DONE






## note that this boxplot will open a new window, because you're not saving it
boxplot(temphrdata$temperature ~ temphrdata$gender,
	main = "Temperature by Gender", cex = 0.4,
	xlab = "Gender", ylab = "Temperature")





## Boxplots only give min, 1st quartile, median, 3rd quartile, max
## Add means to your boxplots
meantempf <- mean(temphrdata$temperature[temphrdata$gender=="female"])
meantempm <- mean(temphrdata$temperature[temphrdata$gender=="male"])
means <- c(meantempf,meantempm)

## points() only ADDs to an existing graph
boxplot(temphrdata$temperature~temphrdata$gender,
	main = "Temperature by Gender", cex = 1.5,
	xlab = "Gender", ylab = "Temperature")
points(means, col="darkgrey", pch=22, lwd=5)
points(c(100,99), col = "red", pch = 23, lwd = 5)		## adding arbitrary points



##### Outliers #####
## Interquartile Range (IQR) is Q3-Q1
## Potential Outliers:
##	Less than Q1 - 1.5*IQR
##	More than Q3 + 1.5*IQR

## Q1 = 97.8
## Q3 = 98.7
## IQR = Q3 - Q1 = 0.9
## Potential Outliers
##	Less than 97.8 - 1.5(0.9) = 96.45
##	More than 98.7 + 1.5(0.9) = 100.05
boxplot(temphrdata$temperature)
text(100.8, labels = "              100.8")		## adding a text label to the 100.8 outlier


table(temphrdata$temperature > 100.05 | temphrdata$temperature < 96.45)

which(temphrdata$temperature > 100.05 | temphrdata$temperature < 96.45)
## which() will tell you the observation numbers that are satisfy the criteria

temphrdata[which(temphrdata$temperature > 100.05 | temphrdata$temperature < 96.45), ]

boxplot(temphrdata$temperature)
text(100.8, labels = "              100.8")
text(96.3, labels = "              96.3")
text(96.4, labels = "              96.4")



##### Fancier Boxplots #####
## We will need bwplot (box and whiskers) and 
## panel.violin from the lattice library
##	You only need to install once on a given computer	
##
##	In R:	"packages" at top > "Install package(s)" 
##	> select the location closest to you > select "lattice"

##	In RStudio: 
##	By click:	"tools" at top > "install packages"
##	> search for "lattice" and be sure to install dependencies

##	Alternatively, you can type this line of code:
## 	install.packages("lattice")



## every time you want to use this package in R (on a new session), you have to type:
library(lattice)


## Boxplot separated by x1 (gender) and x2 (low/middle/high heartrate)
bwplot(temperature ~ gender, data = temphrdata, xlab = "Gender", ylab = "Temperature")

## this is also okay:
## bwplot(temphrdata$temperature ~ temphrdata$gender, xlab = "Gender", ylab = "Temperature")


bwplot(temperature ~ gender | heartratelvl, data = temphrdata,layout = c(3,1),
        xlab = "Gender", ylab = "Temperature")
bwplot(temperature ~ gender | heartratelvl + templvl, data = temphrdata,layout = c(3,3),
        xlab = "Gender", ylab = "Temperature")



## Violin plot using "panel = panel.violin"
bwplot(temperature ~ gender, data = temphrdata, panel = panel.violin,
       xlab = "Gender", ylab = "Temperature")
bwplot(temperature ~ gender | heartratelvl, data = temphrdata,layout = c(3,1),
        xlab = "Gender", ylab = "Temperature", panel = panel.violin)


## try swapping things around to see what happens
bwplot(gender ~ temperature | heartratelvl, data = temphrdata,layout = c(1,3),
        xlab = "Temperature", ylab = "Gender", panel = panel.violin)
bwplot(temperature ~ heartratelvl| gender, data = temphrdata,layout = c(2,1),
        xlab = "Heart Rate Level", ylab = "Temperature", panel = panel.violin)

## Let's change the y-axis range
bwplot(temperature~gender, data = temphrdata, panel = panel.violin,
       xlab = "Gender", ylab = "Temperature", ylim = c(95,102))
bwplot(temperature~gender | heartratelvl, data = temphrdata, panel = panel.violin,
       xlab = "Gender", ylab = "Temperature",layout = c(3,1), ylim = c(95,102), main = "Temperature by Gender, HR Level Panels")




##### Stem-and-Leaf Plots #####
## install.packages("multcomp")		## you only need to run this once per computer
library(multcomp)

## R actually comes with some built-in datasets
## Libraries also come with some additional datasets
## In this case, the dataset "sbp" is already loaded

head(sbp)
?sbp

stem(sbp$sbp)		#sbp variable in the sbp dataset (they just both happen to have the same name)
## one data point at 110, one at 114, one at 116,
## two data points at 120, three at 124,...
## Not many people between 110-119, not many people between 180-189
## Quite a few people between 150-159


stem(sbp$sbp, scale=2)
		## Instead of the original 110-119, 120-129 groups... 
		## our groups are now 110-114, 115-119, 120-124, 125-129
		## More bins, and smaller groups
stem(sbp$sbp, scale=0.5)
		##	Now, our groups are 100-119, 120-139, 140-159, ...


stem(temphrdata$temperature)
stem(temphrdata$temperature, scale = 0.5)

stem(temphrdata$heartrate)
stem(temphrdata$heartrate, scale = 2)




##### Histogram #####
hist(sbp$sbp)	## This lets R decide the bin sizes (where to split the boxes)
				## 5 people from 111-120
				## 7 people from 121-130
				##	...
hist(sbp$sbp, breaks = c(109,119,129,139,149,159,169,179,189))	## Exactly where you want breaks (to match stem and leaf plot)
hist(sbp$sbp, breaks = c(110,135,160,185))			## Exactly where you want breaks

hist(sbp$sbp, breaks = c(110, 140, 145, 150, 152, 155, 160,185))	## They don't need to be the same size
											## But highly NOT recommended (misleading)
hist(sbp$sbp, breaks = 30)					## How many breaks (bins-1)
hist(sbp$sbp, breaks = seq(110,190,15))		## First, last (before this number), increment of breaks

seq(110,190,15)



## Note, you need to make sure your breaks covers beyond the range of the data
## The following will give an error, because the breaks does not cover the full range of the data
## Make sure the smallest break is smaller than the min
## Make sure the largest break is larger than the max
## >> hist(sbp$sbp, breaks = seq(120,180,15))		## the min is smaller than 120, the max is larger than 180, so this will not work


hist(sbp$sbp, label = T, col = c("red","blue"), 
	main = "Systolic Blood Pressure", xlab = "Systolic Blood Pressure")
hist(sbp$sbp, label = T, col = c("red","skyblue"), ylim = c(0,15),
	main = "Systolic Blood Pressure", xlab = "Systolic Blood Pressure")


## Overlapping histograms
?rgb		## rgb(red, green, blue, transparency)


## In the following, I have SBP of females only
hist(sbp$sbp[sbp$gender == "female"], col=rgb(1,0,0,0.25),
	ylim = c(0,15),main = "Systolic Blood Pressure",
	xlab = "Systolic Blood Pressure", xlim = c(100,200))


## the following ***** "add = T" ***** adds this new graph to what we already have
## No need to add labels or set-up axes here, because it was already done in the previous line
hist(sbp$sbp[sbp$gender == "male"], col=rgb(0,0,1,0.25),add=T)



legend("topright", c("Female", "Male"), col=c(rgb(1,0,0,0.25),rgb(0,0,1,0.25)), lwd=20)

## Key things:
## 1) set up the window in the first graph (make sure you have your xlim and ylim correct)
##		make sure you have your labels
## 2) for adding graphs, make sure you type "add = T" for the second and beyond graph
## 3) make sure your breaks are the same for both graphs; 
##		different breaks means possible misleading graph

## Bad example #1
## NOTE, whatever settings you have for your first graph will be the graph settings
hist(sbp$sbp[sbp$gender == "female"], col=rgb(1,0,0,0.25),
	ylim = c(0,7),main = "Systolic Blood Pressure",
	xlab = "Systolic Blood Pressure", xlim = c(100,180))
hist(sbp$sbp[sbp$gender == "male"], col=rgb(0,0,1,0.25),add=T)	## Notice that this hist gets cut off
## to fix this, use summary(sbp) to make sure you cover the min to max using xlim and ylim arguments; you might have to experiment for ylim


## Bad example #2
hist(sbp$sbp[sbp$gender == "female"], col=rgb(1,0,0,0.25))
hist(sbp$sbp[sbp$gender == "male"], col=rgb(0,0,1,0.25),add=T)


## Good example
hist(sbp$sbp[sbp$gender == "female"], col=rgb(1,0,0,0.25),
	ylim = c(0,10),main = "Systolic Blood Pressure",				## adjust ylim so that male hist does not get cut off
	xlab = "Systolic Blood Pressure", xlim = c(100,200))
hist(sbp$sbp[sbp$gender == "male"], col=rgb(0,0,1,0.25),add=T)
legend("topright", c("Female", "Male"), col=c(rgb(1,0,0,0.25),rgb(0,0,1,0.25)), lwd=10)



## Bad example #3 (different bin sizes) -- MISLEADING
hist(sbp$sbp[sbp$gender == "female"], col=rgb(1,0,0,0.25),
	ylim = c(0,10),main = "Systolic Blood Pressure",				## adjust ylim so that male hist does not get cut off
	xlab = "Systolic Blood Pressure", xlim = c(100,200), breaks = 20)
hist(sbp$sbp[sbp$gender == "male"], col=rgb(0,0,1,0.25),add=T) 
## 	ESPECIALLY BAD because the graph is misleading; the female group looks smaller than the male group
##	Between 140 and 150, there are 5 + 1 = 6 females total
##	Between 140 and 150, there are 6 males total
## 	Both groups have 6 people, but the male histogram looks wider and taller because of the inconsistent breaks
##	DO NOT DO THIS!


## Correction for bad example #3 -- specify exactly where the breaks should be
hist(sbp$sbp[sbp$gender == "female"], col=rgb(1,0,0,0.25),
	ylim = c(0,10),main = "Systolic Blood Pressure",				## adjust ylim so that male hist does not get cut off
	xlab = "Systolic Blood Pressure", xlim = c(100,200), breaks = seq(110,190,5))
hist(sbp$sbp[sbp$gender == "male"], col=rgb(0,0,1,0.25),add=T, breaks = seq(110,190,5)) 





##??? PRACTICE!
##	(a) Generate a violin plot for heart rate based on gender
##	(b) Generate a violin plot for heart rate based on gender separated by
##		temperature level (panel)



bwplot(heartrate~gender, data = temphrdata, panel = panel.violin,
       xlab = "Gender", ylab = "Heart Rate", ylim = c(40,100))
bwplot(heartrate~gender | templvl, data = temphrdata, panel = panel.violin,
       xlab = "Gender", ylab = "Heart Rate",layout = c(3,1), ylim = c(40,100))


## Yes, it's possible to create additional panels based on more levels
bwplot(heartrate~gender | templvl + heartratelvl, data = temphrdata, panel = panel.violin,
       xlab = "Gender", ylab = "Heart Rate",layout = c(3,3), ylim = c(40,100))




##??? PRACTICE!
##	Generate a stem-and-leaf plot for heart rate from the temphr dataset
##	Try changing scale = __ to see if you can get a better visual of the data



##??? PRACTICE!
##	Using the original "temphr.csv", generate histograms for
##	(a) temperature using the default bin sizes
##	(b) heart rate using at least 12 bins
##	(c) temperature for males and females (overlapping)
##	(d) heart rate for males and females (overlapping)


temphrdata<-read.csv("temphr.csv",header=T)
temphrdata$gender <- factor(temphrdata$gender,levels = c(1,2),labels = c("female","male"))

## (a)
hist(temphrdata$temperature, main = "Histogram of Temperature", xlab = "Temperatures in F",)

## (b)
hist(temphrdata$heartrate, breaks = 13)

## (c)
hist(temphrdata$temperature[temphrdata$gender == "female"],
	col=rgb(1,0,0,0.25), ylim = c(0,22), breaks = seq(95,102,0.5))
## the following "add = T" adds this new graph to what we already have
hist(temphrdata$temperature[temphrdata$gender == "male"], 
	breaks = seq(95,102,0.5), col=rgb(0,0,1,0.25),add=T)
legend("topright", c("Female", "Male"), col=c(rgb(1,0,0,0.25),rgb(0,0,1,0.25)), lwd=10)




##### Density Plot #####
## a smoother version of histograms
## IMPORTANT! area under density plot equals 1.0 (or 100%)
dens1 <- density(sbp$sbp)
plot(dens1, xlim = c(100,190),main = "Systolic Blood Pressure Density Plot")
dens1


dens2 <- density(sbp$sbp, bw = 4)		##bw, narrower bandwidth have more detail
dens3 <- density(sbp$sbp, bw = 10)	##bw, wider bandwidths are more general, but it might be too general
plot(dens2, xlim = c(100,190),main = "Systolic Blood Pressure Density Plot")
plot(dens3, xlim = c(100,190),main = "Systolic Blood Pressure Density Plot")

## the problem with too much detail (bw = 1)
plot(density(sbp$sbp, bw = 1), xlim = c(100,190),main = "Systolic Blood Pressure Density Plot")

## the problem with too little detail (bw = 100)
plot(density(sbp$sbp, bw = 100), xlim = c(100,190),main = "Systolic Blood Pressure Density Plot")



## IMPORTANT: use freq = F when combining hist and density
## Without freq = F, the y-axis in histograms will give us the count numbers


## Overlaying density on top of histograms
## In the following line, freq = F makes it so that the 
## area of the histogram rectangles adds up to one
## (to represent 100% of your data)
## las = 1 rotates the numbers on the y-axis

hist(sbp$sbp, main = "Systolic Blood Pressure Histogram with Density",
	cex.axis = 0.8, freq = F, xlab = "Systolic Blood Pressure")
lines(dens1, lwd = 2)		## lines does the same thing as plot, but "adds" to current graph
lines(dens2, lwd = 2, col = "red", lty = 3)
lines(dens3, lwd = 2, col = "blue", lty = 2)



## BAD EXAMPLE
## Note, without freq = F, we have a problem! (notice the scale of the y-axis)
hist(sbp$sbp, main = "Systolic Blood Pressure Histogram with Density",
	cex.axis = 0.8, xlab = "Systolic Blood Pressure")
lines(dens1, lwd = 2)			## density and frequency are on different scales
lines(dens2, lwd = 2, col = "red", lty = 3)
lines(dens3, lwd = 2, col = "blue", lty = 2)

## Frequency (in this example) goes from 0 to 14	(counts of "how many")
## Density (in this example) goes from 0 to 0.02	("area" representative of the percentage of data that falls in the rectangle)


hist(sbp$sbp, main = "Systolic Blood Pressure Histogram with Density",
	cex.axis = 0.8, freq = F, xlab = "Systolic Blood Pressure", labels = T)



##### Bar plots #####
## Like histograms, but for categorical variables
## We'll need the salaries.csv dataset
## NOTE: there are two types of barplots...
##		table() to get counts
##		aggregate() to get averages (mean, median) or min, max, sd,...


salaries <- read.csv("salaries.csv", header=T)
ranktab <- table(salaries$rank)	## table with counts of each category
ranktab
barplot(ranktab, ylab = "Count", col = "red", main = "Faculty by Rank")



## the following will give us the mean salary by rank
## you can use other functions like FUN = min, FUN = max, FUN = sd, FUN = median
avgsal <-aggregate(salary ~ rank, data=salaries, FUN = mean)
avgsal



############# barplot(___<<numbers>>____, ylab = "Avg Salary", names.arg = ____<<labels>>____,
#############	 col = "red", main = "Faculty Salaries")
barplot(avgsal$salary, ylab = "Avg Salary", names.arg = avgsal$rank,
	col = "red", main = "Faculty Salaries")

## re-scaling salary so its in thousands (easier to read)
avgsal$salaryK <- avgsal$salary/1000
barplot(avgsal$salaryK, ylab = "Avg Salary (in thousands)", names.arg = avgsal$rank,
	col = "red", main = "Faculty Salaries")


## See them side-by-side
par(mfrow=c(1,2))		##one row, two columns of GRAPHS!
barplot(ranktab, ylab = "Count", col = "red", main = "Faculty by Rank")
barplot(avgsal$salaryK, ylab = "Avg Salary (in thousands)", names.arg = avgsal$rank,
	col = "red", main = "Faculty Salaries")


par(mfrow = c(1,1))	## reset to 1 row, 1 column of graphs


## Faculty by Gender
rankgen <- table(salaries$rank,salaries$sex)
rankgen



barplot(rankgen,ylab = "Count", names.arg = c("Female","Male"),
	main = "Faculty by Rank and Sex",col = c("red","black","blue"))
legend("topleft",c("Prof","Asst","Assoc"),text.col=c("blue","black","red"))

barplot(rankgen,ylab = "Count", names.arg = c("Female","Male"),
	main = "Faculty by Rank and Sex",col = c("red","black","blue"), beside=T)
legend("topleft",c("Prof","Asst","Assoc"),text.col=c("blue","black","red"))




## in the following, you can either use t() or table(salaries$sex,salaries$rank)
## to swap the columns and rows of rankgen
t(rankgen)		##gets the transpose




barplot(t(rankgen),ylab = "Count", names.arg = c("Assoc","Asst","Prof"),
	main = "Faculty by Rank and Sex",col = c("red","blue"),beside=T)
legend("topleft",c("Female","Male"),text.col=c("red","blue"))


## to avoid re-typing colors/labels, you can store them
sexcol <- c("red","blue")
sexlab <- c("Female","Male")
barplot(t(rankgen),ylab = "Count", names.arg = c("Assoc","Asst","Prof"),
	main = "Faculty by Rank and Sex",col = sexcol,beside=T)
legend("topleft",sexlab,text.col=sexcol)



##??? PRACTICE!
##	From the temphr.csv dataset, generate a histogram with an 
##	overlaid density plot for (a) temperatures, and (b) heart rate



##??? PRACTICE!
##	The following three lines give you the (1) avesrage salary based on
##	sex and rank, (2-3) the barplot for salary based on sex and rank
##	(a) Add the argument ..., space = 1.5, ... to the barplot. What happens?
##	(b) What if you change it to space = c(1,0,1,0,1,0)?
##	(c) What happens when you add ..., horiz = T, ... ?




dens1 <- density(temphrdata$temperature)
hist(temphrdata$temperature, main = "Body Temperature",
	cex.axis = 0.8, freq = F, xlab = "Temperature (F)")
lines(dens1, lwd = 2)



plot(dens1)
grpsal <- aggregate(salary ~ sex * rank, data = salaries, FUN = mean)	#(1)
barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
	names.arg = c("Assoc","","Asst","","Prof",""), col = c("red","blue"))
barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
	names.arg = c("Assoc","","Asst","","Prof",""), space = 1.5, col = c("red","blue"))
barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
	names.arg = c("Assoc","","Asst","","Prof",""), space = c(1,0,1,0,1,0), col = c("red","blue"))
barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
	names.arg = c("Assoc","","Asst","","Prof",""), horiz=T, col = c("red","blue"))