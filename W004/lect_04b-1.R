#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 4
## 1. Changing factors to numeric variables
## 2. Generating Dummy/Indicator Variables
## 3. Transforming Variables
## 4. Scatter Plots and Regression (2 Examples)
## 5. Other Scatter Plot Specials
## 6. Scatter Plot Transformations
## 7. QQ Plots (and Z Scores)
#################################################


## Good practice to load all of your libraries at the beginning
## install.packages("readxl")
## install.packages("car")
## install.packages("hexbin")

library(readxl)
library(car)
library(hexbin)


avggrade <- c("A","A","C","D","F","B","B","A","D","C")
major <- c("Math","CS","Math","CS","CS","Stats","Math","CS","Math","Stats")
units <- c(48, 32, 50, 42, 48, 26, 40, 36, 40, 52)
courses <- c("4", "8", "12", "unknown", "12", "6", "10", "9", "10", "13")

samp <- data.frame(major, units, courses, avggrade)
remove(avggrade, major, units, courses)

## RStudio Users -- replace the above 6 lines of code with:
samp <- read.csv("samp.csv", header = T)







########## Changing factors to numeric variables ##########
##### Convert major to numeric
num_major <- as.numeric(samp$major)
levels(samp$major)	## based on this, CS = 1, Math = 2, Stats = 3


##### Convert courses to numeric
num_courses <- as.numeric(samp$courses)
levels(samp$courses)	## "10" is going to be 1, "12" = 2, "13" =3



## To get the actual numbers, convert to characters first
## Need to go from factor -> character -> number in order to convert
## factors to numbers
num_courses <- as.numeric(as.character(samp$courses))



## By default, A = 1, B = 2, C = 3, D = 4, F = 5
##### Avggrade to numeric (based on 4.0 scale)
num_avggrade_temp <- factor(samp$avggrade, levels = c("A", "B", "C", "D", "F"), label = 4:0)
## A becomes 4, B becomes 3, C becomes 2, D becomes 1, and F becomes 0

## same as:
## factor(samp$avggrade, levels = c("A", "B", "C", "D", "F"), label = c(4,3,2,1,0))
num_avggrade <- as.numeric(as.character(num_avggrade_temp))



## Another example: Strongly disagree (0), Disagree (1), Somewhat disagree (2), Neutral (3), ... 
## OR: Strongly Disagree (1), Disagree (2), Agree (3), Strongly Agree (4)
## newvar <- factor(oldvar, levels = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"), label = 1:4)
## newvar <- as.numeric(as.character(newvar))



########## Generating Dummy/Indicator Variables (0-1 Variable) ##########
##### Variable equals 1 if Math major, 0 if not
## Is this person a math major or not (No = 0, Yes = 1)
mathmaj <- samp$major == "Math"		## Reminder, == means check if this is equal, = means MAKE this equal to
str(mathmaj)
mathmaj <- as.numeric(mathmaj)			## T will become 1, False will become 0


factor(mathmaj, levels = 0:1, labels = c("Not Math Major", "Math Major"))		## Note, I didn't store this anywhere
## Levels, what we have
## labels, what we want to call it





########## Transforming Variables ##########
## Linear transformation (new_x = m * old_x + b) will keep the same distribution shape
## Non-linear transformations (example: new_x = sqrt(old_x) ) will change the distribution shape

## Common non-linear transformations include exp(), log(), sqrt(),or power (usually squared or cubed)
newdat <- read.table("rand1.txt", sep = "\t", header = T)		## sep = ";", sep = ".", sep = ","
head(newdat)
summary(newdat)
		## 4.000e+00 	means 4 * 10^0	 = 4
		## 3.877e+11 means 3.877 * 10^11 = 387,700,000,000
		## 1.166e+22   this is the same as 1.166 * 10^22 (scientific notation)


options(scipen=999)	## gets rid of scientific notation
## options(digits = ___) to get decimal places


boxplot(newdat$var1)
boxplot(newdat$var1, ylim = c(0, 963865843746000068348820642244250) ) 	## zoom in from 0 to Q3


hist(newdat$var1)		## high concentration of points in the left, a far off right tail (right skew)
hist(newdat$var1, breaks = 10000)

## If you have a lot of data on the left side and some data (very little) on the right side,
## then the data is right skewed. (Mean is much higher than the median)
## If you have a LEFT SKEWED data (stretched out tail is on the left side), then the mean is less than the median


boxplot(log(newdat$var1))			## I transformed my var1 using log()	(logarithms in math)
								## The higher your number, the more it gets scaled back
								## ** Good for right skewed data
boxplot(log(newdat$var1), ylab = "Log of var1")			## e^y = x
													## log(e^y) = log(x)
													## y = log(x)
													## by default, the base is e
hist(log(newdat$var1))	

## Some examples of where you might see log applied to data: Economics, Healthcare



boxplot(newdat$var2)		## this might be okay to leave as is!
hist(newdat$var2)

boxplot(log(newdat$var2))		## probably not a good idea
hist(log(newdat$var2))
boxplot(sqrt(newdat$var2))		## rule of thumb: keep it simple, or go with prior literature
hist(sqrt(newdat$var2))		##		or use your best judgement
							##		* you have to be able to justify why you performed a certain transformation

## Let's compare original vs. log() vs. sqrt()
par(mfrow = c(1,3))
boxplot(newdat$var2, main = "Original")
boxplot(log(newdat$var2), main = "Log Transform")
boxplot(sqrt(newdat$var2), main = "Sqrt Transform")
## In original, long tail in the higher values (right skew)
## In log transform, long tail in the lower values (left skew)
## In sqrt transform, approximately evenly balanced on left and right (symmetric)



boxplot(newdat$var6)
hist(newdat$var6)
boxplot(log(newdat$var6))
hist(log(newdat$var6))


## squared values
## 1 -> 1
## 2 -> 4
## 3 -> 9
## 10 -> 100		## all numbers are gaining more space as we get into higher-valued numbers

## 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
## sqrt() will scale the "HIGH" numbers back 1, 2, 3, 4, 5, 6, 7, 8, 9, 10


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


## ifelse(if this is true , then do this, otherwise this)
poverty <- cbind(poverty, gnpcat = ifelse(poverty$gnp < 1690, "Low GNP", "High GNP"))





##### Overall Scatter Plot (just numeric variables) #####
plot(poverty[,1:7])		## columns 1-7, numeric variables
					## this only works if all variables are in the same data set
					## RStudio -- this may not work if your graphing screen is too small


########## Scatter Plots ##########
## The basic form follows	>> plot(xvariable, yvariable, arguments...)
## Typically want x to be the independent (explanatory) variable - "Which comes first?"
## y should be the dependent (response) variable - "What is predicted/affected by x?"
plot(poverty$birthrt, poverty$deathrt)








## Main labels, text labels, and gridlines
plot(poverty$birthrt, poverty$deathrt, main = "Birth and Death Rates Scatterplot", 
	pch = 17, col = "red", xlab = "Birth Rate", ylab = "Death Rate")
text(25,20, labels = "Label centered at (25,20)", adj=0.5)
text(24, 6, labels = "Albania")
## adj = 0 is left/bottom aligned, 1 is right/top aligned, and 0.5 is centered
grid(col = "gray70")


plot(poverty$lexpf, poverty$infmort, main = "Infant Mortality and Female Life Expectancy", 
	xlab = "Female Life Expectancy", ylab = "Infant Mortality", pch = 17, col = "red")
grid(col = "blue")


## Let's plot by region
## * Use the first plot() to set-up axes and labels
## * Use points(x,y) to ADD to an existing plot
plot(poverty$lexpf[poverty$region == 1], poverty$infmort[poverty$region == 1],
	main = "Infant Mortality and Female Life Expectancy", 
	xlim = c(38,84), ylim = c(0,200), xlab = "Female Life Expectancy", 
	ylab = "Infant Mortality", pch = 17, col = "red")
	## MAKE SURE xlim and ylim cover the full data (otherwise, other points might get cut-off later)
points(poverty$lexpf[poverty$region == 2], poverty$infmort[poverty$region == 2],
	pch = 18, col = "green")
points(poverty$lexpf[poverty$region == 3], poverty$infmort[poverty$region == 3],
	pch = 19, col = "blue")
points(poverty$lexpf[poverty$region == 4], poverty$infmort[poverty$region == 4],
	pch = 20, col = "gray50")
points(poverty$lexpf[poverty$region == 5], poverty$infmort[poverty$region == 5],
	pch = 21, col = "black")
points(poverty$lexpf[poverty$region == 6], poverty$infmort[poverty$region == 6],
	pch = 15, col = "purple")
legend("topright", c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "Region 6"), 
	col = c("red", "green", "blue", "gray50", "black", "purple"),pch = c(17:21,15))




plot(poverty$lexpm, poverty$lexpf, main = "Life Expectancy Scatterplot", 
	xlab = "Life Expectancy of Males (Years)",
	ylab = "Life Expectancy of Females (Years)",
	pch = 17, col = "red")
grid(col = "gray70")


##??? PRACTICE: Graph male life expectancy vs. female life expectancy by region
plot(poverty$lexpm[poverty$region == 1], poverty$lexpf[poverty$region == 1],
	xlim = c(45, 80), ylim = c(40,80))
points(poverty$lexpm[poverty$region == 2], poverty$lexpf[poverty$region == 2],
	pch = 18, col = "green")


########## Linear Regression ##########
## The basic form follows	>> lm(yvar ~ xvar)
## ALERT: LM(Y~X) IS SWITCHED FROM PLOT(X, Y) 
model0 <- lm(lexpf~lexpm, data=poverty)
model0
summary(model0)

## Remember from math? y = b + mx
## 
## Based on results, the regression equation looks like
## lexpf_predicted = -2.937 + 1.124*lexpm
## INTERPRETATION:
## For each additional year of male life expectancy in a given country,
## female life expectancy is associated with an increase by 1.124 years.
## For a country with male life expectancy equal to 0, female life expectancy
## is about -2.94 years. (Of course, this part doesn't make sense in practice).



## AN EXAMPLE:
## Set lexpm = 50,	lexpf = -2.937 + 1.124*50
##				lexpf = 53.263
## In a country where male life expectancy is about 50 years,
## female life expectancy is about 53.263 years
## Say lexpm = 51 (increased by 1)
##					lexpf = -2.937 + 1.124*51
##					lexpf = 54.387				<-increased by 1.124

## If lexpm = 0, then lexpf = -2.937 + 1.124*0
##					lexpf = -2.937	Doesn't make sense, can't have negative years!


########## Scatter Plots with Regression Line ##########
## Use plot as usual for the scatter portion
## Add the regression line using:	>> abline(__lm variable__)
plot(poverty$lexpm, poverty$lexpf, main = "Life Expectancy Scatterplot", 
	xlab = "Life Expectancy of Males (Years)",
	ylab = "Life Expectancy of Females (Years)",
	pch = 17, col = "red")
grid(col = "gray70")
abline(model0, col = "black", lty = 1, lwd = 2)
	## note, 	lty is line type (solid, dotted, dashed)
	##		lwd is line thickness (width)
text(44,45,labels = "Y = -2.937 + 1.124X", adj = 0)



########## Linear Regression (Example 2) ##########
## The basic form follows	>> lm(yvar~xvar)
model1 <- lm(infmort~lexpf, data=poverty)
model1
summary(model1)
##
## Equation: predicted_infmort = 317.383 - 3.969 * lexpf
##
## Interpretation: as female life expectancy increases by 
## 1 unit infant mortality decreases on average by 3.97 units. For a country with 0 years 
## female life expectancy, we predict infant mortality to be about 317.38. 
## (Be careful with how far you apply this regression 
## model; make sure it's within the scope of the dataset; 
## in this case, the intercept interpretation doesn't make sense)

## Be careful with extrapolating; lexpf is never close to 0 (the minimum is 41.20)
## Interpreting for a country with lexpf = 0 is beyond the scope of our dataset (if the country even exists)

plot(poverty$lexpf, poverty$infmort, xlim = c(0,90), ylim = c(0,320))
abline(model1)



## Based on results, the regression equation looks like
## infmort = 317.383 - 3.969*lexpf
## INTERPRETATION:
## For each additional year of female life expectancy in a given country,
## infmort is associated with an decrease by 3.969 infant deaths per 1000 
## (that's what the infmort variable means--deaths per 1000),
## For a country with female life expectancy equal to 0, infant mortality
## is about 317.383 deaths per 1000. (Again, this part doesn't make sense in practice).
## AN EXAMPLE:
## Set lexpf = 70,	infmort = 317.383 - 3.969*70
##				infmort = 39.553
## In a country where female life expectancy is about 70 years,
## infant mortality is about 39.553 infant deaths per 1000



## For multivariate (more than 2 variables)
model2 <- lm(infmort ~ lexpf + birthrt, data=poverty)
model2
summary(model2)
##
## Equation: predicted_infmort = 312.135 - 3.913 * lexpf + 0.051 * birthrt
##
## Keeping birth rate constant, for each additional year of female life expectancy in a given country,
## infmort is associated with a decrease by 3.913 infant deaths per 1000.
## Keeping female life expectancy constant, for each 1 unit increase in birthrate,
## infmort is associated with an increase by 0.051 infant deaths per 1000.
## If female life expectancy is 0 and birth rate is 0, then we predict infant deaths per 1000
## to be 312.135. (Again, this doesn't make sense because our dataset does not have such a country or anything similar)




########## Scatter Plots with Regression Line (Example 2) ##########
## Use plot as usual for the scatter portion
## Add the regression line using:	>> abline(__lm variable__)
## abline adds lines to the existing graph
## To add vertical and horizontal lines, you can also use...
## >> abline(v = __)
## >> abline(h = __)
plot(poverty$lexpf, poverty$infmort, main = "Infant Mortality and Female Life Expectancy", 
	xlab = "Female Life Expectancy", ylab = "Infant Mortality", pch = 17, col = "red")
abline(model1, col = "black", lty = 1, lwd = 2)


## Vertical and horizontal line examples
## Let's draw vertical and horizontal lines at the mean of each variable
abline(v = mean(poverty$lexpf), col = "blue", lty = 2, lwd = 3)		## vertical at x = 66.03
abline(h = mean(poverty$infmort), col = "blue", lty = 3, lwd = 1.5)		## horizontal at y = 55.28	


## Note here, that the regression line always crosses at (mean-x, mean-y)
## In this case, (mean lexpf, mean infmort) = (66.03, 55.28)



########## Linear Regression Diagnostic Plots ##########
## How do you know if you have a good model?
## What happens when you plot(model1)?
plot(model1)

## R - press Enter or click through 4 graphs
## RStudio - left/right arrow in top left corner of graphing window

## Residuals vs. Fitted (aka Residual Plot)
##	- want a straight horizontal line
##	- no patterns in the points (no curves, no fanning)
## Normal Q-Q
##	- want points to follow the dotted line
##	- if it doesn't, we have some skewness or heavy tails in the residuals
##	- important assumption of linear regression is normality
## Scale-Location (checking for consistent variance)
##	- Similar to Residuals vs. Fitted (we essentially folded the graph in half)
##	- want straight line, no patterns
## Residuals vs. Leverage
## 	- Any influential points?
##	- You want to be careful of influential points because your model should be about
##	  the same in the presence/absence of each point

modelx <- lm(poverty$deathrt ~ poverty$birthrt)
plot(poverty$birthrt, poverty$deathrt)
abline(modelx)
plot(modelx)




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


## You can also use plot for 1 variable only
plot(poverty$lexpm)		## Left-to-right in order of your dataset

## This will give you the same thing as plot()
## except in two steps (axes first, then points)
plot(poverty$lexpm, type = "n")
lines(poverty$lexpm, col = "red", type = "p")	## adding points to the graph


plot(poverty$lexpm, type = "l")		## this doesn't quite make sense for this dataset
	
								## connecting obs. 1 to obs 2 to 3 to 4...

## However, some types aren't very useful out of order
plot(poverty$lexpm, poverty$lexpf, type = "l")


## This next line of code (I)
plot(poverty$lexpm[order(poverty$lexpm,poverty$lexpf)], 
	poverty$lexpf[order(poverty$lexpm,poverty$lexpf)], type = "l")

## is the same as (II-III)...
poverty_orderlexpm <- poverty[order(poverty$lexpm,poverty$lexpf),]
plot(poverty_orderlexpm$lexpm, poverty_orderlexpm$lexpf, type = "l")


## In (I), we are plotting the two variables BOTH in order of lexpm followed by lexpf
## In (II-III), we are storing the data in order of lexpm followed by lexpf
## and the plotting the variables from the newly sorted dataset


plot(poverty_orderlexpm$lexpm, poverty_orderlexpm$lexpf, type = "b")
plot(poverty_orderlexpm$lexpm, poverty_orderlexpm$lexpf, type = "c")
plot(poverty_orderlexpm$lexpm, poverty_orderlexpm$lexpf, type = "o")
plot(poverty_orderlexpm$lexpm, poverty_orderlexpm$lexpf, type = "s")
plot(poverty_orderlexpm$lexpm, poverty_orderlexpm$lexpf, type = "n")


## For type = "h", it looks like histograms, 
## but be careful with the interpretations
## and your data form. For this example, we cannot assume the same
## interpretations as your typical histogram
plot(poverty$lexpm, poverty$lexpf, type = "h")	## order doesn't matter for this one


## The following two can be interpreted as a histogram because we are
## using count or percentage/density as our y-axis
## plot(xvariable, countvariable, type = "h")
## plot(xvariable, percentvariable, type = "h")


gpa <- c(4,3,2,1,0)
count<- c(40,35,20,5,3)
plot(gpa,count,type = "h")
plot(gpa,count,type = "h", lwd=70, xlim = c(-0.5,4.5),col=c("blue","red"))
lines(gpa,count,type = "p", col = "red")







########## Non-linear Plots  -- TRANSFORMATIONS ##########
plot(poverty$lexpf, poverty$birthrt)

## Use smooth.spline() to get a moving average of the graph
smooth <- smooth.spline(poverty$lexpf, poverty$birthrt, spar=1)
lines(smooth, col='red', lwd=2)


##### An example of a not-so-good linear regression model
## The basic form follows	>> lm(yvar~xvar)
model2 <- lm(birthrt~lexpf, data=poverty)
abline(model2)
summary(model2)
plot(model2)	## fanning in residual plot, non-horizonal line in 3rd plot


##### Try changing one of your variables slightly...
## Common transformations: log(), sqrt(), ^2, exp(), ^3, ... keep simple
plot(poverty$lexpf, sqrt(poverty$birthrt))	## not common to change y-variable
plot(sqrt(poverty$lexpf), poverty$birthrt)
plot(poverty$lexpf, log(poverty$birthrt))	## (see above)
plot(log(poverty$lexpf), poverty$birthrt)
plot(poverty$lexpf^2, poverty$birthrt)		## maybe this one
plot(poverty$lexpf, poverty$birthrt^2)		## maybe???

plot(poverty$lexpf^2, poverty$birthrt)
smooth <- smooth.spline(poverty$lexpf^2, poverty$birthrt, spar=1)
lines(smooth, col='red', lwd=2)


model3 <- lm(birthrt~I(lexpf^2), data=poverty)
summary(model3)
plot(model3)

#####
##	Originally it should have be 	y = b + mx
##	With transformation			y = b + m * x^2
## birthrate = 69.134 -0.00885*lexpf^2
## A country with lexpf = 50?
## birthrate = 69.134 -0.00885*(50)^2
## birthrate = 47.009
## 
## If lexpf^2 increase by 1, then birthrate decreases by 0.00885


## If lexpf = 0 (meaning lexpf^2 = 0), then birthrate = 69.134




########## QQ Plots ##########
##### First, a crash course on Z-scores
## Z-scores are "standardized units".  How many standard deviations from the mean?
## Positive Z-score, you are __ std deviations ABOVE the mean
## Negative Z-score, you are __ std deviations BELOW the mean
## Zero Z-score, you are right at the mean.



## Density plot of the normal distribution
normalpoints <- rnorm(1000000) ## a million random points drawn from the normal distribution
plot(density(normalpoints))
polygon(density(normalpoints), col = "red")		## if you want to color it in

## Z = (score - mean) / sd
## Like converting from inches to cm, we're converting from whatever we're given
## to a standardized scale (standard deviations) with mean = 0 and sd = 1





## For example, Test A is normally distributed mean score of 81 and standard deviation of 5
## You scored 88 on the test
## Z = (88 - 81)/5 = 1.4		## You are 1.4 standard deviations (1.4 * 5 points) above the mean
pnorm(1.4)					## Converting to %, you scored above 91.92% of other test takers
plot(density(normalpoints))
abline(v = 1.4, lty = 2, col = "red")
text(-4.5,0.4, labels = "100% under the curve", adj=0)
text(-1,0.1, labels = "91.92%\nbelow/left of\nyour score", adj=0)
pnorm(1.4, lower.tail = F)
text(2,0.1, labels = "8.08%\nabove your score", adj=0)
## You did better than 91.92% of test A takers



## Another example, Test B had a mean score of 81 and standard deviation of 8
## You scored 72 on the test
## Z = (72 - 81)/8 = -1.125		## You are 1.125 standard deviations below the mean	(1.125 * 8pts)
pnorm(-1.125)
plot(density(normalpoints))
abline(v = -1.125, lty = 2, col = "red")
text(-4.7,0.4, labels = "100% under the curve", adj=0)
text(-4.6,0.1, labels = "13.03%\nbelow your score", adj=0)
pnorm(-1.125, lower.tail = F)
text(-1,0.06, labels = "86.97%\nabove your score", adj=0)
## You did better than 13.03% of test B takers	-- pnorm(-1.125)
## You did worse than 86.97% of test B takers	-- pnorm(-1.125, lower.tail = F)


pnorm(72, mean = 81, sd = 8)		## you don't have to do the calculation by hand
pnorm(72, mean = 81, sd = 8, lower.tail = F)

plot(density(normalpoints))
abline(v = -1.96, lty = 3, col = "blue")
text(-4,0.3,labels = "Critical Value\n-1.96", adj = 0)
abline(v = 1.96, lty = 3, col = "blue")
text(3,0.3,labels = "Critical Value\n1.96", adj = 0)
## 95% of data is between -1.96 an 1.96 standard deviations
## In regression, when we talk about p-values, we're also talking about the 95% cut-off
## Anything beyond this means that the number is so far off, that it's unlikely to be by chance


##### PRACTICE!
##??? Test A had mean = 81, standard deviation = 5. On Test A, you scored 85.
##	Test B had mean = 81, standard deviation = 8. On Test B, you scored 87.
##	1. Which test did you do better on?
##	For each Test:
##	2. Draw the normal density plot and line showing where you landed.
##	3. Label on your plot how many students you did better/worse than

z_testa <- (85-81)/5
z_testb <- (87-81)/8



pnorm(85, mean = 81, sd =5)
plot(density(normalpoints))
abline(v = z_testa, lty = 2, col = "red")
text(-1.5,0.1, label="78.81%\ndid worse", adj =0)
text(2,0.1, label = "21.19% did better", adj = 0)


pnorm(87, mean = 81, sd =8)
plot(density(normalpoints))
abline(v = z_testb, lty = 2, col = "red")
text(-1.5,0.1, label="77.34%\ndid worse", adj =0)
text(2,0.1, label = "22.66% did better", adj = 0)



##	For Test A, what percentage of people scored between 75 and 85
z75 <- (75-81)/5
z85 <- (85-81)/5
plot(density(normalpoints))
abline(v = z75, lty = 2, col = "red")
abline(v = z85, lty = 2, col = "red")
## cut off the two tails from 100%
1 - pnorm(75, mean = 81, sd =5) - pnorm(85, mean = 81, sd =5, lower.tail = F)

text(-4.5,0.2, label="11.51%\nin lower tail", adj =0)
text(2,0.2, label = "21.19%\nin upper tail", adj = 0)
text(-1,0.2, label = "67.31%\nin middle", adj = 0)


##### Working backwards (Z-scores)
## pnorm -- score to percentile
## qnorm -- percentile to score

## What is the minimum score needed to be in the top 10%?
qnorm(0.9,mean = 81, sd = 5)


## Max score for bottom 30%?
qnorm(0.3,mean = 81, sd = 5)

## What are the scores for the middle 40%?
qnorm(0.3,mean = 81, sd = 5)
qnorm(0.7,mean = 81, sd = 5)






##### Comparing the density plot to our variable in question
## We will first need the standardize scores for our variable
## what it does for each data point : (number - mean )/sd
zinfmort <- scale(poverty$infmort)	## gets all values in z-scores
								## Z = (value - mean) / sd

## Row 1:
## infmort = 30.8			-->		Z = (30.8 - 55.28)/46.30 = -0.5287 (compare this to obs 1 of zinfmort)
## mean(infmort) is 55.28
## sd(infmort) is 46.30



## Density plot of the normal distribution
plot(density(normalpoints), ylim = c(0,0.5))
polygon(density(normalpoints), col = "red")		## if you want to color it in



## ADD the infmort density plot
lines(density(zinfmort), col = "blue", lty = 2, lwd = 2)

## We can see that infmort is more right skewed than the normal curve


##### QQ Plot
qqnorm(poverty$infmort, main = "QQ plot of infmort")
qqline(poverty$infmort, col = "red", lty = 2, lwd = 2)

hist(poverty$infmort)		## right skewed

## the actual percentages for this variable
qinfmort <- quantile(poverty$infmort, seq(0,1,0.005))

## In the case of regression, we aren't just interested in left/right skewed.
## We want the RESIDUALS to be as close to normal as possible (no heavy tails either)
## In regression diagnostics, we are talking about how the residuals compare to the normal distribution
## residuals should be approximately normal.