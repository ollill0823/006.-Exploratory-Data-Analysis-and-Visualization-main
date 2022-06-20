#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 5
##
## Agenda:
## 1. Data Cleaning (NAs)
## 2. Regression with Transformation
##		predict()
## 3. Multivariable Regression	(one y, multiple x variables)
## 4. Interactions
## 5. High Density Plots
#################################################
library(car)

## no scientific notation
options(scipen = 999)


######### Read in the "randomdat.csv" dataset ##########
randomdat <- read.csv("randomdat.csv", header = T)


########## Data Cleaning ##########
## Check variables
str(randomdat)
summary(randomdat)	## Variables of Concern: var5 (high max), var8 (GroupC), var9 (3 NA's)
head(randomdat)

randomdat[is.na(randomdat$var9),] 


table(randomdat$var9,randomdat$var10)
table(randomdat$var8,randomdat$var9)
## Group A and Group B should be "With XYZ" (guess)
## Group C should be "Without XYZ" (guess)

## None of the Group A or Group B are "Without XYZ" -- rows 45, 88, and 134 look like they should be "With XYZ"
## It looks like NA var9's should be "With XYZ"
## Change the NA's to "With XYZ"
randomdat$var9[is.na(randomdat$var9)] <- "With XYZ"




## "Group A", "Group B", and "GroupC" inconsistent
## Change "GroupC" to "Group C"
## We can't do just the following:
## randomdat$var8[randomdat$var8=="GroupC"] <- "Group C"
## "Group C" is not a factor group for var8


## Quick fix, change to character first
randomdat$var8 <- as.character(randomdat$var8)
randomdat$var8[randomdat$var8=="GroupC"] <- "Group C"
randomdat$var8 <- as.factor(randomdat$var8)


## Another option is to use factor()


##### Let's get an overview
plot(randomdat[,c(1:7,11)])



boxplot(randomdat$var5)

## Keep only the non-outlier rows (delete the var5 row with 999)
randomdat <- randomdat[which(randomdat$var5 < 800),]


boxplot(randomdat$var6)
## The current maximum for variable 6 is concerning, but it appears to be part of a right skewed distribution
## because the points for the higher values are getting more spaced out
## Don't delete yet-- but if it's problematic for our data, we'll need to delete later and re-run the whole analysis


########## Task ##########
## We want to get a sense of how Variable 1 predicts
## (or relates to) Variable 2
plot(randomdat$var1,randomdat$var2)



## Positive correlation	-- the higher our var1, the higher our var2
## We know that our slope is positive




## Linear Regression, Y = b0 + b1*X
m1<-lm(var2~var1, data=randomdat)
abline(m1)
plot(m1)
## Note, even though #43 and 41 look like outliers here,
## we don't want to take these points out yet. It might 
## just be that we have a bad model


## Let's see what the relationship might actually be...
plot(randomdat$var1,randomdat$var2)
abline(m1)
smooth <- smooth.spline(randomdat$var1,randomdat$var2, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)







##### Try a transformation
## Typically, you'll want to do transformations of x (what you have control of)
## Instead of sqrt(y) = x... do y = x^2



## x^2
plot(randomdat$var1^2,randomdat$var2)
smooth2 <- smooth.spline(randomdat$var1^2,randomdat$var2, spar=1)
lines(smooth2, col='red', lwd = 2, lty = 2)


## log (Definitely NOT THIS)
plot(log(randomdat$var1), randomdat$var2)


## exp (even worse!)
plot(exp(randomdat$var1), randomdat$var2)










##### Our best line might be to work with x^2
##### Quadratic Regression
## For practice purposes, I will go with var1^2
## create a variable in the randomdat dataset called "var1sq"
randomdat$var1sq <- randomdat$var1^2


## var1sq is now a part of our dataset
names(randomdat)
summary(randomdat)


m2<-lm(var2~var1sq, data=randomdat)
plot(randomdat$var1sq,randomdat$var2)
abline(m2)
plot(m2)		## not perfect, but better than before







m2_no43 <- lm(var2~var1sq, data=randomdat[-c(41,43),])
plot(randomdat$var1sq,randomdat$var2)
abline(m2_no43)					## compare new line (without points 41 and 43)
abline(m2, col = "blue", lty = 2)	## to the old line (with 41 and 43)
plot(m2_no43)	
## Your call, keep the original m2 model (with 41 and 43) 
## or say 41 and 43 are anomalies and remove them from analyses
## Be able to justify your answer




summary(m2)			##Both are similar, so I'll just keep 43
summary(m2_no43)
## Our regression equation is currently:		y = b + mx^2
## y_hat = b0 + b1 * var1^2
## var2_prediction = -870.925 + 0.647 * var1^2
## If var1 = 2...
## var2_prediction = -870.925 + 0.647 * (2)^2
## var2_prediction = -868.337




## Good practice -- whenever you have a power, keep the lower power terms (e.g. for X^3, keep X^2 and X)
## Standard equation: y = Ax^2 + Bx + C
## Note that most quadratic equations are y = b0 + b1 * x + b2 * x^2
## We see a quadratic relationship. Let's stick with the 
## quadratic model
## Common practice: if you include x^2, you should also include x


## You can continue to use var1sq
## m3 <- lm(var2 ~ var1 + var1sq, data = randomdat)



## Alternatively, you can use I()
## This will help work around needing to create a var1sq (a whole other variable)
## If you create a var1sq, you need to keep track of both var1 and var1sq together

## without I(X^2), we don't get what we want
## note, we don't get var1^2 in our summary()
m3<-lm(var2 ~ var1 + var1^2, data = randomdat)		##BAD EXAMPLE
summary(m3)








m3 <- lm(var2 ~ var1 + I(var1^2), data = randomdat)	##GOOD EXAMPLE

## Note, the above line is the same as 
## m3<-lm(var2~var1+var1sq, data = randomdat)
## Downside to using var1sq is that you have to remember that var1 and var1sq are related
## Whereas R will remember var1 and I(var1^2) for you

## If you use var1sq, you have to manually ALWAYS create var1sq
## That means you have to remember to make var1sq = 4 if var1 = 2
##		var1 = 6, var1sq = 36
##		var1 = 8, var1sq = 36		THIS IS WRONG!! var1sq = 64



summary(m3)
## Y = 1149.82 - 65.10*X + 1.04*X^2



## Let's plot the regression model over our scatterplot
plot(randomdat$var1,randomdat$var2)
abline(m3)					## won't work!!	-- abline() is only for adding straight lines




var1seq <- seq(-60, 220, 10)
## var1seq gives you... -60, -50, -40, ... 210, 220






## Predict using m3, plug in -60, -50, -40, ..., 210, 220
m3_predict <- predict(m3,list(var1=var1seq))

## points at (-60, 8790.5055), (-50, 6998.3592), (-40, 5413.6926), ... (220, 37037.7336)

plot(randomdat$var1,randomdat$var2)
points(var1seq, m3_predict, col = "blue", pch = 17)	## exclude this in your actual work, but this is just so you have an idea of where the curved line is coming from
lines(var1seq, m3_predict, col = "blue", lwd = 3)
## in the above code, var1seq is x and m3_predict is y



## predict using m3 when var 1 = 0
predict(m3,list(var1 = 0))


## predict using m3 when var 1 = 100
predict(m3,list(var1 = 100))


predict(m3, list(var1=30))
## If var1  = 30, we predict var2 = 130.4578


## predict using m3 with the original var1 numbers in randomdat
randomdat$predictedvar2_usingm3 <- predict(m3,list(var1 = randomdat$var1))
head(randomdat)

## For row 1, var1 = 61.9243 and using m3, we predict var2 to be 1096.5310. In actuality, var2 = 4406.593
## For row 2, var1 = 42.6140 and using m3, we predict var2 to be 259.4852. In actuality, var2 = -3891.991




## Residuals = actual - predicted
## Residual for row 1 is 4406.593 - 1096.5310 = 3310.062		Point to plot in residual plot (61.9243, 3310.062)
## Residual for row 2 is -3891.911 - 259.4852 = -4151.396	Point to plot in residual plot (42.6140, -4151.396)

randomdat$residvar2_usingm3 <- randomdat$var2 - randomdat$predictedvar2_usingm3

##Residual Plot
## No pattern at all (like the Residuals vs. Fitted in the diagnostics)
## No correlation
plot(randomdat$var1, randomdat$residvar2_usingm3, xlab = "Var 1", ylab = "Var 2 Residual")

plot(m3)








##### What exactly is happening when we draw our curved plot?? Why use so many values? #####

## predict using m3 when var 1 = -50, 0, 50, 100, 150, 200, 250
var1seq2 <- seq(-50,250,50)
m3_predict2 <- predict(m3,list(var1 = var1seq2))
## using points (-50, 6998), (0, 1150), (50, 488), ... (250, 49712)

plot(randomdat$var1,randomdat$var2)
points(var1seq2, m3_predict2, col = "red", pch = 17)
lines(var1seq2, m3_predict2, col = "red", lty = 1, lwd = 3)



plot(randomdat$var1,randomdat$var2)
lines(var1seq, m3_predict, type = "p", pch = 17, col = "blue")
lines(var1seq, m3_predict, col = "blue", lwd = 3)
lines(var1seq2, m3_predict2, type = "p", pch = 16, col = "red")
lines(var1seq2, m3_predict2, col = "red", lty = 1, lwd = 3)


## Check diagnostic
plot(m3)




########## Task ##########
## We want to get a sense of how Variable 1 predicts
## (or relates to) Variable 3
plot(randomdat$var1,randomdat$var3)
smooth <- smooth.spline(randomdat$var1,randomdat$var3, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)



## y = Ax^3 + Bx^2 + Cx + D

m4<-lm(var3~var1+I(var1^2)+I(var1^3), data=randomdat)
m4_predict<- predict(m4,list(var1=var1seq))			## var1seq was -60 to 220 by 10's...
lines(var1seq, m4_predict, col = "blue", lwd = 3)
summary(m4)
## Our regression equation looks like:
## y = b0 + b1 * var1 + b2 * var1^2 + b3 * var1^3
## var3 = -173804.07 + 11069.23*var1 - 183.90*var1^2 + 1.01*var1^3

predict(m4, list(var1=65))


## options(scipen = 999)		## fixed notation
## options(scipen = 1)		## back to scientific notation
## from "help": scipen is a penalty score
## ...fixed notation will be preferred unless it is more than 
## scipen digits wide





## You can also rescale var3 (variable 3 in millions)
par(mfrow = c(1,2))
plot(randomdat$var1,randomdat$var3, main = "Original", ylab = "Var3 in Ones")
lines(var1seq, m4_predict, col = "blue", lwd = 3)



randomdat$var3mil <- randomdat$var3 /1000000
m4mil<-lm(var3mil ~ var1+I(var1^2)+I(var1^3), data=randomdat)
m4mil_predict<- predict(m4mil,list(var1=var1seq))
plot(randomdat$var1, randomdat$var3mil, main = "Rescaled", ylab= "Var3 in Millions")
lines(var1seq, m4mil_predict, col = "blue", lwd = 3)

summary(m4mil)


## Common transformations include: sqrt(), exp(), log(), ^2, ^3



########## Task ##########
## We want to get a sense of how var1 predicts
## (or relates to) var4
plot(randomdat$var1,randomdat$var4)
smooth <- smooth.spline(randomdat$var1,randomdat$var4, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)


## We can try sqrt transformation, but the problem is that we have negative values

## add 50 to get address negatives

## BUT FIRST make sure it makes sense!
## Suppose we have 0 representing January 1, 2020
## Does it make sense to add 50?
## Does it make sense that -50 is now your new 0?
## In this case, yes. January 1 is 50, 12/31/19 would be 49, 12/30/19 would be 48, ...
## So then 0 would be November 11, 2019



plot(sqrt(randomdat$var1+50),randomdat$var4)
smooth <- smooth.spline(sqrt(randomdat$var1+50),randomdat$var4, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)



randomdat$var1sqrt <- sqrt(randomdat$var1+50)

## Two models, compare no transformation to sqrt transformation
m5_orig <- lm(var4 ~ var1, data=randomdat)
m5_sqrt <- lm(var4 ~ var1sqrt, data=randomdat)
## You can also use
## m5_sqrt <- lm(var4~I(sqrt(var1+50)), data = randomdat)





summary(m5_orig)
summary(m5_sqrt)
## Look at R-squared of both models
## R is the correlation (takes on values -1 and 1)
##	r = 0 means no correlation (no relationship) between x and y
##	r = 1 means a perfectly positive correlation between x and y (the higher x is, the higher y is)
## 	r = -1 means a perfectly negative correlation between x and y (the higher x is, the lower y is)
## 	You'll usually never get 0, 1, or -1. Usually it's a decimal # in between


## R-squared is the coefficient of variation (takes on values 0 to 1)
## 87.1% of variability in var4 can be explained by the model m5_orig
## 87.9% of variability in var4 can be explained by the model m5_sqrt
## In general, higher R-squared (adjusted) is better, but keep in mind model complexity/simplicity.
## Better to have a simpler model (but also as high of R-squared as possible)
## You want to consider the trade-off between complexity and fit

## Be careful not to overfit (have too many variables) because your model might work for your current data
## but it won't apply to other dataset, because you used so many variables.
## Look at adjusted R-squared when you have a lot of x-values
## Adjusted R-squared penalizes for having too many variables


## Also look at diagnostics!
plot(m5_orig)
plot(m5_sqrt)		## this might be the better option

## m5_orig vs m5_sqrt... similar variable significance, similar R-squared
				## but m5_sqrt had better diagnostic plots

m5_predict<- predict(m5_sqrt,list(var1sqrt=sqrt(var1seq+50)))


plot(sqrt(randomdat$var1+50),randomdat$var4)
lines(sqrt(var1seq+50), m5_predict, col = "blue", lwd = 3)



## Our model looks like
##  Y_hat = 2.5244 + 0.8892*sqrt(X + 50)
predict(m5_sqrt, list(var1sqrt=100))






##### Models for different groups #####
m4_a <- lm(var3~var1+I(var1^2)+I(var1^3), data=randomdat[randomdat$var8=="Group A",])
m4_b <- lm(var3~var1+I(var1^2)+I(var1^3), data=randomdat[randomdat$var8=="Group B",])
m4_c <- lm(var3~var1+I(var1^2)+I(var1^3), data=randomdat[randomdat$var8=="Group C",])
m4_a
m4_b
m4_c




##### Or just consider one model for all groups #####
## Based on our scatterplot, it doesn't make sense that we should have a
## different model for each group or different var1, I(var1^2), I(var1^3) coefficients
## We use both var1 and var8 in a single model
m6 <- lm(var3 ~ var1 + I(var1^2) + I(var1^3) + var8, data=randomdat)
summary(m6)




## What happened to Group A?
## You will always need a base case or comparison group
## Group B versus Group A
## Group C versus Group A


## Model: Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 18057GroupB - 43590GroupC


## What is my model if I am in Group A?
## That means GroupB = 0, GroupC = 0
## Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 18057(0) - 43590(0)
## Y = -153784 + 10888var1 - 180var1^2 + var1^3


## What is my model if I am in Group B?
## That means GroupB = 1, GroupC = 0
## Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 18057(1) - 43590(0)
## Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 18057
## Y = -171841 + 10888var1 - 180var1^2 + var1^3


## What is my model if I am in Group C?
## That means GroupB = 0, GroupC = 1
## Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 18057(0) - 43590(1)
## Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 43589
## Y = -197373 + 10888var1 - 180var1^2 + var1^3

## Holding var1 constant, Group B is predicted to be lower than Group A by 18057.
## Holding var1 constant, Group C is predicted to be lower than Group A by 43590.




## p-value is greater than 0.05 (general rule of thumb for cut-off)
## 	-- 0.1 (.) for smaller sample sizes (and certain fields, n < 30 or 40)
##	-- 0.01 (**) or 0.001 (***) for larger sample sizes (and depending on field)
## that means the coefficient for Group B is not significantly different from 0
##	-- possibly remove this from our regression model??


## Note the significance, Group B is NOT significant, so that means the coefficient for Group B
## is practically 0, so maybe we can remove it?
## Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 0GroupB - 43590GroupC
## Y = -153784 + 10888var1 - 180var1^2 + var1^3 - 43590GroupC	
## Now we're talking about Group C versus not Group C
## If you're going to exclude specific groups, you need to be able to justify whether or not it makes sense
## EXAMPLE: Freshman (comparison), Sophomores (significant), Juniors (not significant), Seniors (significant)
##	-- it doesn't make sense to take out Juniors because that makes Juniors AND Freshman the comparison group
##	-- but Juniors and Freshman don't quite belong in the same group

## If it doesn't make sense to take a group out of your model, just leave it in there.

## Does it make sense to combine Group A and Group B?
## It makes sense to combine Groups A and B (With XYZ group)
## Group C is the Without XYZ

## Instead of Group AB versus Group C, let's do With XYZ (same people as Group AB) versus Without XYZ (same as Group C)
m7 <- lm(var3~var1+I(var1^2)+I(var1^3)+var9, data=randomdat)
summary(m7)



## Our model looks like...
## Y = -162934.42 + 10927.87var1 - 180.83var1^2 + var1^3 - 34451.43WithoutXYZ
## For With XYZ group:
##		Y = -162934.42 + 10927.87var1 - 180.82var1^2 + var1^3
## For without XYZ group:
##		Y = -162934.42 + 10927.87var1 - 180.82var1^2 + var1^3 - 34451.43(1)
##		Y = -197385.85 + 10927.87var1 - 180.82var1^2 + var1^3



m7_predictwith <- predict(m7,list(var1=var1seq, var9 = rep("With XYZ", 29)))
m7_predictwout <- predict(m7,list(var1=var1seq, var9 = rep("Without XYZ", 29)))



plot(randomdat$var1,randomdat$var3)
lines(var1seq, m7_predictwith, col = "blue", lwd = 3)
lines(var1seq, m7_predictwout, col = "red", lty=2, lwd = 3)



##??? PRACTICE!
## Conduct some sort of transformation to see how var1 predicts
## (or relates to) var5
## a. Generate plots to help you identify the appropriate transformation
## b. Generate a regression model that shows this transformation
## c. Overlay your regression line to see the fit of your model
## d. Examine diagnostic plots
## e. Based on your model, estimate what var5 would be if var1 = 100
plot(randomdat$var1, randomdat$var5)


m6<-lm(var5~var1, data = randomdat)
abline(m6)
summary(m6)
plot(m6)



## try other transformations to see if we get a better model




##??? PRACTICE!
## Predict var5 considering both var1 and var10
## a. Generate scatter plots separating the three groups to help you identify
##	whether the graphs of "var5 vs. var1" have the same shape (and therefore 
##	do not need a different model for each category)
## b. Based off of your scatterplot observations, generate lm model(s)
## c. Overlay your regression line(s) on your scatter plot and compare to a smooth spline

m6_2<-lm(var5~var1 + var10, data = randomdat)
summary(m6_2)

## or try other transformations



## var5 ~ var1 + var10
## var5_hat = 4.96 + 0.0072var1 - 0.524Square + 0.482Triangle
## Holding var10 constant, for every 1 unit increase in var1, we predict that var5 will increase by 0.0072
## For Circles, if var1 is 0, then we predict var5_hat to be 4.96
## Holding var1 constant, we predict Squares to have var5 on average 0.524 units lower than Circles
## Holding var1 constant, we predict Triangles to have var5 on average 0.482 units higher than Circles
## The base comparison case is those that are not Squares and not Triangles (in this case, Circles)





############# Multiple Numeric and categorical Variables #############

## You can create models with multiple numeric and categorical variables
## In the following model, I did not use transformations--but you can add transformed terms as well
## Note, var9's withoutXYZ and var8's Group C are the exact same group (so we'll get NAs)
m8 <- lm(var11~var1+var2+var3+var4+var5+var8+var10, data=randomdat)
summary(m8)



## Interpretation: Holding all other variables constant, a 1 unit increase in var2 predicts a
## 2 unit increase in var11
## Holding all other variables constant, a 1 unit increase in var4 predicts 
## a 4.99 unit increase in var11
## Holding all other variables constant, Group B is predicted to have a 0.04 higher var11 than Group A
## Holding all other variables constant, Group C is predicted to have a 0.06 lower var11 than Group A
## Circle is the comparison group for var10. If you want a different comparison group, use factor() to refactor your data
## Alphabetical order: Circle, Square, Triangle



## There are a lot of different methods to help you determine what variables
## to include and exclude in your regression model. Most basically, we can go step-wise backwards
## by including everything and taking out the insignificant variables

summary(lm(var11~var1+var2+var3+var4+var5+var6+var7+var8+var9+var10, data=randomdat))

## Group C and the without XYZ group are exactly the same groups (Without XYZ is NA)
## Including without XYZ will not tell us anything more than what Group C already tells us

## summary(lm(var11~var1+var2+var3+var4+var5+var6+var7+var9+var8+var10, data=randomdat))
## if you swap the order of var8 and var9, Group C becomes NA for the same reason

## Removed var9
## Since without XYZ and Group C are exactly the same group, 
## so we cannot include both in the same model. Regression doesn't know
## how to split the coefficient!
## The same would apply for having group A, group B, and with XYZ in the same model

## Whenever you see a row of NAs, you are experiencing a multicollinearity problem
## You have redundant variables (without XYZ and Group C are the same group)


summary(lm(var11~var1+var2+var3+var4+var5+var6+var7+var8+var10, data=randomdat))



## Take out one-by-one nonsignificant terms (by highest p-value)
summary(lm(var11~var1+var2+var4+var5+var6+var7+var8+var10, data=randomdat))
summary(lm(var11~var1+var2+var4+var5+var6+var7+var10, data=randomdat))
summary(lm(var11~var1+var2+var4+var5+var7+var10, data=randomdat))



## We can also work step-by-step going forwards by adding one-by-one terms that we think
## could be important



## We can also use the "step()" function -- which automates our step-by-step investigation
## based on a "likelihood" calculation. By default, the calculation uses Akaike's Information
## Criterion (AIC; when k=2), but there's also Schwarz's Bayesian Criterion (BIC, when k=log(n))
## From help:
## step(object, scope, scale = 0, direction = c("both", "backward", "forward"),
##     trace = 1, keep = NULL, steps = 1000, k = 2, …)
##
m9 <- lm(var11~var1+var2+var3+var4+var5+var6+var7+var8+var10, data=randomdat)
step(m9, direction = "backward")

step(m9, direction = "both")
## + to add variable, - to take out



## Lower AIC values = better fit (AIC to be as negative as possible)
## Suppose we have AIC values 100 and 101 for models A and B. exp((100-101)/2) = 0.607 times
## as probable as the first model to minimize the information loss. 
## However, Models A and B might not be that different (compared to 0.001 times info loss)

## Model var11 ~ var1 + var2 + var4 + var5 + var7 + var10
## If we look at the AIC
## Taking out var1, var7, var10, var5 probably aren't such a big deal
## The AICs are similar

## However, taking out var4 and var2 will be detrimental to our model

summary(lm(var11~var2 + var4, data = randomdat))	## R-squared suggests this is still a really good model with var2 and var4 alone
												## We can go with a super simple model using var2 and var4 alone
summary(lm(var11~var1+var5+var7+var10, data = randomdat))	## Without var2 and var4, R-squared drops pretty far



## Looking at the step() results, we certainly do not want to remove var4 or var2







##### Regression Interactions #####
##### A single model, but consider the possibility of different lines for different groups
plot(randomdat$var1, randomdat$var7)


m9a <- lm(var7~var1, data = randomdat[randomdat$var8 == "Group A",])
m9b <- lm(var7~var1, data = randomdat[randomdat$var8 == "Group B",])
m9c <- lm(var7~var1, data = randomdat[randomdat$var8 == "Group C",])


m9 <- lm(var7~var1*var8, data = randomdat)
summary(m9)

## Model:
## var7_predicted = 420 - 2var1 - 365B - 278C + 4var1*B + 4var1*C

## What if you are Group A?
## GroupB = 0, GroupC = 0
## var7_predicted = 420 - 2var1 - 365(0) - 278(0) + 4var1*(0) + 4var1*(0)
## var7_predicted = 420 - 2var1

## What if you are Group B?
## GroupB = 1, GroupC = 0
## var7_predicted = 420 - 2var1 - 365(1) - 278(0) + 4var1*(1) + 4var1*(0)
## var7_predicted = 420 - 2var1 - 365 + 4var1
## var7_predicted = 55 + 2var1

## What if you are Group C?
## GroupB = 0, GroupC = 1
## var7_predicted = 420 - 2var1 - 365(0) - 278(1) + 4var1*(0) + 4var1*(1)
## var7_predicted = 420 - 2var1 - 278 + 4var1
## var7_predicted = 142 + 2var1


m9a
m9b
m9c


## Notice that the values in m9 are the same as m9a, m9b, m9c
## But better statistically to have all models in one
## You can see that Group B and Group C have different intercepts from Group A
## You can also see that Group B and group C have different slopes from Group A
## Also, if you include other variables (not interacted), you will burn 1 df for each model
## For example: lm(var7~var1*var8 + var2, data = randomdat)


## If we ran separate models, we aren't able to determine if the different groups should have different shapes.
## In addition, if we have more than just these two variables, we end up using more degrees of freedom by separating the different groups into differents...
## It's better to put all the groups in one model and add interactions for just the variables that should have different shapes.


## Note that we have used the * in our lm() function. This gives us every possible 
## combination of whatever is being *'d together
## For example
summary(lm(var7~var1*var8*var10, data=randomdat))

## If you don't want the different combinations, you can use : instead
m10 <- lm(var7~var1:var8, data = randomdat)
## no var1 alone
## no var8 alone


m11 <- lm(var7~var1 + var1:var8, data= randomdat)	## Manually adding var1



## As a general rule of thumb, models with interactions should include the
## lower order variables (so * should be used even when lower order variables
## aren't significant, unless you have a really good reason not to)



## In the example above, I showed you var1*var8 (numeric x categorial), 
## which is easily interpretable
## Categorical x categorical is also easily interpretable...
m12 <- lm(var7~var8*var10, data = randomdat)

## What happens if I am a group B Square?	B = 1, C = 0, Square = 1, Triangle = 0
## Based off the coefficients:
## For circles, we predict that Group B is 76.5 units lower than group A
## For circles, we predict that Group C is 12.7 units higher than group A
## For Group A, we predict that Squares are 27.1 units higher than Circles
## For Group A, we predict that Triangles are 19.4 units lower than Circles
## For Group B, we predict that Squares are (-39.581 + 27.055) units higher than Circles (since it's a negative #, it should be lower)
## Group Triangles, we predict that Group C is (12.683 + 1.736) units higher than Group A



## Model:
##		var7_pred = 282 - 74B + 13C + 27Square - 19Tri - 42B*Square - 16C*Square + 18B*Tri + 2C*Tri

## What if you are Group B, Circle?
## B = 1, C = 0
## Square = 0, Tri = 0
##		var7_pred = 282 - 74(1) + 13(0) + 27(0) - 19(0) - 42(1)(0) - 16(0)(0) + 18(1)(0) + 2(0)(0)
##		var7_pred = 282 - 74
##		var7_pred = 208


## What if you are a Group A, Triangle?
## B = 0, C = 0
## Square = 0, Tri = 1
##		var7_pred = 282 - 19(1)
##		var7_pred = 263


m12_extra <- lm(var7~var1 + var8*var10, data = randomdat)
## Model:
##		var7_pred = 230 + .75var1 - 77B + 11C + ...


## Reorder levels -- changes the order
## or you can use factor()
levels(randomdat$var8) <- c("Group C", "Group B", "Group A")



## However, numeric x numeric (interactions) is a little more challenging to work with
## You can do it, and it does (sometimes) make sense to do so
m13 <- lm(var3~var1*var2, data = randomdat)



## Model:
##		var3_pred = -55932 + 1268var1 - 50var2 + 0.625var1*var2

## Compare var1 = -40 to var1 = 0 to var1 = 200
##	var1 = -40		var3_pred = -55932 + 1268(-40) - 50var2 + 0.625(-40)*var2
##					var3_pred = -55932 - 50720 - 50var2 -25var2
##					var3_pred = -106652 - 75var2
##	var1 = 0			var3_pred = -55932 + 1268(0) - 50var2 + 0.625(0)*var2
##					var3_pred = -55932 - 50var2
##	var1 = 200		var3_pred = -55932 + 1268(200) - 50var2 + 0.625(200)*var2
##					var3_pred = -55932 + 253600 - 50var2 + 125var2
##					var3_pred = 197668 + 75var2



## Compare var2 = -5200 to var2 = 0 to var2 = 34500
##	var2 = -5200		var3_pred = ...
##	var2 = 0
##	var2 = 34500


## This term 0.625var1*var2 tells use 2 things:

##	As var1 increases, the slope for var2 becomes more and more positive
	## -50var2 start out negative for var1 = 0, so we're going to see a point where it
	## transitions from negative to positive

##	As var2 increases, the slope for var1 becomes more and more positive
	## 1268var1 is already positive, so we're only getting it to become more positive



########## Some CAUTIONS ##########

#####***** ALWAYS CHECK YOUR CORRELATIONS *****#####
## cor() works for numeric variables only
cor(randomdat[,c(1:7,11)])


## If you put two highly-correlated variables in the same lm model, you
## may run into a multi-collinearity problem

## with multi-collinearity, coefficients and standard errors will not be stable
## you might see really big changes in your model

summary(lm(var11 ~ var1 + var4, data = randomdat))
summary(lm(var11 ~ var4, data = randomdat))
## The first model says that there is a negative relationship between var4 and var11
## The second model (without var1) says that there is now a positive relationship between var4 and var11

## What's going on here?
plot(randomdat$var4, randomdat$var11)
plot(randomdat$var1, randomdat$var11)

## var4 in fact is positively correlated with var11, but with var1 in the first model
## it absorbs some of the positive relationship and leads us to think that the relationship
## is negative
## In short, var1 is taking all the credit

## In this case, DO NOT INCLUDE BOTH VAR1 AND VAR4 IN THE SAME MODEL IN THE FIRST PLACE
## Pick the one that's more important to you.


cor(randomdat[,c(1:7,11)])
## Between var2 and var3, probably want to keep var2 to predict var11 (it looks like an excellent predictor)


## Rule of thumb for avoiding multi-collinearity
## 	There are some methods
##	Field-specific, but try to keep independent variable correlations under 0.6 (it depends)
##	High correlations with the dependent variable is GREAT!




#####***** THE SAME GOES FOR CATEGORICAL VARIABLES *****#####
## var8 and var9 cannot be in the same model because 
## "Without XYZ" and "Group C" refer to the same 100 observations
m14 <- lm(var3~var9+var8, data = randomdat)
summary(m14)



##***** MAKE SURE YOU HAVE ENOUGH OBSERVATIONS TO RUN YOUR MODEL *****#####
## using 10 observations and 8 explanatory variables is a terrible idea
## called "overfitting"



#####***** CORRELATION DOES NOT EQUAL CAUSATION *****#####
## Just because you see that var_x predicts var_y does not 
## mean that var_x caused var_y to increase/decrease
## You can't say that ice cream sales causes drowning
## it could be that a hot day increases more ice cream sales AND more swimming (they just so happen to be occurring at the same time)


##***** WE CANNOT SIMPLY USE OUR REGRESSION MODELS TO MAKE CAUSATION STATEMENTS *****##




########## High Density Plots ##########
vocab <- read.csv("vocab.csv",header=T)
plot(vocab$education, vocab$vocabulary)


## For scatter plots, can also use sp() in the car library
## sp() has the jitter argument
sp(vocab$education, vocab$vocabulary, jitter = list(x=2,y=2))



sunflowerplot(vocab$education, vocab$vocabulary)
smoothScatter(vocab$education, vocab$vocabulary)


dat$newvar[grep("pop", dat$genre)] <- "pop"