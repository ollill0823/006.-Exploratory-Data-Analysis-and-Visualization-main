#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 1
##
## Agenda:
## 1. Getting Started with R
## 2. Algebraic Operators
## 3. Getting Help
## 4. Storing Variables
## 5. Matrices
## 6. Data Frames
## 7. Strip Charts
## 8. Dot Charts
## 9. Box Plots
#################################################


## Note, anything behind "#" sign in the same line is ignored
"This is not ignored" ## but this is
getwd()	## to get your current working directory
setwd("____") ## fill in the blank with your working directory


## Files in directory
dir()


##### Algebraic Operators #####
## (+, -, *, /, ^ or **, %%, %/%)

## %% gives remainder of division
44 %% 7		# this will give us 2 because 44/7 is 6 REMAINDER 2

## %/% gives division rounded down
44 %/% 7		## this will give us 6 because 44/7 is 6 remainder 2





## Other math functions
cos(4)   	## cosine
sin(3)	 	## sine
tan(2)   	## tangent
sqrt(25)
log(5)		## make sure you are familiar with logarithms and exponents (graph shape)
exp(2)			## e^2






sum(1,2,3,4,5)

c(1,2,3,4,5)				## c() groups your numbers together "vector"

c(1, 2, 3, 4, 5) + c(10, 14, 18, 22, 26)


## Two types of measures of the middle: mean() and median()
## mean() -- Add all the numbers together, and divide by how many we have
## median() -- Sort the number from smallest to largest and pick the number in the middle
mean(c(1,2,3,4,5))			## (1 + 2 + 3 + 4 + 5) / 5
median(c(1,2,3,4,5))			## the middle number of (1,2,3,4,5) sorted


mean(c(4, 9, 1, 7, 3))		# (4 + 9 + 1 + 7 + 3) / 5
median(c(4, 9, 1, 7, 3))		## in order, we have 1, 3, 4, 7, 9


mean(c(4, 9, 1, 7, 3, 11))		# (4 + 9 + 1 + 7 + 3 + 11) / 6
median(c(4, 9, 1, 7, 3, 11))	## in order, we have 1, 3, 4, 7, 9, 11
							## since we have two middle numbers, median will pick the middle value between 4 and 7
							## (4 + 7) / 2 = 5.5


mean(c(1,2,3,4,555))	 		## mean is affected by outliers
median(c(1,2,3,4,555))  		## but not median

## order does not matter
mean(c(555, 3, 1, 2, 4))



# median for even number of numbers
median(c(4, 7, 29, 23, 15, 14))		## 4, 7, 14, 15, 23, 29
									## Pick middle of two middle numbers (14 and 15)


min(c(1,2,3,4,5))
max(c(1,2,3,4,5))


## Variance
var(c(1,2,3,4,5))			## spread of your data (how far apart your datapoints are)
var(c(6,7,8,9,10))
var(c(10,20,30,40,50))	## higher variance values means your datapoints are more spread out


## Standard Deviation
sd(c(1,2,3,4,5))			## square-root of variance
sd(c(6,7,8,9,10))
sd(c(10,20,30,40,50))



## Note, 1:5 will give you numbers 1 through 5
4:12
-2:5

mean(1:5)		## same as mean(c(1,2,3,4,5))
median(1:5)		## same as median(c(1,2,3,4,5))


## All even numbers between 2 through 50?
2 * 1:25
seq(2, 50, 2)		## Start at 2, go to 50, in intervals of 2

2 * 1:25 - 1



##??? What are two different ways to get all even numbers between 100 and 200?
2 * 50:100
2 * 0:50 + 100		## 2 * 0 + 100		2 * 1 + 100		2 * 2 + 100	...
seq(100, 200, 2)



##### Getting help #####
?mean

help(mean)		## help() and ? give us the same thing
				## You can only use these two for functions in R

## ?variance -- we get an error; variance() is not a function

## mean() is a function



??variance  ##anything with "stats::" in front of it
			 ## is already loaded for us



##### Storing variables #####
## Variables can take on almost any name, upper or lower case
## you can also use special characters like . and _
## Spaces cannot be a part of the variable name; however, in R code, it does get ignored

## Storing single items into variables
a <- 3
b <- -3			## -3

c <- "Hello"		##" " or ' ' are okay
d <- 'Hello'

e <- T				## capitalization matters
f <- TRUE			## but there's two ways to write true
g <- F				## and false
h <- FALSE			## true is 1 in R, false is 0 in R


## spacing does not matter!
e <- T
e<-T			##both mean the same thing


## <- versus = both allow for storing, but it's more common (and clearer) to use <-
## You can use:
## a = 3		## means the same thing as a <- 3


## booleans
a >= b		## Is a greater than or equal b?
a < b
a == b		## checking if a is the same value as b
			## Be careful not to mix this up with a = b, which means to MAKE a equal to b



ls()			## this gives you the list of variables currently in your workspace
str(a)		## str() stands for structure		
remove(g)
ls()			## g is no longer in the list because we removed it


## Note, capitalization matters!!!

##??? What do you get when you type a * b? a * c? e * f? e + f?
##> a*b
##[1] -9
##> a*c
##Error in a * c : non-numeric argument to binary operator
##> e*f
##[1] 1
##> e + f
##[1] 2


##??? Practice storing -7 in variable x, "statistics" in variable y, 
##    and false in variable z

x <- -7
y <- "statistics"
z <- F


## Vectors (multiple values of the same type)
i <- c(1,2,3,4,55)
mean(i)
median(i)
var(i)
sd(i)


j<-c("apple", "banana", "cherry", "dragonfruit", "eggplant")
k<-c(T,T,F,T,F,F)
l<-c(1, "two", 3.0, F)
## l is a vector/list of strings
##> l
##[1] "1"     "two"   "3"     "FALSE"




str(i)
str(j)
str(k)
str(l) ## note, all converted to strings



## You can put multiple lines of code in the same line, separated by a ";"
## But this is not recommended
str(i); str(j); str(k); str(l)


## You can convert data types -- more on this in upcoming weeks
m <- "4"
as.numeric(m)
remove(m)



## Accessing the different elements in the vector
## use [n] after the variable to access the nth element
j[2]
j[2:4]		## This gives you the second-fourth elements of j
j[c(1,4)] 	## 1st element and 4th element
j[-3]		## everything but 3
j[-c(2,5)]	## excluding 2 and 5



## be careful with - (negative), minus nothing means minus EVERYTHING



## Note, j[c(2,5]) will give you error -- check your parentheses
j[c(2,5)]



## Make sure you start with > at the start of each line of code
## Notice that j[c(2,5) is missing a close bracket,
## this will cause a "+" symbol to show up in your next line.
## That "+" symbol means that you are still continuing your previous line of code
## You're missing a close parenthesis/bracket somewhere.
## You either close the parenthesis or you can type "Esc" to escape the current line of code


##??? Store a list of 10 vegetable names in variable veggies
##    hint: veggies <- c(...)
veggies <- c("broccoli", "kale", "spinach", "carrot", "celery",
	"peas", "bok choy", "lettuce", "cabbage", "asparagus")

##it's okay to break your code into multiple lines



##### Matrices #####
## matrix(.........)
## use matrix(insert vector, nrow = number of rows, ncol = number of columns, byrow = T/F)
m <- matrix(c(2,5,8,9,10,11),nrow=2,ncol=3,byrow = T)
m



## help(matrix)
## from help page... 
## 		matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
##       		dimnames = NULL)
## these are the default settings

n<-matrix(c(2,5,8,9,10,11),nrow=2,ncol=3,byrow = F)

p<-matrix(c(2,5,8,9,10,11),nrow = 2, ncol = 3)

n == p		## check if the matrices are the same



## Accessing the different elements in a matrix
## use [i,j] after the variable to access the ith row and jth column
m[2,3]
# m[3,2] gives an error, no row 3


## Note, single numbers between [_] will also work too
## but it will give you the _th element by column
m[5]



##??? Create a 4 row x 7 column matrix using just c(1,4,8,13,22) by row
##    Note, there are fewer numbers than elements
##    How are the remaining missing elements in the matrix filled in?

matrix(c(1,4,8,13,22),nrow=4,ncol=7,byrow = T)
##	Warning message because 4 * 7 gives us 28 different spots
## 	However, 5 is not a factor of 28	(28 is not perfectly divisible by 5)


##??? Create a 3 row x 4 column matrix using the veggies list that you created
##		by column
matrix(veggies, nrow = 3, ncol = 4)	##by default, byrow = F
c(veggies,NA, NA)
matrix(c(veggies,NA, NA),nrow = 3, ncol = 4)

matrix(c(veggies,NA, NA, NA, NA),nrow = 3, ncol = 4, byrow = F)		##it's okay to have extra, but you'll get a "warning"



## You can only have one data type in a matrix
## You cannot mix chr, logi, and num into one matrix




##### Data Frames #####
## first, let's create some vectors
var1<-3:12
var2<-c(3,5,8,2,3,5,2,3,2,1)
var3<-rep(3,10)				##repeat 3 ten times
var4<-c(1,rep(2,3),rep(4,3),rep(5,3))

## > rep(3:5,5)
## [1] 3 4 5 3 4 5 3 4 5 3 4 5 3 4 5
## > rep(c(1,5,10,1),4)
## [1]  1  5 10  1  1  5 10  1  1  5 10  1  1  5 10  1


## Creating your data frame
mydata <- data.frame(var1, var2, var3, var4)
ls()								## we currently have two copies of var1, var2, var3, var4
								## one copy is under mydata
remove(var1, var2, var3, var4)		## redundant to have two copies of the variables
ls()

mydata					## we removed the copy in our workspace, but not the copy under mydata


mydata$var1				## to access the variables under a data frame, use $
						## data$variable




## To add an additional column, use cbind(original, new columns)
var5<-c(rep(5,5),rep(10,5))
mydata <- cbind(mydata, var5)		## replacing the original "mydata"
remove(var5)						## remove the extra copy of var5


foods<-c(j,"fig", "grape", "honeydew", "jackfruit", "kiwi")
foods
mynewdata<-cbind(mydata,foods)		##creating a new data frame called "mynewdata"
mynewdata
mydata								##mydata has not been replaced


## To add a new row, use rbind()
newperson <- c(1,2,3,4,5)
mydata <- rbind(mydata, newperson)

newfood <- data.frame(var1 = 3, var2 = 5, var3 = 3, var4 = 10, var5 = 10, foods = "mango")
mynewdata <- rbind(mynewdata, newfood)



##??? Create a data frame "veggiesdata" that combines two vectors:
##    your veggies vector and a vector that gives the estimated cost for
##    each veggie per pound (in the same order as veggies)
cost <- c(5,4,6,7,10,1,1.5,2.5,3.75,0.10)
veggiesdata <- data.frame(veggies,cost)



## variable names
## cost.per.pound <- c(5,4,6,7,10,1,1.5,2.5,3.75,0.10) is okay
## cost_per_pound <- c(5,4,6,7,10,1,1.5,2.5,3.75,0.10) is okay
## costPerPound <- c(5,4,6,7,10,1,1.5,2.5,3.75,0.10) is okay
## CostPerPound <- c(5,4,6,7,10,1,1.5,2.5,3.75,0.10) is okay

## The following does not work
## cost per pound <- c(5,4,6,7,10,1,1.5,2.5,3.75,0.10) does not work -- your variable name cannot have spaces or special characters outside of . and _




## Difference between Matrices and Data Frames
veggiesdat <- cbind(veggies,cost)			## R will think this is a matrix -- in this case, everything becomes a string
str(veggiesdata)
str(veggiesdat)					## you can mix numbers, logicals, and strings in data frames but not matrices




##### Saving Data #####
## csv stands for comma separated values
## General coding format is:	write.csv(data, "filename")

write.csv(mynewdata,"lect1data1.csv")			##if you have a file with this name open, make sure you close it first
write.csv(mynewdata,"lect1data1.csv", row.names = F) 	## This will remove the row numbers


##??? Save your "veggiesdata" into a file called "veggiesdata.csv" without row numbers

write.csv(veggiesdata,"veggiesdata.csv", row.names = F)



?write		## Other file-saving possibilities

## write(x, file = "data",
##      ncolumns = if(is.character(x)) 1 else 5,
##      append = FALSE, sep = " ")
write.table(veggiesdata, "veggiesdata.txt", sep = "\t", row.names = F)
write.table(veggiesdata, "veggiesdata2.txt", sep = ";", row.names = F)





##### Reading in files #####
## Two basic reading functions are read.table() and read.csv()
## These are used for delimited data files (tab, space, comma)
## We'll talk about other types of data files (including Excel) later
##??? Use the help function to look up the read function


## Make sure you are in the correct directory
mydata<-read.csv("lect1data1.csv",header=T)
dim(mydata)		## dimensions
head(mydata)		## first 6 rows
tail(mydata)		## last 6 rows
tail(mydata, 3)	## just the last 3
str(mydata)		## structure of data
summary(mydata) ## summary stats for numbers, counts for categories
mydata[1:6,]
mydata[,3:6]
mydata[1,2]		## data point in row 1, column 2


## You can also use $ to access specific columns
mydata$var2
mydata$var2[2]		## in dataset "mydata", use variable "var3", element 2
mydata$foods[4]		## in dataset "mydata", use variable "foods", element 4
mydata[4,5]			## I don't remember what column 5 is....



oddrows <- mydata[c(1,3,5,7,9),]
oddrows

mysubset <- mydata[which(mydata$var1 > 5),]		## getting a subset of the data, rows which var1 > 5
									## which(mydata$var1 > 5) gives us 4:10 for rows 4 through 10

mysubset2 <- mydata[which(mydata$var5 == 10),]	





##### Strip Charts #####
irisdata<-read.csv("iris.csv", header = T)
dim(irisdata)
names(irisdata)
summary(irisdata)
stripchart(irisdata$Petal.Width)

stripchart(irisdata$Petal.Width, method = "jitter")		
stripchart(irisdata$Petal.Width, method = "stack")



## Note, jitter will display the points in a completely random fashion
## so each time you run a stripchart with jitter, you won't get the exact same graph
##??? What is a benefit of jitter?


## You're able to see if there's just 1 or more than 1 observation
## associated with a number (x)


?stripchart
?points
stripchart(irisdata$Petal.Width, method = "jitter", pch = 17, xlab = "Petal Width")



##??? Create strip charts for the sepal length, sepal width, and petal lengths
##	Try to use different pch values and create labels for your charts

stripchart(irisdata$Sepal.Width, method = "jitter", pch = 14, xlab = "Sepal Width")
stripchart(irisdata$Sepal.Length, method = "jitter", pch = 7, xlab = "Sepal Length")
stripchart(irisdata$Petal.Length, method = "jitter", pch = 19, xlab = "Petal Length", col = "red")

## For petal length, we see several iris with petals lengths between 1 and 2.
## There are no iris with petal lengths between 2 and 3.
## There's quite a few that span the range between 3 and 6.9.




##### Dot Charts #####
usarrests<-read.csv("usarrests.csv",header=T)
dotchart(usarrests$Murder)
dotchart(usarrests$Murder, labels = usarrests$State, cex = 0.5)


## cex changes the size of the points
## labels gives labels to the left hand side of the graph


## access the old dataset usarrests
## [order(___), ] sorts the dataset by whatever is in ___


order(usarrests$Murder)			## Row number in order from least to most
								## Row 34 has the lowest murder rate, row 19 second lowest, ... , row 10 has the highest murder rate
usarrests[c(34, 19, 29, 15, 45),]

usarrests[order(usarrests$Murder),]		## dataset in order from smallest to largest murder rate

## the first thing in my new usarrests list will be observation # 34
## followed by 19, 29, 15, 45, 12, 49, ...
## For decreasing order, use > order(usarrests$Murder, decreasing = T)

usarrests_ordered <- usarrests[order(usarrests$Murder),]
head(usarrests_ordered)

dotchart(usarrests_ordered$Murder, labels=usarrests_ordered$State, cex = 0.5)


dotchart(usarrests_ordered$Murder, labels=usarrests_ordered$State, 
	cex = 0.5, main = "Murder arrests by state, 1973",
	xlab = "Murder arrests per 100,000 population")


dotchart(usarrests_ordered$Murder, labels=usarrests_ordered$State, 
	cex = 0.5, main = "Murder arrests by state, 1973",
	xlab = "Murder arrests per 100,000 population",
	pch = 19, col = c("darkblue","black","red"), 
	lcolor = "gray90", cex.main = 2, cex.lab = 1.5)



##??? Create ordered dotcharts for assault, urban population, and rape
##	You may need to create new datasets, or replace the existing "usarrests_ordered"

usarrests_assault <- usarrests[order(usarrests$Assault),]

dotchart(usarrests_assault$Assault, labels=usarrests_assault$State, 
	cex = 0.5, main = "Assault arrests by state, 1973",
	xlab = "Assault arrests per 100,000 population",
	pch = 19, col = c("green","orange","purple"), 
	lcolor = "gray80", cex.main = 2, cex.lab = 1.5)

usarrests_assault <- usarrests[order(usarrests$Assault, decreasing = T),]		## Changed to decreasing order
dotchart(usarrests_assault$Assault, labels=usarrests_assault$State, 
	cex = 0.5, main = "Assault arrests by state, 1973",
	xlab = "Assault arrests per 100,000 population",
	pch = 19, col = c("green","orange","purple"), 
	lcolor = "gray80", cex.main = 2, cex.lab = 1.5)




##### Boxplots #####
summary(irisdata)
boxplot(irisdata$Petal.Width, main = "Iris Petal Widths", ylab = "centimeters")

## the boxplot matches up with the summary
## 25% of our data (petal width) is between min and Q1 (between 0.1 and 0.3)
## 25% of our data (petal width) is between Q1 and median (between 0.3 and 1.3)
## 25% of our data (petal width) is between median and Q3 (between 1.3 and 1.8)
## 25% of our data (petal width) is between Q3 and max (between 1.8 and 2.5)
## 50% of our data is between Q1 and Q3 (interquartile range, IQR)
## 100% of our data is between min and max (range)



## The following line sets the graphics so that you can see two graphs side-by-side
par(mfrow = c(1,2))			##1 row, 2 columns in graphics



## here, we have two completely different columns in our dataset
boxplot(irisdata$Petal.Width, main = "Iris Petal Widths", ylab = "centimeters")
boxplot(irisdata$Sepal.Width, main = "Iris Sepal Widths", ylab = "centimeters")

## y-axis was misleading, so adjusting ylim so that the y-axis matches on both
boxplot(irisdata$Petal.Width, main = "Iris Petal Widths", ylab = "centimeters", ylim = c(0,5))
boxplot(irisdata$Sepal.Width, main = "Iris Sepal Widths", ylab = "centimeters", ylim = c(0,5))

	## small dots on boxplots == potential outliers

## Outliers are calculated using 1.5 * IQR 
## Anything that is beyond
##	IQR = Q3 - Q1
## Lower limit:		Q1 - 1.5 * IQR
## Upper limit:		Q3 + 1.5 * IQR
## will show up as "outliers" or "dots



## You'll need to reset the graphics window to get one graph only
par(mfrow = c(1,1))
boxplot(Petal.Width~Species, data = irisdata, col = "blue",
	main = "Iris Petal Width by Species", cex = 0.4)

boxplot(Sepal.Width~Species, data = irisdata, col = "blue",
	main = "Iris Sepal Width by Species", cex = 0.4)

## ~ means "as a function of"


par(mfrow = c(1,2))
boxplot(Petal.Width~Species, data = irisdata, col = "blue",
	main = "Iris Petal Width by Species", cex = 0.4)

boxplot(Sepal.Width~Species, data = irisdata, col = "blue",
	main = "Iris Sepal Width by Species", cex = 0.4)




par(mfrow = c(1,2))	
boxplot(irisdata$Petal.Width, main = "Iris Petal Widths", ylab = "centimeters")
boxplot(Petal.Width~Species, data = irisdata, col = "blue",
	main = "Iris Petal Width by Species", cex = 0.4, ylab = "Petal Width")

## note, we used data = irisdata in the second line; irisdata$Petal.Width or data = irisdata is okay
## ~ in line 2 means petal.width by species  ----or---- Petal.Width is "y" and Species is "x"
## boxplot(y~x,...)


## we can look at summary statistics of each individual species
summary(irisdata[which(irisdata$Species == "setosa"),])
summary(irisdata[which(irisdata$Species == "versicolor"),])
summary(irisdata[which(irisdata$Species == "virginica"),])



##??? What happens when you add  ..., horizontal = T, ... to your boxplot function?
##??? Create boxplots for sepal width, petal length, and sepal length by species
boxplot(irisdata$Petal.Width, main = "Iris Petal Widths", xlab = "centimeters", horizontal = T)



##??? For more practice, load the dataset mtcars and generate strip charts,
##	dot charts, and box plots for different variables (combinations).
##	For example, you can create an ordered dot chart by MPG with car labels
##	You can also create boxplots of MPG by cylinders or gears
##	What happens when you type boxplot(mpg~cyl*gear, data = mtcarsdata)?











##### Strip Chart versus Box Plot #####
par(mfrow = c(1,2))
stripchart(irisdata$Petal.Width, main = "Iris Petal Widths", method = "stack")
boxplot(irisdata$Petal.Width, main = "Iris Petal Widths", xlab = "centimeters", horizontal = T)

par(mfrow = c(1,1))
summary(irisdata$Petal.Width)
stripchart(irisdata$Petal.Width, main = "Iris Petal Widths", method = "stack")
abline(v = 0.3, col = "red")
abline(v = 1.3, col = "red")
abline(v = 1.8, col = "red")



## To add horizontal lines, you can use abline(h = ___)

