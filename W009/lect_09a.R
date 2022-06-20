#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 9
##
## Agenda:
## 1. Missing Data
## 2. Two-Sample T-Test and Two-Sample Proportion Test
## 3. Missing Data Group Discussion
## 4. Handling Missing Data
## 5. Clustering (Quick Review)
## 6. GG Plot
## 7. Watch Group Projects (on your own)
#################################################
library(cluster)
library(ggplot2)
library(readxl)
library(datasets)
library(scatterplot3d)


########## Missing Data ##########
grades<-read_excel("MissingDataPractice.xlsx", sheet = "TestGrades")
grades<-as.data.frame(grades)
summary(grades)
grades$Gender <- as.factor(grades$Gender)


## NA test by Gender (categorical/categorical)
natest <- is.na(grades$Test)
prop.table(table(grades$Gender,natest))
## 42% of data are female with valid test score
## 7% of data are female with missing test score

prop.table(table(grades$Gender,natest),1)		## proportion by rows
prop.table(table(grades$Gender,natest),2)   ## proportion by columns
## 85.71% of female in the dataset have valid test score
## 14.29% of female in the dataset are missing test score


## NA test by scores (categorical/quantitative)
boxplot(grades$Physics1~natest)	## by excluding NA's, we have fewer low-performing Physics students
boxplot(grades$Chem1~natest)
boxplot(grades$Pretest~natest)		## by excluding NA's, we have fewer low pre-test scoring students
## One possibility is due to failing the class; maybe the student dropped the class or decided not to even try
## We won't actually know unless we investigate the data collection and/or source



## NA Physics1 by Gender (categorical/categorical) -- NA Physics1 is either missing or not
naphys <- is.na(grades$Physics1)
prop.table(table(grades$Gender,naphys))
prop.table(table(grades$Gender,naphys),1)

## NA Physics 1 by scores (categorical quantitative)
boxplot(grades$Chem1~naphys)		## by excluding students without Physics 1 scores, we are also excluding students who have high Chem scores
boxplot(grades$Pretest~naphys)		## by excluding students without Physics 1 scores, we exclude high pretest scores
boxplot(grades$Test~naphys)		## by excluding students without Physics 1 scores, we exclude high test scores
## Maybe the students who are missing Physics 1 scores are high performing students (AP students?)
## who were able to skip Physics 1. Again, we don't actually know.



## Missing Completely at Random (MCAR)
## - 	Pattern of missingness is independent of missing values
## 	of any measured variables
## - 	In other words, missingness not related to the other variables
## -	Ex: Researcher accidentally deletes a survey response
## -	Ex: Randomly select 3 questions out of 10 to ask respondents
		## For the second example, the missingness is not due to any characteristic of the respondent
		## We simply randomly assigned the question to ask (systematically)

## Missing at Random (MAR)
## - 	Pattern of missingness depends on other variables, but conditional
##	on observed variables; no relation to the actual missing value
## -	Ex: Athletes less likely to report class time (the missing variable is class time, but the conditional is college student status)
## -	Ex: Students with health history more likely to have missing test scores
		## the missing variable is the test score, but the conditional or the reason why it's missing is because of the health history
## -Ex: High performing students are more likely to have missing "Introductory" course scores
		## the missing variable is the course score, but the conditional is the prior experience in the field


## Missing Not at Random (NMAR)
## - 	Pattern of missingness depends on the variable that is missing
## -	Ex: High income earners less likely to report income
## -	Ex: Drug users less likely to report drug use


## Least Problematic: MCAR
## Most Problematic: NMAR


########## Two Sample T-Test ##########
## Dependent variable must be continuous
## Observations must be independent of one another
## Dependent variable must be approximately normally distributed
## Dependent variable cannot have outliers
plot(density(grades$Physics1, na.rm=T))		## approximately normal
## na.rm means ignoring the Null data

boxplot(grades$Physics1)					## no outliers
t.test(grades$Physics1~natest)				

## p-value is less than 0.05 (generally accepted 
## cut-off for "Statistically significant")
## Because p-value is less than 0.05, 
## there is a statistically significant 
## difference in Physics 1 score
## between the missing test score students (75.07)
## and the non-missing test score students (86.5)
## By removing rows with missing test scores, we are deleting a group of students who have low Physics 1 scores


plot(density(grades$Pretest, na.rm=T))		## approximately normal
boxplot(grades$Pretest)					## no outliers
t.test(grades$Pretest~natest)	
## Because the p-value is less than 0.05, there is a statistically significant difference
## in Pretest score between the missing test score scores (Pretest average 69.33) and 
## the non-missing test score students (Pretest average 85.15)
## By removing rows with missing test scores, we are deleting a group of students who have low Pretest scores


plot(density(grades$Chem1, na.rm=T))		## approximately normal
boxplot(grades$Chem1)						## no outliers
t.test(grades$Chem1~natest)	
## p-value is not less than 0.05, so we do not have evidence that there is a
## statistically significant difference in Chem 1 score between the missing
## test score students (Chem1 average 82.30) and the non-missing test score students (Chem1 average 84.77)
## We don't need to be too concerned about differences in Chem 1 performance if we delete rows with missing test scores
## but we still need to be concerned about differences in other variables (Physics1 and Pretest)


########## Two Sample Proportion Test ##########
## CATEGORICAL / CATEGORICAL

table(grades$Gender,natest)
prop.test(x = c(84,89), n =c(98,102))		## we used x representing valid test scores
prop.test(x = c(14, 13), n =c(98, 102))		## same result, even though we used x representing missing test scores
## p-value is not less than 0.05, so we do not have evidence that there is a
## statistically significant difference between males and females in missing
## test scores
## We don't need to be too concerned about differences in gender if we delete rows with missing test scores,
## but we still need to be concerned about differences in other variables (Physics1 and Pretest)




##### ANOTHER EXAMPLE #####
summary(airquality)
boxplot(airquality$Ozone)		## we have two outliers, but it looks like the reason why is just because of skewed distribution
hist(airquality$Ozone)		## we can confirm that we have a skewed distribution and the outliers belong in our data (don't delete!)

naozone <-is.na(airquality$Ozone)

## Can be used at naozone == True or False
boxplot(airquality$Wind~naozone)
boxplot(airquality$Temp~naozone)
boxplot(airquality$Month~naozone)		## might want to convert Month/Day to a continuous "date" variable
boxplot(airquality$Day~naozone)
t.test(airquality$Wind~naozone)
t.test(airquality$Temp~naozone)		## So far, from what we can see, removing missing ozone rows should be fine
									## but, there might be other unobserved variables (e.g. traffic rate or weather)
									## that we haven't looked at


nasolar <-is.na(airquality$Solar.R)
boxplot(airquality$Ozone~nasolar)
boxplot(airquality$Wind~nasolar)
boxplot(airquality$Temp~nasolar)
airquality[which(nasolar),]
t.test(airquality$Ozone~nasolar)
t.test(airquality$Wind~nasolar)
t.test(airquality$Temp~nasolar)


## BREAK -- MEET BACK AT 7:40 pm


########## Handling Missing Data ##########
##### Categorical Variables #####
## (1) Delete
## (2) Add new category "missing"
## (3) Educated guess or change the category slightly	(e.g. instead of comparing males vs. females, compare males vs. non-males)

##### Numerical Variables #####
## (1) Listwise Deletion - Delete the row
##	- less data
grades1dat <- grades[!is.na(grades$Physics1) & !is.na(grades$Test),]
dim(grades)
dim(grades1dat)		## we got rid of 20% of our data




## (2) Pairwise Deletion - Work with data as available
## 	- not favored in (non-experimental) research, because results should be consistent
##	- cannot compare analyses bc sample is always different
mean(grades$Physics1, na.rm=T)		## in this calculation, I have 187 students
mean(grades$Test, na.rm=T)			## in this calculation, I have 173 students
								## NOT THE SAME SAMPLE

		## (Compare to the Listwise means)
		mean(grades1dat$Physics1)
		mean(grades1dat$Test)		## By using listwise deletion, I'm comparing the same groups of students
								## I can say that for the 160 students, they had a higher Physics 1 score than Test score


## (3) Single Imputation - Replace the missingness with a single sensible value (mean, median)
##	- reduces variability
##	- weakens covariance/correlation estimates in data bc it ignores the relationship between variables
## 	* You can optionally add a dummy variable to say "missing" or "not missing"
grades2dat <- grades
grades2dat$Physics1[is.na(grades2dat$Physics1)] <- mean(grades2dat$Physics1, na.rm = T)
summary(grades2dat)
summary(grades1dat)
cor(grades2dat$Physics1, grades2dat$Chem1)		## compare the results between the single imputation (grades2dat)
cor(grades1dat$Physics1, grades1dat$Chem1)		## and the results for those with complete data (grades1dat) from listwise deletion
											## ignored the fact that Physics 1 is related to Chem 1 (missingness is mostly high Chem 1 student)
											## AND we weakened the correlation in single imputation	


## (4) Multiple Imputation - Use the other variables to help make an educated guess
##	- overestimates model fit and correlation estimates (you artificially created a relationship that might not even exist)
##	- weakens variance
##	* use predict(__, list(var1 = __, var2 = __))
mphys <- lm(Physics1~Chem1+Pretest+Gender, data = grades)

grades[is.na(grades$Physics1),]
grades$Chem1[is.na(grades$Physics1)]

grades$Physics1[is.na(grades$Physics1)] <- predict(mphys, list(
	Chem1 = grades$Chem1[is.na(grades$Physics1)], 
	Pretest = grades$Pretest[is.na(grades$Physics1)],
	Gender = grades$Gender[is.na(grades$Physics1)]
	))

grades[c(6,20,22,35,46,58,59,80,109,116,187,188,196),]

cor(grades$Physics1, grades$Pretest)
cor(grades1dat$Physics1, grades1dat$Pretest)


mtest <- lm(Test~Gender+Physics1+Chem1+Pretest, data = grades)
grades$Test[is.na(grades$Test)] <- predict(mtest, list(
	Gender = grades$Gender[is.na(grades$Test)], 
	Physics1 = grades$Physics1[is.na(grades$Test)],
	Chem1 = grades$Chem1[is.na(grades$Test)], 
	Pretest = grades$Pretest[is.na(grades$Test)]))


## You can use logistic regression to "impute" values for missing categories
## But, you still need to decide-- if the logistic regression says "0.5" for a missing variable, is this 1 or is this 0?


## Which method is best?
## It depends. What your goal? What do people in the field do? 
## Do you have enough data to just delete? If so, who are you deleting?
## Try multiple methods, but use the same analyses. 
## IMPORTANT: Are your results consistent regardless of which method you choose?










########## Clustering (Quick Review) ##########
## Use grades1dat (listwise deletion version)

## kmeans
grades1dat$female <- rep(0, 160)
grades1dat$female[grades1dat$Gender == "Female"] <- 1
summary(grades1dat)
k2grades <- kmeans(grades1dat[,3:7], 2)
k2gradestest <- data.frame(k2grades$cluster)
## Splitting the data into two groups,
## Group A: tends to have higher Physics (closer to Q3), higher Pretest, and higher Test (Chem1 and female are about the same, close to the mean)
## Group B: tends to have lower Physics (closer to Q1), lower Pretest (closer to Q1), and lower Test (closer to Q1)

## By excluding those with lower Physics and lower Pretest, I'm also most likely excluding those with low Test scores


## hclust
hc <- hclust(dist(grades1dat[,3:7]), "ave")
plot(hc)
memb <- cutree(hc, k = 2)
grades1dat<-cbind(grades1dat, memb) ## combining with memb
grades1dat2<-cbind(grades1dat, k2gradestest)
colMeans(grades1dat[memb == 1, 3:7, drop = FALSE])
colMeans(grades1dat[memb == 2, 3:7, drop = FALSE])
## Similar results as kmeans
## Group A: tends to have higher Physics (closer to Q3), higher Pretest, and higher Test
##	Chem 1 in this case is a little closer to Q1 (female are about the same, close to the mean)
## Group B: tends to have lower Physics (closer to Q1), lower Pretest (closer to Q1), and lower Test (closer to Q1)
##	Chem 1 is a little closer to Q3	

memb1 <- cutree(hc, k = 4)
grades1dat<-cbind(grades1dat, memb1)
colMeans(grades1dat[memb1 == 1, 3:7, drop = FALSE])
colMeans(grades1dat[memb1 == 2, 3:7, drop = FALSE])
colMeans(grades1dat[memb1 == 3, 3:7, drop = FALSE])
colMeans(grades1dat[memb1 == 4, 3:7, drop = FALSE])





##### GGPlot, some fancier functions on R #####
## https://ggplot2.tidyverse.org/reference/
## Guidelines for more ggplot2 functions
poverty<-read_excel("poverty.xls", sheet = 1)
poverty<-as.data.frame(poverty)
poverty$gnp<-as.numeric(poverty$gnp)
poverty <- poverty[!is.na(poverty$gnp),]
poverty <- cbind(poverty, gnpcat = ifelse(poverty$gnp < 1690, "Low GNP", "High GNP"))

ggplot(poverty, aes(x = lexpm, y = lexpf))

ggplot(poverty, aes(x = lexpm, y = lexpf)) + geom_point(size = 1.5)
ggplot(poverty, aes(x = lexpm, y = lexpf)) + geom_point(size = 3, alpha = 0.5)

ggplot(poverty, aes(x = lexpm, y = lexpf, shape = as.factor(region),
	colour = as.factor(region))) + geom_point()
ggplot(poverty, aes(x = lexpm, y = lexpf, shape = as.factor(region), 
	colour = as.factor(region))) + geom_point() + 
	scale_shape_manual(values = 2:7) + scale_colour_brewer(palette = "Set1")




## For shapes 21, 22, 23, 24, 25, you can use "fill" to fill the shape with a color based on category
ggplot(poverty, aes(x = lexpm, y = lexpf, shape = gnpcat, colour = gnpcat)) + 
	geom_point() + scale_shape_manual(values = 21:22) + 
	scale_colour_brewer(palette = "Set1")
ggplot(poverty, aes(x = lexpm, y = lexpf, shape = gnpcat, fill = as.factor(region))) + 
	geom_point() + scale_shape_manual(values = 21:22) + 
	scale_fill_manual(values = c(NA, "red", "yellow","blue","purple","black"), guide = guide_legend(override.aes = list(shape = 21)))




## You can use "fill" to fill the shape with a color based on a third numeric variable
ggplot(poverty, aes(x = lexpm, y = lexpf, fill = deathrt)) + 
	geom_point(shape = 23, size = 3) + scale_fill_gradient(low = "red", high = "blue", breaks = seq(0,25,5), guide = guide_legend())



## You can change the size of the shape based on a third numeric variable
ggplot(poverty, aes(x = lexpm, y = lexpf, shape = as.factor(region), size = deathrt)) +
	geom_point() + scale_size_area()
ggplot(poverty, aes(x = lexpm, y = lexpf, shape = as.factor(region), size = deathrt)) +
	geom_point() + scale_size_continuous(range = c(0.1,10))




##### Labels #####
ggplot(poverty, aes(x = lexpm, y = lexpf, fill = deathrt)) + 
	geom_point(shape = 23, size = 3) + scale_fill_gradient(low = "red", high = "blue", breaks = seq(0,25,5), guide = guide_legend()) +
	annotate("text",x = 45, y = 80, label = "Test label")
ggplot(poverty, aes(x = lexpm, y = lexpf, fill = deathrt)) + 
	geom_point(shape = 23, size = 3) + scale_fill_gradient(low = "red", high = "blue", breaks = seq(0,25,5), guide = guide_legend()) +
	geom_text(aes(label = country), size = 3)
library(ggrepel)
ggplot(poverty, aes(x = lexpm, y = lexpf, fill = deathrt)) + 
	geom_point(shape = 23, size = 3) + scale_fill_gradient(low = "red", high = "blue", breaks = seq(0,25,5), guide = guide_legend()) +
	geom_text_repel(aes(label = country), size = 3)
ggplot(poverty, aes(x = lexpm, y = lexpf, fill = deathrt)) + 
	geom_point(shape = 23, size = 3) + scale_fill_gradient(low = "red", high = "blue", breaks = seq(0,25,5), guide = guide_legend()) +
	geom_label_repel(aes(label = country), size = 3)





##### Linear Regression Line #####

## Adding a linear regression line with 99% confidence region (by default, it's 95%)
ggplot(poverty, aes(x = lexpm, y = lexpf)) + 
	geom_point(shape = 23, size = 3) + stat_smooth(method = lm, level = 0.999999999)
## Higher confidence region will lead to more shading (in order to cover more area)

## No confidence region
ggplot(poverty, aes(x = lexpm, y = lexpf)) + 
	geom_point(shape = 23, size = 3)+ stat_smooth(method = lm, se = F)





##### Going back to our models above #####
## We can use GG Plot to graph our plots and models
## However, we'll need to look at adding the line (geom_line) closely
## Note that we cannot use var1seq as we previously did

randomdat <- read.csv("randomdat.csv", header = T)

m4<-lm(var3~var1+I(var1^2)+I(var1^3), data=randomdat)
m4_predict<-predict(m4,list(var1=randomdat$var1))

## m4_predict must be the same length as the rest of your data in order to use GG Plot



## Let's generate a new set of predicted values using the actual var1s
## We can use the var1's that we already have OR we can use generate
## the numbers using:
## >> seq(min,max, increment)
## For example:
## >> summary(randomdat$var1)		## To figure out my min and max
## >> var1seq2 <- seq(-45,210,0.5)


var1seq2 <- randomdat$var1			## For now, I'll use the var1 values
m4_predict2<-predict(m4,list(var1=randomdat$var1))


ggplot(randomdat, aes(x = var1, y = var3, colour = var8, shape = var8)) + 
	geom_point() + geom_line(aes(x=var1seq2 ,y=m4_predict2))



## to get just one line, change the default
ggplot(randomdat, aes(x = var1, y = var3, colour = var8, shape = var8)) + 
	geom_point() + geom_line(aes(x=var1seq2 ,y=m4_predict2, colour = NA, shape = NA))



## Note that because we specified a separation by color based on var8,
## we get three regression lines. The lines mostly overlap, but not perfectly.
## Even though we are using the same model for all three lines, the model drawn
## is based on the predicted values from var1



## One way to address this and get one line only--
## move the color and shape separation into geom_point()
## Earlier we ran:
## >> var1seq2 <- randomdat$var1
## >> m4_predict2<-predict(m4,list(var1=randomdat$var1))
ggplot(randomdat, aes(x = var1, y = var3)) + 
	geom_point(aes(colour = var8, shape = var10)) + 
	geom_line(aes(x=var1seq2 ,y=m4_predict2))


## In GG Plot, we can also have geom_smooth() do the modeling work for us.
ggplot(randomdat, aes(x = var1, y = var3)) + 
	geom_point(aes(colour = var8, shape = var8)) + 
	geom_smooth(method = "lm", formula = y~x+I(x^2)+I(x^3))
	## notice that in geom_smooth, I can use x because it's referring to the default "x = var1"



## Again, if we put "colour = var8" in the main ggplot(), we will get 3 lines
ggplot(randomdat, aes(x = var1, y = var3, colour = var8, shape = var8)) + 
	geom_point() + 
	geom_smooth(method = "lm", formula = y~x+I(x^2)+I(x^3))



##### Multiple regression lines (only for categorical) #####
ggplot(poverty, aes(x = lexpm, y = lexpf, fill = gnpcat)) + 
	geom_point(shape = 23, size = 3) + geom_smooth(method = "lm")

## Doesn't work for numeric
ggplot(poverty, aes(x = lexpm, y = lexpf, fill = gnp)) + 
	geom_point(shape = 23, size = 3) + geom_smooth(method = "lm")


m7_predictwith <- predict(m7,list(var1=randomdat$var1, var9 = rep("With XYZ",299)))
m7_predictwout <- predict(m7,list(var1=randomdat$var1, var9 = rep("Without XYZ",299)))
ggplot(randomdat, aes(x = var1, y = var3)) + 
	geom_point(aes(colour = var8, shape = var8)) + 
	geom_line(aes(x=var1seq2 ,y=m7_predictwith), color = 'red', lty = 2) + 
	geom_line(aes(x=var1seq2 ,y=m7_predictwout), color = 'black', lty = 3)







##### Balloon Plot #####
poverty$value <- 1	## add value = 1 to say this is "one" country
povagg <- aggregate(value~region*gnpcat, data=poverty, FUN = length)
## FUN = length means count all of the "values"
ggplot(povagg, aes(x = region, y=gnpcat, size = value)) + geom_point() + scale_size_continuous(range = c(1,20))
ggplot(povagg, aes(x = region, y=gnpcat, size = value)) + geom_point(pch=21) + scale_size_continuous(range = c(1,16)) + xlim(0,7) + geom_text(aes(label = value), size = 3)





##### GG Plot Histogram #####
ggplot(poverty, aes(x=lexpm, fill = gnpcat)) + geom_histogram(position = "stack", alpha = 0.4, bins = 20)








##### GG Plot Density Graphs #####
ggplot(poverty, aes(x=lexpm, fill = gnpcat)) + geom_density(alpha=0.4)
ggplot(poverty, aes(x=lexpm, fill = gnpcat)) + geom_density(alpha=0.4) + facet_grid(region~.)







##### GG Plot Density Graphs #####
ggplot(poverty, aes(x=lexpm, fill = gnpcat)) + geom_freqpoly(binwidth = 3)








##### Other graphs... #####
## For a more detailed list of graphing functions, visit https://ggplot2.tidyverse.org/reference/
## geom_boxplot()
## geom_violin()
## geom_dotplot()
## coord_flip       Changes the x and y axes

