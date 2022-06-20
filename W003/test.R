library(readxl)


## change directory to 
R_LIBS_SITE="C:\\R-4.1.3\\library"

dir()

poverty <- read_excel("poverty.xls", sheet = 1)
head(poverty)
summary(poverty)
poverty_legend <- read_excel("poverty.xls", sheet = 'Legend')
remove(poverty)



summary(poverty)


poverty$gnp <- as.numeric(poverty$gnp)
summary(poverty)


poverty$country <- as.factor(poverty$country)
summary(poverty)


is.na(poverty$gnp)

which(is.na(poverty$gnp))

which(!is.na(poverty$gnp))

poverty[which(is.na(poverty$gnp)),]

poverty$gnp <- as.character(poverty$gnp)

hist(poverty$birthrt[which(is.na(poverty$gnp))], xlim = c(8,60), breaks=seq(8,59,6), col = rgb(1,0,0,0.25),freq = F)
hist(poverty$birthrt[which(!is.na(poverty$gnp))], add = T, breaks=seq(8,59,6),col = rgb(0,0,1, 0.25),freq=F)


hist(poverty$lexpm[which(is.na(poverty$gnp))], xlim = c(30,80), breaks=seq(30,80,5), col = rgb(1,0,0,0.25),freq = F)
hist(poverty$lexpm[which(!is.na(poverty$gnp))], breaks=seq(30,80,5), add = T,col = rgb(0,0,1, 0.25),freq=F)


poverty$gnp[61] <- 10
poverty[59:65,]
poverty$gnp[61] <- NA
poverty[59:65,]
poverty$gnp[is.na(poverty$gnp)] <- 0
poverty$gnp
is.na(poverty$gnp)					## At this point, we don't have any NAs
poverty$gnp[poverty$gnp == 0] <- NA			## "==" looks for poverty$gnp such that it is equal to 0
poverty$gnp									## "=" make it equal to


poverty$gnp[is.na(poverty$gnp) & poverty$region == 1] <- mean(poverty$gnp[poverty$region == 1], na.rm = T)
poverty$gnp[poverty$gnp == mean(poverty$gnp)] <- NA

poverty$gnp[is.na(poverty$gnp) & poverty$region == 1] <- mean(poverty$gnp[poverty$region == 1], na.rm = T)
poverty$gnp[is.na(poverty$gnp) & poverty$region == 2] <- mean(poverty$gnp[poverty$region == 2], na.rm = T)
poverty$gnp[is.na(poverty$gnp) & poverty$region == 3] <- mean(poverty$gnp[poverty$region == 3], na.rm = T)
poverty$gnp[is.na(poverty$gnp) & poverty$region == 4] <- mean(poverty$gnp[poverty$region == 4], na.rm = T)
poverty$gnp[is.na(poverty$gnp) & poverty$region == 5] <- mean(poverty$gnp[poverty$region == 5], na.rm = T)
poverty$gnp[is.na(poverty$gnp) & poverty$region == 6] <- mean(poverty$gnp[poverty$region == 6], na.rm = T)
poverty$gnp

str(poverty)


poverty$gnp[c(4,8)] <- NA
poverty$gnp
which(is.na(poverty$gnp))


poverty <- poverty[!is.na(poverty$gnp),]
poverty$gnp


library(readxl)
poverty<-read_excel("poverty.xls", sheet = 1)
poverty<-as.data.frame(poverty)
head(as.numeric(poverty$gnp))			## not necessary, but just checking-- it looks good
poverty$gnp<-as.numeric(poverty$gnp)
poverty <- poverty[!is.na(poverty$gnp),]	## Exclude missing GNP countries (since there's only 6)

library(readxl)
econdata <- read_xls("econdata.xls")
econdata <- as.data.frame(econdata)
econdata$consumerconfidence <- as.numeric(econdata$consumerconfidence)
econdata  <- econdata[!is.na(econdata$consumerconfidence),]
econdata$consumerconfidence


library(multcomp)

stem(poverty$birthrt, scale = 2)


hist(poverty$birthrt, col = c("red","skyblue"), breaks = 20,
     main = "Birth Rate", xlab = "Birth Rate", freq = F, xlim = c(-3,70))
densbirthrt <- density(poverty$birthrt, bw = 4)
lines(densbirthrt)



hist(poverty$birthrt[poverty$region == 1], col=rgb(1,0,0,0.5),
     main = "Birth Rate",xlab = "Birth Rate")
hist(poverty$birthrt[poverty$region == 4], col=rgb(0,0,1,0.5),add=T)
legend("topright", c("Eastern Europe", "Middle East"), 
       col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), lwd=10)

## UH-OH! Our graph is cut off... use xlim and ylim to adjust
## breaks WIDTHS also do not match across the two graphs -- MISLEADING


hist(poverty$birthrt[poverty$region == 1], col=rgb(1,0,0,0.5),
     main = "Birth Rate",xlab = "Birth Rate", xlim = c(10,50),
     breaks=seq(10,50,2))
hist(poverty$birthrt[poverty$region == 4], col=rgb(0,0,1,0.5),
     breaks=seq(10,50,2),add=T)
legend("topright", c("Eastern Europe", "Middle East"), 
       col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pch=15)
## Instead of guessing the xlim, you can use summary() to help 
## summary(poverty$birthrt[poverty$region == 4])
reg3gnpdens <- density(poverty$gnp[poverty$region == 3])
plot(reg3gnpdens , xlim = c(0,38000),main = "GNP")



hist(poverty$gnp[poverty$region == 3], main = "GNP Histogram with Density",
     breaks = 10, las = 1,cex.axis = 0.8, freq = F, xlab = "GNP")
lines(reg3gnpdens, lwd = 2)

regiontab <- table(poverty$region)



gnpcat <- c()
gnpcat[poverty$gnp < 1690] <- "low"
gnpcat[poverty$gnp >= 1690] <- "high"
poverty<-cbind(poverty,gnpcat)
remove(gnpcat)
poverty$gnpcat <- factor(poverty$gnpcat,levels = c("low","high"))



## (1) Using table() to get count - one variable
regiontab <- table(poverty$region)	## table with counts of each category (9 countries in region 1, 12 in region 2, ...)
regiontab
barplot(regiontab)
barplot(regiontab , ylab = "Count", col = "red", main = "Number of Countries in Each Region")


regionnames<-c()
regionnames[poverty$region == 1] <- "E. Europe"
regionnames[poverty$region == 2] <- "Latin America"
regionnames[poverty$region == 3] <- "Developed Countries"
regionnames[poverty$region == 4] <- "Middle East"
regionnames[poverty$region == 5] <- "Asia"
regionnames[poverty$region == 6] <- "Africa"
poverty<-cbind(poverty,regionnames)
remove(regionnames)			## remove the extra copy



regiontab2 <- table(poverty$regionnames)
regiontab2
##regiontab2 <-regiontab2[c(3,2,1,6,5,4)]		##changes the column order
barplot(regiontab2 , ylab = "Count", col = "red", main = "Region Count")



## (1) Using table() to get count - two variables
regiontab2 <- table(poverty$regionnames, poverty$gnpcat)	## table with counts of each category
regiontab2 
barplot(regiontab2,ylab = "Count", names.arg = c("Low GNP","High GNP"),
        main = "Count by High/Low GNP and Region", ylim=c(0,80),
        col = c("red","green","yellow","gray","black","blue"))
legend("topleft",c("Africa", "Asia", "Developed Countries", "E. Europe", "Latin America", "Middle East"),
       text.col=c("red","green","yellow","gray","black","blue"))

barplot(regiontab2,ylab = "Count", names.arg = c("Low GNP","High GNP"),
        main = "Count by High/Low GNP and Region", ylim=c(0,25),
        col = c("red","green","yellow","gray","black","blue"), beside = T)




## Average birth rate by region
avgbirthrt <-aggregate(birthrt~regionnames, data=poverty,FUN = mean)		##"FUN = median" will give you the median
avgbirthrt 		## treat this as a data set
barplot(avgbirthrt$birthrt , ylab = "Avg Birth Rate",
        names.arg = avgbirthrt$regionnames,
        col = "red", main = "Average Birth Rate by Region")


barplot(avgbirthrt$birthrt , ylab = "Avg Birth Rate",
        names.arg = avgbirthrt$regionnames,
        col = "red", main = "Average Birth Rate by Region", space = 0.9)	## space changes the bar widths



## Average birth rate by region AND high/low GNP
avgbirthrt2 <- aggregate(birthrt~regionnames * gnpcat, data=poverty,FUN = mean)
avgbirthrt2			## this is like a dataset
## drop = F counts zero data (do not drop cases with 0 countries -- in other words, Low Developed)
avgbirthrt2 <- aggregate(birthrt~regionnames* gnpcat, data=poverty,drop = F,FUN = mean)
avgbirthrt2


t(avgbirthrt2)
barplot(avgbirthrt2$birthrt , ylab = "Avg Birth Rate",
        names.arg = avgbirthrt2$regionnames,
        col = "red", main = "Average Birth Rate by Region")


## Low and high (same country) next to each other
avgbirthrt3 <-aggregate(birthrt ~ gnpcat * regionnames, data=poverty,drop = F,FUN = mean)

barplot(avgbirthrt3$birthrt , ylab = "Avg Birth Rate",
        names.arg = avgbirthrt3$regionnames,
        col = "red", main = "Average Birth Rate by Region")
barplot(avgbirthrt3$birthrt , ylab = "Avg Birth Rate",
        names.arg = avgbirthrt3$regionnames,
        col = c("red", "blue"),
        main = "Average Birth Rate by Region")
legend("topright",c("Low GNP", "High GNP"),text.col=c("red","blue"))

## Eliminate the gap
barplot(avgbirthrt3$birthrt , ylab = "Avg Birth Rate",
        names.arg = c("Africa", "", "Asia","", "Dev. Countries", "", "E. Europe", "", "Lat. Amer.", "", "Middle East", ""),
        col = c("red", "blue"),
        main = "Average Birth Rate by Region",
        space = c(1,0))



##??? PRACTICE!
##	Using the poverty dataset with NA rows removed,
## 	create an overlapping histogram for infant mortality (infmort)
##	for Asia (region == 5) and Africa (region == 6)


hist(poverty$infmort[poverty$region == 5], 
     col=rgb(1,0,0,0.5), main = "infant mortality",xlab = "infant mortality", 
     xlim = c(5,185), breaks=seq(5,185,10), ylim = c(0,8))

hist(poverty$infmort[poverty$region == 6], 
     col=rgb(0,1,0,0.5), 
     add = T, breaks=seq(5,185,10))
legend ('topright', c('Asia', 'Africa'), 
          col = c(rgb(1,0,0,0.5),rgb(0,1,0,0.5)), pch = 22, lwd = 8)


##??? PRACTICE!
##	Using the poverty dataset with NA rows removed,
## 	create an overlapping histogram for infant mortality (infmort)
##	for Asia (region == 5) and Africa (region == 6)
##	Add 2 density curves for infant mortality (1 for Asia, 1 for Africa)
##	Remember, you'll need the argument freq = F in your histograms

hist(poverty$infmort[poverty$region == 5], 
     col=rgb(1,0,0,0.5), main = "Density of infant mortality",xlab = "infant mortality", 
     xlim = c(5,185), breaks=seq(5,185,10), ylim = c(0,0.03), freq = F)
hist(poverty$infmort[poverty$region == 6], 
     col=rgb(0,1,0,0.5), freq = F,
     add = T, breaks=seq(5,185,10))
dens1 <- density (poverty$infmort[poverty$region == 5], bw = 4)
lines(dens1, lty = 2, lwd = 2, col = 'red')
dens2 <- density (poverty$infmort[poverty$region == 6], bw = 4)
lines(dens2, lty = 1, lwd = 2, col = 'green')
legend ('topright', c('Asia', 'Africa'), 
        col = c(rgb(1,0,0,0.5),rgb(0,1,0,0.5)), pch = 22, lwd = 8)



##??? PRACTICE!
##	The following three lines give you the (1) average salary based on
##	sex and rank, (2-3) the barplot for salary based on sex and rank
##	(a) Add the argument ..., space = 1.5, ... to the barplot. What happens?
##	(b) What if you change it to space = c(1,0,1,0,1,0)?
##	(c) What happens when you add ..., horiz = T, ... ?

salary <- read.csv("salaries.csv")
avesal <- aggregate (salary~sex * rank, data = salary, FUN = mean)

barplot(avesal$salary, main = 'Salaries', names.arg = avesal$sex, 
        col = c('red', 'green'), ylab = 'salaries', space = c(1.5,0))
legend (2, 120000, c('Female', 'Male'), col = c('red', 'green'), 
         lwd = 8 )

barplot(avesal$salary, main = 'Salaries', names.arg = avesal$sex, xlab = 'sex',
        col = c('red', 'green'), ylab = 'salaries', space = c(1,0,1,0,1,0))
legend (2, 120000, c('Female', 'Male'), col = c('red', 'green'), 
        lwd = 8 )

barplot(avesal$salary, main = 'Salaries', names.arg = avesal$sex, xlab = 'salaries',
        col = c('red', 'green'), ylab = 'sex', space = c(1,0,1,0,1,0),
        horiz = T)
legend (2, 120000, c('Female', 'Male'), col = c('red', 'green'), 
        lwd = 8 )
        


##??? PRACTICE!
##	Using the "salaries.csv" dataset from last week
##	a. Use table() to generate a bar plot of the count 
##		of individuals in each discipline
##	b. Use table() to generate a bar plot of the count
##		of male/females in each discipline
##	c. Use aggregate() to generate a bar plot of the average
##		yrs.service in each discipline
##	d. Use aggregate() to generate a bar plot of the average
##		yrs.service by discipline and gender

tablesex <- table(salary$sex)
tabledis <- table(salary$discipline)
aggreser <- aggregate(yrs.service~discipline, data = salary, FUN = mean)
barplot(tabledis, ylab = "Count", col = "red", main = "discipline of salaries")

barplot(tablesex, ylab = "Count", col = "green", main = "sex of salaries")

barplot(aggreser$yrs.service, ylab = "Average Years of Service", col = "blue", names.arg = aggreser$discipline)

aggreall <- aggregate(yrs.service~discipline * sex, data = salary, FUN = mean)
barplot(aggreall$yrs.service, ylab = "Average Years of Service", col = c('red', 'blue'), 
        names.arg = aggreall$discipline)
legend("topright",c("Female", "Male"),col=c("red","blue"), text.col=c("red","blue"), lwd = 8)



salaries <- read.csv("salaries.csv", header=T)
rankgen2 <- table(salaries$sex,salaries$rank)
rankgen2
rankg <- table(salaries$sex, salaries$rank, salaries$discipline)
spineplot(rankgen2,col=c("red","black","blue"),main="Faculty by Sex and Rank")

require(stats)
mosaicplot(~ gear + carb, data = mtcars, color = TRUE)
mosaicplot(~ gear + carb+ vs, data = mtcars, color = TRUE)
mosaicplot(~ gear + carb + vs + am, data = mtcars, color = 1:6, las = 1)

mtcars <- mtcars


mosaicplot(~ sex + rank, data = salaries, color = 1:6)
mosaicplot(~ sex + rank+ discipline, data = salaries, color = 1:6)


allocation <- c(40,25,18,10,7)
task <- c("study","social","games","shop","email")
taskcol <- c("red","blue","gold3","maroon","green4")


## You can let R choose the colors
pie(allocation,labels = task, main = "Time Online")

## You can also choose the colors yourself
pie(allocation,labels = task, col = taskcol, main = "Time Online")


## Pie charts show something similar to bar plots
barplot(allocation,names.arg = task, col = taskcol, main = "Time Online")


## Earlier avgsal$salary -- this won't quite make sense for pie charts, as pie charts represent the idea of 100%
avgsal <- aggregate(salary ~ rank, data = salaries, FUN = mean)
pie(avgsal$salary, labels = avgsal$rank, main = "Average Salary by Rank")

## Sum of salary by rank
sumsal <- aggregate(salary ~ rank, data = salaries, FUN = sum)
pie(sumsal$salary, labels = sumsal$rank, main = "Sum of Salary by Rank")
## Professor salaries are eating up a larger chunk of the pie (because there's more professors)



## Let's change the labels
regionlab <- paste("Region: ", names(regiontab))	##Concatenate
regionlab
pie(regiontab, labels = regionlab, main = "Region")



## The character "\n" gives the return character
## regiontab is 1-dimensional ("Africa", "Asia", ..., are the "names" and not part of the table)
## \n means new line
regionlab <- paste(names(regiontab), "\n", regiontab)
pie(regiontab, label = regionlab, main = "Number of Countries by Region")


## You can also quickly get proportions from tables
## prop.table(___ table ___)
prop.table(regiontab)
roundedtab <- round(prop.table(regiontab), 2)		##round(____, 2)
regionlab <- paste(names(regiontab), "\n", roundedtab)
pie(regiontab, label = regionlab, main = "Region")



## Pie Chart with percentages
## We're going to use the rounded proportion table above
## First, multiply by 100 to get percent
## concatenate the "%" symbol
roundedtab100 <- roundedtab * 100
regionlab <- paste(names(regiontab), "\n", roundedtab100, "%", sep = '')
## by default paste() adds in a space (" ") between each thing you're concatenating
## add sep = "" to change the separator to  nothing
pie(regiontab, label = regionlab, main = "Region")

R_LIBS_SITE="C:\\R-4.1.3\\library"
install.packages('plotrix')

library(plotrix)


pie3D(allocation,labels = task, col = taskcol, explode = 0.1,
      labelcex = 0.95, radius = 1, main = "Time Online")
fan.plot(allocation,labels = task, col = taskcol,ticks = 10,
         main = "Time Online")
fan.plot(allocation,labels = task, col = taskcol,ticks = 4,
         main = "Time Online", max.span = pi)