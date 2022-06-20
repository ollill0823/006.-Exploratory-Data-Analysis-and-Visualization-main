

install.packages(reshape2)
install.packages(corrplot)
install.packages(GGally)
install.packages(scatterplot3d)
install.packages(car)
install.packages(rgl)

library(reshape2)
library(ggplot2)
library(corrplot)
library(GGally)
library(scatterplot3d)
library(car)
library(rgl)


mydata <- data.frame(var1 = c(1,2,3,4), var2 = c(5,6,7,8))
mydata
newdata <- data.frame(var2 = c(11,12,13,14), var1 = c(15,16,17,18))	## note that I swapped my columns
newdata
bothdata <- rbind(mydata,newdata)	## rbind() will bind the data by rows
bothdata					## if it is a data frame, then it will match the variables
baddata <- cbind(mydata,newdata)	## cbind() will bind the data by columns 
baddata	

bothdata <- rbind(mydata, newdata)



(authors <- data.frame(
  ## I(*) : use character columns of names to get sensible sort order
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4))))


(authors2 <- data.frame(
  ## I(*) : use character columns of names to get sensible sort order
  surname = I(c("C", "A", "B", "E", "D")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4))))


(authorN2 <- within(authors2, Zoo <- surname, rm(surname)))



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


books
authors

(new <- merge(books, authorN))

(m1 <- merge(books, authors, by.x = 'name', by.y = 'surname'))


(m0 <- merge(authorN, books))
(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname"))



(m3 <- merge(authorN, books, all = T))
(m4 <- merge(authors, books, by.x = "surname", by.y = "name", all.x = T))
(m5 <- merge(books, authors, by.x = "name", by.y = "surname", all.x = T))
(m6 <- merge(authors, books, by.x = "surname", by.y = "name", all.y = T))



mydata2 <- data.frame(id = c(111,112,113,114,115), var1_2019 = c(1,2,3,4,5), var2_2019=c(5,6,7,8,9))
newdata2 <- data.frame(id = c(113,115,111,112,114), var1_2020 = c(77,43,65,21,54), var2_2020=c(27,23,25,36,76))


mydata2
newdata2

(merge <- merge (mydata2, newdata2))



mydata3 <- data.frame(id = c(111,112,113,114,115,116), var1_2019 = c(1,2,3,4,5,6), var2_2019=c(5,6,7,8,9,10))
newdata3 <- data.frame(id = c(113,115,111,112,114,117), var1_2020 = c(77,43,65,21,54,44), var2_2020=c(27,23,25,36,76,33))


mydata3
newdata3

(merge3 <- merge (mydata3, newdata3, all = T))


mydata4 <- data.frame(id = c(111,112,113,114,115,115), attempt = c(1, 1, 1, 1, 1, 2), var1_2019 = c(1,2,3,4,5,6), var2_2019=c(5,6,7,8,9,10))
newdata4 <- data.frame(id = c(113,115,111,112,114,115),attempt = c(1, 1, 1, 1, 1, 2), var1_2020 = c(77,43,65,21,54,44), var2_2020=c(27,23,25,36,76,33))

mydata4
newdata4

(merge4 <- merge (mydata4, newdata4, by = 'id', all = T))
(merge4_2 <- merge (mydata4, newdata4, all = T))
(merge4_3 <- merge (mydata4, newdata4, by = 'id', all.x = T))


alldata <- merge(mydata3,newdata3,by="id", all = T)
write.csv(alldata,file = "superimportantdata.csv", row.names = F)
write.csv(alldata,file = "superimportantdata2.csv")


rm(list= ls())


(reshapedata <- read.csv("reshapeexample.csv",header=T))

reshapedata2 <- reshapedata[reshapedata$gender == 'M', c('scoreB', 'scoreC')]

which(reshapedata$gender == 'M')

reshapedata[which(reshapedata$gender == "M"), c("scoreA", "scoreB")]

reshapedata[reshapedata$gender == 'M', c('scoreA', 'scoreB')]

subset(reshapedata, gender == 'M', select = c('scoreA', 'scoreB'))

reshapedata[reshapedata$gender == 'M' & reshapedata$month == 'Jan', c('scoreA', 'scoreB')]



(longdata <- melt(reshapedata, id=c("id","gender","month")))
(longdata <- melt(reshapedata, 1:3))

names(longdata)[4:5] <- c('test', 'score')

head(longdata)
levels(longdata$test) <- c('A', 'B', 'C')



## You can select which scores to keep (in this example, we have scoreA and scoreB only, no scoreC)
longdata2 <- melt(reshapedata, id=c("id","gender","month"), 
                  measure = c("scoreA","scoreB"),  value.name = "score")
levels(longdata2$test) <- c("A","B")


windsdata <- dcast(reshapedata, id + gender~month, value.var = 'scoreA')


test <- read.csv('reshapeexample-2.csv')

windsdata_2 <- dcast(test, id + gender~month, value.var = 'scoreA')


wideAdata <- dcast(reshapedata, id + gender~month, value.var = 'scoreA')
wideBdata <- dcast(reshapedata, id + gender~month, value.var = 'scoreB')
wideCdata <- dcast(reshapedata, id + gender~month, value.var = 'scoreC')

wideAdata <- wideAdata[,c(1,2,4,3,5)]
wideBdata <- wideBdata[,c(1,2,4,3,5)]
wideCdata <- wideCdata[,c(1,2,4,3,5)]

wideABdata <- merge(wideAdata,wideBdata, by = c('id', 'gender'), suffixes = c('1','2'))

(widedata <- merge(wideABdata,wideCdata, by = c('id', 'gender'), suffixes = c('1', '3')))

names(widedata)[9:11] <- c('Jan3', 'Feb3', 'Mar3')

wideAdata2 <- dcast(longdata, id + gender~month + test, value.var = 'score')
wideAdata2


library(corrplot)


randomdat <- read.csv("randomdat.csv", header = T)

## rename "GroupC" as "Group C" under var8
levels(randomdat$var8)[levels(randomdat$var8)=="GroupC"] <- "Group C"

## remove observation 123 because var5 is an outlier
randomdat<-randomdat[-123,]

randomdat$var9[which(is.na(randomdat$var9))]<-"With XYZ"


cor(randomdat[,c(1:7,11)])

cormat <- cor(randomdat[,c(1:7,11)])
corrplot(cormat)

## One other type of corrgram... -- this uses GGally library
ggscatmat(randomdat,columns = c(1:7,11))


library(GGally)

ggscatmat(randomdat, columns = c(1:7,11))


########## Data Example with Interactions ##########
schdat <- read.csv("schooldata.csv", header = T)
head(schdat)
summary(schdat)
 

table(schdat$id)

mlog1 <- glm(ontimegrad~gender+finaid+mcollege+studytime+hsrating+pretest, data = schdat, family = binomial())
summary(mlog1)
