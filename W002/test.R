
temp <- read.csv('temphr.csv')

temp$color[ temp$gender == 1 ] <- 'Green'
temp$color[ temp$gender == 2 ] <- 'Blue'


boxplot(temp$temperature~temp$gender, col = 'green',
        main = "Temperature by Gender", cex = 2, ylab = "Temperature (F)", xlab = "Gender" )


dotchart (order(temp$temperature), col = c(temp$color))


temp$genderlvls <- factor(temp$gender, levels = c(1,2), labels = c('female', 'male'))

boxplot(temp$temperature~temp$genderlvls, col = 'green',
        main = "Temperature by Gender", cex = 2, ylab = "Temperature (F)", xlab = "Gender" )


temp[131,1] = 98.1
temp[131,2] = 2
temp[131,3] = 74
temp$color[131] = 'Blue'
temp$genderlvls[131] = 'male' 

temp[nrow(temp)+1,1] <- 97.8
temp[nrow(temp)+1,2] <- 1
temp$color[nrow(temp)+1] <- 'Green'
temp$genderlvls[nrow(temp)+1] <- 'male'

temp <- temp[1:131,]

n <- nrow(temp) +1
temp[n,1] <- 97.8
temp[n,2] <- 1
temp$color[n] <- 'Green'
temp$genderlvls[n] <- 'Male'


temp$genderlvls <- as.character(temp$genderlvls)

temp[,4] <- as.factor(temp[,4])
temp[,5] <- as.factor(temp[,5])
str(temp)
summary(temp)

temp[,4] <- as.character(temp[,4])
temp[,5] <- as.factor(temp[,5])


temp<- temp[1:131,]
nrow(temp)
summary(temp)

boxplot(temp$temperature~temp$genderlvls, col = 'green',
        main = "Temperature by Gender", cex = 2, ylab = "Temperature (F)", xlab = "Gender" )

temp[,5] <- as.character(temp[,5])
temp[,5] <- as.factor(temp[,5])


tempa <- temp[,c(1,2,5,4,3)]
tempb <- temp
tempa <- temp

temp <- read.csv('temphr.csv')

tempa <- tempa[order(tempa$temperature),]
tempa <- tempa[order(tempa$temperature, decreasing = T),]
tempa <- tempa[seq(130,1,-1),]
tempa <- tempa[seq(130,5,-10),]

tempmale <- temp[temp$genderlvls == 'male', ]

heartlvls <- c()
heartlvls[temp$heartrate <='70'] <- 'Low' 
heartlvls[temp$heartrate >'70' & temp$heartrate <='80'] <- 'Middle'
heartlvls[temp$heartrate >='80'] <- 'High' 

tempc <- cbind(temp, heartlvls)

remove(heartlvls)
remove(n)
remove(tempc$heartlvls)

tempc$heartlvls <- factor(tempc$heartlvls, levels = c('Low', 'Middle', 'High'))

##??? PRACTICE!
##	Starting a new with "temphr.csv", add the following individuals to your dataset
##	(a) Add a female(1) with temperature 97.5 and heart rate 70
##	(b) Add a male(2) with heart rate 66 and unknown temperature
##	(c) Add a "other" gender (new category) with HR73 & temp 98.2
##	    For (c), the "gender" column can be either NA or 3

temper <- read.csv('temphr.csv')
n <- nrow(temper)+1
temper[n,1] <- 97.5
temper[n,2] <- 1
temper$heartrate [n] = 70

n2 <- n+1
temper[n2,1] <- NA
temper[n2,2] <- 2
temper$heartrate [n2] = 66

n3 <- n2+1
temper[n3,1] <- 98.2
temper[n3,2] <- 3
temper$heartrate [n3] = 73

newobs <- data.frame(temperature= 98,gender = 1,heartrate= 60)
temper <- rbind(temper, newobs)


temper$temperature[is.na(temper$temperature)] <- 100.1

##??? PRACTICE!
##	Generate a new variable "templvl" such that it is
##	low if temp < 97.6, middle if>=97.6 and <98.9, and high otherwise
##	You can start with a brand new "temphr.csv" dataset

temper$templv1[temper$temperature < '97.6'] <- 'low'
temper$templv1[temper$temperature >= '97.6' & temper$temperature < '98.9'] <- 'middle'
temper$templv1[temper$temperature >= '98.9'] <- 'high'

temper$templv1 <- as.factor(temper$templv1)
summary(temper)
temper$templv1 <- factor(temper$templv1, levels = c('low', 'middle', 'high'))
temper$templv1 <- as.character(temper$templv1)
summary(temper)



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


library(lattice)
bwplot(temperature~gender, data = temper, xlab = 'Gender', ylab = 'Temperature')

## Boxplot separated by x1 (gender) and x2 (low/middle/high heartrate)
bwplot(temperature ~ gender, data = temphrdata, xlab = "Gender", ylab = "Temperature")


bwplot(temphrdata$temperature~temphrdata$gender, data = temphrdata, xlab = 'Gender', ylab = 'Temperature')


bwplot(temphrdata$temperature~temphrdata$gender, data = temphrdata, xlab = 'Gender', ylab = 'Temperature')



bwplot(temperature~ gender | heartratelvl, data = temphrdata, layout = c(3,1), xlab = 'Gender', ylab = 'Temperature')


bwplot(temperature~ gender | heartratelvl, data = temphrdata, layout = c(1,3), xlab = 'Gender', ylab = 'Temperature')


bwplot(temperature~ gender | heartratelvl + templvl, data = temphrdata, layout = c(3,3), xlab = 'Gender', ylab = 'Temperature')


bwplot(temperature~ gender | heartratelvl + templvl, data = temphrdata,  layout = c(9,1), xlab = 'Gender', ylab = 'Temperature')


bwplot(temperature~ gender, data = temphrdata, panel = panel.violin,
       xlab = 'Gender', ylab = 'Temperature' )


library(multcomp)

head(sbp)

stem(temper$heartrate,scale = 3 )
stem(temper$heartrate,scale = 2 )
stem(temper$heartrate,scale = 1 )
stem(temper$heartrate,scale = 0.5 )
stem(temper$heartrate,scale = 0.1 )


hist(sbp$sbp)
hist(sbp$sbp, breaks = c(105,120,135,150,165,180,195))
hist(sbp$sbp, breaks = 20)
hist(sbp$sbp, breaks = seq(140,170,20))


hist(sbp$sbp, breaks = 20, label = T, col = c('red', 'skyblue'), 
     main = ' pressure', xlab = 'age', ylim = c(-1, 11))


hist(sbp$sbp[sbp$gender == "female"], col=rgb((0:15)/15,0,0,0.5),
     ylim = c(0,15),main = "Systolic Blood Pressure",
     xlab = "Systolic Blood Pressure", xlim = c(100,200))

hist(sbp$sbp[sbp$gender == "female"], col=rgb(0,(0:15)/15,0,0.5),
     ylim = c(0,15),main = "Systolic Blood Pressure",
     xlab = "Systolic Blood Pressure", xlim = c(100,200))

hist(sbp$sbp[sbp$gender == "female"], col=rgb(0,0,(0:15)/15,0.5),
     ylim = c(0,15),main = "Systolic Blood Pressure",
     xlab = "Systolic Blood Pressure", xlim = c(100,200))


hist(sbp$sbp[sbp$gender == "female"], col=rgb(0, 1, 0, 0.5),
     ylim = c(0,15),main = "Systolic Blood Pressure",
     xlab = "Systolic Blood Pressure", xlim = c(100,200))

hist(sbp$sbp[sbp$gender == "male"], col=rgb(1, 0, 0, 0.2), add = T,
     ylim = c(0,15),main = "Systolic Blood Pressure",
     xlab = "Systolic Blood Pressure", xlim = c(100,200))

legend('bottom', c('female', 'male'), 
          col= c(rgb(0, 1, 0, 0.5),rgb(1, 0, 0, 0.2) ), 
          lwd = 5 )



dens1 <- density(sbp$sbp)
plot(dens1, xlim = c(90,210), main = 'pressure')

dens2 <- density(sbp$sbp, bw = 4)
plot(dens2, xlim = c(90,210), main = 'pressure')

dens2 <- density(sbp$sbp, bw = 1)
plot(dens2, xlim = c(90,210), main = 'pressure')


salaries <- read.csv("salaries.csv", header=T)
summary(salaries)
ranktab <- table(salaries$rank)
barplot(ranktab, col = c('red','green', 'blue'), main = "Faculty by Rank" )

barplot(ranktab, names.arg= c('1','2','3'), col = c('red','green', 'blue'), 
        main = "Faculty by Rank" ) 

barplot(ranktab, col = c('red','green', 'blue'), 
        main = "Faculty by Rank" ) 

arrange <- aggregate(salary~rank, data = salaries, FUN = mean )
arrange

arrange <- aggregate(salary~rank, data = salaries, FUN = max )
arrange

arrange <- aggregate(salary~rank, data = salaries, FUN = sum )
arrange

together <- table(salaries$rank, salaries$sex)
together

check <- aggregate(salary~rank, data = salaries, FUN = max )
check


check1 <- aggregate(salary~sex, data = salaries, FUN = max )
check1

combine <- cbind(check, check1)

rankgen <- table(salaries$rank,salaries$sex)
rankgen
barplot(rankgen,ylab = "Count", names.arg = c("Female","Male"),
        main = "Faculty by Rank and Sex",col = c("red","black","blue"))
legend("topleft",c("Prof","Asst","Assoc"),text.col=c("blue","black","red"))

barplot(rankgen,ylab = "Count", names.arg = c("Female","Male"),
        main = "Faculty by Rank and Sex",col = c("red","black","blue"), beside=T)
legend("topleft",c("Prof","Asst","Assoc"),text.col=c("blue","black","red"))


rankgen
t(rankgen)		##gets the transpose

barplot(t(rankgen),ylab = "Count", names.arg = c("Female","Male"),
        main = "Faculty by Rank and Sex",col = c("red","black","blue"))
legend("topleft",c("Prof","Asst","Assoc"),text.col=c("blue","black","red"))



barplot(t(rankgen),ylab = "Count", names.arg = c("Female","Male"),
        main = "Faculty by Rank and Sex",col = c("red","black","blue"), beside=T)
legend("topleft",c("Prof","Asst","Assoc"),text.col=c("blue","black","red"))




grpsal <- aggregate(salary ~ sex * rank, data = salaries, FUN = mean)
grpsal


barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
        names.arg = c("Assoc","","Asst","","Prof",""), col = c("red","blue"))
barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
        names.arg = c("Assoc","","Asst","","Prof",""), space = 1.5, col = c("red","blue"))
barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
        names.arg = c("Assoc","","Asst","","Prof",""), space = c(1,0,1,0,1,0), col = c("red","blue"))
barplot(grpsal$salary, ylab = "Average Salary", main = "Faculty Salaries",
        names.arg = c("Assoc","","Asst","","Prof",""), horiz=T, col = c("red","blue"))

