# read the file 'bodyfat.csv
bodyfat <- read.csv('bodyfat.csv')

## 1. Read in the bodyfat.csv data file and generate a variable 
####  "bodycat" to categorize body fat into the three categories above. 
####  Make sure all 252 observations are categorized into either athlete, 
####  average, or obese.
bodyfat$bodycat[bodyfat$bodyfat < 14 ] <- 'Athlete'
bodyfat$bodycat[bodyfat$bodyfat >= 14 & bodyfat$bodyfat <= 25 ] <- 'Average'
bodyfat$bodycat[bodyfat$bodyfat > 25 ] <- 'Obese'
bodyfat$bodycat


## 2. Using summarize to identify the four height quartiles, 
##    create a new variable "htcat" to categorize height into "short", 
##    "below average", "above average", and "tall".
summary(bodyfat$height)
bodyfat$htcat[bodyfat$height < 68.25 ] <- 'Short'
bodyfat$htcat[bodyfat$height >= 68.25 & bodyfat$height < 70.00] <- 'Below average'
bodyfat$htcat[bodyfat$height >= 70.00 & bodyfat$height < 72.25] <- 'Above average'
bodyfat$htcat[bodyfat$height >=72.25 ] <- 'Tall'
bodyfat$htcat

## 3. Create a violin plot of weight separated by bodycat. 
##    Make sure your plots show up in some kind of order that makes 
##    sense. In complete sentences, summarize what the violin plots 
##    tell you. Are the weights evenly distributed within a range for 
##    all categories? Do athletes tend to be within a certain weight 
##    range? You may use summarize() to help you. Rough estimates are 
##    also okay.

library(lattice)
bodyfat$wtcol[bodyfat$bodyfat < 14 ] <- 'hotpink'
bodyfat$wtcol[bodyfat$bodyfat >= 14 & bodyfat$bodyfat < 25 ] <- 'royalblue'
bodyfat$wtcol[bodyfat$bodyfat >= 25 ] <- 'seagreen1'
remove(bodyfat$wtcol)

names(bodyfat)

## normal
bwplot(weight~bodycat, data = bodyfat, panel = panel.violin, 
       ylab = 'pounds', col = 'hotpink', 
       ylim = c(80,400), main = "weight measurements v.s. bodyfat")

## extended1
bwplot (weight~bodycat, data = bodyfat, horizontal=FALSE, 
       panel = function(..., box.ratio) 
       {panel.violin(..., col = 'chartreuse', varwidth = T, box.ratio = 5)
        panel.bwplot(..., col='black', cex=1, pch=23, fill="deepskyblue3", box.ratio = 0.5)},
       par.settings = list(box.rectangle=list(col='black'),
       plot.symbol = list(pch=22, cex = 0.1)), ylim= c(80,400),
       main = "weight measurements v.s. bodyfat"
       )



## 4. Create a stem-and-leaf plot for weight. 
## Be sure to find an appropriate scale for the data.

library(multcomp)
stem(bodyfat$bodyfat, scale = 1)
stem(bodyfat$bodyfat, scale = 2)


## 5A.Create overlapping histograms of neck for the 
##    three body categories. For this exercise, do not use 
##    the default breaks. Use breaks that you think make sense. 
##    Remember to make sure that the first histogram is an appropriate 
##    window size so when you "add" the other graphs, those histograms 
##    aren't cut-off. Also, remember to use the same break widths for 
##    overlapped histograms.
## 5B.In the same window, add 3 density plots-1 for each body category. 
##    Do not use the default bandwidth. Use a bandwidth that you think 
##    makes sense. (Note, you will need to have used freq = F in your 
##    histograms.)
## 5C.In complete sentences, compare neck circumference across 
##    the three body categories using your histograms and density 
##    plots.

##5A.Frequency
hist(bodyfat$neck[bodyfat$bodycat == 'Athlete'], breaks = seq(25,55,2),
     col = rgb(1,0,1,0.25), xlab = " Neck(cm)",
     ylim = c(-1,50), main = 'Histogram of neck circumcenter difference:\ngroup by bodyfat')
hist(bodyfat$neck[bodyfat$bodycat == 'Average'], breaks = seq(25,55,2), 
     col = rgb(0,1,1,0.25), add = T)
hist(bodyfat$neck[bodyfat$bodycat == 'Obese'], breaks = seq(25,55,2), 
     col = rgb(1,1,0,0.25), add = T)
legend ('topright', c('Athlete', 'Average', 'Obese'), 
        col = c(rgb(1,0,1,0.25), rgb(0,1,1,0.25), rgb(1,1,0,0.25)), 
        lwd = 10)

##5B. 'Athete'
hist(bodyfat$neck[bodyfat$bodycat == 'Athlete'], breaks = seq(25,55,2),
     col = rgb(1,0,1,0.25), xlab = "Neck(cm)", ylim = c(0,0.2),
     freq = F, main = "Histogram of body fat ofthree categories: \n'Athlete'")
dens1 <- density (bodyfat$neck[bodyfat$bodycat == 'Athlete'], bw = 2)
lines(dens1, lwd=2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(1,0,1,0.25), 'Black'), lwd = c(10,2))

##5B. 'Average'
hist(bodyfat$neck[bodyfat$bodycat == 'Average'], breaks = seq(25,55,2),
     col = rgb(0,1,1,0.25), xlab = "Neck(cm)", ylim = c(0,0.2),
     freq = F, main = "Histogram of body fat ofthree categories: \n'Average'")
dens2 <- density (bodyfat$neck[bodyfat$bodycat == 'Average'], bw = 2)
lines(dens2, lwd=2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(0,1,1,0.25), 'Black'), lwd = c(10,2))

##5B. 'Obese'
hist(bodyfat$neck[bodyfat$bodycat == 'Obese'], breaks = seq(25,55,2),
     col = rgb(1,1,0,0.25), xlab = "Neck(cm)", ylim = c(0,0.2),
     freq = F, main = "Histogram of body fat ofthree categories: \n'Obese'")
dens3 <- density (bodyfat$neck[bodyfat$bodycat == 'Obese'], bw = 2)
lines(dens3, lwd=2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(1,1,0,0.25), 'Black'), lwd = c(10,2))

##5C.


## Repeat Question 5 for abdomen.
##6A.Frequency
hist(bodyfat$abdomen[bodyfat$bodycat == 'Athlete'], breaks = seq(65,155,4),
     col = rgb(1,0,1,0.25), xlab = "Abdomen(cm)",
     ylim = c(-1,30), main = 'Histogram of abdomen circumcenter difference:\ngroup by bodyfat')
hist(bodyfat$abdomen[bodyfat$bodycat == 'Average'], breaks = seq(65,155,4),
     col = rgb(0,1,1,0.25), add = T)
hist(bodyfat$abdomen[bodyfat$bodycat == 'Obese'], breaks = seq(65,155,4),
     col = rgb(1,1,0,0.25), add = T)
legend ('topright', c('Athlete', 'Average', 'Obese'), 
        col = c(rgb(1,0,1,0.25), rgb(0,1,1,0.25), rgb(1,1,0,0.25)), 
        lwd = 10)


par(mfrow= c(1,1))
##6B. 'Athete'
par(mfrow= c(1,3))
hist(bodyfat$abdomen[bodyfat$bodycat == 'Athlete'], breaks = seq(65,155,4),
     col = rgb(1,0,1,0.25), xlab = "Abdomen(cm)", ylim = c(0,0.1),
     freq = F, main = "Histogram of body fat ofthree categories: \n'Atlete'")
dens4 <- density (bodyfat$abdomen[bodyfat$bodycat == 'Athlete'], bw = 2)
lines(dens4, lwd=2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(1,0,1,0.25), 'Black'), lwd = c(10,4))

##6B. 'Average'
hist(bodyfat$abdomen[bodyfat$bodycat == 'Average'], breaks = seq(65,155,4),
     col = rgb(0,1,1,0.25), xlab = "Abdomen(cm)", ylim = c(0,0.1),
     freq = F, main = "Histogram of body fat ofthree categories: \n'Average'")
dens5 <- density (bodyfat$abdomen[bodyfat$bodycat == 'Average'], bw = 2)
lines(dens5, lwd=2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(0,1,1,0.25), 'Black'), lwd = c(10,4))

##6B. 'Obese'
hist(bodyfat$abdomen[bodyfat$bodycat == 'Obese'], breaks = seq(65,155,4),
     col = rgb(1,1,0,0.25), xlab = "Abdomen(cm)", ylim = c(0,0.2),
     freq = F, main = "Histogram of body fat ofthree categories: \n'Obese'")
dens6 <- density (bodyfat$abdomen[bodyfat$bodycat == 'Obese'], bw = 1)
lines(dens6, lwd=2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(1,1,0,0.25), 'Black'), lwd = c(10,4))

##6C.



heavilyobese <- bodyfat[(bodyfat$neck > 50),]
par(mfrow= c(1,3))
hist(bodyfat$neck[bodyfat$htcat == 'Short'], breaks = seq(25,55,2),
     col = rgb(1,0,1,0.25), xlab = " Neck(cm)",
     ylim = c(-1,50), main = 'Histogram of body fat of three categories')
hist(bodyfat$neck[bodyfat$htcat == 'Below average'], breaks = seq(25,55,2), 
     col = rgb(0,1,1,0.25), add = T)
hist(bodyfat$neck[bodyfat$htcat == 'Above average'], breaks = seq(25,55,2), 
     col = rgb(1,1,0,0.25), add = T)
hist(bodyfat$neck[bodyfat$htcat == 'Tall'], breaks = seq(25,55,2), 
     col = rgb(1,1,1,0.25), add = T)
legend ('topright', c('Athlete', 'Average', 'Obese'), 
        col = c(rgb(1,0,1,0.25), rgb(0,1,1,0.25), rgb(1,1,0,0.25)), 
        lwd = 10)
