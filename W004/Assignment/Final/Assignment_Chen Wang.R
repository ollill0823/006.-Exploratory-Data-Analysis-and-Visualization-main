
Election_2016 <- read.csv('2016_Pre_election.csv')
Election_2020 <- read.csv('2020_Pre_election.csv')


Election_2016$year <- '2016'
Election_2020$year <- '2020'



Election_2016$Win[Election_2016$DEM > Election_2016$GOP] <- 'DEM'
Election_2016$Win[Election_2016$GOP > Election_2016$DEM] <- 'GOP'

Election_2020$Win[Election_2020$DEM > Election_2020$GOP] <- 'DEM'
Election_2020$Win[Election_2020$GOP > Election_2020$DEM] <- 'GOP'

## Order('DEM', 'GOP') = (1, 0)
Election_2016$Order <- factor(Election_2016$Win, levels = c('DEM', 'GOP'), labels = 1:0)
Election_2020$Order <- factor(Election_2020$Win, levels = c('DEM', 'GOP'), labels = 1:0)

## Change order to numeric
Election_2016$Order <- as.numeric(Election_2016$Order)
Election_2020$Order <- as.numeric(Election_2020$Order)

change <- c(Election_2020$Order - Election_2016$Order)
swingstate <- cbind(Election_2016$ï..State, change)
swingstate <- as.data.frame(swingstate)
names(swingstate) <-c('State', 'Change')
swingstate$change < as.numeric(swingstate$change)
barplot(change, names.arg = Election_2016$ï..State, cex.lab = 0.5, cex.names = 0.5)


# Define Color
Election_2016$Color_2016[Election_2016$DEM > Election_2016$GOP] <- 'Blue'
Election_2016$Color_2016[Election_2016$GOP > Election_2016$DEM] <- 'Red'

barplot(Election_2016$DEM, names.arg = Election_2016$ï..State, col = 'Blue')


Election_2020$Color_2020[Election_2020$DEM > Election_2020$GOP] <- rgb(0,0,1,0.5)
Election_2020$Color_2020[Election_2020$GOP > Election_2020$DEM] <- rgb(1,0,0,0.25)


## Offset
Election_2016$offset_2016 <- Election_2016$DEM-Election_2016$GOP
Election_2020$offset_2020 <- Election_2020$DEM-Election_2020$GOP
offsetsummary <- cbind(Election_2016$ï..State,Election_2016$offset_2016, Election_2020$offset_2020,
                       Election_2016$Color_2016, Election_2020$Color_2020)
offsetsummary <- as.data.frame(offsetsummary)


names(offsetsummary) <- c('State', 'offset_2016', 'offset_2020', 'color_2016', 'color_2020')
offsetsummary$offset_2016 <- as.numeric(offsetsummary$offset_2016)
offsetsummary$offset_2020 <- as.numeric(offsetsummary$offset_2020)
order_offset <- offsetsummary[order(offsetsummary$offset_2016, decreasing = T),]
par(mfrow=c(1,1))
barplot(order_offset$offset_2016/1000, names.arg = order_offset$State, col = order_offset$color_2016, 
        horiz = T, xlim = c(-1000,5000), xlab = "Votes_offset(thousand)",
        ylab = "State", main = "2016 to 2020 USA presidential votes shift \nby state",
        cex.main = 1.5, cex.lab = 1, lwd = 3, cex.axis = 1)

barplot(order_offset$offset_2020/1000, col = order_offset$color_2020, add = T, horiz = T)
legend("topright", c("2016 offset(GOP+)", "2016 offset(DEM+)", "2020 offset(GOP+)", "2020 offset(DEM+)"), 
       col = c("Red", "Blue", rgb(1,0,0,0.25), rgb(0,0,1,0.5)), lwd = 10)
text(300,41.5,labels = "Georgia", adj = 0)
text(300,30.8,labels = "Arizona", adj = 0)
text(300,28.6,labels = "Pennsylvania", adj = 0)
text(300,27.3,labels = "Wisconsin", adj = 0)
text(300,26,labels = "Michigan", adj = 0)
arrows(x0 = 280, y0 = 26, x1 = 200, y1 = 26, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 27.3, x1 = 200, y1 = 27.3, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 28.6, x1 = 200, y1 = 28.6, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 30.8, x1 = 100, y1 = 30.8, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 41.5, x1 = 100, y1 = 41.5, length = 0.15, angle = 30)


barplot(order_offset$offset_2016, names.arg = order_offset$State, 
        col = order_offset$color_2016, xlim = c(-200000,200000), horiz = T)
barplot(order_offset$offset_2020, col = order_offset$color_2020, add = T, horiz = T)


# swing state#
State <- Election_2016$ï..State[Election_2016$Order != Election_2020$Order]

#combine into a row
combine <- rbind(Election_2016[Election_2016$Order != Election_2020$Order,], 
                 Election_2020[Election_2016$Order != Election_2020$Order,])
combine2_DEM <- combine[,c(1:3,9:12)]
names(combine2_DEM)[3] <- 'Vote'
combine2_DEM$Party <- 'DEM'
combine2_GOP <- combine[,c(1:2,6,9:12)]
names(combine2_GOP)[3] <- 'Vote'
combine2_GOP$Party <- 'GOP'
combine2 <- rbind(combine2_DEM,combine2_GOP)

combine2$Party <- factor(combine2$Party, levels = c('GOP', 'DEM'))
combine2$Party <- factor(combine2$Party, levels = c('DEM', 'GOP'))
levels(combine2$Party)

names(combine2)[1] <- 'State'
combine2$year <- factor(combine2$year, levels = c('2016','2020'))

combine2Order <- combine2[order(combine2$State, combine2$Party),]


barplot(combine2Order$Vote[combine2Order$year == 2016]/1000, names.arg = combine2Order$State[combine2Order$year == 2016], 
        col = c("Blue", "Red"), ylim = c(0,3500), space = c(3,0,1,0,1,0,1,0,1,0),
        xlab = 'Swing State', ylab = 'Votes(thousand)')

barplot(combine2Order$Vote[combine2Order$year == 2020]/1000, names.arg = combine2Order$State[combine2Order$year == 2020], 
        col = c(rgb(0,0,1,0.25), rgb(1,0,0,0.5)), space = c(3,0,1,0,1,0,1,0,1,0), add = T)
legend(3,3500, c("2016 DEM", "2016 GOP", "2020 DEM", "2020 GOP"), 
       col = c("Blue", "Red", rgb(0,0,1,0.5), rgb(1,0,0,0.25)), lwd = 10)






## no need
combine2Order_Arizona <-combine2Order[combine2Order$State == 'Arizona',]

barplot(combine2Order$Vote[combine2Order$State =='Arizona'], names.arg = combine2Order$year[combine2Order$State =='Arizona'],
        col = combine2Order$Color, space = c(1,0,1,0), ylim = c(0,4000000))
legend(1,3000000, c("DEM", "GOP"), col = c("Red", "Blue"), lwd = 10)

barplot(combine2Order$Vote[combine2Order$State =='Georgia'], names.arg = combine2Order$year[combine2Order$State =='Georgia'],
        col = rgb(1,0,0,0.5), space = c(1,0,1,0), add = T)
legend(3,2300000, c("DEM", "GOP"), col = c("Red", "Blue"), lwd = 10)



