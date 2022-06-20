weather <- read.csv('weather.csv')
weather_2016 <- weather[weather$Date.Year == '2016',]


LA <- weather_2016[weather_2016$Station.City == 'Los Angeles',]
CA <- weather_2016[weather_2016$Station.State == 'California',]
Long_beach <- weather_2016[weather_2016$Station.City == 'Long Beach',]
LA_LB = rbind(LA, Long_beach)



library(multcomp)

## 1 stem-and-leaf plot
stem(LA$Data.Temperature.Avg.Temp, scale = 1)


## 2 histogram with overlaid density plot 
## (remember to set freq = F, otherwise your density plot 
## might not be visible)
par(mfrow=c(1,1))
hist(LA$Data.Temperature.Avg.Temp, breaks = seq(50,80,2),  
     xlab = 'Temperature',col=rgb(1,0,1,0.25), ylim = c(0,12), 
     main = "LA and Long Beach's weather", label = T)
hist(Long_beach$Data.Temperature.Avg.Temp, breaks = seq(50,80,2), 
     col=rgb(0,1,1,0.5), add = T)
legend ('topright', c('Los Angeles', 'Long Beach'), 
        col = c(rgb(1,0,1,0.25), rgb(0,1,1,0.5)), lwd = 10)

par(mfrow=c(1,3))
hist( LA$Data.Temperature.Avg.Temp, freq = F, breaks = seq(50,80,2),  
     xlab = 'Temperature', col=rgb(1,0,1,0.25), ylim = c(0,0.13),
     main = "Los Angeles's weather")
dens1 <- density (LA$Data.Temperature.Avg.Temp, bw = 2)
lines(dens1, lwd = 2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(1,0,1,0.25), 'Black'), lwd = c(10,2))
hist(Long_beach$Data.Temperature.Avg.Temp, freq = F, ylim = c(0,0.13),
     breaks = seq(50,80,2), xlab = 'Temperature', col=rgb(0,1,1,0.5),  
     main = "Long Beach's weather")
dens2 <- density (Long_beach$Data.Temperature.Avg.Temp, bw = 2)
lines(dens2, lwd = 2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(0,1,1,0.25), 'Black'), lwd = c(10,2))
hist( LA_LB$Data.Temperature.Avg.Temp, freq = F, breaks = seq(45,80,2),  
      xlab = 'Temperature', col=rgb(1,1,0,0.25), ylim = c(0,0.13),
      main = "LA and Long beach's weather")
dens3 <- density (LA_LB$Data.Temperature.Avg.Temp, bw = 2)
lines(dens3, lwd = 2)
legend ('topright', c('Density', 'Lines'), 
        col = c(rgb(1,1,0,0.25), 'Black'), lwd = c(10,2))


par(mfrow=c(1,1))
hist( LA$Data.Temperature.Avg.Temp, freq = F, breaks = seq(45,80,2),  
      xlab = 'Temperature', col=rgb(1,0,1,0.25), ylim = c(0,0.11),
      main = "LA and Long beach's weather")
hist(Long_beach$Data.Temperature.Avg.Temp, freq = F, 
     breaks = seq(45,80,2), xlab = 'Temperature', col=rgb(0,1,1,0.5),  
     main = "Long Beach's weather", add = T)
hist( LA_LB$Data.Temperature.Avg.Temp, freq = F, breaks = seq(45,80,2),  
      xlab = 'Temperature', col=rgb(1,1,0,0.25), ylim = c(0,0.11),
      main = "LA and Long beach's weather", add =T)
legend ('topright', c('Los Angeles', 'Long Beach'), 
        col = c(rgb(1,0,1,0.25), rgb(1,1,0,0.5)), lwd = 10)
legend ('topleft', c('Los Angeles', 'Long Beach', 'LA + Long Beach'), 
        col = c("violetred", "mediumblue", 'Black'), lwd = 2)
lines(dens1, lwd = 2, col = "violetred", lty = 4)
lines(dens2, lwd = 2, col = "mediumblue", lty = 4)
dens3 <- density (LA_LB$Data.Temperature.Avg.Temp, bw = 2)
lines(dens3, lwd = 2)









LA$season[LA$Date.Month <=3] <- 'Winter'
LA$season[LA$Date.Month >3 & LA$Date.Month <=6] <- 'Spring'
LA$season[LA$Date.Month >6 & LA$Date.Month <=9] <- 'Summer'
LA$season[is.na(LA$season)] <- 'Fall'
LA$season <- as.factor(LA$season)
LA$season <- factor(LA$season, levels = c('Winter', 'Spring', 'Summer', 'Fall'))
LA$season <- as.character(LA$season)

avgtemp <-aggregate(Data.Precipitation~season, data = LA, FUN = sum)
avgtemp
avgtemp <- factor(avgtemp, levels =  c('Winter', 'Spring', 'Summer', 
                                               'Fall'))

barplot (avgtemp$Data.Precipitation, ylab = 'Average precipitation',
         names.arg = avgtemp$season, col = c('deepskyblue1', 'green','darkgreen', 'skyblue'), 
         main = "2016 LA's precipitation by season")



LA_LB$season[LA_LB$Date.Month <=3] <- 'Winter'
LA_LB$season[LA_LB$Date.Month >3 & LA$Date.Month <=6] <- 'Spring'
LA_LB$season[LA_LB$Date.Month >6 & LA$Date.Month <=9] <- 'Summer'
LA_LB$season[is.na(LA_LB$season)] <- 'Fall'
LA_LB$season <- as.factor(LA_LB$season)
LA_LB$season <- factor(LA_LB$season, levels = c('Winter', 'Spring', 'Summer', 'Fall'))

avgtemp2 <-aggregate(Data.Precipitation~Station.City*season, data = LA_LB, FUN = sum)
avgtemp2


barplot (avgtemp2$Data.Precipitation, ylab = 'Average precipitation(inches)',
         names.arg = c('Winter','', 'Spring','', 'Summer','', 'Fall',''), col = c('violetred1', 'deepskyblue1'), 
         , space = c(1,0,1,0,1,0,1,0), 
         main = "2016 LA and Long Beach's precipitation by season")
legend ('center', c('Long Beach', 'Los Angeles'), 
        col = c('violetred1', 'deepskyblue1'), lwd = 10)
