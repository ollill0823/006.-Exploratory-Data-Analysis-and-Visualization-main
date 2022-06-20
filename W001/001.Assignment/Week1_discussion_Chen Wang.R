


weather <- read.csv('weather.csv')
weather2 <- weather[which(weather$Date.Year == "2016"),]

California <- weather2[which(weather2$Station.State == 'California'),]

California_low32 <- California[which(California$Data.Temperature.Avg.Temp
                                     < 32),]



LA <- California[which(California$Station.Location == 'Los Angeles, CA'),]

stripchart (California$Data.Temperature.Avg.Temp, method = 'jitter',
            main = "California's average temperature(F)", xlab = '¢XF')
abline(v=32, col='Blue', lty=2, lwd=3)

stripchart (LA$Data.Temperature.Avg.Temp, method = 'jitter', 
            main = "Los Angeles's average temperature(F)", xlab = '¢XF')

dotchart (California$Data.Temperature.Avg.Temp, labels = California$Date.Full, cex = 0.5)
dotchart (LA$Data.Temperature.Avg.Temp, labels = LA$Date.Full, cex = 0.7, main = 
            "Los Angeles's average temperature(F)", xlab = "Temperature(F)",
          cex.main = 2, cex.lab = 1.5, pch = 19)

dotchart (LA$Data.Temperature.Avg.Temp, labels = LA$Date.Full, cex = 0.6, main = 
            "Los Angeles's average temperature(F)", xlab = "Temperature(F)", ylab = 'Date',
          cex.main = 2, cex.lab = 1.5, col = c('green', 'red', 'black'))

par(mfrow = c(1,1))
boxplot(Data.Temperature.Avg.Temp~Date.Month, data = California, 
        col = 'green', main= "CA's weather in 2016", ylab= '¢XF')
boxplot(Data.Temperature.Avg.Temp~Date.Month, data = LA, col = 'green', 
        main= "LA's weather in 2016", ylab= '¢XF' )

par(mfrow = c(1,2))
boxplot(Data.Temperature.Avg.Temp~Date.Month, data = California, col = 'slateblue1', main= "CA's weather in 2016",
        ylim = c(25,95))
boxplot(Data.Temperature.Avg.Temp~Date.Month, data = LA, col = 'green', main= "LA's weather in 2016",
        ylim = c(25,95), ylab= '¢XF')


par(mfrow = c(1,1))
boxplot(Data.Temperature.Avg.Temp~Date.Month, data = California, col = 'green', main= "CA's weather in 2016", horizontal = T)
boxplot(Data.Temperature.Avg.Temp~Date.Month, data = LA, col = 'green', main= "LA's weather in 2016", horizontal = T )

weather2$color[weather2$Date.Month <=3] <- 'chartreuse3'
weather2$color[weather2$Date.Month >=4 & weather2$Date.Month <=6] <- 'Red'
weather2$color[weather2$Date.Month >=7 & weather2$Date.Month <=9] <- 'chocolate'
weather2$color[weather2$Date.Month >=10 & weather2$Date.Month <=12] <- 'Blue'

California <- weather2[which(weather2$Station.State == 'California'),]

California_low32 <- California[which(California$Data.Temperature.Avg.Temp
                                     < 32),]

LA <- California[which(California$Station.Location == 'Los Angeles, CA'),]

dotchart (LA$Data.Temperature.Avg.Temp, labels = LA$Date.Full, cex = 0.7, main = 
            "Los Angeles's average temperature(F)", xlab = "Temperature(F)",
          cex.main = 2, cex.lab = 1.5, pch = 19, col = c(LA$color))
