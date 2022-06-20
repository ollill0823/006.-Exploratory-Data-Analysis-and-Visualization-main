hydropower <- read.csv("hydropower.csv")

summary(hydropower)
dim(hydropower)

#2
hydropower$Identity.Project.Year[hydropower$Identity.Project.Year == 0] <-- NA
hydropower$Identity.Project.Year[hydropower$Identity.Project.Year == 19] <-- NA
hydropower$Identity.Project.Year[hydropower$Identity.Project.Year == 1076] <-- NA
hydropower$Identity.Project.Year

which(!is.na(hydropower$Identity.Project.Year))
hydropower <- hydropower[which(!is.na(hydropower$Identity.Project.Year)),]
hydropower$Identity.Project.Year

dim(hydropower)

summary(hydropower$Identity.Project.Year)

#3
hydropower$yearcheck[hydropower$Identity.Project.Year < 1918] <- 'Before WW1'
hydropower$yearcheck[hydropower$Identity.Project.Year >= 1918 & hydropower$Identity.Project.Year < 1945] <- 'WW1 to WW2'
hydropower$yearcheck[hydropower$Identity.Project.Year >= 1945] <- 'After WW2'
hydropower$yearcheck

hydropower$yearcheck <- factor(hydropower$yearcheck, labels = c('Before WW1','WW1 to WW2', 'After WW2' ))

boxplot(Dimensions.Structural.Height~yearcheck, data = hydropower,
        col = c('firebrick2','green3', 'dodgerblue'),
        main = "Dams' structural height in three different group", ylab = "Dams' height",
        xlab = 'year', cex.axis = 1.2)


summary(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'WW1 to WW2'],)
summary(hydropower$yearcheck)
summary(hydropower)


#4.0.
hist(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'Before WW1'],
     breaks = seq(0, 800, 50), main = "Dams hight distribution in three different group of year",
     xlab = 'Height', col = rgb(1,0,0,0.25), freq = T, ylim = c(0,50))
hist(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'WW1 to WW2'],
     breaks = seq(0, 800, 50), col = rgb(0,1,0,0.25), add = T, freq = T)

hist(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'After WW2'],
     breaks = seq(0, 800, 50), col = rgb(0,0,1,0.25), add = T, freq = T)
legend('topright', c(rep(c('Before WW1','WW1 to WW2', 'After WW2'))),
       col = c(rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,0,1,0.25)), lwd = 10 )

#4.1.




hist(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'Before WW1'],
     breaks = seq(0, 800, 50), main = "Dams hight distribution in three different group of year",
     xlab = 'Height', col = rgb(1,0,0,0.25), ylim = c(0,0.012), freq = F)
hist(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'WW1 to WW2'],
     breaks = seq(0, 800, 50), col = rgb(0,1,0,0.25), add = T, freq = F)
hist(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'After WW2'],
     breaks = seq(0, 800, 50), col = rgb(0,0,1,0.25), add = T, freq = F)
dens1 <- density(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'Before WW1'],
                 bw = 30,)
dens2 <- density(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'WW1 to WW2'],
                 bw = 30)
dens3 <- density(hydropower$Dimensions.Structural.Height[hydropower$yearcheck == 'After WW2'],
                 bw = 30)
lines(dens1, col = 'deeppink1', lty = 1, lwd = 2)
lines(dens2, col = 'green4', lty = 1, lwd = 2)
lines(dens3, col = 'navy', lty = 1, lwd = 2)
legend('topright', c(rep(c('Before WW1','WW1 to WW2', 'After WW2'),2)),
       col = c(rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,0,1,0.25),'deeppink1',
               'green4', 'navy'), lwd =c(rep(10,3), rep(2,3)))





#5.
hydropower$Location.State <- as.factor(hydropower$Location.State)
library("viridis")
state <- table(hydropower$Location.State)
state
stateorder<- head(state[order(state, decreasing = T)],10)
barplot(stateorder, col = viridis(10), ylab = 'Count', xlab = 'State', legend.text = T,
        main = 'Dams count in different states')




#6
library(RColorBrewer)
stateelev <- aggregate(Dimensions.Structural.Height~Location.State, data = hydropower, 
                     FUN = mean)
stateelev
stateelevorder <- head(stateelev[order(stateelev$Dimensions.Structural.Height, decreasing = T),],10)
stateelevorder
barplot(stateelevorder$Dimensions.Structural.Height, names.arg = stateelevorder$Location.State,
        col = plasma(10), ylab = 'Average height', xlab = 'Top 10 State', legend.text = stateelevorder$Location.State,
        main = "Dams average structural hegiht in different states")

hydropower[hydropower$Location.State == 'Nevada and Arizona',]


stateorder <- data.frame(stateorder)
top10state <- cbind (state, stateelev)
top10state <- top10state[,-3]
top10stateorder <- head(top10state[order(top10state$Freq, decreasing = T),],10)
top10heightorfer <- top10stateorder[order(top10stateorder$Dimensions.Structural.Height, decreasing = T),]
barplot(top10heightorfer$Dimensions.Structural.Height, names.arg = top10heightorfer$Location.State,
        col = plasma(10), ylab = 'Average height', xlab = 'Top 10 State', legend.text = top10heightorfer$Location.State,
        main = "Top 10 dams average structural hegiht in top 10 states", ylim = c(0,300))

#7

stateorder <- data.frame(stateorder)
propelev <- round(prop.table(stateorder$Freq)*100,2)
heightprop <- paste(stateorder$Var1, "\n", propelev, "%",  sep ="")
pie(stateorder$Freq, labels = heightprop, col = viridis(10), lty =2, radius = 1,
    main = "Dams' distribution in the United States")


