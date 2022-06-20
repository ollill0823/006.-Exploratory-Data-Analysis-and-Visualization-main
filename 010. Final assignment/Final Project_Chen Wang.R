# 1. Arrange data ----

# Read file from https://baseballsavant.mlb.com/statcast_search choosing Shohei Othani 
pitching_othani <- read.csv("Shohei_Othani_Pitching.csv")

#* A. Data arrange/cleaning ----
#* Date transfer to the data --> for the following separation
pitching_othani$game_date <- as.Date(pitching_othani$game_date, format="%m/%d/%Y")

#* Sort dataset from old to new (in order to draw scatter plot in the following graph)
pitching_othani <- pitching_othani[order(pitching_othani$game_date, decreasing = F),]

#* In order to separate into different season, 
#* I try to use format to separate year from date, and create a column 'year'
pitching_othani$year <- c(format(as.Date(pitching_othani$game_date, format="%m/%d/%Y"),"%Y"))

#* Transfer game_date from character to Date format
pitching_othani$game_date <- as.Date(pitching_othani$game_date, format="%m/%d/%Y")

#* Found there is some row with NULL
othani_pitch_aggre <- aggregate(release_speed~year*ï..pitch_type, data = pitching_othani, FUN = mean)
othani_pitch_aggre

#* Double-check which row is NULL, the answer is in
is.na(pitching_othani$ï..pitch_type)

#* in order to show all data in the Console board, I extend the print to 100,000
options(max.print=100000)

#* use function to check NULL, but can not find
is.na(pitching_othani$ï..pitch_type)

#* Check the origin data, bingo, at #134
pitching_othani$ï..pitch_type

#* Finally, I remove the #134 row 
pitching_othani <- pitching_othani[-134,]

#** B. Data separating and further exclude ----
#** Separate Othani's pitching data into different pitching types
pitching_othani_CU <- pitching_othani[pitching_othani$ï..pitch_type == 'CU',]
pitching_othani_FC <- pitching_othani[pitching_othani$ï..pitch_type == 'FC',]
pitching_othani_FF <- pitching_othani[pitching_othani$ï..pitch_type == 'FF',]
pitching_othani_FS <- pitching_othani[pitching_othani$ï..pitch_type == 'FS',]
pitching_othani_SL <- pitching_othani[pitching_othani$ï..pitch_type == 'SL',]

#** Thoroughly check if I still need to do cleaning of the table
plot(pitching_othani_CU[,c(1,3,5,28,29,48,49,50,57,58)])
plot(pitching_othani_FC[,c(1,3,5,28,29,48,49,50,57,58)])
plot(pitching_othani_FF[,c(1,3,5,28,29,48,49,50,57,58)])
plot(pitching_othani_FS[,c(1,3,5,28,29,48,49,50,57,58)])
plot(pitching_othani_SL[,c(1,3,5,28,29,48,49,50,57,58)])

#** Create new variable to remove outliners from the data
pitching_othani_CU_2 <- pitching_othani_CU[pitching_othani_CU$release_spin_rate >1000,]
pitching_othani_FC_2 <- pitching_othani_FC[pitching_othani_FC$release_spin_rate >1000,]
pitching_othani_FF_2 <- pitching_othani_FF[pitching_othani_FF$release_spin_rate >1500 & pitching_othani_FF$release_spin_rate <2800,]
pitching_othani_SL_2 <- pitching_othani_SL[pitching_othani_SL$release_spin_rate >1500 & pitching_othani_SL$release_spin_rate <3200,]


## 2. Analyze the data ----
##* A. box plot ----
##* Othani's Curve Ball's pitching speed ##
par(mfrow = c(1,1))
boxplot(release_speed~year*ï..pitch_type, data = pitching_othani_CU, col = c('lightskyblue'), main = "Shohei Othani's Curve ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(othani_pitch_aggre$release_speed[othani_pitch_aggre$ï..pitch_type == 'CU'], add = T, lty = 3, lwd = 5, col = 'red', pch = 19)

##* Othani's Cutter Ball's pitching speed ##
par(mfrow = c(1,1))
boxplot(release_speed~year*ï..pitch_type, data = pitching_othani_FC, col = c('lightskyblue'), main = "Shohei Othani's Cutter ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(othani_pitch_aggre$release_speed[othani_pitch_aggre$ï..pitch_type == 'FC'], add = T, lty = 3, lwd = 5, col = 'red', pch = 19)

##* Othani's Fast Ball's pitching speed ##
par(mfrow = c(1,1))
boxplot(release_speed~year*ï..pitch_type, data = pitching_othani_FF, col = c('lightskyblue'), main = "Shohei Othani's Fast ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(othani_pitch_aggre$release_speed[othani_pitch_aggre$ï..pitch_type == 'FF'], add = T, lty = 3, lwd = 5, col = 'red', pch = 19)

##* Othani's Split-Finger Ball's pitching speed ##
par(mfrow = c(1,1))
boxplot(release_speed~year*ï..pitch_type, data = pitching_othani_FS, col = c('lightskyblue'), main = "Shohei Othani's Split-Finger ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(othani_pitch_aggre$release_speed[othani_pitch_aggre$ï..pitch_type == 'FS'], add = T, lty = 3, lwd = 5, col = 'red', pch = 19)

##* Othani's Slider Ball's pitching speed ##
par(mfrow = c(1,1))
boxplot(release_speed~year*ï..pitch_type, data = pitching_othani_SL, col = c('lightskyblue'), main = "Shohei Othani's Slider ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(othani_pitch_aggre$release_speed[othani_pitch_aggre$ï..pitch_type == 'SL'], add = T, lty = 3, lwd = 5, col = 'red', pch = 19)

##** B. Scatter plot ----
##** Scatter plot of Othani's pitching speed V.S. date

par(mfrow = c(1,1))

plot(pitching_othani_CU$game_date, pitching_othani_CU$release_speed, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(65,120), xlab = 'Year', ylab = 'release speed', main = "Shohei Othani's pitching speed(release) by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$release_speed, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$release_speed, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$release_speed, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$release_speed, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's pitching spin rate V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$release_spin_rate, type = 'l', lty = 1, lwd = 2, ylim = c(800,3500), col = 'firebrick1', xlab = 'Year', ylab = 'release spin rate', main = "Shohei Othani's pitching spin rate by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$release_spin_rate, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$release_spin_rate, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$release_spin_rate, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$release_spin_rate, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

##** Scatter plot of Othani's release ball's position V.S. date (no detail check by pitching type)
plot(pitching_othani$game_date, pitching_othani$release_extension, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(5,7.5), xlab = 'Year', ylab = 'Release extension(in feet)', main = "Shohei Othani's pitching situation by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )

##** Scatter plot of Othani's release ball's position V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$release_extension, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(5,7.5), xlab = 'Year', ylab = 'Release extension(in feet)', main = "Shohei Othani's pitching release point \nto homeplate by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$release_extension, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$release_extension, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$release_extension, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$release_extension, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('bottomright', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's pfx_x V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-2,3.5), xlab = 'Year', ylab = 'pfx_x(in feet)', main = "Shohei Othani's pitching horizontal movement \nto homeplate by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's pfx_z V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-2,3.5), xlab = 'Year', ylab = 'pfx_z(in feet)', main = "Shohei Othani's pitching vertical movement \nto homeplate by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's plate_x V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$plate_x, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-4,6), xlab = 'Year', ylab = 'plate_x(in feet)', main = "Shohei Othani's pitching plate_x \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$plate_x, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$plate_x, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$plate_x, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$plate_x, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's plate_z V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$plate_z, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-3,10), xlab = 'Year', ylab = 'plate_z(in feet)', main = "Shohei Othani's pitching plate_z \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$plate_z, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$plate_z, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$plate_z, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$plate_z, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's vx0 V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$vx0, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-5,20), xlab = 'Year', ylab = 'vx0(in feet)', main = "Shohei Othani's pitching vx0 \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$vx0, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$vx0, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$vx0, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$vx0, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's vy0 V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$vy0, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-150,-70), xlab = 'Year', ylab = 'vy0(in feet)', main = "Shohei Othani's pitching vy0 \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$vy0, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$vy0, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$vy0, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$vy0, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topright', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's vz0 V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$vz0, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-15,20), xlab = 'Year', ylab = 'vz0(in feet)', main = "Shohei Othani's pitching vz0 \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$vz0, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$vz0, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$vz0, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$vz0, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** Scatter plot of Othani's spin_axis V.S. date
plot(pitching_othani_CU$game_date, pitching_othani_CU$spin_axis, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(20,450), xlab = 'Year', ylab = 'spin_axis(in feet)', main = "Shohei Othani's pitching spin_axis \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(pitching_othani_FC$game_date, pitching_othani_FC$spin_axis, type = 'l', lty = 1, lwd = 2, col = 'deeppink1', add = T)
lines(pitching_othani_FF$game_date, pitching_othani_FF$spin_axis, type = 'l', lty = 1, lwd = 2, col = 'gold1', add = T)
lines(pitching_othani_FS$game_date, pitching_othani_FS$spin_axis, type = 'l', lty = 1, lwd = 2, col = 'green3', add = T)
lines(pitching_othani_SL$game_date, pitching_othani_SL$spin_axis, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2', add = T)
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##*** C. Check correlation ----
##*** Use original data to check variables plot if predictable
plot(pitching_othani[,c(1,3,5,28,29,48,49,50,57,58,90)])

##*** Use Curveball's and Curveball+ modified data to check variables plot if predictable
plot(pitching_othani_CU[,c(1,3,5,28,29,48,49,50,57,58,90)])
plot(pitching_othani_CU_2[,c(1,3,5,28,29,48,49,50,57,58,90)])

##*** Use Cutter and Cutter + modified data to check variables plot if predictable
plot(pitching_othani_FC[,c(1,3,5,28,29,48,49,50,57,58,90)])
plot(pitching_othani_FC_2[,c(1,3,5,28,29,48,49,50,57,58,90)])

##*** Use Fast and Fast + modified data to check variables plot if predictable
plot(pitching_othani_FF[,c(1,3,5,28,29,48,49,50,57,58,90)])
plot(pitching_othani_FF_2[,c(1,3,5,28,29,48,49,50,57,58,90)])

##*** Use Split-Finger to check variables plot if predictable
plot(pitching_othani_FS[,c(1,3,5,28,29,48,49,50,57,58,90)])

##*** Use Slider and Slider + modified data to check variables plot if predictable
plot(pitching_othani_SL[,c(1,3,5,28,29,48,49,50,57,58,90)])
plot(pitching_othani_SL_2[,c(1,3,5,28,29,48,49,50,57,58,90)])

##*** check spin rate vs speed and pitch_type linear regression model (overall)
m1 <- lm(release_spin_rate~release_speed+ï..pitch_type, data = pitching_othani)
summary(m1)

##*** check speed vs spin rate and pitch_type linear regression model (overall)
m2 <- lm(release_speed~release_spin_rate+ï..pitch_type, data = pitching_othani)
summary(m2)

##*** check speed vs key variables linear regression model (overall)
m3 <- lm(release_speed~release_spin_rate+ï..pitch_type + release_pos_z + ax + ay + az + spin_axis, data = pitching_othani)
summary(m3)
step(m3, direction = "backward")

##*** check horizontal movement vs key variables linear regression model (overall)
m4 <- lm(pfx_x~release_speed + release_spin_rate+ï..pitch_type + release_pos_z + ax + ay + az, data = pitching_othani)
summary(m4)
step(m4, direction = "backward")

##*** check vertical movement vs key variables linear regression model (overall)
m5 <- lm(pfx_z~release_speed + release_spin_rate+ï..pitch_type + release_pos_z + ax + ay + az, data = pitching_othani)
summary(m5)
step(m5, direction = "backward")


##*** check spin axis movement vs key variables linear regression model (overall)
m6 <- lm(spin_axis~release_speed + release_spin_rate+ï..pitch_type + release_pos_z + ax + ay + az + pfx_x + pfx_z, data = pitching_othani)
summary(m6)
step(m6, direction = "backward")


library(corrplot)

##*** check correlation
##*** Column number = 57(release_spin_rate) , 58(release_extension), 90(spin_axis) can not create correlation number
cor(pitching_othani[,c(3,5,28,29,48,49,50,57,58,90)])
cor(pitching_othani[,c(3,5,28,29,48,49,50)])

##*** Check Curveball only found its ok
##*** Maybe there are some cluster inside the overall data cause it is difficult to predict
cor(pitching_othani_CU_2[,c(3,5,28,29,48,49,50,57,58,90)])
cor(pitching_othani_CU_2[,c(3,57,58)])

##*** plot correlation Curveball: 
corrplot(cor(pitching_othani_CU_2[,c(3,5,28,29,48,49,50,57,58,90)]))
##*** plot correlation Cutterball: 
corrplot(cor(pitching_othani_FC_2[,c(3,5,28,29,48,49,50,57,58,90)]))
##*** plot correlation Fastball: it still can not generate data, maybe there still found different clusters
corrplot(cor(pitching_othani_FF_2[,c(3,5,28,29,48,49,50,57,58,90)]))
##*** plot correlation Split-Finger: it still can not generate data, maybe there still found different clusters
corrplot(cor(pitching_othani_FS[,c(3,5,28,29,48,49,50,57,58,90)]))
##*** plot correlation Slider: it still can not generate data, maybe there still found different clusters
corrplot(cor(pitching_othani_SL_2[,c(3,5,28,29,48,49,50,57,58,90)]))

##**** D. Pie chart ----

library("viridis")

##**** Calculate 2021 othani's pitching in different pitching type
pie_table_ohtani_2021 <- table(pitching_othani$ï..pitch_type[pitching_othani$year == '2021'])
##**** create a data frame
pie_table_ohtani_2021 <- data.frame(pie_table_ohtani_2021)
##**** create a paste
prop_othani_2021 <- round(prop.table(pie_table_ohtani_2021$Freq)*100,2)
paste_othani_2021 <- paste(pie_table_ohtani_2021$Var1, "\n", prop_othani_2021, "%",  sep ="")


##**** Calculate 2022 othani's pitching in different pitching type
pie_table_ohtani_2022 <- table(pitching_othani$ï..pitch_type[pitching_othani$year == '2022'])
##**** create a data frame
pie_table_ohtani_2022 <- data.frame(pie_table_ohtani_2022)
##**** create a paste
prop_othani_2022 <- round(prop.table(pie_table_ohtani_2022$Freq)*100,2)
paste_othani_2022 <- paste(pie_table_ohtani_2022$Var1, "\n", prop_othani_2022, "%",  sep ="")



par(mfrow = c(1,2))
pie(pie_table_ohtani_2021$Freq, label = paste_othani_2021, col = viridis(5), lty =3, radius = 1,
    main = "2021 Ohtani pitching's variance")
pie(pie_table_ohtani_2022$Freq, label = paste_othani_2022, col = viridis(5), lty =3, radius = 1,
    main = "2022 Ohtani pitching's variance")
par(mfrow = c(1,1))


## Create histogram of Othani's 2021 speed distribution 
hist(pitching_othani$release_speed[pitching_othani$year == '2021' &pitching_othani$ï..pitch_type == 'CU'], breaks = seq(60, 110, 2),
      main = "Othani's speed distribution \nby differnt pitching types",
     xlab = 'Speed(mph)', ylab = '# (count)', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,350), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2021' &pitching_othani$ï..pitch_type == 'FC'],
     breaks = seq(60, 110, 2), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2021' &pitching_othani$ï..pitch_type == 'FF'],
     breaks = seq(60, 110, 2), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2021' &pitching_othani$ï..pitch_type == 'FS'],
     breaks = seq(60, 110, 2), col = rgb(0,1,1,0.25), add = T, freq = T, cex.axis= 1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2021' &pitching_othani$ï..pitch_type == 'SL'],
     breaks = seq(60, 110, 2), col = rgb(1,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
legend(60,300, c('Curve Ball','Cutter Ball', "Fast Ball", "Split-Finger Ball", "Slider"),
       col = c(rgb(0,0,1,0.25), rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,1,1,0.25), rgb(1,1,0,0.25)), lwd = c(rep(10,5)), cex= 1 )

## Create histogram of Othani's 2022 speed distribution 
hist(pitching_othani$release_speed[pitching_othani$year == '2022' &pitching_othani$ï..pitch_type == 'CU'], breaks = seq(60, 110, 2),
     main = "Othani's speed distribution \nby differnt pitching types",
     xlab = 'Speed(mph)', ylab = '# (count)', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,350), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2022' &pitching_othani$ï..pitch_type == 'FC'],
     breaks = seq(60, 110, 2), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2022' &pitching_othani$ï..pitch_type == 'FF'],
     breaks = seq(60, 110, 2), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2022' &pitching_othani$ï..pitch_type == 'FS'],
     breaks = seq(60, 110, 2), col = rgb(0,1,1,0.25), add = T, freq = T, cex.axis= 1.5)
hist(pitching_othani$release_speed[pitching_othani$year == '2022' &pitching_othani$ï..pitch_type == 'SL'],
     breaks = seq(60, 110, 2), col = rgb(1,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
legend(60,300, c('Curve Ball','Cutter Ball', "Fast Ball", "Split-Finger Ball", "Slider"),
       col = c(rgb(0,0,1,0.25), rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,1,1,0.25), rgb(1,1,0,0.25)), lwd = c(rep(10,5)), cex= 1 )


## Create histogram of Othani's 2021 speed distribution 
hist(pitching_othani$release_speed[pitching_othani$year == '2021'], breaks = seq(60, 110, 2),
     main = "Othani's speed distribution \nby differnt pitching types",
     xlab = 'Speed(mph)', ylab = '# (count)', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,350), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


par(mfrow = c(2,1))

## Create histogram of Othani's 2021 speed distribution density
dense_2021_Othani_CU <- density(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'CU'], bw = 2)
plot(dense_2021_Othani_CU, main = "Othani's speed distribution density \nby differnt pitching types\n2021",xlab ='Speed(mph)', ylab = 'density', col = 'firebrick1', xlim = c(50,110), ylim = c(0,0.2), lty = 1, lwd = 2, cex.main = 1.5, cex.axis= 1.5, cex.lab= 1.5)
dense_2021_Othani_FC <- density(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'FC'], bw = 2)
lines(dense_2021_Othani_FC, col = 'deeppink1', lty = 1, lwd = 2)
dense_2021_Othani_FF <- density(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'FF'], bw = 2)
lines(dense_2021_Othani_FF, col = 'gold1', lty = 1, lwd = 2)
dense_2021_Othani_FS <- density(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'FS'], bw = 2)
lines(dense_2021_Othani_FS, col = 'green3', lty = 1, lwd = 2)
dense_2021_Othani_SL <- density(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'SL'], bw = 2)
lines(dense_2021_Othani_SL, col = 'dodgerblue', lty = 1, lwd = 2)
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'CU']), lty=2, lwd=2, col="red")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'FC']), lty=2, lwd=2, col="deeppink1")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'FF']), lty=2, lwd=2, col="goldenrod1")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'FS']), lty=2, lwd=2, col="green")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2021' & pitching_othani$ï..pitch_type == 'SL']), lty=2, lwd=2, col="blue")
legend(50,0.20, c('Curve Ball','Cutter Ball', "Fast Ball"), col = c('firebrick1', 'deeppink1', 'gold1'), lwd = 2, cex= 1 )




## Create histogram of Othani's 2022 speed distribution density
dense_2022_Othani_CU <- density(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'CU'], bw = 2)
plot(dense_2022_Othani_CU, main = "2022",xlab ='Speed(mph)', ylab = 'density', col = 'firebrick1', xlim = c(50,110), ylim = c(0,0.2), lty = 1, lwd = 2, cex.main = 1.5, cex.axis= 1.5, cex.lab= 1.5)
dense_2022_Othani_FC <- density(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'FC'], bw = 2)
lines(dense_2022_Othani_FC, col = 'deeppink1', lty = 1, lwd = 2, side = 5)
dense_2022_Othani_FF <- density(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'FF'], bw = 2)
lines(dense_2022_Othani_FF, col = 'gold1', lty = 1, lwd = 2, side = 5)
dense_2022_Othani_FS <- density(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'FS'], bw = 2)
lines(dense_2022_Othani_FS, col = 'green3', lty = 1, lwd = 2, side = 5)
dense_2022_Othani_SL <- density(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'SL'], bw = 2)
lines(dense_2022_Othani_SL, col = 'dodgerblue', lty = 1, lwd = 2, side = 5)
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'CU']), lty=2, lwd=2, col="red")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'FC']), lty=2, lwd=2, col="deeppink1")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'FF']), lty=2, lwd=2, col="goldenrod1")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'FS']), lty=2, lwd=2, col="green")
abline(v=median(pitching_othani$release_speed[pitching_othani$year == '2022' & pitching_othani$ï..pitch_type == 'SL']), lty=2, lwd=2, col="blue")
legend(50,0.20, c("Split-Finger Ball", "Slider"), col = c('green3', 'dodgerblue'), lwd = 2, cex= 1 )


arrows(x0 = 75, y0 = 0.15, x1 = 78, y1 = 0.15, length = 0.15, angle = 30, col =  'red', lwd = 2)
arrows(x0 = 87.2, y0 = 0.08, x1 = 90, y1 = 0.08, length = 0.15, angle = 30, col =  'deeppink1', lwd = 2)
arrows(x0 = 95.8, y0 = 0.18, x1 = 97, y1 = 0.18, length = 0.15, angle = 30, col =  'goldenrod1', lwd = 2)
arrows(x0 = 88.3, y0 = 0.18, x1 = 89.9, y1 = 0.18, length = 0.15, angle = 30, col =  'green', lwd = 2)
arrows(x0 = 82.3, y0 = 0.18, x1 = 84.75, y1 = 0.18, length = 0.15, angle = 30, col =  'blue', lwd = 2)

text(75,0.18,labels = "+3.3", adj = 0.5, lwd = 2)
text(87.1,0.11,labels = "+3.2", adj = 0, lwd = 2)
text(95.8,0.19,labels = "+1.2", adj = 1, lwd = 2)
text(88.3,0.19,labels = "+1.6", adj = 1, lwd = 2)
text(82.3,0.19,labels = "+2.45", adj = 1, lwd = 2)