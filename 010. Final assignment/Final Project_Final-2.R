# 1.0 Arranging data ----

# Read file from Baseball Savant: https://baseballsavant.mlb.com/statcast_search choosing Shohei Ohtani's pitching result
# Documentation : https://baseballsavant.mlb.com/csv-docs
Pitching_Ohtani <- read.csv("Shohei_Ohtani_Pitching_Baseball_Savant.csv")

# Read file from Baseball Savant: https://baseballsavant.mlb.com/statcast_search choosing Shohei Ohtani's bsatting result
# Documentation : https://baseballsavant.mlb.com/csv-docs
Batting_Ohtani <- read.csv("Shohei_Ohtani_Batting_Baseball_Savant.csv")


# Read file from Fangraphs:  https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2022&month=0&season1=2022&ind=0 choosing players by different year
Allplayers_Pitching_2022 <- read.csv("2022_MLB_pitching.csv")
Allplayers_Pitching_2021 <- read.csv("2021_MLB_pitching.csv")
Allplayers_Pitching_2020 <- read.csv("2020_MLB_pitching.csv")
Allplayers_Pitching_2019 <- read.csv("2019_MLB_pitching.csv")
Allplayers_Pitching_2018 <- read.csv("2018_MLB_pitching.csv")
Allplayers_Batting_2022 <- read.csv("2022_MLB_batting.csv")
Allplayers_Batting_2021 <- read.csv("2021_MLB_batting.csv")
Allplayers_Batting_2020 <- read.csv("2020_MLB_batting.csv")
Allplayers_Batting_2019 <- read.csv("2019_MLB_batting.csv")
Allplayers_Batting_2018 <- read.csv("2018_MLB_batting.csv")


#* 1.1 Data arranging/cleaning (Title only) ----
#* 1.1.0 Introducing variables----
#* I will include the key variables I will use in this table
#* Notice: Q: How to recognize if the table is belong to hitter or pitchers?
#*         A: Check the column "batter" and "pitcher"
#* Pitching_Ohtani's table:
#*    pitch_type       : The type of pitch derived from Statcast.
#*    game_date        : Date of the Game.
#*    release_speed    : Pitch velocities from 2008-16 are via Pitch F/X, and adjusted to roughly out-of-hand release point. All velocities from 2017 and beyond are Statcast, which are reported out-of-hand.
#*    player_name      : Player's name tied to the event of the search.
#*    description      : Description of the resulting pitch.
#*    batter           : MLB Player Id tied to the play event.
#*    pitcher          : MLB Player Id tied to the play event.
#*    events           : Event of the resulting Plate Appearance.
#*    description      :  Description of the resulting pitch.
#*    pfx_x            : Horizontal movement in feet from the catcher's perspective.
#*    pfx_z            : Vertical movement in feet from the catcher's perpsective.
#*    vx0              : The velocity of the pitch, in feet per second, in x-dimension, determined at y=50 feet.
#*    vy0              : The velocity of the pitch, in feet per second, in y-dimension, determined at y=50 feet.
#*    vz0              : The velocity of the pitch, in feet per second, in z-dimension, determined at y=50 feet.
#*    hit_distance     : Projected hit distance of the batted ball.
#*    launch_speed     : Exit velocity of the batted ball as tracked by Statcast. For the limited subset of batted balls not tracked directly, estimates are included based on the process described here.
#*    launch_angle     : Launch angle of the batted ball as tracked by Statcast. For the limited subset of batted balls not tracked directly, estimates are included based on the process described here.
#*    release_extension: Release extension of pitch in feet as tracked by Statcast.
#*    spin_axis        : The Spin Axis in the 2D X-Z plane in degrees from 0 to 360, such that 180 represents a pure backspin fastball and 0 degrees represents a pure topspin (12-6) curveball
#*    release_spin_rate: Spin rate of pitch tracked by Statcast.
#*    pfx_x            : Horizontal movement in feet from the catcher's perspective.
#*    pfx_z            : Vertical movement in feet from the catcher's perpsective.
#*    plate_x          : Horizontal position of the ball when it crosses home plate from the catcher's perspective.
#*    plate_z          : Vertical position of the ball when it crosses home plate from the catcher's perspective.
#*    release_pos_x    : Horizontal Release Position of the ball measured in feet from the catcher's perspective.
#*    release_pos_z    : Vertical Release Position of the ball measured in feet from the catcher's perspective.


#* 1.1.1 Data arranging/cleaning of Ohtani's pitching ----
#* Re-naming the column: pitch type to avoid violating UTF-8 rule
names(Pitching_Ohtani)[1] <- c('pitch_type') 

#* Converting game_date from character to Date format
Pitching_Ohtani$game_date <- as.Date(Pitching_Ohtani$game_date, format="%m/%d/%Y")

#* Sorting data set from the oldest to the newest (in order to draw a scatter plot later)
Pitching_Ohtani <- Pitching_Ohtani[order(Pitching_Ohtani$game_date, decreasing = F),]

#* In order to separate data into different season, 
#* creating a column 'year' to separate year from date 
Pitching_Ohtani$year <- c(format(as.Date(Pitching_Ohtani$game_date, format="%m/%d/%Y"),"%Y"))

#* Found there is some row with NULL
(Pitching_Ohtani_aggre <- aggregate(release_speed~year*pitch_type, data = Pitching_Ohtani, FUN = mean))


#* Double-check which row is NULL, the answer is in it
is.na(Pitching_Ohtani$pitch_type)

summary(Pitching_Ohtani$pitch_type)
Pitching_Ohtani$pitch_type <- as.factor(Pitching_Ohtani$pitch_type)
Pitching_Ohtani$pitch_type <- as.character(Pitching_Ohtani$pitch_type)
table(is.na(Pitching_Ohtani$pitch_type))
table(Pitching_Ohtani$pitch_type)
dim(Pitching_Ohtani$pitch_type)
Pitching_Ohtani[Pitching_Ohtani$pitch_type == '',]

#* In order to show all data in the Console board, extending the print to 100,000
options(max.print=100000)

#* Using function to check NULL, but with no result
is.na(Pitching_Ohtani$pitch_type)

#* Checking the original data, and found Null on the #134 row
Pitching_Ohtani$pitch_type

#* Removing the 134th row 
Pitching_Ohtani <- Pitching_Ohtani[-134,]

#* Checking the summary table if there was an empty one and create an average table.
(Pitching_Ohtani_aggre <- aggregate(release_speed~year*pitch_type, data = Pitching_Ohtani, FUN = mean))

#* Creating an average standard deviation table.
(Pitching_Ohtani_aggre_sd <- aggregate(release_speed~year*pitch_type, data = Pitching_Ohtani, FUN = sd))


#* 1.1.2 Data arranging/cleaning of Ohtani's batting ----
#* Re-naming the column: pitch type to avoid violating UTF-8 rule
names(Batting_Ohtani)[1] <- c('pitch_type') 

#* Converting game_date from character to Date format
Batting_Ohtani$game_date <- as.Date(Batting_Ohtani$game_date, format="%m/%d/%Y")

#* Sorting data set from the oldest to the newest (in order to draw a scatter plot later)
Batting_Ohtani <- Batting_Ohtani[order(Batting_Ohtani$game_date, decreasing = F),]

#* In order to separate data into different seasons, 
#* creating a column 'year' to separate year from date 
Batting_Ohtani$year <- c(format(as.Date(Batting_Ohtani$game_date, format="%m/%d/%Y"),"%Y"))

#* Found there are some rows with NULL
aggregate(release_speed~year*pitch_type, data = Batting_Ohtani, FUN = mean)

#* Checking the original data, bingo, found them at row #329, #5867, #5870~5876
Batting_Ohtani$pitch_type


#* Removing the 329th, 5867th, 5870~5876th rows 
Batting_Ohtani <- Batting_Ohtani[-c(329, 5867, 5870:5876),]

#* Double-checking the summary table if there is an empty one
aggregate(release_speed~year*pitch_type, data = Batting_Ohtani, FUN = mean)



#* 1.1.3 Data arranging/cleaning of All pitchers' data set  ----
#* Creating a new variable 'year' for the following merging different data set
Allplayers_Pitching_2022$year <- c('2022')
Allplayers_Pitching_2021$year <- c('2021')
Allplayers_Pitching_2020$year <- c('2020')
Allplayers_Pitching_2019$year <- c('2019')
Allplayers_Pitching_2018$year <- c('2018')


#* 1.1.4 Data arranging/cleaning of All batters' data sets  ----
#* Creating a new variable 'year' for the following merging different data sets
Allplayers_Batting_2022$year <- c('2022')
Allplayers_Batting_2021$year <- c('2021')
Allplayers_Batting_2020$year <- c('2020')
Allplayers_Batting_2019$year <- c('2019')
Allplayers_Batting_2018$year <- c('2018')

#** 1.2.0 Data separating and further excluding (Title only) ----
#** 1.2.1 Data separating and further excluding of Ohtani's pitching ----
#** Separating Ohtani's pitching data into 4 different pitching types
Pitching_Ohtani_CU <- Pitching_Ohtani[Pitching_Ohtani$pitch_type == 'CU',]
Pitching_Ohtani_FC <- Pitching_Ohtani[Pitching_Ohtani$pitch_type == 'FC',]
Pitching_Ohtani_FF <- Pitching_Ohtani[Pitching_Ohtani$pitch_type == 'FF',]
Pitching_Ohtani_FS <- Pitching_Ohtani[Pitching_Ohtani$pitch_type == 'FS',]
Pitching_Ohtani_SL <- Pitching_Ohtani[Pitching_Ohtani$pitch_type == 'SL',]

#** Thoroughly checking if further cleaning of the table is needed with plots
plot(Pitching_Ohtani_CU[,c(1,3,5,28,29,48,49,50,57,58)])
plot(Pitching_Ohtani_FC[,c(1,3,5,28,29,48,49,50,57,58)])
plot(Pitching_Ohtani_FF[,c(1,3,5,28,29,48,49,50,57,58)])
plot(Pitching_Ohtani_FS[,c(1,3,5,28,29,48,49,50,57,58)])
plot(Pitching_Ohtani_SL[,c(1,3,5,28,29,48,49,50,57,58)])

#** Creating new variables to remove out-liners from the data
Pitching_Ohtani_CU_2 <- Pitching_Ohtani_CU[Pitching_Ohtani_CU$release_spin_rate >1000 & Pitching_Ohtani_CU$spin_axis <60,]
Pitching_Ohtani_FC_2 <- Pitching_Ohtani_FC[Pitching_Ohtani_FC$release_spin_rate >1000,]
Pitching_Ohtani_FF_2 <- Pitching_Ohtani_FF[Pitching_Ohtani_FF$release_spin_rate >1500 & Pitching_Ohtani_FF$release_spin_rate <2800,]
Pitching_Ohtani_SL_2 <- Pitching_Ohtani_SL[Pitching_Ohtani_SL$release_spin_rate >1500 & Pitching_Ohtani_SL$release_spin_rate <3200,]


##**** Calculating 2021 Ohtani's pitching in different pitching types
pie_table_Ohtani_2021 <- table(Pitching_Ohtani$pitch_type[Pitching_Ohtani$year == '2021'])
##**** Creating a data frame
pie_table_Ohtani_2021 <- data.frame(pie_table_Ohtani_2021)
##**** Creating a paste
prop_Ohtani_2021 <- round(prop.table(pie_table_Ohtani_2021$Freq)*100,2)
paste_Ohtani_2021 <- paste(pie_table_Ohtani_2021$Var1, "\n", prop_Ohtani_2021, "%",  sep ="")


##**** Calculating 2022 Ohtani's pitching in different pitching types
pie_table_Ohtani_2022 <- table(Pitching_Ohtani$pitch_type[Pitching_Ohtani$year == '2022'])
##**** Creating a data frame
pie_table_Ohtani_2022 <- data.frame(pie_table_Ohtani_2022)
##**** Creating a paste
prop_Ohtani_2022 <- round(prop.table(pie_table_Ohtani_2022$Freq)*100,2)
paste_Ohtani_2022 <- paste(pie_table_Ohtani_2022$Var1, "\n", prop_Ohtani_2022, "%",  sep ="")



#** 1.2.2 Data separating and further excluding of Ohtani's batting ----
#** 1.2.2.1 Data separating Ohtani's batting

#** Transferring the column 'events' to be factor from character
Batting_Ohtani$events <- as.factor(Batting_Ohtani$events)

#** As a result, I filter the column 'description', and only choose description = hit_into_play
Batting_Ohtani <- Batting_Ohtani[Batting_Ohtani$description == 'hit_into_play',]


#** Setting distance >100 ft for the following analysis
Batting_Ohtani_100 <- Batting_Ohtani[!is.na(Batting_Ohtani$hit_distance_sc),]
Batting_Ohtani_100 <- Batting_Ohtani_100[Batting_Ohtani_100$hit_distance_sc >= 100,]



#** 1.2.2.2 Aggregate Othani's batting launch speed by mean/median/standard deviation
#** Used for section 3.1
#** Using aggregate function to calculate Ohtani's average launch speed
aggregate(launch_speed~year, data = Batting_Ohtani_100, FUN = median)
aggregate(launch_speed~year, data = Batting_Ohtani_100, FUN = mean)
aggregate(launch_speed~year, data = Batting_Ohtani_100, FUN = sd)
Batting_Ohtani_100_aggre_ls <- aggregate(launch_speed~year, data = Batting_Ohtani_100, FUN = mean)

#** Using aggregate function to calculate Ohtani's average launch angle
aggregate(launch_angle~year, data = Batting_Ohtani_100, FUN = median)
aggregate(launch_angle~year, data = Batting_Ohtani_100, FUN = mean)
aggregate(launch_angle~year, data = Batting_Ohtani_100, FUN = sd)
Batting_Ohtani_100_aggre_la <- aggregate(launch_angle~year, data = Batting_Ohtani_100, FUN = mean)


#** 1.2.2.3 Aggregate Othani's batting launch speed + pitching type by mean + melt by year
#** Used for section 3.2
#** Using aggregation function to calculate Ohtani's average launch speed
Batting_Ohtani_100_ls_year <- aggregate(launch_speed~pitch_type*year, data = Batting_Ohtani_100, FUN = mean)
#** Creating a data frame
Batting_Ohtani_100_ls_year <- as.data.frame(Batting_Ohtani_100_ls_year)
#** Rounding average lanuch speed to decimal = 1
Batting_Ohtani_100_ls_year$launch_speed <- round(Batting_Ohtani_100_ls_year$launch_speed,1)


#** Melting year to summary a table with year V.S. launch speed
library(reshape2)
(Batting_Ohtani_100_ls_tyear <- dcast(Batting_Ohtani_100_ls_year, pitch_type ~ year, value.var = 'launch_speed'))



#** Using aggregation function to calculate Ohtani's average launch angle
Batting_Ohtani_100_la_year <- aggregate(launch_angle~pitch_type*year, data = Batting_Ohtani_100, FUN = mean)
#** Creating a data frame
Batting_Ohtani_100_la_year <- as.data.frame(Batting_Ohtani_100_la_year)
#** Rounding average lanuch angle to decimal = 1
Batting_Ohtani_100_la_year$launch_angle <- round(Batting_Ohtani_100_la_year$launch_angle,1)
#** Re-naming the pitch type to avoid viloating UTF-8 rule
names(Batting_Ohtani_100_la_year) <- c('pitch_type', 'year', 'launch_angle')

#** Melting year to summary a table with year V.S. launch angle
(Batting_Ohtani_100_la_tyear <- dcast(Batting_Ohtani_100_la_year, pitch_type ~ year, value.var = 'launch_angle'))


#** Melting year to summary a table with year V.S. spin rate
Pitching_Ohtani_spinrate_table <- aggregate(release_spin_rate~pitch_type, data = Pitching_Ohtani, FUN = mean)
Pitching_Ohtani_spinrate_table_year <- aggregate(release_spin_rate~pitch_type*year, data = Pitching_Ohtani, FUN = mean)

(Pitching_Ohtani_spinrate_table_year <- dcast(Pitching_Ohtani_spinrate_table_year, pitch_type ~ year, value.var = 'release_spin_rate'))


#** Calculating 2022's launch speed sd and non-2022' launch speed sd.
Batting_Ohtani_100$checkyear <- 'non_2022'
Batting_Ohtani_100$checkyear[Batting_Ohtani_100$year == '2022'] <- '2022'


Batting_Ohtani_100_ls_checkyear_mean <- aggregate(launch_speed~pitch_type*checkyear, data = Batting_Ohtani_100, FUN = mean)
Batting_Ohtani_100_ls_checkyear_mean$launch_speed <- round(Batting_Ohtani_100_ls_checkyear_mean$launch_speed,1)
(Batting_Ohtani_100_ls_checkyear_mean <- dcast(Batting_Ohtani_100_ls_checkyear_mean, pitch_type ~ checkyear, value.var = 'launch_speed'))


Batting_Ohtani_100_ls_checkyear_sd <- aggregate(launch_speed~pitch_type*checkyear, data = Batting_Ohtani_100, FUN = sd)
Batting_Ohtani_100_ls_checkyear_sd$launch_speed <- round(Batting_Ohtani_100_ls_checkyear_sd$launch_speed,1)
(Batting_Ohtani_100_ls_checkyear_sd <- dcast(Batting_Ohtani_100_ls_checkyear_sd, pitch_type ~ checkyear, value.var = 'launch_speed'))



#** Save data into a file with two sheets
library(openxlsx)
sheet <- list("launch_speed" = Batting_Ohtani_100_ls_tyear, "launch_speed_by_year_mean" = Batting_Ohtani_100_ls_checkyear_mean, "launch_speed_by_year_sd" = Batting_Ohtani_100_ls_checkyear_sd, "launch_angle" = Batting_Ohtani_100_la_tyear, "spin_rate" = Pitching_Ohtani_spinrate_table_year)
write.xlsx(sheet, file = "Save_Ohtani_batting.xlsx",  rowNames = F, overwrite = TRUE)


#** Removing needless variables
remove(sheet)

#** 1.2.3 Data merging of different years of pitchers ----
#** 1.2.3.1 Merging MLB players' data and calculating pitchers' HR/FB by different years
#** Used at Section 4.1
#** Definition HR/FB (Home Run To Fly Ball Rate) : https://www.mlb.com/glossary/advanced-stats/home-run-to-fly-ball-rate
#** Merging MLB players' data from different years
Allplayers_Pitching <- rbind(Allplayers_Pitching_2022, Allplayers_Pitching_2021, Allplayers_Pitching_2020, Allplayers_Pitching_2019, Allplayers_Pitching_2018)

#** Re-naming the column: name to avoid violating UTF-8 rule
names(Allplayers_Pitching)[1] <- 'name'

#** Removing needless variables
remove(Allplayers_Pitching_2022, Allplayers_Pitching_2021, Allplayers_Pitching_2020, Allplayers_Pitching_2019, Allplayers_Pitching_2018)


#** Converting to numeric from percentage (%)
Allplayers_Pitching$HR.FB <- as.numeric(sub("%", "", Allplayers_Pitching$HR.FB))
#** Aggregating the pitching HR/FB by different years
(Allplayers_Pitching_HRFB <- aggregate(HR.FB~year, data = Allplayers_Pitching, FUN = mean))



#** 1.2.4 Data merging of different years of batters ----
#** 1.2.4.1 Merging MLB batters' data and calculating batters' AVG/OBP/SLG by different years
#** Used at Section 4.1
#** Merging of different years of batters
Allplayers_Batting <- rbind(Allplayers_Batting_2022, Allplayers_Batting_2021, Allplayers_Batting_2020, Allplayers_Batting_2019, Allplayers_Batting_2018)

#** Re-naming the column: name to avoid violating UTF-8 rule
names(Allplayers_Batting)[1] <- 'name'

#** Removing the needless variables
remove(Allplayers_Batting_2022, Allplayers_Batting_2021, Allplayers_Batting_2020, Allplayers_Batting_2019, Allplayers_Batting_2018)

#** Calculating batters' AVG by different years
(Allplayers_Batting_AVG <- aggregate(AVG~year, data = Allplayers_Batting, FUN = mean))
#** Calculating batters' OBP by different years
(Allplayers_Batting_OBP <- aggregate(OBP~year, data = Allplayers_Batting, FUN = mean))
#** Calculating batters' SLG by different years
(Allplayers_Batting_SLG <- aggregate(SLG~year, data = Allplayers_Batting, FUN = mean))



## 2.0 Analyzing and plotting (using Ohtani's pitching table) (Title only) ----
##* 2.1 Plotting Scatter plots ----
##* 2.1.1 Ohtani's pitching result of different types pitching V.S 
##* releasing speed and standard deviation group by year
##* Ohtani's Curve-ball's pitching average release speed and its standard deviation ##
par(mfrow = c(1,1))
plot(type = "b", Pitching_Ohtani_aggre$year[Pitching_Ohtani_aggre$pitch_type == 'CU'], Pitching_Ohtani_aggre$release_speed[Pitching_Ohtani_aggre$pitch_type == 'CU'], main = "Shohei Ohtani's Curve ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', lty = 3, lwd = 5, col = 'red', pch = 19, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
par(new = TRUE)
plot(type = "b", Pitching_Ohtani_aggre_sd$release_speed[Pitching_Ohtani_aggre_sd$pitch_type == 'CU'], ylim = c(0,10), main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', lty = 3, lwd = 5, col = 'green', pch = 19)
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Standard deviation", side = 4, line = -1.2, font = 1, cex = 1.5)
legend("topleft", c("Average speed", "Stand deviation of speed"), lty = 3, lwd = 5, col = c('red', 'green'), pch = 19 )


##* Ohtani's Cutter-ball's pitching average release speed and its standard deviation ##
par(mfrow = c(1,1))
plot(type = "b", Pitching_Ohtani_aggre$year[Pitching_Ohtani_aggre$pitch_type == 'FC'], Pitching_Ohtani_aggre$release_speed[Pitching_Ohtani_aggre$pitch_type == 'FC'], main = "Shohei Ohtani's Cutter ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', lty = 3, lwd = 5, col = 'red', pch = 19, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
par(new = TRUE)
plot(type = "b", Pitching_Ohtani_aggre_sd$release_speed[Pitching_Ohtani_aggre_sd$pitch_type == 'FC'], ylim = c(0,10), main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', lty = 3, lwd = 5, col = 'green', pch = 19)
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Standard deviation", side = 4, line = -1.2, font = 1, cex = 1.5)
legend("topleft", c("Average speed", "Stand deviation of speed"), lty = 3, lwd = 5, col = c('red', 'green'), pch = 19 )

##* Ohtani's Fast-ball's pitching average release speed and its standard deviation ##
par(mfrow = c(1,1))
plot(type = "b", Pitching_Ohtani_aggre$year[Pitching_Ohtani_aggre$pitch_type == 'FF'], Pitching_Ohtani_aggre$release_speed[Pitching_Ohtani_aggre$pitch_type == 'FF'], main = "Shohei Ohtani's Fast ball's \npitching speed(release)", ylim = c(90,100), xlab = 'Year', ylab = 'Release speed', lty = 3, lwd = 5, col = 'red', pch = 19, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
par(new = TRUE)
plot(type = "b", Pitching_Ohtani_aggre_sd$release_speed[Pitching_Ohtani_aggre_sd$pitch_type == 'FF'], ylim = c(0,10), main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', lty = 3, lwd = 5, col = 'green', pch = 19)
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Standard deviation", side = 4, line = -1.2, font = 1, cex = 1.5)
legend("topleft", c("Average speed", "Stand deviation of speed"), lty = 3, lwd = 5, col = c('red', 'green'), pch = 19 )


##* Ohtani's Split-finger-ball's pitching average release speed and its standard deviation ##
par(mfrow = c(1,1))
plot(type = "b", Pitching_Ohtani_aggre$year[Pitching_Ohtani_aggre$pitch_type == 'FS'], Pitching_Ohtani_aggre$release_speed[Pitching_Ohtani_aggre$pitch_type == 'FS'], main = "Shohei Ohtani's Split-finger ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', lty = 3, lwd = 5, col = 'red', pch = 19, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
par(new = TRUE)
plot(type = "b", Pitching_Ohtani_aggre_sd$release_speed[Pitching_Ohtani_aggre_sd$pitch_type == 'FS'], ylim = c(0,10), main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', lty = 3, lwd = 5, col = 'green', pch = 19)
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Standard deviation", side = 4, line = -1.2, font = 1, cex = 1.5)
legend("topleft", c("Average speed", "Stand deviation of speed"), lty = 3, lwd = 5, col = c('red', 'green'), pch = 19 )


##* Ohtani's Slider-ball's pitching average release speed and its standard deviation ##
par(mfrow = c(1,1))
plot(type = "b", Pitching_Ohtani_aggre$year[Pitching_Ohtani_aggre$pitch_type == 'SL'], Pitching_Ohtani_aggre$release_speed[Pitching_Ohtani_aggre$pitch_type == 'SL'], main = "Shohei Ohtani's Slider ball's \npitching speed(release)", xlab = 'Year', ylab = 'Release speed', lty = 3, lwd = 5, col = 'red', pch = 19, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
par(new = TRUE)
plot(type = "b", Pitching_Ohtani_aggre_sd$release_speed[Pitching_Ohtani_aggre_sd$pitch_type == 'SL'], ylim = c(0,10), main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', lty = 3, lwd = 5, col = 'green', pch = 19)
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Standard deviation", side = 4, line = -1.2, font = 1, cex = 1.5)
legend("topleft", c("Average speed", "Stand deviation of speed"), lty = 3, lwd = 5, col = c('red', 'green'), pch = 19 )

##* Ohtani's pfx_x V.S pfx_z
plot(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'CU'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'CU'], lty = 3, lwd = 1, ylim = c(-2,2.5), xlim = c(-2,2.3), col = 'red', pch = 5, xlab ='pfx_x', ylab = 'pfx_z', main ='pfx_x V.S pfx_z', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FC'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FC'], lty = 3, lwd = 1, col = 'orange', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FF'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FF'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FS'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FS'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'SL'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'SL'], lty = 3, lwd = 1, col = 'cyan4', pch = 5)
legend("bottomleft", c("Curve", "Cutter", "Fast", "Split-finger", "Slider"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =1, pch = 5)

##* Ohtani Curveball's pfx_x V.S pfx_z by year
plot(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(0,1.5), ylim = c(-2,0), col = 'red', pch = 5, xlab ='pfx_x', ylab = 'pfx_z', main ="Ohtani Curveball's pfx_x V.S pfx_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)

##* Ohtani Cutter's pfx_x V.S pfx_z by year
plot(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-0.5,1.5), ylim = c(-0.5,1.5), col = 'red', pch = 5, xlab ='pfx_x', ylab = 'pfx_z', main ="Ohtani Cutter ball's pfx_x V.S pfx_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)

##* Ohtani Fast's pfx_x V.S pfx_z by year
plot(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-1.5,0.5), ylim = c(0.5,2), col = 'red', pch = 5, xlab ='pfx_x', ylab = 'pfx_z', main ="Ohtani Fast ball's pfx_x V.S pfx_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)

##* Ohtani Split-finger's pfx_x V.S pfx_z by year
plot(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-2,1), ylim = c(-1,2), col = 'red', pch = 5, xlab ='pfx_x', ylab = 'pfx_z', main ="Ohtani Split finger's pfx_x V.S pfx_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)

##* Ohtani Slider's pfx_x V.S pfx_z by year
plot(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(0,2.5), ylim = c(-1,2), col = 'red', pch = 5, xlab ='pfx_x', ylab = 'pfx_z', main ="Ohtani Slider's pfx_x V.S pfx_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)


##* Ohtani's plate_x V.S plate_z 
plot(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'CU'], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'CU'], lty = 3, lwd = 1, xlim = c(-4,4.5), ylim = c(-2,7), col = 'red', pch = 5, xlab ='pfate_x', ylab = 'pfate_z', main ="Ohtani's pitching pfate_x V.S pfate_z ", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FC' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FC'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FF'], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FF'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FS' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FS' ], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'SL' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'SL'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("bottomleft", c("Curve", "Cutter", "Fast", "Split-finger", "Slider"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =1, pch = 5)



##* Ohtani Curveball's plate_x V.S plate_z by year
plot(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-3,4.5), ylim = c(-2,7), col = 'red', pch = 5, xlab ='plate_x', ylab = 'plate_z', main ="Ohtani Curve ball's plate_x V.S plate_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)



##* Ohtani Cutter's plate_x V.S plate_z by year
plot(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-3,4.5), ylim = c(-2,7), col = 'red', pch = 5, xlab ='plate_x', ylab = 'plate_z', main ="Ohtani Cutter ball's plate_x V.S plate_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)


##* Ohtani Fast ball's plate_x V.S plate_z by year
plot(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-3,4.5), ylim = c(-2,7), col = 'red', pch = 5, xlab ='plate_x', ylab = 'plate_z', main ="Ohtani Fast ball's plate_x V.S plate_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)


##* Ohtani Split-finger's plate_x V.S plate_z by year
plot(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-3,4.5), ylim = c(-2,7), col = 'red', pch = 5, xlab ='plate_x', ylab = 'plate_z', main ="Ohtani Split-finger's plate_x V.S plate_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)



##* Ohtani Slider's plate_x V.S plate_z by year
plot(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-3,4.5), ylim = c(-2,7), col = 'red', pch = 5, xlab ='plate_x', ylab = 'plate_z', main ="Ohtani Slider's plate_x V.S plate_z", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$plate_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$plate_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)



##* Ohtani 2018's release_pos_x V.S release_pos_z
plot(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'CU'], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'CU'], lty = 3, lwd = 1, xlim = c(-4,-1), ylim = c(5.2,7), col = 'red', pch = 5, xlab ='release_pos_x', ylab = 'release_pos_z', main ='release_pos_x V.S release_pos_z', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FC'], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FC'], lty = 3, lwd = 1, col = 'orange', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FF'], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FF'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FS'], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FS'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL'], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL'], lty = 3, lwd = 1, col = 'cyan4', pch = 5)
legend("bottomleft", c("Curve", "Cutter", "Fast", "Split-finger", "Slider"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =1, pch = 5)


##* Ohtani Slider's release_pos_x V.S release_pos_z in different years
plot(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018'], lty = 3, lwd = 1, xlim = c(-3,-1), ylim = c(5.2,6.5), col = 'red', pch = 5, xlab ='release_pos_x', ylab = 'release_pos_z', main ='release_pos_x V.S release_pos_z \nby year', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2019' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2019'], lty = 3, lwd = 1, col = 'orange', pch = 5, )
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2020' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2020'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022'], lty = 3, lwd = 1, col = 'darkorchid2', pch = 5)
legend("topright", c("2018", "2019", "2020", "2021", "2022"), col = c("red", "orange", "gold1", "limegreen", 'darkorchid2'), lty = 3, lwd =1, pch = 5)



par(mfrow = c(1,1))
##* Ohtani 2018's release_pos_x V.S release_pos_z in different years
plot(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2018' ], lty = 3, lwd = 1, xlim = c(-4,-1), ylim = c(5.2,7), col = 'red', pch = 5, xlab ='release_pos_x', ylab = 'release_pos_z', main ='release_pos_x V.S release_pos_z in 2018', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2018' ], lty = 3, lwd = 1, col = 'orange', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2018' ], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2018' ], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2018' ], lty = 3, lwd = 1, col = 'cyan4', pch = 5)
legend("bottomleft", c("Curve", "Cutter", "Fast", "Split-finger", "Slider"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =1, pch = 5)




par(mfrow = c(1,1))
##* Ohtani 2021's release_pos_x V.S release_pos_z in different years
plot(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2021' ], lty = 3, lwd = 1, xlim = c(-4,-1), ylim = c(5.2,7), col = 'red', pch = 5, xlab ='release_pos_x', ylab = 'release_pos_z', main ='2021', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2021' ], lty = 3, lwd = 1, col = 'orange', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2021' ], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2021' ], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2021' ], lty = 3, lwd = 1, col = 'cyan4', pch = 5)
legend("bottomleft", c("Curve", "Cutter", "Fast", "Split-finger", "Slider"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =1, pch = 5)


par(mfrow = c(1,1))
##* Ohtani 2022's release_pos_x V.S release_pos_z in different years
plot(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'CU' & Pitching_Ohtani$year == '2022' ], lty = 3, lwd = 1, xlim = c(-4,-1), ylim = c(5.2,7), col = 'red', pch = 5, xlab ='release_pos_x', ylab = 'release_pos_z', main ='2022', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FC' & Pitching_Ohtani$year == '2022' ], lty = 3, lwd = 1, col = 'orange', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FF' & Pitching_Ohtani$year == '2022' ], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'FS' & Pitching_Ohtani$year == '2022' ], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$release_pos_x[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022' ], Pitching_Ohtani$release_pos_z[Pitching_Ohtani$pitch_type == 'SL' & Pitching_Ohtani$year == '2022' ], lty = 3, lwd = 1, col = 'cyan4', pch = 5)
legend("bottomleft", c("Curve", "Cutter", "Fast", "Split-finger", "Slider"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =1, pch = 5)


par(mfrow = c(1,1))
##* Removing needless variables
remove(Pitching_Ohtani_aggre, Pitching_Ohtani_aggre_sd)


##* 2.2.2 Plotting scatter plot of Ohtani's pitching speed V.S. date

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's pitching release speed V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$release_speed, type = 'b', lty = 1, lwd = 1, col = 'firebrick1', ylim = c(65,120), xlab = 'Year', ylab = 'release speed', main = "Shohei Ohtani's pitching speed(release) \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$release_speed, type = 'b', lty = 1, lwd = 1, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$release_speed, type = 'b', lty = 1, lwd = 1, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$release_speed, type = 'b', lty = 1, lwd = 1, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$release_speed, type = 'b', lty = 1, lwd = 1, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's pitching spin rate V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$release_spin_rate, type = 'b', lty = 1, lwd = 1, ylim = c(800,3500), col = 'firebrick1', xlab = 'Year', ylab = 'release spin rate', main = "Shohei Ohtani's pitching spin rate by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$release_spin_rate, type = 'b', lty = 1, lwd = 1, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$release_spin_rate, type = 'b', lty = 1, lwd = 1, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$release_spin_rate, type = 'b', lty = 1, lwd = 1, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$release_spin_rate, type = 'b', lty = 1, lwd = 1, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's release ball's position V.S. date (no detail check by pitching type)
plot(Pitching_Ohtani$game_date, Pitching_Ohtani$release_extension, type = 'b', lty = 1, lwd = 1, col = 'firebrick1', ylim = c(5,7.5), xlab = 'Year', ylab = 'Release extension(in feet)', main = "Shohei Ohtani's ball's release position \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's release ball's position V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$release_extension, type = 'b', lty = 1, lwd = 1, col = 'firebrick1', ylim = c(5,7.5), xlab = 'Year', ylab = 'Release extension(in feet)', main = "Shohei Ohtani's ball's release position \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$release_extension, type = 'b', lty = 1, lwd = 1, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$release_extension, type = 'b', lty = 1, lwd = 1, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$release_extension, type = 'b', lty = 1, lwd = 1, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$release_extension, type = 'b', lty = 1, lwd = 1, col = 'dodgerblue2')
legend('bottomright', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's pfx_x V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-2,3.5), xlab = 'Year', ylab = 'pfx_x(in feet)', main = "Shohei Ohtani's pitching horizontal movement \nto homeplate by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$pfx_x, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's pfx_z V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-2,3.5), xlab = 'Year', ylab = 'pfx_z(in feet)', main = "Shohei Ohtani's pitching vertical movement \nto homeplate by date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$pfx_z, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's plate_x V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$plate_x, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-4,6), xlab = 'Year', ylab = 'plate_x(in feet)', main = "Shohei Ohtani's pitching plate_x \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$plate_x, type = 'l', lty = 1, lwd = 2, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$plate_x, type = 'l', lty = 1, lwd = 2, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$plate_x, type = 'l', lty = 1, lwd = 2, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$plate_x, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's plate_z V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$plate_z, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(-3,10), xlab = 'Year', ylab = 'plate_z(in feet)', main = "Shohei Ohtani's pitching plate_z \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$plate_z, type = 'l', lty = 1, lwd = 2, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$plate_z, type = 'l', lty = 1, lwd = 2, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$plate_z, type = 'l', lty = 1, lwd = 2, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$plate_z, type = 'l', lty = 1, lwd = 2, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's vx0 V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$vx0, type = 'b', lty = 1, lwd = 1, col = 'firebrick1', ylim = c(-5,20), xlab = 'Year', ylab = 'vx0(in feet)', main = "Shohei Ohtani's pitching vx0 \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$vx0, type = 'b', lty = 1, lwd = 1, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$vx0, type = 'b', lty = 1, lwd = 1, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$vx0, type = 'b', lty = 1, lwd = 1, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$vx0, type = 'b', lty = 1, lwd = 1, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's vy0 V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$vy0, type = 'b', lty = 1, lwd = 1, col = 'firebrick1', ylim = c(-150,-70), xlab = 'Year', ylab = 'vy0(in feet)', main = "Shohei Ohtani's pitching vy0 \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$vy0, type = 'b', lty = 1, lwd = 1, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$vy0, type = 'b', lty = 1, lwd = 1, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$vy0, type = 'b', lty = 1, lwd = 1, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$vy0, type = 'b', lty = 1, lwd = 1, col = 'dodgerblue2')
legend('topright', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's vz0 V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$vz0, type = 'b', lty = 1, lwd = 1, col = 'firebrick1', ylim = c(-15,20), xlab = 'Year', ylab = 'vz0(in feet)', main = "Shohei Ohtani's pitching vz0 \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$vz0, type = 'b', lty = 1, lwd = 1, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$vz0, type = 'b', lty = 1, lwd = 1, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$vz0, type = 'b', lty = 1, lwd = 1, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$vz0, type = 'b', lty = 1, lwd = 1, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)

par(mfrow = c(1,1))
##* Plotting scatter plot of Ohtani's spin_axis V.S. date
plot(Pitching_Ohtani_CU$game_date, Pitching_Ohtani_CU$spin_axis, type = 'b', lty = 1, lwd = 1, col = 'firebrick1', ylim = c(20,450), xlab = 'Year', ylab = 'spin_axis(in feet)', main = "Shohei Ohtani's pitching spin_axis \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(Pitching_Ohtani_FC$game_date, Pitching_Ohtani_FC$spin_axis, type = 'b', lty = 1, lwd = 1, col = 'deeppink1')
lines(Pitching_Ohtani_FF$game_date, Pitching_Ohtani_FF$spin_axis, type = 'b', lty = 1, lwd = 1, col = 'gold1')
lines(Pitching_Ohtani_FS$game_date, Pitching_Ohtani_FS$spin_axis, type = 'b', lty = 1, lwd = 1, col = 'green3')
lines(Pitching_Ohtani_SL$game_date, Pitching_Ohtani_SL$spin_axis, type = 'b', lty = 1, lwd = 1, col = 'dodgerblue2')
legend('topleft', c('Curve', 'Cutter', 'Fast', 'Split-finger', 'Slider'), col = c('firebrick1', 'deeppink1', 'gold1', 'green3', 'dodgerblue2'), lwd = 2, lty = 1)


##** 2.2 Checking correlation ----
##** Checking some variables if there were several correlations
plot(Pitching_Ohtani[,c(1,3,28,29,45,46,47,57,58,90)])


##** Checking some variables by ggscatmat
library(GGally)
ggscatmat(Pitching_Ohtani, column = c(1,3,28,29,45,46,47,57,58,90) )
ggscatmat(Pitching_Ohtani_CU_2, column = c(1,3,28,29,45,46,47,57,58,90) )


##** Using Curve-ball's and Curve-ball+ modified data to check some variables if there were several correlations
plot(Pitching_Ohtani_CU[,c(1,3,28,29,45,46,47,57,58,90)])
plot(Pitching_Ohtani_CU_2[,c(1,3,28,29,45,46,47,57,58,90)])

##** Using Cutter-ball and Cutter-ball + modified data to check some variables if there were several correlations
plot(Pitching_Ohtani_FC[,c(1,3,28,29,45,46,47,57,58,90)])
plot(Pitching_Ohtani_FC_2[,c(1,3,28,29,45,46,47,57,58,90)])

##** Using Fast and Fast + modified data to check some variables if there were several correlations
plot(Pitching_Ohtani_FF[,c(1,3,28,29,45,46,47,57,58,90)])
plot(Pitching_Ohtani_FF_2[,c(1,3,28,29,45,46,47,57,58,90)])

##** Using Split-Finger to check some variables if there were several correlations
plot(Pitching_Ohtani_FS[,c(1,3,28,29,45,46,47,57,58,90)])

##** Using Slider and Slider + modified data to check some variables if there were several correlations
plot(Pitching_Ohtani_SL[,c(1,3,28,29,45,46,47,57,58,90)])
plot(Pitching_Ohtani_SL_2[,c(1,3,28,29,45,46,47,57,58,90)])

##** Checking the spin rate vs speed and pitch_type linear regression model (overall)
Ohtani_pitching_m1 <- lm(release_spin_rate~release_speed+pitch_type, data = Pitching_Ohtani)
summary(Ohtani_pitching_m1)

##** Checking release speed vs spin rate, and pitch_type and plotting a linear regression model
Ohtani_pitching_m2 <- lm(release_speed~release_spin_rate+pitch_type, data = Pitching_Ohtani)
summary(Ohtani_pitching_m2)

##** Checking the release speed vs all key variables, and plotting a linear regression model (overall)
Ohtani_pitching_m3 <- lm(release_speed~ pitch_type + vx0 + vy0 + vz0 + release_spin_rate + release_extension +spin_axis, data = Pitching_Ohtani)
summary(Ohtani_pitching_m3)
step(Ohtani_pitching_m3, direction = "backward")

##** Checking the horizontal movement vs key variables, and plotting a linear regression model (overall)
Ohtani_pitching_m4 <- lm(pfx_x~release_speed + release_spin_rate+pitch_type + release_pos_z + vx0 + vy0 + vz0, data = Pitching_Ohtani)
summary(Ohtani_pitching_m4)
step(Ohtani_pitching_m4, direction = "backward")

##** Checking the vertical movement vs key variables, and plotting a linear regression model (overall)
Ohtani_pitching_m5 <- lm(pfx_z~release_speed + release_spin_rate+pitch_type + release_pos_z + vx0 + vy0 + vz0, data = Pitching_Ohtani)
summary(Ohtani_pitching_m5)
step(Ohtani_pitching_m5, direction = "backward")


##** Checking the spin axis movement vs key variables, and plot a linear regression model (overall)
Ohtani_pitching_m6 <- lm(spin_axis~release_speed + release_spin_rate+pitch_type + release_pos_z + ax + ay + az + pfx_x + pfx_z, data = Pitching_Ohtani)
summary(Ohtani_pitching_m6)
step(Ohtani_pitching_m6, direction = "backward")


library(corrplot)

##*** Checking correlation
##*** Column number = 57(release_spin_rate) , 58(release_extension), 90(spin_axis) can not create correlation number
cor(Pitching_Ohtani[,c(3,5,28,29,45,46,47,57,58,90)])
cor(Pitching_Ohtani[,c(3,5,28,29,45,46,47)])

##*** Checking Curve-ball only found it's ok
##*** Maybe there are some clusters inside the overall data which causes it difficult to predict
cor(Pitching_Ohtani_CU_2[,c(3,5,28,29,45,46,47,57,58,90)])
cor(Pitching_Ohtani_CU_2[,c(3,57,58)])

##*** Plotting correlation graph using Curve-ball table: 
corrplot(cor(Pitching_Ohtani_CU_2[,c(3,5,28,29,45,46,47,57,58,90)]))
##*** Plotting correlation graph using Cutter-ball table: 
corrplot(cor(Pitching_Ohtani_FC_2[,c(3,5,28,29,45,46,47,57,58,90)]))
##*** Plotting correlation graph using Fastball table: it still can not generate data, maybe there are still different clusters
corrplot(cor(Pitching_Ohtani_FF_2[,c(3,5,28,29,45,46,47,57,58,90)]))
##*** Plotting correlation graph using Split-Finger table: it still can not generate data, maybe there are still different clusters
corrplot(cor(Pitching_Ohtani_FS[,c(3,5,28,29,45,46,47,57,58,90)]))
##*** Plotting correlation graph using Slider table: it still can not generate data, maybe there are still different clusters
corrplot(cor(Pitching_Ohtani_SL_2[,c(3,5,28,29,45,46,47,57,58,90)]))




##*** Removing needless variables
remove(Ohtani_pitching_m1, Ohtani_pitching_m2, Ohtani_pitching_m3, Ohtani_pitching_m4, Ohtani_pitching_m5, Ohtani_pitching_m6)


##*** 2.3 Plotting Pie charts ----
##*** Open library "viridis"
library("viridis")



##*** Drawing pie charts
par(mfrow = c(1,2))
pie(pie_table_Ohtani_2021$Freq, label = paste_Ohtani_2021, col = viridis(5), lty =3, radius = 1,
    main = "2021 \nOhtani pitching's variance")
pie(pie_table_Ohtani_2022$Freq, label = paste_Ohtani_2022, col = viridis(5), lty =3, radius = 1,
    main = "2022 \n")
par(mfrow = c(1,1))


##*** Removing needless variables
remove(paste_Ohtani_2021, paste_Ohtani_2022, prop_Ohtani_2021, prop_Ohtani_2022, pie_table_Ohtani_2021, pie_table_Ohtani_2022)


##**** 2.4 Creating histogram charts ---- 
##**** Creating a histogram chart of Ohtani's 2021 release speed distribution
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' &Pitching_Ohtani$pitch_type == 'CU'], breaks = seq(60, 110, 2),
      main = "Ohtani's speed distribution \nby differnt pitching types",
     xlab = 'Speed(mph)', ylab = '# (count)', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,350), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' &Pitching_Ohtani$pitch_type == 'FC'],
     breaks = seq(60, 110, 2), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' &Pitching_Ohtani$pitch_type == 'FF'],
     breaks = seq(60, 110, 2), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' &Pitching_Ohtani$pitch_type == 'FS'],
     breaks = seq(60, 110, 2), col = rgb(0,1,1,0.25), add = T, freq = T, cex.axis= 1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' &Pitching_Ohtani$pitch_type == 'SL'],
     breaks = seq(60, 110, 2), col = rgb(1,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
legend(60,300, c('Curve Ball','Cutter Ball', "Fast Ball", "Split-Finger Ball", "Slider"),
       col = c(rgb(0,0,1,0.25), rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,1,1,0.25), rgb(1,1,0,0.25)), lwd = c(rep(10,5)), cex= 1 )

##**** Creating a histogram chart of Ohtani's 2022 speed distribution 
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' &Pitching_Ohtani$pitch_type == 'CU'], breaks = seq(60, 110, 2),
     main = "Ohtani's speed distribution \nby differnt pitching types",
     xlab = 'Speed(mph)', ylab = '# (count)', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,350), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' &Pitching_Ohtani$pitch_type == 'FC'],
     breaks = seq(60, 110, 2), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' &Pitching_Ohtani$pitch_type == 'FF'],
     breaks = seq(60, 110, 2), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' &Pitching_Ohtani$pitch_type == 'FS'],
     breaks = seq(60, 110, 2), col = rgb(0,1,1,0.25), add = T, freq = T, cex.axis= 1.5)
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' &Pitching_Ohtani$pitch_type == 'SL'],
     breaks = seq(60, 110, 2), col = rgb(1,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
legend(60,300, c('Curve Ball','Cutter Ball', "Fast Ball", "Split-Finger Ball", "Slider"),
       col = c(rgb(0,0,1,0.25), rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,1,1,0.25), rgb(1,1,0,0.25)), lwd = c(rep(10,5)), cex= 1 )


##**** Creating a histogram chart of Ohtani's 2021 speed distribution 
hist(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021'], breaks = seq(60, 110, 2),
     main = "Ohtani's speed distribution \nby differnt pitching types",
     xlab = 'Speed(mph)', ylab = '# (count)', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,350), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


par(mfrow = c(2,1))
##**** Creating a histogram chart of Ohtani's 2021 speed distribution density
dense_2021_Ohtani_CU <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'CU'], bw = 2)
plot(dense_2021_Ohtani_CU, main = "2021 Ohtani's speed distribution density \nby differnt pitching types\n",xlab ='Speed(mph)', ylab = 'density', col = 'firebrick1', xlim = c(40,110), ylim = c(0,0.2), lty = 1, lwd = 2, cex.main = 1.5, cex.axis= 1.5, cex.lab= 1.5)
dense_2021_Ohtani_FC <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'FC'], bw = 2)
lines(dense_2021_Ohtani_FC, col = 'deeppink1', lty = 1, lwd = 2)
dense_2021_Ohtani_FF <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'FF'], bw = 2)
lines(dense_2021_Ohtani_FF, col = 'gold1', lty = 1, lwd = 2)
dense_2021_Ohtani_FS <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'FS'], bw = 2)
lines(dense_2021_Ohtani_FS, col = 'green3', lty = 1, lwd = 2)
dense_2021_Ohtani_SL <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'SL'], bw = 2)
lines(dense_2021_Ohtani_SL, col = 'dodgerblue', lty = 1, lwd = 2)
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'CU']), lty=2, lwd=2, col="red")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'FC']), lty=2, lwd=2, col="deeppink1")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'FF']), lty=2, lwd=2, col="goldenrod1")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'FS']), lty=2, lwd=2, col="green")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2021' & Pitching_Ohtani$pitch_type == 'SL']), lty=2, lwd=2, col="blue")
legend(40,0.20, c('Curve Ball','Cutter Ball', "Fast Ball"), col = c('firebrick1', 'deeppink1', 'gold1'), lwd = 2, cex= 1 )
##**** Creating a histogram chart of Ohtani's 2022 speed distribution density
dense_2022_Ohtani_CU <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'CU'], bw = 2)
plot(dense_2022_Ohtani_CU, main = "2022\n\n",xlab =' ', ylab = ' ', col = 'firebrick1', xlim = c(40,110), ylim = c(0,0.2), lty = 1, lwd = 2, cex.main = 1.5, cex.axis= 1.5, cex.lab= 1.5)
dense_2022_Ohtani_FC <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'FC'], bw = 2)
lines(dense_2022_Ohtani_FC, col = 'deeppink1', lty = 1, lwd = 2)
dense_2022_Ohtani_FF <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'FF'], bw = 2)
lines(dense_2022_Ohtani_FF, col = 'gold1', lty = 1, lwd = 2)
dense_2022_Ohtani_FS <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'FS'], bw = 2)
lines(dense_2022_Ohtani_FS, col = 'green3', lty = 1, lwd = 2)
dense_2022_Ohtani_SL <- density(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'SL'], bw = 2)
lines(dense_2022_Ohtani_SL, col = 'dodgerblue', lty = 1, lwd = 2)
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'CU']), lty=2, lwd=2, col="red")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'FC']), lty=2, lwd=2, col="deeppink1")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'FF']), lty=2, lwd=2, col="goldenrod1")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'FS']), lty=2, lwd=2, col="green")
abline(v=median(Pitching_Ohtani$release_speed[Pitching_Ohtani$year == '2022' & Pitching_Ohtani$pitch_type == 'SL']), lty=2, lwd=2, col="blue")
legend(40,0.20, c("Split-Finger Ball", "Slider"), col = c('green3', 'dodgerblue'), lwd = 2, cex= 1 )
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

par(mfrow = c(1,1))




##**** Removing needless variables
remove(dense_2021_Ohtani_CU, dense_2021_Ohtani_FC, dense_2021_Ohtani_FF, dense_2021_Ohtani_FS, dense_2021_Ohtani_SL, dense_2022_Ohtani_CU, dense_2022_Ohtani_FC, dense_2022_Ohtani_FF, dense_2022_Ohtani_FS, dense_2022_Ohtani_SL)


### 3.0 Analyzing and plotting (using Ohtani's batting table) (Title only) ----
###* 3.1 box plot ----
###* Drawing a box plot to check the Ohtani's launch speed sorting by the date
par(mfrow = c(1,1))
boxplot(launch_speed~year, data = Batting_Ohtani_100, col = c('yellow'), main = "Shohei Ohtani's batting launch speed \nby year", xlab = 'Year', ylab = 'Launch speed(mph)', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(type = 'b', Batting_Ohtani_100_aggre_ls$launch_speed, lty = 3, lwd = 5, col = 'red', pch = 19)
legend("bottomleft", c("Average launch speed"), lty = 3, lwd = 5, col = c('red'), pch = 19 )



###* Drawing a box plot to check the Ohtani's launch angle sorting by the date
par(mfrow = c(1,1))
boxplot(launch_angle~year, data = Batting_Ohtani_100, col = c('yellow'), main = "Shohei Ohtani's batting launch angle \nby year", xlab = 'Year', ylab = 'Launch angle', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(type = 'b', Batting_Ohtani_100_aggre_la$launch_angle, lty = 3, lwd = 5, col = 'red', pch = 19)
legend("topleft", c("Average launch angle"), lty = 3, lwd = 5, col = c('red'), pch = 19 )

###* Zooming-in to check the launch_angle's mean
par(mfrow = c(1,1))
boxplot(launch_angle~year, data = Batting_Ohtani_100, col = c('yellow'), main = "Shohei Ohtani's batting launch angle \nby year", ylim = c(10, 50), xlab = 'Year', ylab = 'Launch angle', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(type = 'b', Batting_Ohtani_100_aggre_la$launch_angle, lty = 3, lwd = 5, col = 'red', pch = 19)
legend("topleft", c("Average launch speed"), lty = 3, lwd = 5, col = c('red'), pch = 19 )


###* Removing needless variables
remove(Batting_Ohtani_100_aggre_la, Batting_Ohtani_100_aggre_ls)

###** 3.2 Scatter plot ----
###** Drawing a scatter plot the check Ohtani's launch speed by date
par(mfrow = c(1,1))
plot(Batting_Ohtani_100$game_date, Batting_Ohtani_100$launch_speed, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(40,120), xlab = 'Year', ylab = 'Launch speed(mph)', main = "Shohei Ohtani's batting launch speed \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("bottomleft", c("Average launch speed"), lty = 3, lwd = 5, col = c('red'), pch = 19 )

par(mfrow = c(1,1))
###** Drawing a scatter plot the check Ohtani's launch speed by date
plot(Batting_Ohtani_100$game_date, Batting_Ohtani_100$launch_angle, type = 'l', lty = 1, lwd = 2, col = 'firebrick1', ylim = c(0,80), xlab = 'Year', ylab = 'Launch angle', main = "Shohei Ohtani's batting launch speed \nby date", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("topleft", c("Average launch angle"), lty = 3, lwd = 5, col = c('red'), pch = 19 )



par(mfrow = c(1,1))
###** Plotting by Ohtani's batting launch speed by pitching type by year
plot(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'CH'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'CH'], lty = 1, lwd =2, ylim = c(35,112), pch = 19, col = 'red', xlab = "year", ylab = "launch_speed(mph)", main = "Ohtani's batting launch_speed \nin different years", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'CU'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'CU'], lty = 1, lwd =2, pch = 19, col = 'orange')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'FA'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'FA'], lty = 1, lwd =2, pch = 19, col = 'gold1')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'FC'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'FC'], lty = 1, lwd =2, pch = 19, col = 'limegreen')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'FF'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'FF'], lty = 1, lwd =2, pch = 19, col = 'cyan4')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'FO'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'FO'], lty = 1, lwd =2, pch = 19, col = 'darkorchid2')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'FS'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'FS'], lty = 1, lwd =2, pch = 19, col = 'mediumorchid4')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'KC'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'KC'], lty = 1, lwd =2, pch = 19, col = 'olivedrab4')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'SI'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'SI'], lty = 1, lwd =2, pch = 19, col = 'seagreen')
lines(type = 'b', Batting_Ohtani_100_ls_year$year[Batting_Ohtani_100_ls_year$pitch_type == 'SL'], Batting_Ohtani_100_ls_year$launch_speed[Batting_Ohtani_100_ls_year$pitch_type == 'SL'], lty = 1, lwd =2, pch = 19, col = 'tomato')
legend("bottomleft", c("CH (-14.9)", "CU (-9.7)", "FA (NA)", "FC (+7.4)", "FF (-1)", "FO (NA)", "FS (-27.5)", "KC (NA)", "SI (-4.2)", "SL (-5.1)"), col = c("red", "orange", "gold1", "limegreen", "cyan4", "darkorchid", "mediumorchid4", "olivedrab4", "seagreen", "tomato"), lty = 1, lwd =2, text.col = c("blue", "blue", "black", "black", "blue", "black", "blue", "black", "blue", "blue"), pch = 19)


par(mfrow = c(1,1))
###** Plotting by Ohtani's batting launch angle by pitching type by year
plot(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'CH'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'CH'], lty = 1, lwd =2, ylim = c(-30,60), pch = 19, col = 'red', xlab = "year", ylab = "launch_angle(mph)", main = "Ohtani's batting launch_angle \nin different years", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'CU'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'CU'], lty = 1, lwd =2, pch = 19, col = 'orange')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'FA'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'FA'], lty = 1, lwd =2, pch = 19, col = 'gold1')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'FC'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'FC'], lty = 1, lwd =2, pch = 19, col = 'limegreen')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'FF'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'FF'], lty = 1, lwd =2, pch = 19, col = 'cyan4')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'FO'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'FO'], lty = 1, lwd =2, pch = 19, col = 'darkorchid2')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'FS'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'FS'], lty = 1, lwd =2, pch = 19, col = 'mediumorchid4')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'KC'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'KC'], lty = 1, lwd =2, pch = 19, col = 'olivedrab4')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'SI'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'SI'], lty = 1, lwd =2, pch = 19, col = 'seagreen')
lines(type = 'b', Batting_Ohtani_100_la_year$year[Batting_Ohtani_100_la_year$pitch_type == 'SL'], Batting_Ohtani_100_la_year$launch_angle[Batting_Ohtani_100_la_year$pitch_type == 'SL'], lty = 1, lwd =2, pch = 19, col = 'tomato')
legend("bottomright", c("CH (-3.1)", "CU (3.4)", "FA (NA)", "FC (-3.4)", "FF (+3.4)", "FO (NA)", "FS (+7.5)", "KC (-1.6)", "SI (-0.4)", "SL (-0.8)"), col = c("red", "orange", "gold1", "limegreen", "cyan4", "darkorchid", "mediumorchid4", "olivedrab4", "seagreen", "tomato"), lty = 1, lwd =2, text.col = c("blue", "black", "black", "blue", "black", "black", "black", "blue", "blue", "blue"), pch = 19)

###** Removing needless variables
remove(Batting_Ohtani_100_la_year, Batting_Ohtani_100_ls_year)


###*** 3.3 T test for exit velocity (launch speed) & launch angle---- 
###*** Defining variables:
###*** Batting_Split finger ball launch speed:
Launch_speed_FS_2022 <- Batting_Ohtani_100$launch_speed[Batting_Ohtani_100$pitch_type == 'FS' & Batting_Ohtani_100$year == '2022']
Launch_speed_FS_not2022 <- Batting_Ohtani_100$launch_speed[Batting_Ohtani_100$pitch_type == 'FS' & Batting_Ohtani_100$year != '2022']

###*** Batting_Changeup ball launch speed:
Launch_speed_CH_2022 <- Batting_Ohtani_100$launch_speed[Batting_Ohtani_100$pitch_type == 'CH' & Batting_Ohtani_100$year == '2022']
Launch_speed_CH_not2022 <- Batting_Ohtani_100$launch_speed[Batting_Ohtani_100$pitch_type == 'CH' & Batting_Ohtani_100$year != '2022']


###*** Batting_Curve ball launch speed:
Launch_speed_CU_2022 <- Batting_Ohtani_100$launch_speed[Batting_Ohtani_100$pitch_type == 'CU' & Batting_Ohtani_100$year == '2022']
Launch_speed_CU_not2022 <- Batting_Ohtani_100$launch_speed[Batting_Ohtani_100$pitch_type == 'CU' & Batting_Ohtani_100$year != '2022']

###*** Batting_changeup ball launch angle:
Launch_angle_CH_2022 <- Batting_Ohtani_100$launch_angle[Batting_Ohtani_100$pitch_type == 'CH' & Batting_Ohtani_100$year == '2022']
Launch_angle_CH_not2022 <- Batting_Ohtani_100$launch_angle[Batting_Ohtani_100$pitch_type == 'CH' & Batting_Ohtani_100$year != '2022']

###*** Batting_Cutter ball launch angle:
Launch_angle_FC_2022 <- Batting_Ohtani_100$launch_angle[Batting_Ohtani_100$pitch_type == 'FC' & Batting_Ohtani_100$year == '2022']
Launch_angle_FC_not2022 <- Batting_Ohtani_100$launch_angle[Batting_Ohtani_100$pitch_type == 'FC' & Batting_Ohtani_100$year != '2022']



###*** Checking T test of Ohtani's launch speed at Fast ball in 2022 and not in 2022
t.test(Launch_speed_FS_2022, Launch_speed_FS_not2022)

###*** Drawing density curve at Split finger ball
plot(density(Launch_speed_FS_not2022), main = "Ohtani's FS batting", xlab ='Speed(mph)', ylab = 'density', col = 'firebrick1', xlim = c(50,130), ylim = c(0,0.05), lty = 1, lwd = 2, cex.main = 1.5, cex.axis= 1.5, cex.lab= 1.5)
lines(density(Launch_speed_FS_2022), col = 'blue', lwd =2)
legend(60,0.05, c("not in 2022", "2022"), col = c('red', 'blue'), lwd = 2, cex= 1 )



###*** Checking T test of Ohtani's exit velocity at Curve ball in 2022 and not in 2022
t.test(Launch_speed_CU_2022, Launch_speed_CU_not2022)

###*** Drawing density curve at Curve ball
plot(density(Launch_speed_CU_not2022), main = "Ohtani's CU batting", xlab ='Speed(mph)', ylab = 'density', col = 'firebrick1', xlim = c(50,130), ylim = c(0,0.05), lty = 1, lwd = 2, cex.main = 1.5, cex.axis= 1.5, cex.lab= 1.5)
lines(density(Launch_speed_CU_2022), col = 'blue', lwd =2)
legend(60,0.05, c("not in 2022", "2022"), col = c('red', 'blue'), lwd = 2, cex= 1 )



###*** Checking T test of Ohtani's launch speed at changeup in 2022 and not in 2022
t.test(Launch_speed_CH_2022, Launch_speed_CH_not2022)

###*** Drawing density curve at Curve ball 
plot(density(Launch_speed_CH_not2022), main = "Ohtani's CH batting", xlab ='Speed(mph)', ylab = 'density', col = 'firebrick1', xlim = c(50,130), ylim = c(0,0.08), lty = 1, lwd = 2, cex.main = 1.5, cex.axis= 1.5, cex.lab= 1.5)
lines(density(Launch_speed_CH_2022), col = 'blue', lwd =2)
legend(60,0.08, c("not in 2022", "2022"), col = c('red', 'blue'), lwd = 2, cex= 1 )



###*** Checking T test of Ohtani's launch angle at Changeup ball in 2022 and not in 2022
t.test(Launch_angle_CH_2022, Launch_angle_CH_not2022)

###*** Checking T test of Ohtani's launch angle at Cutter ball in 2022 and not in 2022
t.test(Launch_angle_FC_2022, Launch_angle_FC_not2022)

###*** Removing needless variables
remove(Launch_speed_FS_2022, Launch_speed_FS_not2022, Launch_speed_CH_2022, Launch_speed_CH_not2022, Launch_speed_CU_2022, Launch_speed_CU_not2022, Launch_angle_CH_2022, Launch_angle_CH_not2022, Launch_angle_FC_2022, Launch_angle_FC_not2022 )



#### 4.0 Analyzing MLB players' batting/pitching (Title only) ----
####* 4.1 Scatter plot ----
par(mfrow = c(2,2))
####* Plotting Pitchers' HR/FB
plot(Allplayers_Pitching_HRFB$year, Allplayers_Pitching_HRFB$HR.FB, type = 'b', lty = 1, lwd = 2, col = 'firebrick1', xlab = 'Year', ylim = c(7,16), ylab = '#', main = "MLB's HR/FB by different years", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("bottomleft", c("Average launch speed"), lty = 3, lwd = 5, col = c('red'), pch = 19 )


####* Plotting Batters' AVG
plot(Allplayers_Batting_AVG$year, Allplayers_Batting_AVG$AVG, type = 'b', lty = 1, lwd = 2, col = 'firebrick1', xlab = 'Year', ylim = c(0.20,0.24), ylab = 'AVG(%)', main = "MLB's AVG by different years", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("bottomleft", c("Average launch speed"), lty = 3, lwd = 5, col = c('red'), pch = 19 )

####* Plotting Batters' OBP
plot(Allplayers_Batting_OBP$year, Allplayers_Batting_OBP$OBP, type = 'b', lty = 1, lwd = 2, col = 'firebrick1', xlab = 'Year', ylim = c(0.265,0.31), ylab = 'OBP(%)', main = "MLB's OBP by different years", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("bottomleft", c("Average launch speed"), lty = 3, lwd = 5, col = c('red'), pch = 19 )

####* Plotting Batters' SLG
plot(Allplayers_Batting_SLG$year, Allplayers_Batting_SLG$SLG, type = 'b', lty = 1, lwd = 2, col = 'firebrick1', xlab = 'Year', ylim = c(0.32,0.40), ylab = 'SLG(%)', main = "MLB's SLG by different years", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("bottomleft", c("Average launch speed"), lty = 3, lwd = 5, col = c('red'), pch = 19 )

par(mfrow = c(1,1))

####* Removing the needless variables
remove(Allplayers_Pitching_HRFB, Allplayers_Batting_AVG, Allplayers_Batting_OBP, Allplayers_Batting_SLG)


##### 5.0 Analyzing Ohtani's by year's avg (Title only) ----
#####* 5.1 Scatter plot ----
par(mfrow = c(2,2))
#####* Ohtani's by year AVG
plot(Allplayers_Pitching$year[Allplayers_Pitching$name == 'Shohei Ohtani'], Allplayers_Pitching$ERA[Allplayers_Pitching$name == "Shohei Ohtani"], ylim = c(2.5,3.5), type = 'b', lty = 1, lwd = 2, col = 'firebrick1', xlab = 'Year', ylab = 'ERA', main = "Ohtani by year's ERA", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("bottomleft", c("Ohtani's by year AVG"), lty = 3, lwd = 5, col = c('red'), pch = 19 )
#####* Ohtani's by year K/9
plot(Allplayers_Pitching$year[Allplayers_Pitching$name == 'Shohei Ohtani'], Allplayers_Pitching$K.9[Allplayers_Pitching$name == "Shohei Ohtani"], ylim = c(10,15), type = 'b', lty = 1, lwd = 2, col = 'firebrick1', xlab = 'Year', ylab = 'K.9', main = "Ohtani by year's K.9", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("topleft", c("Ohtani's by year AVG"), lty = 3, lwd = 5, col = c('red'), pch = 19 )
#####* Ohtani's by year BB/9
plot(Allplayers_Pitching$year[Allplayers_Pitching$name == 'Shohei Ohtani'], Allplayers_Pitching$BB.9[Allplayers_Pitching$name == "Shohei Ohtani"], ylim = c(1,4), type = 'b', lty = 1, lwd = 2, col = 'firebrick1', xlab = 'Year', ylab = 'BB.9', main = "Ohtani by year's BB.9", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("bottomleft", c("Ohtani's by year AVG"), lty = 3, lwd = 5, col = c('red'), pch = 19 )
#####* Ohtani's by year AVG
plot(Allplayers_Pitching$year[Allplayers_Pitching$name == 'Shohei Ohtani'], Allplayers_Pitching$WAR[Allplayers_Pitching$name == "Shohei Ohtani"], type = 'b', lty = 1, lwd = 2, ylim = c(0,5), col = 'firebrick1', xlab = 'Year', ylab = 'WAR', main = "Ohtani by year's WAR", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
legend("topleft", c("Ohtani's by year AVG"), lty = 3, lwd = 5, col = c('red'), pch = 19 )

par(mfrow = c(1,1))

###### 6.0 Clusters (Title only)
######* 6.1 Clusters
library(viridis)

par(mfrow = c(1,2))
(Pitching_Ohtani_pfx_cl <- kmeans(Pitching_Ohtani[,c("pfx_x","pfx_z")], 5))
plot(Pitching_Ohtani[,c("pfx_x","pfx_z")], col =Pitching_Ohtani_pfx_cl$cluster)
points(Pitching_Ohtani_pfx_cl$centers, col = viridis(5), pch = 19, cex = 2)



##* Ohtani's pfx_x V.S pfx_z
plot(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'CU'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'CU'], lty = 3, lwd = 1, ylim = c(-2,2.5), xlim = c(-2,2.3), col = 'red', pch = 5, xlab ='pfx_x', ylab = 'pfx_z', main ='pfx_x V.S pfx_z', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FC'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FC'], lty = 3, lwd = 1, col = 'orange', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FF'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FF'], lty = 3, lwd = 1, col = 'gold1', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'FS'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'FS'], lty = 3, lwd = 1, col = 'limegreen', pch = 5)
lines(type = 'p', Pitching_Ohtani$pfx_x[Pitching_Ohtani$pitch_type == 'SL'], Pitching_Ohtani$pfx_z[Pitching_Ohtani$pitch_type == 'SL'], lty = 3, lwd = 1, col = 'cyan4', pch = 5)
legend("bottomleft", c("Curve", "Cutter", "Fast", "Split-finger", "Slider"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =1, pch = 5)

par(mfrow = c(1,1))

######* 6 Scatterplot 3D ----

######* Drawing a scatter 3D plot
library(scatterplot3d)
scatterplot3d(Pitching_Ohtani$vx0, Pitching_Ohtani$vy0, Pitching_Ohtani$vz0, highlight.3d = T, type = "p", lwd = 2, box = F, cex.axis = 0.5, main = "Ohtani's release speed = Vx0i + Vy0j + Vz0k")
######* Too much data, the result is not pretty clear


library(car)
library(rgl)

######* Checking another 3D function
scatter3d(Pitching_Ohtani$vx0, Pitching_Ohtani$vy0, Pitching_Ohtani$vz0, revolutions = 1)
######* It's still too complex, not a good indicator


###### 7 Future perspective ----
###### automatically run the whole pictures


