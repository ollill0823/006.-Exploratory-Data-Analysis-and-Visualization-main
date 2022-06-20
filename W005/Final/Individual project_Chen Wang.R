#################### A.Election file : Pre-handle ()  ####################

## load file ##
Election_2016 <- read.csv('2016_Pre_election.csv')
Election_2020 <- read.csv('2020_Pre_election.csv')


## Define new column: year, for combine use ##
Election_2016$year <- '2016'
Election_2020$year <- '2020'

## Define new column: Win (which party win the state in 2016), for combine use ##
Election_2016$Win[Election_2016$DEM > Election_2016$GOP] <- 'DEM'
Election_2016$Win[Election_2016$GOP > Election_2016$DEM] <- 'GOP'

## Define new column: Win (which party win the state in 2020), for combine use ##
Election_2020$Win[Election_2020$DEM > Election_2020$GOP] <- 'DEM'
Election_2020$Win[Election_2020$GOP > Election_2020$DEM] <- 'GOP'

## Order('DEM', 'GOP') = (1, 0)
Election_2016$Order <- factor(Election_2016$Win, levels = c('DEM', 'GOP'), labels = 1:0)
Election_2020$Order <- factor(Election_2020$Win, levels = c('DEM', 'GOP'), labels = 1:0)

## Change order to numeric
Election_2016$Order <- as.numeric(Election_2016$Order)
Election_2020$Order <- as.numeric(Election_2020$Order)


## Define the color of the 2016 election which is determined by which party wins the state
Election_2016$Color[Election_2016$DEM > Election_2016$GOP] <- 'Blue'
Election_2016$Color[Election_2016$GOP > Election_2016$DEM] <- 'Red'

## Define the color of the 2020 election which is determined by which party wins the state
Election_2020$Color[Election_2020$DEM > Election_2020$GOP] <- rgb(0,0,1,0.5)
Election_2020$Color[Election_2020$GOP > Election_2020$DEM] <- rgb(1,0,0,0.25)


## Determine the votes gap between the votes of DEM and the votes of GOP
Election_2016$offset_2016 <- Election_2016$DEM-Election_2016$GOP
Election_2020$offset_2020 <- Election_2020$DEM-Election_2020$GOP

## Include the needed data to create 1st data frame
offsetsummary <- cbind(Election_2016$ï..State,Election_2016$offset_2016, Election_2020$offset_2020,
                       Election_2016$Color, Election_2020$Color)
offsetsummary <- as.data.frame(offsetsummary)
## give each column a name
names(offsetsummary) <- c('State', 'offset_2016', 'offset_2020', 'color_2016', 'color_2020')


## change the votes gap to number from character
offsetsummary$offset_2016 <- as.numeric(offsetsummary$offset_2016)
offsetsummary$offset_2020 <- as.numeric(offsetsummary$offset_2020)

## Order the votes gap by 2016 votes gap
Votes_2020_order <- offsetsummary[order(offsetsummary$offset_2016, decreasing = T),]

## remove needless tables and variables
remove(offsetsummary)


###### 1. Bar plot for offset ######
## Picture barplot to check the swing state
barplot(Votes_2020_order$offset_2016/1000, names.arg = Votes_2020_order$State, col = Votes_2020_order$color_2016, 
        horiz = T, xlim = c(-1000,5000), xlab = "Votes_offset(thousand)",
        ylab = "State", main = "2016 to 2020 USA presidential votes shift \nby state",
        cex.main = 1.5, cex.lab = 1, lwd = 3, cex.axis = 1.5, cex.lab= 1.5)
barplot(Votes_2020_order$offset_2020/1000, col = Votes_2020_order$color_2020, add = T, horiz = T, cex.axis = 1.5)
legend("topright", c("2016 offset(GOP+)", "2016 offset(DEM+)", "2020 offset(GOP+)", "2020 offset(DEM+)"), 
       col = c("Red", "Blue", rgb(1,0,0,0.25), rgb(0,0,1,0.5)), lwd = 10, cex = 1.5)

## Give five swing states' label
text(300,41.5,labels = "Georgia", adj = 0)
text(300,30.8,labels = "Arizona", adj = 0)
text(300,28.6,labels = "Pennsylvania", adj = 0)
text(300,27.3,labels = "Wisconsin", adj = 0)
text(300,26,labels = "Michigan", adj = 0)

## To emphasize, draw arrows to point out
arrows(x0 = 280, y0 = 26, x1 = 200, y1 = 26, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 27.3, x1 = 200, y1 = 27.3, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 28.6, x1 = 200, y1 = 28.6, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 30.8, x1 = 100, y1 = 30.8, length = 0.15, angle = 30)
arrows(x0 = 280, y0 = 41.5, x1 = 100, y1 = 41.5, length = 0.15, angle = 30)



###### 2. Bar plot for 2016 & 2020 votes ######
## Create a new 2016 new table from old--> revise column name
## Choose swing state data, and pickup needed data only
Election_2016_DEM <- Election_2016[,c(1:3,9:12)][Election_2016$Order != Election_2020$Order,]
Election_2016_GOP <- Election_2016[,c(1:2,6,9:12)][Election_2016$Order != Election_2020$Order,]

## In order to combine DEM/GOP, 2016/2020 data together, modify column name
### 2016 ##
names(Election_2016_DEM)[3] <- "Vote"      ## Votes unify to the same column
Election_2016_DEM$Party <- 'DEM'           ## Name the rows to differentiate data when combine
names(Election_2016_DEM)[1] <- "State"     ## Re-correct the State's column name 

names(Election_2016_GOP)[3] <- "Vote"
Election_2016_GOP$Party <- 'GOP'
names(Election_2016_GOP)[1] <- "State"

### 2020 ##
Election_2020_DEM <- Election_2020[,c(1:3,9:12)][Election_2016$Order != Election_2020$Order,]
Election_2020_GOP <- Election_2020[,c(1:2,6,9:12)][Election_2016$Order != Election_2020$Order,]
names(Election_2020_DEM)[3] <- "Vote"
Election_2020_DEM$Party <- 'DEM'
names(Election_2020_DEM)[1] <- "State"

names(Election_2020_GOP)[3] <- "Vote"
Election_2020_GOP$Party <- 'GOP'
names(Election_2020_GOP)[1] <- "State"


## Combine as above variables into one table
Election_Swing <- rbind(Election_2016_DEM, Election_2016_GOP, Election_2020_DEM, Election_2020_GOP)


## Order the data by State and Party
Elect_SwingOrd <- Election_Swing[order(Election_Swing$State, Election_Swing$Party),]

## draw bar plot 
barplot(Elect_SwingOrd$Vote[Elect_SwingOrd$year == 2016]/1000, names.arg = Elect_SwingOrd$State[Elect_SwingOrd$year == 2016], 
        col = c("Blue", "Red"), ylim = c(0,3500), space = c(3,0,1,0,1,0,1,0,1,0),
        xlab = 'Overturned State', ylab = 'Votes(thousand)', cex.axis= 1.5, cex.lab= 1.5)
barplot(Elect_SwingOrd$Vote[Elect_SwingOrd$year == 2020]/1000,  
        col = c(rgb(0,0,1,0.25), rgb(1,0,0,0.5)), space = c(3,0,1,0,1,0,1,0,1,0), add = T, cex.axis = 1.5)
legend(3,3500, c("2016 DEM", "2016 GOP", "2020 DEM", "2020 GOP"), 
       col = c("Blue", "Red", rgb(0,0,1,0.5), rgb(1,0,0,0.25)), lwd = 10, cex = 1)



## Remove needless table ##
remove (Election_2016_DEM, Election_2016_GOP, Election_2020_DEM, Election_2020_GOP)



#################### B.ANES file : Pre-handle ####################
## load file
ANES <- read.csv("anes_timeseries_2020_csv_20220210.csv", header = T)
## Define needed only
V201600  <- ANES$V201600 # Is the respondent male or female?
V201507x <- ANES$V201507x # respondents' age
V201001  <- ANES$V201001 # Would you like to complete this survey in English or Spanish?
V202054x <- ANES$V202054x # summary : post State of registration
V201575  <- ANES$V201575 # where are you grew up?
V201151  <- ANES$V201151 # How would you rate for Joe Biden?
V201152  <- ANES$V201152 # How would you rate for Donald Trump?
V201103  <- ANES$V201103 # Which one did you vote for in 2016?
V202110x <- ANES$V202110x # summary : 2020 president vote
V201546  <- ANES$V201546 # Are you of Hispanic, Latino, or Spanish origin?
V201549x <- ANES$V201549x # your self-identified race/ethnicity 
V201562  <- ANES$V201562 # language at home
V202468x <- ANES$V202468x # summary : family income

## Combine needed variables
interview <- cbind(V201600, V201507x, V201001, V202054x, V201575, V201151, V201152, 
                   V201103, V202110x, V201546, V201549x, V201562,V202468x)

remove(V201600, V201507x, V201001, V202054x, V201575, V201151, V201152, 
       V201103, V202110x, V201546, V201549x, V201562,V202468x)


interview <- as.data.frame(interview)

## Create columns' name
names(interview) <- c('Sex', 'Age', 'Suvery in Eng/Span', 'State for vote', 'Grew up State',
                      'Rate for Biden', 'Rate for Trump', 'Vote for in 2016', 
                      'Vote for in 2020', 'Origin', 'Ethnicity', 'Language', 'Family income')

## Remove ineffective survey (left Rate for Biden/Trump)
interview_new <- interview[interview$Sex != '-9',]
interview_new <- interview_new[interview_new$Age != '-9',]
interview_new <- interview_new[interview_new$`State for vote` >= 0,]
interview_new <- interview_new[interview_new$`Grew up State` > 0,]
interview_new <- interview_new[interview_new$Ethnicity > 0,]
interview_new <- interview_new[interview_new$`Family income` > 0,]

  
## Define swing state (five states: Arizona(4), Georgia(13), Michigan(26), Pennsylvania(42), Wisconsin(55))
interview_swing <- interview_new[(interview_new$`State for vote` == '4' | interview_new$`State for vote` == '13')
                             | interview_new$`State for vote` == '26' | interview_new$`State for vote` == '42'
                             | interview_new$`State for vote` == '55',]


## Remove ineffective survey (Rate for Biden/Trump)
interview_rate <- interview_swing[interview_swing$`Rate for Biden` != '-9' & interview_swing$`Rate for Biden` != '-4',]
interview_rate <- interview_rate[interview_rate$`Rate for Trump` != '-9' & interview_rate$`Rate for Trump` != '-4',]
remove(interview_new)


## Create a table which state is Arizona ##
##Remove needless row: negative number can represent a invalid row ##
interview_Arizona <- interview_rate[interview_rate$`State for vote` == '4' & interview_rate$`Vote for in 2020` != '-9' 
                                    & interview_rate$`Vote for in 2020` != '-1',]
interview_Arizona$`Vote for in 2020`[interview_Arizona$`Vote for in 2020` == '4' | interview_Arizona$`Vote for in 2020` == '5'  ] <- '3'

## Create a table which state is Georgia ##
## Remove needless row: negative number can represent a invalid row ##
interview_Georgia <- interview_rate[interview_rate$`State for vote` == '13' & interview_rate$`Vote for in 2020` != '-9' 
                                    & interview_rate$`Vote for in 2020` != '-1',]
interview_Georgia$`Vote for in 2020`[interview_Georgia$`Vote for in 2020` == '4' | interview_Georgia$`Vote for in 2020` == '5'  ] <- '3'

## Create a table which state is Michigan ##
## Remove needless row: negative number can represent a invalid row ##
interview_Michigan <- interview_rate[interview_rate$`State for vote` == '26' & interview_rate$`Vote for in 2020` != '-9' 
                                     & interview_rate$`Vote for in 2020` != '-1',]
interview_Michigan$`Vote for in 2020`[interview_Michigan$`Vote for in 2020` == '4' | interview_Michigan$`Vote for in 2020` == '5'  ] <- '3'

## Create a table which state is Pennsylvania ##
## Remove needless row: negative number can represent a invalid row ##
interview_Pennsylvania <- interview_rate[interview_rate$`State for vote` == '42' & interview_rate$`Vote for in 2020` != '-9' 
                                         & interview_rate$`Vote for in 2020` != '-1',]
interview_Pennsylvania$`Vote for in 2020`[interview_Pennsylvania$`Vote for in 2020` == '4' | interview_Pennsylvania$`Vote for in 2020` == '5'  ] <- '3'

## Create a table which state is Wisconsin ##
## Remove needless row: negative number can represent a invalid row ##
interview_Wisconsin <- interview_rate[interview_rate$`State for vote` == '55' & interview_rate$`Vote for in 2020` != '-9' 
                                      & interview_rate$`Vote for in 2020` != '-1',]
interview_Wisconsin$`Vote for in 2020`[interview_Wisconsin$`Vote for in 2020` == '4' | interview_Wisconsin$`Vote for in 2020` == '5'  ] <- '3'


## remove needless tables and variables
remove(ANES)


###### 1.1 Plot correlation plot ######
plot(interview_rate[,c(1:11)])

###### 1.2 Plot Biden's rate and Trump's rate ######
plot(interview_rate$`Rate for Biden`, interview_rate$`Rate for Trump`)

###### 2.1 Pie chart comparison: Arizona(4) #####
### Questionnaire in Arizona ###

## Arrange by candidates: Joe Biden(1), Donald Trump(2), 
## Jo Jorgensen(3), Howie Hawkins (4), Others(5), but always, I will merge (3)~(5) into 3
Arizona <- table(interview_Arizona$`Vote for in 2020`)
Round_Arizona <- round(prop.table(Arizona),4)*100 ## Round the percentage by two decimal
## Create pie chart label
President_Arizona <- c('Joe Biden','Donald Trump' , 'Others')
Arizonalab <- paste(President_Arizona, "\n", Round_Arizona, '%', sep ="")


### Actual election in Arizona ###
## Collect data from Election_2020 by state = Arizona ##
Election_2020[Election_2020$ï..State == 'Arizona',]
## Create new vector and store it into a new table ##
Pie_Arizona_2020 <- c(0.4936,0.4906, 1-0.4936-0.4906 )
Arizona_candidate <- c('Joe Biden', 'Donald Trump', 'Others')
Pie_Arizona_2020 <- cbind(Arizona_candidate, Pie_Arizona_2020)

Pie_Arizona_2020 <-  as.data.frame(Pie_Arizona_2020)
Pie_Arizona_2020$Pie_Arizona_2020 <-  as.numeric(Pie_Arizona_2020$Pie_Arizona_2020) 
## Create pie chart label ##
Pie_Arizona_2020lab <- paste(Pie_Arizona_2020$Arizona_candidate, "\n", Pie_Arizona_2020$Pie_Arizona_2020*100, '%', sep ="")

### Draw a pie chart in Arizona ###
par(mfrow = c(1,2))
pie(Arizona, label =Arizonalab , main = 'Interview_2020: Arizona', col = c('blue', 'red', 'green'),
    radius = 1.0, lwd = 2, init.angle = 300)

pie(Pie_Arizona_2020$Pie_Arizona_2020, label =Pie_Arizona_2020lab , main = 'Arizona in 2020', col = c('blue', 'red', 'green'),
    radius = 1.0, lwd = 2, init.angle = 300)

## remove needless tables and variables
remove(Pie_Arizona_2020lab, Arizonalab, Arizona_candidate, Arizona, President_Arizona, Round_Arizona )

###### 2.2 Pie chart comparison: Georgia(13) #####
### Questionnaire in Georgia ###
## Arrange by candidates: Joe Biden(1), Donald Trump(2), 
## Jo Jorgensen(3), Howie Hawkins (4), Others(5), but always, I will merge (3)~(5) into 3
Georgia <- table(interview_Georgia$`Vote for in 2020`)
Round_Georgia <- round(prop.table(Georgia),4)*100 
## Create pie chart label
President_Georgia <- c('Joe Biden','Donald Trump' , 'Others')
Georgialab <- paste(President_Georgia, "\n", Round_Georgia, '%', sep ="")


### Actual election in Georgia ###
## Collect data from Election_2020 by state = Georgia ##
Election_2020[Election_2020$ï..State == 'Georgia',]
## Create new vector and store it into a new table ##
Pie_Georgia_2020 <- c(0.4950,0.4926, 1-0.4950-0.4926 )
Georgia_candidate <- c('Joe Biden', 'Donald Trump', 'Others')
Pie_Georgia_2020 <- cbind(Georgia_candidate, Pie_Georgia_2020)
Pie_Georgia_2020 <-  as.data.frame(Pie_Georgia_2020)
Pie_Georgia_2020$Pie_Georgia_2020 <-  as.numeric(Pie_Georgia_2020$Pie_Georgia_2020) 
## Create pie chart label ##
Pie_Georgia_2020lab <- paste(Pie_Georgia_2020$Georgia_candidate, "\n", Pie_Georgia_2020$Pie_Georgia_2020*100, '%', sep ="")

### Draw a pie chart in Georgia ###
par(mfrow = c(1,2))
pie(Georgia, label =Georgialab , main = 'Interview_2020: Georgia', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)

pie(Pie_Georgia_2020$Pie_Georgia_2020, label =Pie_Georgia_2020lab , main = 'Georgia in 2020', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)


## remove needless tables and variables
remove(Pie_Georgia_2020lab, Georgia_candidate, Georgia, Georgialab, President_Georgia, Round_Georgia )



###### 2.3 Pie chart comparison: Michigan(26) #####
### Questionnaire in Michigan ###
## Arrange by candidates: Joe Biden(1), Donald Trump(2), 
## Jo Jorgensen(3), Howie Hawkins (4), Others(5), but always, I will merge (3)~(5) into 3
Michigan <- table(interview_Michigan$`Vote for in 2020`)
Round_Michigan <- round(prop.table(Michigan),4)*100 
## Create pie chart label
President_Michigan <- c('Joe Biden','Donald Trump' , 'Others')
Michiganlab <- paste(President_Michigan, "\n", Round_Michigan, '%', sep ="")


### Actual election in Michigan ###
## Collect data from Election_2020 by state = Michigan ##
Election_2020[Election_2020$ï..State == 'Michigan',]
## Create new vector and store it into a new table ##
Pie_Michigan_2020 <- c(0.5062,0.4784, 1-0.5062-0.4784 )
Michigan_candidate <- c('Joe Biden', 'Donald Trump', 'Others')
Pie_Michigan_2020 <- cbind(Michigan_candidate, Pie_Michigan_2020)
Pie_Michigan_2020 <-  as.data.frame(Pie_Michigan_2020)
Pie_Michigan_2020$Pie_Michigan_2020 <-  as.numeric(Pie_Michigan_2020$Pie_Michigan_2020) 
## Create pie chart label ##
Pie_Michigan_2020lab <- paste(Pie_Michigan_2020$Michigan_candidate, "\n", Pie_Michigan_2020$Pie_Michigan_2020*100, '%', sep ="")

### Draw a pie chart in Michigan ###
par(mfrow = c(1,2))
pie(Michigan, label =Michiganlab , main = 'Interview_2020: Michigan', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)

pie(Pie_Michigan_2020$Pie_Michigan_2020, label =Pie_Michigan_2020lab , main = 'Michigan in 2020', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)

## remove needless tables and variables
remove(Pie_Michigan_2020lab, Michigan_candidate, Michigan, Michiganlab, President_Michigan, Round_Michigan )

###### 2.4 Pie chart comparison: Pennsylvania(42) #####
### Questionnaire in Pennsylvania ###
## Arrange by candidates: Joe Biden(1), Donald Trump(2), 
## Jo Jorgensen(3), Howie Hawkins (4), Others(5), but always, I will merge (3)~(5) into 3
Pennsylvania <- table(interview_Pennsylvania$`Vote for in 2020`)
Round_Pennsylvania <- round(prop.table(Pennsylvania),4)*100 
## Create pie chart label
President_Pennsylvania <- c('Joe Biden','Donald Trump' , 'Others')
Pennsylvanialab <- paste(President_Pennsylvania, "\n", Round_Pennsylvania, '%', sep ="")


### Actual election in Pennsylvania ###
## Collect data from Election_2020 by state = Pennsylvania ##
Election_2020[Election_2020$ï..State == 'Pennsylvania',]
## Create new vector and store it into a new table ##
Pie_Pennsylvania_2020 <- c(0.5001,0.4884, 1-0.5001-0.4884 )
Pennsylvania_candidate <- c('Joe Biden', 'Donald Trump', 'Others')
Pie_Pennsylvania_2020 <- cbind(Pennsylvania_candidate, Pie_Pennsylvania_2020)
Pie_Pennsylvania_2020 <-  as.data.frame(Pie_Pennsylvania_2020)
Pie_Pennsylvania_2020$Pie_Pennsylvania_2020 <-  as.numeric(Pie_Pennsylvania_2020$Pie_Pennsylvania_2020) 
## Create pie chart label ##
Pie_Pennsylvania_2020lab <- paste(Pie_Pennsylvania_2020$Pennsylvania_candidate, "\n", Pie_Pennsylvania_2020$Pie_Pennsylvania_2020*100, '%', sep ="")

### Draw a pie chart in Pennsylvania ###
par(mfrow = c(1,2))
pie(Pennsylvania, label =Pennsylvanialab , main = 'Interview_2020: Pennsylvania', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)

pie(Pie_Pennsylvania_2020$Pie_Pennsylvania_2020, label =Pie_Pennsylvania_2020lab , main = 'Pennsylvania in 2020', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)


## remove needless tables and variables
remove(Pie_Pennsylvania_2020lab, Pennsylvania_candidate, Pennsylvania, Pennsylvanialab, President_Pennsylvania, Round_Pennsylvania )

###### 2.5 Pie chart comparison: Wisconsin(55) #####
### Questionnaire in Wisconsin ###
## Arrange by candidates: Joe Biden(1), Donald Trump(2), 
## Jo Jorgensen(3), Howie Hawkins (4), Others(5), but always, I will merge (3)~(5) into 3
Wisconsin <- table(interview_Wisconsin$`Vote for in 2020`)
Round_Wisconsin <- round(prop.table(Wisconsin),4)*100 
## Create pie chart label
President_Wisconsin <- c('Joe Biden','Donald Trump' , 'Others')
## Create pie chart label ##
Wisconsinlab <- paste(President_Wisconsin, "\n", Round_Wisconsin, '%', sep ="")


### Actual election in Wisconsin ###
## Collect data from Election_2020 by state = Wisconsin ##
Election_2020[Election_2020$ï..State == 'Wisconsin',]
## Create new vector and store it into a new table ##
Pie_Wisconsin_2020 <- c(0.4945,0.4882, 1-0.4945-0.4882 )
Wisconsin_candidate <- c('Joe Biden', 'Donald Trump', 'Others')
Pie_Wisconsin_2020 <- cbind(Wisconsin_candidate, Pie_Wisconsin_2020)
Pie_Wisconsin_2020 <-  as.data.frame(Pie_Wisconsin_2020)
Pie_Wisconsin_2020$Pie_Wisconsin_2020 <-  as.numeric(Pie_Wisconsin_2020$Pie_Wisconsin_2020) 
Pie_Wisconsin_2020lab <- paste(Pie_Wisconsin_2020$Wisconsin_candidate, "\n", Pie_Wisconsin_2020$Pie_Wisconsin_2020*100, '%', sep ="")

### Draw a pie chart in Wisconsin ###
par(mfrow = c(1,2))
pie(Wisconsin, label =Wisconsinlab , main = 'Interview_2020: Wisconsin', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)

pie(Pie_Wisconsin_2020$Pie_Wisconsin_2020, label =Pie_Wisconsin_2020lab , main = 'Wisconsin in 2020', col = c('blue', 'red', 'green'),
    radius = 1, lwd = 2, init.angle = 300)


## remove needless tables and variables
remove(Pie_Wisconsin_2020lab, Wisconsin_candidate, Wisconsin, Wisconsinlab, President_Wisconsin, Round_Wisconsin )


###### 3.1 histogram of Arizona votes by the respondent's age ######
par(mfrow = c(1,1))
## Draw a histogram plot : accumulate by the age and the party
hist(interview_Arizona$Age[interview_Arizona$`Vote for in 2020` == '1'],
     breaks = seq(15, 90, 5), main = "Arizona's votes distribution group by the age",
     xlab = 'Age', ylab = 'Respondents counts', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,15), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(interview_Arizona$Age[interview_Arizona$`Vote for in 2020` == '2'],
     breaks = seq(15, 90, 5), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(interview_Arizona$Age[interview_Arizona$`Vote for in 2020` == '3'],
     breaks = seq(15, 90, 5), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
## Put counts and frequency in the same graph ##
par(new = TRUE)
dense_Arizona_Biden <- density(interview_Arizona$Age[interview_Arizona$`Vote for in 2020` == '1'], bw = 2)
plot(dense_Arizona_Biden, main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', breaks = seq(15, 90, 5), col = 'Blue', lty = 1, lwd = 2, ylim = c(-0.05,0.05), cex.axis= 1.5, cex.lab= 1.5)
dense_Arizona_Trump <- density(interview_Arizona$Age[interview_Arizona$`Vote for in 2020` == '2'], bw = 2)
lines(dense_Arizona_Trump, breaks = seq(15, 90, 5), col = 'Red', lty = 1, lwd = 2, side = 5)
## Create the right y-axis
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Frequency", side = 4, line = 3)
legend(20,0.05, c('Joe Biden','Donald Trump', "Biden's density line", "Trump's density line"),
       col = c(rgb(1,0,0,0.25), rgb(0,0,1,0.25), 'Red', 'Blue'), lwd = c(10,10,2,2), cex= 1 )


## remove needless tables and variables
remove(dense_Arizona_Biden, dense_Arizona_Trump )


###### 3.2 histogram of Georgia votes by the respondent's age ######
par(mfrow = c(1,1))
## Draw a histogram plot : accumulate by the age and the party
hist(interview_Georgia$Age[interview_Georgia$`Vote for in 2020` == '1'],
     breaks = seq(15, 90, 5), main = "Georgia's votes distribution group by the age",
     xlab = 'Age', ylab = 'Respondents counts', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,15), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(interview_Georgia$Age[interview_Georgia$`Vote for in 2020` == '2'],
     breaks = seq(15, 90, 5), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(interview_Georgia$Age[interview_Georgia$`Vote for in 2020` == '3'],
     breaks = seq(15, 90, 5), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
## Put counts and frequency in the same graph ##
par(new = TRUE)
dense_Georgia_Biden <- density(interview_Georgia$Age[interview_Georgia$`Vote for in 2020` == '1'], bw = 2)
plot(dense_Georgia_Biden, main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', breaks = seq(15, 90, 5), col = 'Blue', lty = 1, lwd = 2, ylim = c(-0.05,0.05), cex.axis= 1.5, cex.lab= 1.5)
dense_Georgia_Trump <- density(interview_Georgia$Age[interview_Georgia$`Vote for in 2020` == '2'], bw = 2)
lines(dense_Georgia_Trump, breaks = seq(15, 90, 5), col = 'Red', lty = 1, lwd = 2)
## Create the right y-axis ##
axis(side = 4, cex.axis= 1.5, cex.lab= 3)
mtext("Frequency", side = 4, line = 3)
## Put on the mark ##
legend(20,0.053, c('Joe Biden','Donald Trump', "Biden's density line", "Trump's density line"),
       col = c(rgb(1,0,0,0.25), rgb(0,0,1,0.25), 'Red', 'Blue'), lwd = c(10,10,2,2) )


## remove needless tables and variables
remove(dense_Georgia_Biden, dense_Georgia_Trump )




###### 3.3 histogram of Michigan votes by the respondent's age ######
par(mfrow = c(1,1))
## Draw a histogram plot : accumulate by the age and the party
hist(interview_Michigan$Age[interview_Michigan$`Vote for in 2020` == '1'],
     breaks = seq(15, 90, 5), main = "Michigan's votes distribution group by the age",
     xlab = 'Age', ylab = 'Respondents counts', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,25), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(interview_Michigan$Age[interview_Michigan$`Vote for in 2020` == '2'],
     breaks = seq(15, 90, 5), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(interview_Michigan$Age[interview_Michigan$`Vote for in 2020` == '3'],
     breaks = seq(15, 90, 5), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
## Put counts and frequency in the same graph ##
par(new = TRUE)
dense_Michigan_Biden <- density(interview_Michigan$Age[interview_Michigan$`Vote for in 2020` == '1'], bw = 2)
plot(dense_Michigan_Biden, main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', breaks = seq(15, 90, 5), col = 'Blue', lty = 1, lwd = 2, ylim = c(-0.05,0.05), cex.axis= 1.5, cex.lab= 1.5)
dense_Michigan_Trump <- density(interview_Michigan$Age[interview_Michigan$`Vote for in 2020` == '2'], bw = 2)
lines(dense_Michigan_Trump, breaks = seq(15, 90, 5), col = 'Red', lty = 1, lwd = 2)
## Create the right y-axis ##
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Frequency", side = 4, line = 3)
## Put on the mark ##
legend(20,0.053, c('Joe Biden','Donald Trump', "Biden's density line", "Trump's density line"),
       col = c(rgb(1,0,0,0.25), rgb(0,0,1,0.25), 'Red', 'Blue'), lwd = c(10,10,2,2) )

## remove needless tables and variables
remove(dense_Michigan_Biden, dense_Michigan_Trump )



###### 3.4 histogram of Pennsylvania votes by the respondent's age ######
par(mfrow = c(1,1))
## Draw a histogram plot : accumulate by the age and the party
hist(interview_Pennsylvania$Age[interview_Pennsylvania$`Vote for in 2020` == '1'],
     breaks = seq(15, 90, 5), main = "Pennsylvania's votes distribution group by the age",
     xlab = 'Age', ylab = 'Respondents counts', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,20), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(interview_Pennsylvania$Age[interview_Pennsylvania$`Vote for in 2020` == '2'],
     breaks = seq(15, 90, 5), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(interview_Pennsylvania$Age[interview_Pennsylvania$`Vote for in 2020` == '3'],
     breaks = seq(15, 90, 5), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
## Put counts and frequency in the same graph ##
par(new = TRUE)
dense_Pennsylvania_Biden <- density(interview_Pennsylvania$Age[interview_Pennsylvania$`Vote for in 2020` == '1'], bw = 2)
plot(dense_Pennsylvania_Biden, main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', breaks = seq(15, 90, 5), col = 'Blue', lty = 1, lwd = 2, ylim = c(-0.05,0.05), cex.axis= 1.5, cex.lab= 1.5)
dense_Pennsylvania_Trump <- density(interview_Pennsylvania$Age[interview_Pennsylvania$`Vote for in 2020` == '2'], bw = 2)
lines(dense_Pennsylvania_Trump, breaks = seq(15, 90, 5), col = 'Red', lty = 1, lwd = 2)
## Create the right y-axis ##
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Frequency", side = 4, line = 3)
## Put on the mark ##
legend(20,0.053, c('Joe Biden','Donald Trump', "Biden's density line", "Trump's density line"),
       col = c(rgb(1,0,0,0.25), rgb(0,0,1,0.25), 'Red', 'Blue'), lwd = c(10,10,2,2) )

## remove needless tables and variables
remove(dense_Pennsylvania_Biden, dense_Pennsylvania_Trump )


###### 3.5 histogram of Wisconsin votes by the respondent's age ######
par(mfrow = c(1,1))
## Draw a histogram plot : accumulate by the age and the party
hist(interview_Wisconsin$Age[interview_Wisconsin$`Vote for in 2020` == '1'],
     breaks = seq(15, 90, 5), main = "Wisconsin's votes distribution group by the age",
     xlab = 'Age', ylab = 'Respondents counts', col = rgb(0,0,1,0.25), freq = T, ylim = c(0,15), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(interview_Wisconsin$Age[interview_Wisconsin$`Vote for in 2020` == '2'],
     breaks = seq(15, 90, 5), col = rgb(1,0,0,0.25), add = T, freq = T, cex.axis= 1.5)
hist(interview_Wisconsin$Age[interview_Wisconsin$`Vote for in 2020` == '3'],
     breaks = seq(15, 90, 5), col = rgb(0,1,0,0.25), add = T, freq = T, cex.axis= 1.5)
## Put counts and frequency in the same graph ##
par(new = TRUE)
dense_Wisconsin_Biden <- density(interview_Wisconsin$Age[interview_Wisconsin$`Vote for in 2020` == '1'], bw = 2)
plot(dense_Wisconsin_Biden, main = '',xlab ='', ylab = '', xaxt = 'n', yaxt = 'n', breaks = seq(15, 90, 5), col = 'Blue', lty = 1, lwd = 2, ylim = c(-0.05,0.05), cex.axis= 1.5, cex.lab= 1.5)
dense_Wisconsin_Trump <- density(interview_Wisconsin$Age[interview_Wisconsin$`Vote for in 2020` == '2'], bw = 2)
lines(dense_Wisconsin_Trump, breaks = seq(15, 90, 5), col = 'Red', lty = 1, lwd = 2)
## Create the right y-axis ##
axis(side = 4, cex.axis= 1.5, cex= 3)
mtext("Frequency", side = 4, line = 3)
## Put on the mark ##
legend(20,0.052, c('Joe Biden','Donald Trump', "Biden's density line", "Trump's density line"),
       col = c(rgb(1,0,0,0.25), rgb(0,0,1,0.25), 'Red', 'Blue'), lwd = c(10,10,2,2) )

## remove needless tables and variables
remove(dense_Wisconsin_Biden, dense_Wisconsin_Trump )


###### 4.1 histogram of Arizona votes by the respondent's ethnicity ######
par(mfrow = c(1,1))
interview_Arizona$`Vote for in 2020` <- as.numeric(interview_Arizona$`Vote for in 2020`)
interview_Arizona$Ethnicity <- as.numeric(interview_Arizona$Ethnicity)
## Combine candidates and ethnicity into a new table ##
ethnicity_Arizona <- table (interview_Arizona$`Vote for in 2020`, interview_Arizona$Ethnicity, dnn = c('Votes', 'Ethnicity'))
table_Arizona <- data.frame(ethnicity_Arizona)

## Arrange Ethnicity table: 
## Ethnicity: 1(White but no Hispanic), 2(Black but no Hispanic), 3(Hispanic),
##            4(Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone),
##            5(Native American/Alaska Native or other race, non-Hispanic alone),
##            6(Multiple races, non-Hispanic)
## Votes    : 1(Joe Biden), 2(Donald Trump), 3(Jo Jorgensen), 4(Howie Hawkins), 5(Others),
##            but always, I will merge 3~5 into 3
table_Arizona$Ethnicity <- as.numeric(table_Arizona$Ethnicity)
table_Arizona[c(1,3,4,6,7,9,10,12,13,15,16,18),2] <- ''
table_Arizona[c(2,5,8,11,14,17),2] <- c('White', 'Black', 'Hispano', 'Asian', 'AIAN', 'Multi')

## Create a bar plot
barplot(table_Arizona$Freq*100/sum(table_Arizona$Freq), ylim = c(-20,50), 
        space = c(3,0,0,rep(c(1,0,0),5)), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, cex.names = 0.7,
        names.arg = table_Arizona$Ethnicity, 
        col = c("Blue", "Red", "chartreuse4"), 
        main = 'Races in Arizona', ylab = 'percent(%) in all respondents')
offset_Arizona <- round(((table_Arizona$Freq[table_Arizona$Votes == '1'] - table_Arizona$Freq[table_Arizona$Votes == '2'])*100)/ sum(table_Arizona$Freq),1)
barplot(offset_Arizona, space = c(6,3,3,3,3,3), add = T, col = 'Gray70', cex.axis= 1.5)
legend(16,40, c("Vote for Biden", "Vote for Trump", "Vote for Others", "Offset"), 
       col = c("Blue", "Red", "chartreuse4", 'Gray70'), text.col = c("Blue", "Red", "chartreuse4", 'Gray50'), text.font = 2, lwd = 10, cex = 1.3)

## remove needless table
remove (ethnicity_Arizona, offset_Arizona)

###### 4.2 histogram of Georgia votes by the respondent's ethnicity ######
par(mfrow = c(1,1))
interview_Georgia$`Vote for in 2020` <- as.numeric(interview_Georgia$`Vote for in 2020`)
interview_Georgia$Ethnicity <- as.numeric(interview_Georgia$Ethnicity)
## Combine candidates and ethnicity into a new table ##
ethnicity_Georgia <- table (interview_Georgia$`Vote for in 2020`, interview_Georgia$Ethnicity, dnn = c('Votes', 'Ethnicity'))
table_Georgia <- data.frame(ethnicity_Georgia)

## Arrange Ethnicity table: 
## Ethnicity: 1(White but no Hispanic), 2(Black but no Hispanic), 3(Hispanic),
##            4(Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone),
##            5(Native American/Alaska Native or other race, non-Hispanic alone),
##            6(Multiple races, non-Hispanic)
## Votes    : 1(Joe Biden), 2(Donald Trump), 3(Jo Jorgensen), 4(Howie Hawkins), 5(Others),
##            but always, I will merge 3~5 into 3
table_Georgia$Ethnicity <- as.numeric(table_Georgia$Ethnicity)
table_Georgia[c(1,3,4,6,7,9,10,12,13,15,16,18),2] <- ''
table_Georgia[c(2,5,8,11,14,17),2] <- c('White', 'Black', 'Hispano', 'Asian', 'AIAN', 'Multi')

## Create a bar plot
barplot(table_Georgia$Freq*100/sum(table_Georgia$Freq), ylim = c(-20,50), 
        space = c(3,0,0,rep(c(1,0,0),5)), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, cex.names = 0.7,
        names.arg = table_Georgia$Ethnicity, 
        col = c("Blue", "Red", "chartreuse4"), 
        main = 'Races in Georgia', ylab = 'percent(%) in all respondents')
offset_Georgia <- round(((table_Georgia$Freq[table_Georgia$Votes == '1'] - table_Georgia$Freq[table_Georgia$Votes == '2'])*100)/ sum(table_Georgia$Freq),1)
barplot(offset_Georgia, space = c(6,3,3,3,3,3), add = T, col = 'Gray70', cex.axis= 1.5)
legend(16,40, c("Vote for Biden", "Vote for Trump", "Vote for Others", "Offset"), 
       col = c("Blue", "Red", "chartreuse4", 'Gray70'), text.col = c("Blue", "Red", "chartreuse4", 'Gray50'), text.font = 2, lwd = 10, cex = 1.3)

## remove needless table
remove (ethnicity_Georgia, offset_Georgia)

###### 4.3 histogram of Michigan votes by the respondent's ethnicity ######
par(mfrow = c(1,1))
interview_Michigan$`Vote for in 2020` <- as.numeric(interview_Michigan$`Vote for in 2020`)
interview_Michigan$Ethnicity <- as.numeric(interview_Michigan$Ethnicity)
## Combine candidates and ethnicity into a new table ##
ethnicity_Michigan <- table (interview_Michigan$`Vote for in 2020`, interview_Michigan$Ethnicity, dnn = c('Votes', 'Ethnicity'))
table_Michigan <- data.frame(ethnicity_Michigan)

## Arrange Ethnicity table: 
## Ethnicity: 1(White but no Hispanic), 2(Black but no Hispanic), 3(Hispanic),
##            4(Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone),
##            5(Native American/Alaska Native or other race, non-Hispanic alone),
##            6(Multiple races, non-Hispanic)
## Votes    : 1(Joe Biden), 2(Donald Trump), 3(Jo Jorgensen), 4(Howie Hawkins), 5(Others),
##            but always, I will merge 3~5 into 3
table_Michigan$Ethnicity <- as.numeric(table_Michigan$Ethnicity)
table_Michigan[c(1,3,4,6,7,9,10,12,13,15,16,18),2] <- ''
table_Michigan[c(2,5,8,11,14,17),2] <- c('White', 'Black', 'Hispano', 'Asian', 'AIAN', 'Multi')

## Create a bar plot
barplot(table_Michigan$Freq*100/sum(table_Michigan$Freq), ylim = c(-20,50), 
        space = c(3,0,0,rep(c(1,0,0),5)), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, cex.names = 0.7,
        names.arg = table_Michigan$Ethnicity, 
        col = c("Blue", "Red", "chartreuse4"), 
        main = 'Races in Michigan', ylab = 'percent(%) in all respondents')
offset_Michigan <- round(((table_Michigan$Freq[table_Michigan$Votes == '1'] - table_Michigan$Freq[table_Michigan$Votes == '2'])*100)/ sum(table_Michigan$Freq),1)
barplot(offset_Michigan, space = c(6,3,3,3,3,3), add = T, col = 'Gray70', cex.axis= 1.5)
legend(12,50, c("Vote for Biden", "Vote for Trump", "Vote for Others", "Offset: Biden - Trump"), 
       col = c("Blue", "Red", "chartreuse4", 'Gray70'), text.col = c("Blue", "Red", "chartreuse4", 'Gray50'), text.font = 2, lwd = 10, cex = 1.3)
arrows(x0 = 5, y0 = -2, x1 = 5, y1 = -15, length = 0.15, angle = 30, col = 'Gray70', lwd = 4, lty = 1)
text(5.5, -18, 'Support Trump more', cex = 1.3, col = 'gray50')
arrows(x0 = 8, y0 = 7, x1 = 8, y1 = 20, length = 0.15, angle = 30, col = 'Gray70', lwd = 4, lty = 1)
text(8.5, 24, 'Support Biden more', cex = 1.3, col = 'gray50')

## remove needless table
remove (ethnicity_Michigan, offset_Michigan)


###### 4.4 histogram of Pennsylvania votes by the respondent's ethnicity ######
par(mfrow = c(1,1))
interview_Pennsylvania$`Vote for in 2020` <- as.numeric(interview_Pennsylvania$`Vote for in 2020`)
interview_Pennsylvania$Ethnicity <- as.numeric(interview_Pennsylvania$Ethnicity)
## Combine candidates and ethnicity into a new table ##
ethnicity_Pennsylvania <- table (interview_Pennsylvania$`Vote for in 2020`, interview_Pennsylvania$Ethnicity, dnn = c('Votes', 'Ethnicity'))
table_Pennsylvania <- data.frame(ethnicity_Pennsylvania)

## Arrange Ethnicity table: 
## Ethnicity: 1(White but no Hispanic), 2(Black but no Hispanic), 3(Hispanic),
##            4(Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone),
##            5(Native American/Alaska Native or other race, non-Hispanic alone),
##            6(Multiple races, non-Hispanic)
## Votes    : 1(Joe Biden), 2(Donald Trump), 3(Jo Jorgensen), 4(Howie Hawkins), 5(Others),
##            but always, I will merge 3~5 into 3
table_Pennsylvania$Ethnicity <- as.numeric(table_Pennsylvania$Ethnicity)
table_Pennsylvania$Votes <- as.numeric(table_Pennsylvania$Votes)
## No AIAN(Native American), I create an empty row to make the table the same as other states
data1 <- data.frame(Votes=c(1,2,3), Ethnicity=c(5,5,5), Freq=c(0,0,0))
table_Pennsylvania <- rbind(table_Pennsylvania[c(1:12),], data1, table_Pennsylvania[c(13:15),])
table_Pennsylvania[c(1,3,4,6,7,9,10,12,13,15,16,18),2] <- ''
table_Pennsylvania[c(2,5,8,11,14,17),2] <- c('White', 'Black', 'Hispano', 'Asian', 'AIAN', 'Multi')

## Create a bar plot
barplot(table_Pennsylvania$Freq*100/sum(table_Pennsylvania$Freq), ylim = c(-20,50), 
        space = c(3,0,0,rep(c(1,0,0),5)), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, cex.names = 0.7,
        names.arg = table_Pennsylvania$Ethnicity, 
        col = c("Blue", "Red", "chartreuse4"), 
        main = 'Races in Pennsylvania', ylab = 'percent(%) in all respondents')
offset_Pennsylvania <- round(((table_Pennsylvania$Freq[table_Pennsylvania$Votes == '1'] - table_Pennsylvania$Freq[table_Pennsylvania$Votes == '2'])*100)/ sum(table_Pennsylvania$Freq),1)
barplot(offset_Pennsylvania, space = c(6,3,3,3,3,3), add = T, col = 'Gray70', cex.axis= 1.5)
legend(16,40, c("Vote for Biden", "Vote for Trump", "Vote for Others", "Offset"), 
       col = c("Blue", "Red", "chartreuse4", 'Gray70'), text.col = c("Blue", "Red", "chartreuse4", 'Gray50'), text.font = 2, lwd = 10, cex = 1.3)

## remove needless table
remove (ethnicity_Pennsylvania, offset_Pennsylvania, data1)

###### 4.5 histogram of Wisconsin votes by the respondent's ethnicity ######
par(mfrow = c(1,1))
interview_Wisconsin$`Vote for in 2020` <- as.numeric(interview_Wisconsin$`Vote for in 2020`)
interview_Wisconsin$Ethnicity <- as.numeric(interview_Wisconsin$Ethnicity)
## Combine candidates and ethnicity into a new table ##
ethnicity_Wisconsin <- table (interview_Wisconsin$`Vote for in 2020`, interview_Wisconsin$Ethnicity, dnn = c('Votes', 'Ethnicity'))
table_Wisconsin <- data.frame(ethnicity_Wisconsin)

## Arrange Ethnicity table: 
## Ethnicity: 1(White but no Hispanic), 2(Black but no Hispanic), 3(Hispanic),
##            4(Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone),
##            5(Native American/Alaska Native or other race, non-Hispanic alone),
##            6(Multiple races, non-Hispanic)
## Votes    : 1(Joe Biden), 2(Donald Trump), 3(Jo Jorgensen), 4(Howie Hawkins), 5(Others),
##            but always, I will merge 3~5 into 3
names(ethnicity_Wisconsin) <- c('White', 'Black', 'Hispanic', 'Asian', 'Native American', 'Multiple races')
table_Wisconsin$Ethnicity <- as.numeric(table_Wisconsin$Ethnicity)
table_Wisconsin[c(1,3,4,6,7,9,10,12,13,15,16,18),2] <- ''
table_Wisconsin[c(2,5,8,11,14,17),2] <- c('White', 'Black', 'Hispano', 'Asian', 'AIAN', 'Multi')

## Create a bar plot
barplot(table_Wisconsin$Freq*100/sum(table_Wisconsin$Freq), ylim = c(-20,50), 
        space = c(3,0,0,rep(c(1,0,0),5)), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, cex.names = 0.7,
        names.arg = table_Wisconsin$Ethnicity, 
        col = c("Blue", "Red", "chartreuse4"), 
        main = 'Races in Wisconsin', ylab = 'percent(%) in all respondents')
offset_Wisconsin <- round(((table_Wisconsin$Freq[table_Wisconsin$Votes == '1'] - table_Wisconsin$Freq[table_Wisconsin$Votes == '2'])*100)/ sum(table_Wisconsin$Freq),1)
barplot(offset_Wisconsin, space = c(6,3,3,3,3,3), add = T, col = 'Gray70', cex.axis= 1.5)
legend(16,40, c("Vote for Biden", "Vote for Trump", "Vote for Others", "Offset"), 
       col = c("Blue", "Red", "chartreuse4", 'Gray70'), text.col = c("Blue", "Red", "chartreuse4", 'Gray70'), text.font = 2, lwd = 10, cex = 1.3)

## remove needless table
remove (ethnicity_Wisconsin, offset_Wisconsin)

###### 5.0 linear regression of overturned state between Biden and Trump's rate ######
par(mfrow = c(1,1))
plot(interview_rate[,c(1:12)])

## Create a linear regression line ##
plot(interview_rate$`Rate for Biden`, interview_rate$`Rate for Trump`, xlab = "Biden's rate", ylab = "Trump's rate", cex.axis= 1.5, cex.lab= 1.5)
m1_swing <- lm(`Rate for Trump`~`Rate for Biden`, data = interview_rate)
abline(m1_swing, col = 'blue', lwd = 5, lty =4)
text(80,40, "Rate for Trump = 88.0709 - \n0.9681 * Rate for Biden", col = 'blue', cex = 1.2)
summary(m1_swing)

par(mfrow = c(2,2))
plot(m1_swing)












###### 5.1 linear regression of Arizona between Biden and Trump's rate ######
par(mfrow = c(1,1))
plot(interview_Arizona[,c(1:12)])

## Create a linear regression line ##
plot(interview_Arizona$`Rate for Biden`, interview_Arizona$`Rate for Trump`, xlab = "Biden's rate", ylab = "Trump's rate")
m1_Arizona <- lm(`Rate for Trump`~`Rate for Biden`, data = interview_Arizona)
abline(m1_Arizona, col = 'blue', lwd = 2, lty =4)
text(80,30, "Rate for Trump = 89.96778 - \n1.05024 * Rate for Biden", col = 'blue')
summary(m1_Arizona)


plot(m1_Arizona)












###### 5.2 linear regression of Georgia between Biden and Trump's rate ######
par(mfrow = c(1,1))
plot(interview_Georgia[,c(1:12)])

## Create a linear regression line ##
plot(interview_Georgia$`Rate for Biden`, interview_Georgia$`Rate for Trump`, xlab = "Biden's rate", ylab = "Trump's rate")
m1_Georgia <- lm(`Rate for Trump`~`Rate for Biden`, data = interview_Georgia)
abline(m1_Georgia, col = 'blue', lwd = 2, lty =4)
text(80,27, "Rate for Trump = 88.50080 - \n0.93382 * Rate for Biden", col = 'blue')
summary(m1_Georgia)


plot(m1_Georgia)

###### 5.3 linear regression of Michigan between Biden and Trump's rate ######
par(mfrow = c(1,1))
plot(interview_Michigan[,c(1:12)])

## Create a linear regression line ##
plot(interview_Michigan$`Rate for Biden`, interview_Michigan$`Rate for Trump`, xlab = "Biden's rate", ylab = "Trump's rate")
m1_Michigan <- lm(`Rate for Trump`~`Rate for Biden`, data = interview_Michigan)
abline(m1_Michigan, col = 'blue', lwd = 2, lty =4)
text(80,25, "Rate for Trump = 89.61294 - \n0.98372 * Rate for Biden", col = 'blue')
summary(m1_Michigan)


plot(m1_Michigan)

###### 5.4 linear regression of Pennsylvania between Biden and Trump's rate ######
par(mfrow = c(1,1))
plot(interview_Pennsylvania[,c(1:12)])

## Create a linear regression line ##
plot(interview_Pennsylvania$`Rate for Biden`, interview_Pennsylvania$`Rate for Trump`, xlab = "Biden's rate", ylab = "Trump's rate")
m1_Pennsylvania <- lm(`Rate for Trump`~`Rate for Biden`, data = interview_Pennsylvania)
abline(m1_Pennsylvania, col = 'blue', lwd = 2, lty =4)
text(80,25, "Rate for Trump = 91.00536 - \n1.01126 * Rate for Biden", col = 'blue')
summary(m1_Pennsylvania)



plot(m1_Pennsylvania)
###### 5.5 linear regression of Wisconsin between Biden and Trump's rate ######
par(mfrow = c(1,1))
plot(interview_Wisconsin[,c(1:12)])

## Create a linear regression line ##
plot(interview_Wisconsin$`Rate for Biden`, interview_Wisconsin$`Rate for Trump`, xlab = "Biden's rate", ylab = "Trump's rate")
m1_Wisconsin <- lm(`Rate for Trump`~`Rate for Biden`, data = interview_Wisconsin)
abline(m1_Wisconsin, col = 'blue', lwd = 2, lty =4)
text(80,25, "Rate for Trump = 89.51141 - \n0.98755 * Rate for Biden", col = 'blue')
summary(m1_Wisconsin)


plot(m1_Wisconsin)

###### 6.1 box plot for Arizona group by gender ######
library(lattice)


## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex, data = interview_Arizona, ylab = 'Point', col = 'lightcoral', horizontal = F, ylim = c(-10,110), names = c('Male', 'Female'), main = "The Arizona gender V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~Sex, data = interview_Arizona, col = 'lightskyblue', horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Male', 'Female'), main = "The Arizona gender V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)



## rollback
par(mfrow = c(1,1))

###### 6.2 box plot for Georgia group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex, data = interview_Georgia, ylab = 'Point', col = 'lightcoral', horizontal = F, ylim = c(-10,110), names = c('Male', 'Female'), main = "The Georgia gender V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~Sex, data = interview_Georgia, col = 'lightskyblue', horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Male', 'Female'), main = "The Georgia gender V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))

###### 6.3 box plot for Michigan group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex, data = interview_Michigan, ylab = 'Point', col = 'lightcoral', horizontal = F, ylim = c(-10,110), names = c('Male', 'Female'), main = "The Michigan gender V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~Sex, data = interview_Michigan, col = 'lightskyblue', horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Male', 'Female'), main = "The Michigan gender V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))

###### 6.4 box plot for Pennsylvania group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex, data = interview_Pennsylvania, ylab = 'Point', col = 'lightcoral', horizontal = F, ylim = c(-10,110), names = c('Male', 'Female'), main = "The Pennsylvania gender V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~Sex, data = interview_Pennsylvania, col = 'lightskyblue', horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Male', 'Female'), main = "The Pennsylvania gender V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))

###### 6.5 box plot for Wisconsin group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex, data = interview_Wisconsin, ylab = 'Point', col = 'lightcoral', horizontal = F, ylim = c(-10,110), names = c('Male', 'Female'), main = "The Wisconsin gender V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~Sex, data = interview_Wisconsin, col = 'lightskyblue', horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Male', 'Female'), main = "The Wisconsin gender V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))






###### 7.1 box plot for Arizona group by gender ######
library(lattice)


## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~`Vote for in 2020`, data = interview_Arizona, ylab = 'Point', col = c('lightcoral', 'lightskyblue', 'green') , horizontal = F, ylim = c(-10,110), names = c('Biden', 'Trump', 'Others'), main = "The Arizona votes V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~`Vote for in 2020`, data = interview_Arizona, col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Biden', 'Trump', 'Others'), main = "The Arizona votes V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)



## rollback
par(mfrow = c(1,1))

###### 7.2 box plot for Georgia group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~`Vote for in 2020`, data = interview_Georgia, ylab = 'Point', col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), names = c('Biden', 'Trump', 'Others'), main = "The Georgia votes V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~`Vote for in 2020`, data = interview_Georgia, col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Biden', 'Trump', 'Others'), main = "The Georgia votes V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))

###### 7.3 box plot for Michigan group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~`Vote for in 2020`, data = interview_Michigan, ylab = 'Point', col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), names = c('Biden', 'Trump', 'Others'), main = "The Michigan votes V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~`Vote for in 2020`, data = interview_Michigan, col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Biden', 'Trump', 'Others'), main = "The Michigan votes V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))

###### 7.4 box plot for Pennsylvania group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~`Vote for in 2020`, data = interview_Pennsylvania, ylab = 'Point', col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), names = c('Biden', 'Trump', 'Others'), main = "The Pennsylvania votes V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~`Vote for in 2020`, data = interview_Pennsylvania, col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Biden', 'Trump', 'Others'), main = "The Pennsylvania votes V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))

###### 7.5 box plot for Wisconsin group by gender ######
## Create a box plot ##
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~`Vote for in 2020`, data = interview_Wisconsin, ylab = 'Point', col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), names = c('Biden', 'Trump', 'Others'), main = "The Wisconsin votes V.S \nRate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
boxplot(`Rate for Trump`~`Vote for in 2020`, data = interview_Wisconsin, col = c('lightcoral', 'lightskyblue', 'green'), horizontal = F, ylim = c(-10,110), ylab = '',  xlab = '', names = c('Biden', 'Trump', 'Others'), main = "The Wisconsin votes V.S \nRate for Trump", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)


## rollback
par(mfrow = c(1,1))



###### 8.1 box plot for Arizona group by gender ######
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex*`Vote for in 2020`, data = interview_Arizona, ylab = 'Point', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Arizona gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)
boxplot(`Rate for Trump`~Sex*`Vote for in 2020`, data = interview_Arizona, ylab = '', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Arizona gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)


## rollback
par(mfrow = c(1,1))

###### 8.2 box plot for Georgia group by gender ######
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex*`Vote for in 2020`, data = interview_Georgia, ylab = 'Point', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Georgia gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)
boxplot(`Rate for Trump`~Sex*`Vote for in 2020`, data = interview_Georgia, ylab = '', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Georgia gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)


## rollback
par(mfrow = c(1,1))

###### 8.3 box plot for Michigan group by gender ######
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex*`Vote for in 2020`, data = interview_Michigan, ylab = 'Point', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Michigan gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)
boxplot(`Rate for Trump`~Sex*`Vote for in 2020`, data = interview_Michigan, ylab = '', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Michigan gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)


## rollback
par(mfrow = c(1,1))

###### 8.4 box plot for Pennsylvania group by gender ######
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex*`Vote for in 2020`, data = interview_Pennsylvania, ylab = 'Point', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Pennsylvania gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)
boxplot(`Rate for Trump`~Sex*`Vote for in 2020`, data = interview_Pennsylvania, ylab = '', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Pennsylvania gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)


## rollback
par(mfrow = c(1,1))

###### 8.5 box plot for Wisconsin group by gender ######
par(mfrow = c(1,2))
boxplot(`Rate for Biden`~Sex*`Vote for in 2020`, data = interview_Wisconsin, ylab = 'Point', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Wisconsin gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)
boxplot(`Rate for Trump`~Sex*`Vote for in 2020`, data = interview_Wisconsin, ylab = '', col = c(rep('lightcoral',2), rep('lightskyblue', 2),  rep('Green', 2)), horizontal = F, ylim = c(-10,110),  main = "The Wisconsin gender/vote \nV.S Rate for Biden", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, adj = -1)


## rollback
par(mfrow = c(1,1))
