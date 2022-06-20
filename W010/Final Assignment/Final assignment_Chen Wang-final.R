### 0 Loading datasets ----
# Read file from housing.csv
housing <- read.csv("housing.csv")


# Read file from schools.csv
schools <- read.csv("schools.csv")


# Creating image file to store images
if(!file.exists(paste(getwd(), '/Image', sep=''))) 
  dir.create("./Image")

### 1 Data arranging  (Titles only) ----
#* 1.1 Merging two different datasets ----
#* by vlook up elementary(housing.csv) with school(school.csv)
housing <- merge (housing, schools, by.x = "elementary", by.y = "school")

#** 1.2 roughly check data ----
summary(housing)


#** 1.2.1 Can not see character ----
#** Changing character to factor first
housing$neighborhood <- as.factor(housing$neighborhood)
housing$type <- as.factor(housing$type)
housing$levels <- as.factor(housing$levels)
housing$cooling <- as.factor(housing$cooling)
housing$heating <- as.factor(housing$heating)
housing$fireplace <- as.factor(housing$fireplace)
housing$elementary <- as.factor(housing$elementary)
housing$middle <- as.factor(housing$middle)
housing$high <- as.factor(housing$high)

#** 1.2.2 roughly check data again----
summary(housing)

#*** 1.3 check the summary data again ----
#*** There are several oddities I need to check:
#*** oddities:
#***          beds: max is 999 ???
#***          bath: max is 25 ???
#***          type: townhouse*129, town house*2
#*** 
#*** Null:
#***          sqft: 2 NA
#***          lotsize: 20 NA
#***          levels: 6 ?
#***          cooling: 7 empty
#***          heating: 7 empty
#***         fireplace: 6 empty
#***
#*** First of all, checking the oddities(outliers):
plot(housing[,c(1:9)])
plot(housing[,c(10:17)])


#*** checking boxplot to ensure oddities if it exists cluster.
par(mfrow = c(1,3))
boxplot(housing$beds, xlab ="beds", cex.lab = 2)
boxplot(housing$baths, xlab ="baths", cex.lab = 2)
boxplot(housing$year, xlab ="year", cex.lab = 2)
par(mfrow = c(1,1))

### 2 Data cleaning (Titles only) ----
#* 2.1 Remove the oddities(outliers) ----
housingclean <- housing[housing$beds < 100, ]
housingclean <- housingclean[housingclean$baths < 15, ]
housingclean <- housingclean[housingclean$year > 1600 & housingclean$year < 2100, ]
housingclean$type[housingclean$typ == 'townhouse'] <-'town house'


#* Double-checking if still find abnormal data
plot(housingclean[,c(1:9)])
plot(housingclean[,c(10:17)])

#* Compare Null data with its mother data to check if the Null dataset is abnormal
par(mfrow = c(3,4))
nacheck <- is.na(housingclean$lotsize)
boxplot(housingclean$sqft~nacheck, main ="sqft")
boxplot(housingclean$beds~nacheck, main ="beds")
boxplot(housingclean$baths~nacheck, main ="baths")
boxplot(housingclean$year~nacheck, main ="year")
boxplot(housingclean$levels~nacheck, main ="levels")
boxplot(housingclean$cooling~nacheck, main ="cooling")
boxplot(housingclean$heating~nacheck, main ="heating")
boxplot(housingclean$fireplace~nacheck, main ="fireplace")
boxplot(housingclean$soldprice~nacheck, main ="soldprice")
boxplot(housingclean$size~nacheck, main ="size")
boxplot(housingclean$rating~nacheck, main ="rating")
par(mfrow = c(1,1))

#** 2.2 Removing all NULL data ----
housingcleanNA <- housingclean[!is.na(housingclean$sqft),]
housingcleanNA <- housingcleanNA[!is.na(housingcleanNA$lotsize),]
housingcleanNA <- housingcleanNA[which(housingcleanNA$levels !='?'),]
housingcleanNA <- housingcleanNA[which(housingcleanNA$cooling !=''),]
housingcleanNA <- housingcleanNA[which(housingcleanNA$heating !=''),]
housingcleanNA <- housingcleanNA[which(housingcleanNA$fireplace !=''),]


#** remove temporary storage (factor-->character-->factor)
housingcleanNA$neighborhood <- as.character(housingcleanNA$neighborhood)
housingcleanNA$type <- as.character(housingcleanNA$type)
housingcleanNA$levels <- as.character(housingcleanNA$levels)
housingcleanNA$cooling <- as.character(housingcleanNA$cooling)
housingcleanNA$heating <- as.character(housingcleanNA$heating)
housingcleanNA$fireplace <- as.character(housingcleanNA$fireplace)
housingcleanNA$elementary <- as.character(housingcleanNA$elementary)
housingcleanNA$middle <- as.character(housingcleanNA$middle)
housingcleanNA$high <- as.character(housingcleanNA$high)


housingcleanNA$neighborhood <- as.factor(housingcleanNA$neighborhood)
housingcleanNA$type <- as.factor(housingcleanNA$type)
housingcleanNA$elementary <- as.factor(housingcleanNA$elementary)
housingcleanNA$middle <- as.factor(housingcleanNA$middle)
housingcleanNA$high <- as.factor(housingcleanNA$high)





#** Confirm plot again to check if found extra outliers
summary(housingcleanNA)

plot(housingcleanNA[,c(1:9)])
plot(housingcleanNA[,c(10:17)])

### 3 One-variable visuals (Titles only) ----
#* 3.1 box plot function ----
myfavbox <- function(x, y, var = NULL, color = 'yellow', width = 700, height = 600, word = ''){
  x1 <- deparse(substitute(x))
  y1 <- deparse(substitute(y))
  var1 <- deparse(substitute(var))
  title <- paste( "Box plot" , substr(x1, nchar(var1)+2, nchar(x1)) , "VS." , substr(y1, nchar(var1)+2, nchar(y1)), word)
  savefile <-paste('Image//',title, '.png', sep = '')
  png(file= savefile, width = width, height = height)
  boxplot(x~y, col = color, main = title, xlab = substr(y1, nchar(var1)+2, nchar(y1)), ylab = substr(x1, nchar(var1)+2, nchar(x1)), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
  dev.off()
}

#** 3.2 bar plot function ----


#*** 3.3 histogram function ----
myhistogram<- function(x, y=NULL, z = NULL, u = NULL, w = NULL, var = NULL, ylim = NULL, width = 700, height = 600, word = ''){
  xmin <- min(x, y, z, u, w)
  xmax <- max(x, y, z, u, w)
  x1 <- deparse(substitute(x))
  y1 <- deparse(substitute(y))
  z1 <- deparse(substitute(z))
  u1 <- deparse(substitute(u))
  w1 <- deparse(substitute(w))
  var1 <- deparse(substitute(var))
  title <- paste( "Histogram" , substr(x1, nchar(var1)+2, nchar(x1)) , "frequency chart", word )
  savefile <-paste('Image//',title, '.png', sep = '')
  png(file= savefile, width = width, height = height)
  hist(x, main = title, breaks = seq(xmin,xmax,(xmax-xmin)/10), freq = T, xlab = substr(x1, nchar(var1)+2, nchar(x1)), ylab = "Frequency", col = rgb(0,0,1,0.25), xlim = c(xmin, xmax), ylim = ylim, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
  if(!is.null(y)){
    hist(y, breaks = seq(xmin,xmax,(xmax-xmin)/10), freq = T, add = T, col = rgb(0,1,0,0.25))
  }
  if(!is.null(z)){
    hist(z, breaks = seq(xmin,xmax,(xmax-xmin)/10), freq = T, add = T, col = rgb(1,0,0,0.25))
  }
  if(!is.null(u)){
    hist(u, breaks = seq(xmin,xmax,(xmax-xmin)/10), freq = T, add = T, col = rgb(1,1,0,0.25))
  }
  if(!is.null(w)){
    hist(w, breaks = seq(xmin,xmax,(xmax-xmin)/10), freq = T, add = T, col = rgb(0,1,1,0.25))
  }
  dev.off()
}


#**** 3.4 one-variable visuals ----
#**** 3.4.1 box plot


(myfavbox(housingcleanNA$sqft,housingcleanNA$type, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$lotsize,housingcleanNA$type, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$beds,housingcleanNA$type, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$baths,housingcleanNA$type, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$soldprice,housingcleanNA$type, housingcleanNA, width = 1350))


(myfavbox(housingcleanNA$soldprice,housingcleanNA$beds, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$soldprice,housingcleanNA$baths, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$soldprice,housingcleanNA$cooling, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$soldprice,housingcleanNA$heating, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$soldprice,housingcleanNA$fireplace, housingcleanNA, width = 1350))


(myfavbox(housingcleanNA$soldprice,housingcleanNA$size, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$soldprice,housingcleanNA$rating, housingcleanNA, width = 1350))
(myfavbox(housingcleanNA$soldprice,housingcleanNA$levels, housingcleanNA, width = 1350))


#**** 3.4.2 histogram
(myhistogram(housingcleanNA$sqft, var = housingcleanNA))
(myhistogram(housingcleanNA$soldprice, var = housingcleanNA))


#**** 3.4.3 barplot
library("viridis")
Elementary <- aggregate(soldprice~elementary, data = housingcleanNA, FUN = mean)
Elementary  <- data.frame(Elementary)
Elementary_order <- Elementary[order(Elementary$soldprice, decreasing = F),]

png(file= "Image/Sold price by different elementarys.png", width = 1350, height = 950)
barplot(Elementary_order$soldprice/1000, names.arg = Elementary_order$elementary, col  = rainbow(50), 
        main = "Sold price by different elementarys", horiz = T, xlab = "Soldprice(thousand $)", ylab = "Elementary", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
dev.off()

png(file= "Image/Sold price by different elementarys_head.png", width = 1350, height = 950)
barplot(head(Elementary_order$soldprice)/1000, names.arg = head(Elementary_order$elementary), col  = rainbow(50), 
        main = "Sold price by different elementarys", horiz = T, xlab = "Soldprice(thousand $)", ylab = "Elementary", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
dev.off()

png(file= "Image/Sold price by different elementarys_tail.png", width = 1350, height = 950)
barplot(tail(Elementary_order$soldprice)/1000, names.arg = tail(Elementary_order$elementary), col  = rainbow(50), 
        main = "Sold price by different elementarys", horiz = T, xlab = "Soldprice(thousand $)", ylab = "Elementary", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
dev.off()




### 4 multi-variables visuals (Title only) ----
#* 4.1 Scatter plot function ----
myfavscatter <- function(x, y, var = NULL, color = 'red', width = 700, height = 600, word = ''){
  x1 <- deparse(substitute(x))
  y1 <- deparse(substitute(y))
  var1 <- deparse(substitute(var))
  title <- paste( "Scatter plot" , substr(x1, nchar(var1)+2, nchar(x1)) , "VS." , substr(y1, nchar(var1)+2, nchar(y1)), word)
  savefile <- paste('Image//', title, '.png', sep = '')
  png(file= savefile, width = width, height = height)
  plot(x, y, type = "p", xlab = substr(x1, nchar(var1)+2, nchar(x1)), ylab = substr(y1, nchar(var1)+2, nchar(y1)), main = title, lty = 3, lwd = 2, col = color, pch = 21, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
  dev.off()
}


#** 4.2 Density function ----
myhistdensity<- function(x, y=NULL, z = NULL, u = NULL, w = NULL, main ="", xlab = "", ylab = "", ylim = NULL, position = NULL, topic = NULL){
  xmin <- min(x, y, z, u, w)
  xmax <- max(x, y, z, u, w)
  dens1 <- density(x)
  plot(dens1 , lwd = 3, lty = 1, col = 'orchid', main = main, xlab = xlab, ylab = ylab, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5, xlim = c(xmin, xmax), ylim = ylim)
  if(!is.null(y)){
    dens2 <- density(y)
    lines(dens2 , lwd = 3, lty = 1, col = 'blueviolet')
  }
  if(!is.null(z)){
    dens3 <- density(z)
    lines(dens3 , lwd = 3, lty = 1, col = 'red')
  }
  if(!is.null(u)){
    dens4 <- density(u)
    lines(dens4 , lwd = 3, lty = 1, col = 'orange1')
  }
  if(!is.null(w)){
    dens5 <- density(w)
    lines(dens5 , lwd = 3, lty = 1, col = 'green')
  }
  legend(position, topic, col = c('orchid', 'blueviolet', 'red', 'orange1', 'green'), lwd = 3, cex= 1.1 )
  
}







#*** 4.3 multi-variables visuals ----
#*** 4.3.1 Histograms
hist(housingcleanNA$soldprice[housingcleanNA$type == 'condo'], breaks = seq(0, 2400000, 200000),
     main = "sold price in different types of houses",
     xlab = "money($)", ylab = "count(#)", col = rgb(0,0,1,0.25), freq = T, ylim = c(0,100), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanNA$soldprice[housingcleanNA$type == 'condominium'], breaks = seq(0, 2400000, 200000),
     col = rgb(0.7,0,0,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanNA$soldprice[housingcleanNA$type == 'multi-family home'], breaks = seq(0, 2400000, 200000),
     col = rgb(1,0,0,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanNA$soldprice[housingcleanNA$type == 'single-family home'], breaks = seq(0, 2400000, 200000),
     col = rgb(1,1,0,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanNA$soldprice[housingcleanNA$type == 'town house'], breaks = seq(0, 2400000, 200000),
     col = rgb(0,1,1,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
histword <- c("condo", 'condominium', 'multi-family home', 'single-family home', 'town house')
histcolor <- c(rgb(0,0,1,0.25), rgb(1,0,0,0.25), rgb(1,0,0,0.25), rgb(1,1,0,0.25), rgb(0,1,1,0.25))
legend(100000,100, histword, col = histcolor, lwd = 10, cex= 1 )



#** 4.3.2 Density plot
#**  Sold price density in different types of houses
myhistdensity(housingcleanNA$soldprice[housingcleanNA$type == 'condo'],
              housingcleanNA$soldprice[housingcleanNA$type == 'condominium'],
              housingcleanNA$soldprice[housingcleanNA$type == 'multi-family home'],
              housingcleanNA$soldprice[housingcleanNA$type == 'single-family home'],
              housingcleanNA$soldprice[housingcleanNA$type == 'town house'],
              main = "Sold price density in different \ntypes of houses",
              xlab = "Money($)",
              ylab = "Density",
              ylim = c(0,0.000007),
              position = "topright",
              topic = c("condo", 'condominium', 'multi-family home', 'single-family home', 'town house')
)


#*** 4.3.3 scatter
(myfavscatter(housingcleanNA$sqft, housingcleanNA$beds, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$sqft, housingcleanNA$lotsize, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$baths, housingcleanNA$sqft, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$sqft, housingcleanNA$baths, var = housingcleanNA, color = 'blue'))


#*** relevant to the soldprice
(myfavscatter(housingcleanNA$sqft, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$beds, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$baths, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$lotsize, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$size, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$rating, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$levels, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))
(myfavscatter(housingcleanNA$year, housingcleanNA$soldprice, var = housingcleanNA, color = 'blue'))

par(mfrow = c(1,2))
plot(type = 'p', housingcleanNA$year[housingcleanNA$cooling == "No"], housingcleanNA$soldprice[housingcleanNA$cooling == "No"], xlab = "year", ylab = "Sold price", main = "cooling", lty = 3, lwd = 2, col = "red", pch = 21, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p',housingcleanNA$year[housingcleanNA$cooling == "Yes"], housingcleanNA$soldprice[housingcleanNA$cooling == "Yes"], lty = 3, lwd = 2, col = "Blue", pch = 21)
legend ("topleft", c("No", "yes"), col = c("red", "blue"), lty = 3,  pch = 5)
plot(type = 'p', housingcleanNA$year[housingcleanNA$heating == "No"], housingcleanNA$soldprice[housingcleanNA$heating == "No"], xlab = "year", ylab ='', main = "heating", lty = 3, lwd = 2, col = "red", pch = 21, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p',housingcleanNA$year[housingcleanNA$heating == "Yes"], housingcleanNA$soldprice[housingcleanNA$heating == "Yes"], lty = 3, lwd = 2, col = "Blue", pch = 21)

par(mfrow = c(1,1))

### 5 correlation, linear regression, clusters (Title only) ----
#* 5-1 correlation ----
png(file= "Image/key_variables_1_9.png", width = 900, height = 750)
plot(housingcleanNA[,c(1:9)])
dev.off()

png(file= "Image/key_variables_10_17.png", width = 900, height = 750)
plot(housingcleanNA[,c(10:17)])
dev.off()

png(file= "Image/key_variables_1_17.png", width = 1350, height = 750)
plot(housingcleanNA[,c(1:17)])
dev.off()


plot(housingcleanNA[,c(3:6,8,10:12,15:17)])


#* Plotting scatter plot
png(file= "Image/After_plot_key_variables.png", width = 900, height = 750)
plot(housingcleanNA[,c(3:7,9:12,15:17)])
dev.off()

#* change binary factor to numeric in order to do calculating
housingcleanNA$levels <- as.factor(housingcleanNA$levels )
housingcleanNA$cooling <- as.factor(housingcleanNA$cooling )
housingcleanNA$heating <- as.factor(housingcleanNA$heating )
housingcleanNA$fireplace <- as.factor(housingcleanNA$fireplace)

housingcleanNA$levels <- as.numeric(housingcleanNA$levels )
housingcleanNA$cooling <- as.numeric(housingcleanNA$cooling )
housingcleanNA$heating <- as.numeric(housingcleanNA$heating )
housingcleanNA$fireplace <- as.numeric(housingcleanNA$fireplace)


library(corrplot)
library(openxlsx)
#* Calculating correlation between key variables
correlation <- cor(housingcleanNA[,c(3:7,9:12,15,16,17)])






#* Plotting correlation with key variables (pie)
png(file= "Image/After_corrplot_key_variables.png", width = 900, height = 750)
corrplot(cor(housingcleanNA[,c(3:7,9:12,15,16,17)]), method = 'pie')
dev.off()

#* Plotting correlation with key variables (number)
png(file= "Image/After_corrplot_key_variables-2.png", width = 900, height = 750)
corrplot(cor(housingcleanNA[,c(3:7,9:12,15,16,17)]), method = "number")
dev.off()



library(GGally)
#* Plotting correlation ... using ggscatmat
png(file= "Image/After_corrplot_key_variables-3.png", width = 900, height = 750)
ggscatmat(housingcleanNA,columns = c(3:7,9:12,15,16,17))
dev.off()



#** 5-2 linear regression ----
#**  Calculating linear regression between soldprice with other numeric variables
m1 <- lm (soldprice~beds + baths + sqft + lotsize + year + levels + cooling + heating + fireplace + size + rating, data = housingcleanNA)
summary(m1)
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

#**  Calculating linear regression between soldprice^0.5 with other numeric variables
m2 <- lm (soldprice^0.5~beds + baths + sqft + lotsize + year + levels + cooling + heating + fireplace + size + rating, data = housingcleanNA)
summary(m2)
par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))


#** However, if I put remove year only, and re-run m1, the "cooling" and "heating" will have a low P-value,
#** which means the cooling + heating will cause multilinearity.
m21 <- lm (soldprice~beds + baths + sqft + lotsize + levels + cooling + heating + fireplace + size + rating, data = housingcleanNA)
summary(m21)
par(mfrow = c(2,2))
plot(m21)
par(mfrow = c(1,1))




#**  Removing P-value >0.05 item, re-calculating linear regression with soldprice
m3 <- lm (soldprice~beds + year + size + rating, data = housingcleanNA)
summary(m3)
par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))

#**  Removing P-value >0.05 item, re-calculating linear regression between with soldprice^0.5
m4 <- lm (soldprice^0.5~beds + year  + size + rating, data = housingcleanNA)
summary(m4)
par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))


#**  Removing P-value >0.05 item, re-calculating linear regression with soldprice
m5 <- lm (soldprice~beds + year + size + rating + type, data = housingcleanNA)
summary(m5)
par(mfrow = c(2,2))
plot(m5)
par(mfrow = c(1,1))



#**  The same as m4, adding type into calculation
m6 <- lm (soldprice^0.5~beds + year + size + rating + type, data = housingcleanNA)
summary(m6)
par(mfrow = c(2,2))
plot(m6)
par(mfrow = c(1,1))



#*** 5-3 clusters ----
library(cluster)


#*** plotting a scatter plot between sqft & lotsize with grouping these two variables only and color of clusters = 2
png(file= "Image/Scatter plot sqft VS. lotsize cluster_2.png", width = 900, height = 750)
(kmeans_2 <- kmeans(housingcleanNA[,c("sqft","lotsize")], 2))
plot(housingcleanNA[,c("sqft","lotsize")], col = kmeans_2$cluster, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
points(kmeans_2$centers, col = 1:2, pch = 17, cex = 5)
dev.off()

#*** plotting a scatter plot between sqft & lotsize with grouping these two variables only and color of clusters = 3
png(file= "Image/Scatter plot sqft VS. lotsize cluster_3.png", width = 900, height = 750)
(kmeans_3 <- kmeans(housingcleanNA[,c("sqft","lotsize")], 3))
plot(housingcleanNA[,c("sqft","lotsize")], col = kmeans_3$cluster, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
points(kmeans_3$centers, col = 1:5, pch = 17, cex = 2)
dev.off()

#*** plotting a scatter plot between sqft & lotsize with grouping these two variables only and color of clusters = 5
png(file= "Image/Scatter plot sqft VS. lotsize cluster_5.png", width = 900, height = 750)
(kmeans_5 <- kmeans(housingcleanNA[,c("sqft","lotsize")], 5))
plot(housingcleanNA[,c("sqft","lotsize")], col = kmeans_5$cluster, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
points(kmeans_2$centers, col = 1:5, pch = 17, cex = 2)
dev.off()



set.seed(1987)
#*** plotting a scatter plot between beds, baths, sqft, and lotsize with grouping these numeric variables and color of clusters = 3
(k3means<-kmeans(housingcleanNA[,3:6],3))
(myfavscatter(housingcleanNA$beds, housingcleanNA$baths, var = housingcleanNA, color = k3means$cluster, word = 'by cluster = 3'))
(myfavscatter(housingcleanNA$beds, housingcleanNA$sqft, var = housingcleanNA, color = k3means$cluster, word = 'by cluster = 3'))
(myfavscatter(housingcleanNA$baths, housingcleanNA$beds, var = housingcleanNA, color = k3means$cluster, word = 'by cluster = 3'))



#*** plotting a scatter plot between m5 variables with grouping these numeric variables and color of clusters = 3
(k3means_2<-kmeans(housingcleanNA[,c(3,7,15:17)],3))
(myfavscatter(housingcleanNA$soldprice, housingcleanNA$sqft, var = housingcleanNA, color = k3means_2$cluster, word = 'by cluster = 3'))
(myfavscatter(housingcleanNA$soldprice, housingcleanNA$lotsize, var = housingcleanNA, color = k3means_2$cluster, word = 'by cluster = 3'))


#*** plotting a scatter plot between m5 variables with grouping these numeric variables and color of clusters = 5
(k5means_2<-kmeans(housingcleanNA[,c(3,7,15:17)],5))
(myfavscatter(housingcleanNA$soldprice, housingcleanNA$sqft, var = housingcleanNA, color = k5means_2$cluster, word = 'by cluster = 5'))



library(scatterplot3d)
#*** plotting scatter 3D plot by three variables: sold price, beds, year, the color of clusters = 5
png(file= "Image/Scatter plot sold price VS. beds VS. year cluster_5.png", width = 900, height = 750)
scatterplot3d(housingcleanNA$soldprice, housingcleanNA$beds, housingcleanNA$year, xlab = "soldprice", ylab = "beds", zlab = "year", color = k5means_2$cluster, type = "p", lwd = 2, box = F, cex.axis = 1, cex.lab= 1.5, cex.main =1.5, main = "Scatter plot soldprice/beds/year cluser = 5")
dev.off()


#*** Comparing with 2D plots. 
(myfavscatter(housingcleanNA$soldprice, housingcleanNA$beds, var = housingcleanNA, color = k5means_2$cluster, word = 'by cluster = 5-2'))
(myfavscatter(housingcleanNA$beds, housingcleanNA$year, var = housingcleanNA, color = k5means_2$cluster, word = 'by cluster = 5-2'))
(myfavscatter(housingcleanNA$soldprice, housingcleanNA$year, var = housingcleanNA, color = k5means_2$cluster, word = 'by cluster = 5-2'))







#*** Comparing with the real situation : sold price, beds
png(file= "Image/Scatter plot sold price VS. beds by grouping type.png", width = 700, height = 600)
plot(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'condo'], housingcleanNA$beds[housingcleanNA$type == 'condo'], lty = 3, lwd = 3, xlim = c(0,2400000), ylim = c(0,8), col = 'red', pch = 5, xlab ='Soldprice', ylab = 'beds', main ='Soldprice VS. beds by different type', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'condominium'], housingcleanNA$beds[housingcleanNA$type == 'condominium'], lty = 3, lwd = 3, col = 'orange', pch = 5)
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'multi-family home'], housingcleanNA$beds[housingcleanNA$type == 'multi-family home'], lty = 3, lwd = 3, col = 'gold1', pch = 5)
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'single-family home'], housingcleanNA$beds[housingcleanNA$type == 'single-family home'], lty = 3, lwd = 3, col = 'limegreen', pch = 5)
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'town house'], housingcleanNA$beds[housingcleanNA$type == 'town house'], lty = 3, lwd = 3, col = 'cyan4', pch = 5)
legend("topleft", c("condo", "condominium", "multi-family home", "single-family home", "town house"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =3, pch = 5)
dev.off()


#*** Comparing with the real situation : sold price, year
png(file= "Image/Scatter plot sold price VS. year by grouping type.png", width = 700, height = 600)
plot(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'condo'], housingcleanNA$year[housingcleanNA$type == 'condo'], lty = 3, lwd = 3, xlim = c(0,2400000), ylim = c(1900,2020), col = 'red', pch = 5, xlab ='Soldprice', ylab = 'year', main ='Soldprice VS. year by different type', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'condominium'], housingcleanNA$year[housingcleanNA$type == 'condominium'], lty = 3, lwd = 3, col = 'orange', pch = 5)
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'multi-family home'], housingcleanNA$year[housingcleanNA$type == 'multi-family home'], lty = 3, lwd = 3, col = 'gold1', pch = 5)
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'single-family home'], housingcleanNA$year[housingcleanNA$type == 'single-family home'], lty = 3, lwd = 3, col = 'limegreen', pch = 5)
lines(type = 'p', housingcleanNA$soldprice[housingcleanNA$type == 'town house'], housingcleanNA$year[housingcleanNA$type == 'town house'], lty = 3, lwd = 3, col = 'cyan4', pch = 5)
legend("topleft", c("condo", "condominium", "multi-family home", "single-family home", "town house"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =3, pch = 5)
dev.off()



#*** Comparing with the real situation : beds, year
png(file= "Image/Scatter plot beds VS. year by grouping type.png", width = 700, height = 600)
plot(type = 'p', housingcleanNA$beds[housingcleanNA$type == 'condo'], housingcleanNA$year[housingcleanNA$type == 'condo'], lty = 3, lwd = 3, xlim = c(0,8), ylim = c(1900,2020), col = 'red', pch = 5, xlab ='Soldprice', ylab = 'year', main ='beds VS. year by different type', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', housingcleanNA$beds[housingcleanNA$type == 'condominium'], housingcleanNA$year[housingcleanNA$type == 'condominium'], lty = 3, lwd = 3, col = 'orange', pch = 5)
lines(type = 'p', housingcleanNA$beds[housingcleanNA$type == 'multi-family home'], housingcleanNA$year[housingcleanNA$type == 'multi-family home'], lty = 3, lwd = 3, col = 'gold1', pch = 5)
lines(type = 'p', housingcleanNA$beds[housingcleanNA$type == 'single-family home'], housingcleanNA$year[housingcleanNA$type == 'single-family home'], lty = 3, lwd = 3, col = 'limegreen', pch = 5)
lines(type = 'p', housingcleanNA$beds[housingcleanNA$type == 'town house'], housingcleanNA$year[housingcleanNA$type == 'town house'], lty = 3, lwd = 3, col = 'cyan4', pch = 5)
legend("topleft", c("condo", "condominium", "multi-family home", "single-family home", "town house"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =3, pch = 5)
dev.off()



#*** plotting another scatter 3D plot by the three variables: beds, baths, and sqft
library(car)
library(rgl)
scatter3d(housingcleanNA$soldprice, housingcleanNA$sqft, housingcleanNA$lotsize, revolutions = 1, bg.col="black")



### 6 Sensitivity Analysis (Title only) ----
#* 6.1 Data arrranging/cleaning/replacing ----
#* Creating a new variable to replace housingcleanNA in visualization
housingcleanpred <- housingclean

#* Several NULL(include "?") are waiting to be handled:
#*  Numeric:
#*          sqft: 2 NA 
#*          lotsize: 20 NA
#*  Factor:
#*          levels: 6 ?
#*          cooling: 7 empty
#*          heating: 7 empty
#*          fireplace: 6 empty
#*
#* 6.1.1 Replacing factor(binary column) 
#* Factor can be replaced by median(binary table median = mode) 

#* levels: Transferring ?(factor) --> NA(character) --> replacing to "1" (median)
#* --> factor
housingcleanpred$levels[which(housingcleanpred$levels =='?')] <- "1"
housingcleanpred$levels <- as.character(housingcleanpred$levels)
housingcleanpred$levels <- as.factor(housingcleanpred$levels)

#* cooling: Transferring ""(factor) --> NA(character) --> replacing to "No" (median)
#* --> factor
housingcleanpred$cooling <- as.character(housingcleanpred$cooling)
housingcleanpred$cooling[which(housingcleanpred$cooling =='')] <- 'No'
housingcleanpred$cooling <- as.factor(housingcleanpred$cooling)


#* heating: Transferring ""(factor) --> NA(character) --> replacing to "No" (median)
#* --> factor
housingcleanpred$heating <- as.character(housingcleanpred$heating)
housingcleanpred$heating[which(housingcleanpred$heating =='')] <- 'No'
housingcleanpred$heating <- as.factor(housingcleanpred$heating)


#* fireplace: Transferring ""(factor) --> NA(character) --> replacing to "No" (median)
#* --> factor
housingcleanpred$fireplace <- as.character(housingcleanpred$fireplace)
housingcleanpred$fireplace[which(housingcleanpred$fireplace =='')] <- 'No'
housingcleanpred$fireplace <- as.factor(housingcleanpred$fireplace)


which(is.na(housingcleanpred$sqft))
housingcleanpred$sqft[c(115,345)]

#* 6.1.2 Replacing numeric column
#* 6.1.2.1 sqft: Checking NULL data at sqft column
which(is.na(housingcleanpred$sqft))
housingcleanpred$sqft[c(115,345)]


#* Roughly checking linear regression model (sqft with others)
m1_ReplaceTOmode <- lm (sqft~beds + baths + lotsize + levels + cooling + heating + fireplace + soldprice + size + rating + type, data = housingcleanpred)
summary(m1_ReplaceTOmode)

#* The same as m1_ReplaceTOmode, but remove variables with P-value >0.05
m2_ReplaceTOmode <- lm (sqft~beds + baths + lotsize + levels, data = housingcleanpred)
summary(m2_ReplaceTOmode)

#* Fit the NULL column: sqft with regression line model: m2_ReplaceTOmode
housingcleanpred$sqft[is.na(housingcleanpred$sqft)] <- predict(m2_ReplaceTOmode, list(
  beds = housingcleanpred$beds[is.na(housingcleanpred$sqft)],
  baths = housingcleanpred$baths[is.na(housingcleanpred$sqft)],
  lotsize = housingcleanpred$lotsize[is.na(housingcleanpred$sqft)],
  levels = housingcleanpred$levels[is.na(housingcleanpred$sqft)]
))


#* Finally check if replacing with numeric data
housingcleanpred$sqft[c(115,345)]


plot(housingcleanpred[,c(1:9)])
plot(housingcleanpred[,c(10:17)])


#* 6.1.2.2 lot size: Checking NULL data at lot size column
which(is.na(housingcleanpred$lotsize))
housingcleanpred$lotsize[c(31,70,128,134,155,161,208,348,362,376,467,505,564,572,585,603,621,666,678)]



#* Roughly checking linear regression model (lot size with others)
m3_ReplaceTOmode <- lm (lotsize~beds + baths + sqft + lotsize + levels + cooling + heating + fireplace + soldprice + size + rating + type, data = housingcleanpred)
summary(m3_ReplaceTOmode)


#* The same as m3_ReplaceTOmode, but remove variables with P-value >0.05
m4_ReplaceTOmode <- lm (lotsize~beds + sqft + levels, data = housingcleanpred)
summary(m4_ReplaceTOmode)




#* Fit the NULL column: lot size with regression line model: m4_ReplaceTOmode
housingcleanpred$lotsize[is.na(housingcleanpred$lotsize)] <- predict(m4_ReplaceTOmode, list(
  beds = housingcleanpred$beds[is.na(housingcleanpred$lotsize)],
  sqft = housingcleanpred$sqft[is.na(housingcleanpred$lotsize)],
  levels = housingcleanpred$levels[is.na(housingcleanpred$lotsize)]
))



#* Finally check if replacing with numeric data
housingcleanpred$lotsize[c(31,70,128,134,155,161,208,348,362,376,467,505,564,572,585,603,621,666,678)]

#* remove temporary storage (factor-->character-->factor)
housingcleanpred$type <- as.character(housingcleanpred$type)
housingcleanpred$type <- as.factor(housingcleanpred$type)


housingcleanpred$levels <- as.character(housingcleanpred$levels)
housingcleanpred$cooling <- as.character(housingcleanpred$cooling)
housingcleanpred$heating <- as.character(housingcleanpred$heating)
housingcleanpred$fireplace <- as.character(housingcleanpred$fireplace)


#* Finally checking
summary(housingcleanpred)



#** 6.2 Compare and contrast with previous finding ----
#** All the curve follows the previous coding (session 3.4, 4.3, and 5)
#** 6.2.1.0 one-variable visuals
#** 6.2.1.1 box plot
(myfavbox(housingcleanpred$sqft,housingcleanpred$type, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$baths,housingcleanpred$type, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$beds,housingcleanpred$type, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$lotsize,housingcleanpred$type, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$soldprice,housingcleanpred$type, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))

(myfavbox(housingcleanpred$soldprice,housingcleanpred$beds, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$soldprice,housingcleanpred$baths, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$soldprice,housingcleanpred$cooling, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$soldprice,housingcleanpred$heating, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$soldprice,housingcleanpred$fireplace, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$soldprice,housingcleanpred$rating, housingcleanpred, width = 1350, color = "hotpink", word = "after the modification of NULL data"))
(myfavbox(housingcleanpred$soldprice,housingcleanpred$size, housingcleanpred, width = 1350))





#** 6.2.1.2 histogram
library("viridis")
(myhistogram(housingcleanpred$sqft, var = housingcleanpred, word = "after the modification of NULL data"))


#** 6.2.1.3 bar
library("viridis")
Elementary_after <- aggregate(soldprice~elementary, data = housingcleanpred, FUN = mean)
Elementary_after  <- data.frame(Elementary_after)
Elementary_after_order <- Elementary_after[order(Elementary_after$soldprice, decreasing = F),]

png(file= "Image/Sold price by different elementarys after the modification of NULL data.png", width = 1350, height = 950)
barplot(Elementary_after_order$soldprice/1000, names.arg = Elementary_after_order$elementary, col  = rainbow(50), 
        main = "Sold price by different elementarys", horiz = T, xlab = "Soldprice(thousand $)", ylab = "Elementary", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
dev.off()

png(file= "Image/Sold price by different elementarys_head after the modification of NULL data.png", width = 1350, height = 950)
barplot(head(Elementary_after_order$soldprice)/1000, names.arg = head(Elementary_after_order$elementary), col  = rainbow(50), 
        main = "Sold price by different elementarys", horiz = T, xlab = "Soldprice(thousand $)", ylab = "Elementary", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
dev.off()

png(file= "Image/Sold price by different elementarys_tail after the modification of NULL data.png", width = 1350, height = 950)
barplot(tail(Elementary_after_order$soldprice)/1000, names.arg = tail(Elementary_after_order$elementary), col  = rainbow(50), 
        main = "Sold price by different elementarys", horiz = T, xlab = "Soldprice(thousand $)", ylab = "Elementary", cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
dev.off()



#** 6.2.2.0 Multi-variable visuals
#** 6.2.2.1 Histograms: Sold price in different types of houses after the modification of NULL data
hist(housingcleanpred$soldprice[housingcleanpred$type == 'condo'], breaks = seq(0, 2400000, 200000),
     main = "Sold price in different types of houses after the modification of NULL data",
     xlab = "money($)", ylab = "count(#)", col = rgb(0,0,1,0.25), freq = T, ylim = c(0,100), cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanpred$soldprice[housingcleanpred$type == 'condominium'], breaks = seq(0, 2400000, 200000),
     col = rgb(0.7,0,0,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanpred$soldprice[housingcleanpred$type == 'multi-family home'], breaks = seq(0, 2400000, 200000),
     col = rgb(1,0,0,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanpred$soldprice[housingcleanpred$type == 'single-family home'], breaks = seq(0, 2400000, 200000),
     col = rgb(1,1,0,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
hist(housingcleanpred$soldprice[housingcleanpred$type == 'town house'], breaks = seq(0, 2400000, 200000),
     col = rgb(0,1,1,0.25), freq = T, add = T, cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5)
histword <- c("condo", 'condominium', 'multi-family home', 'single-family home', 'town house')
histcolor <- c(rgb(0,0,1,0.25), rgb(1,0,0,0.25), rgb(1,0,0,0.25), rgb(1,1,0,0.25), rgb(0,1,1,0.25))
legend(100000,100, histword, col = histcolor, lwd = 10, cex= 1 )


#** 6.2.2.2 Density: Sold price density in different types of houses after the modification of NULL data
myhistdensity(housingcleanpred$soldprice[housingcleanpred$type == 'condo'],
              housingcleanpred$soldprice[housingcleanpred$type == 'condominium'],
              housingcleanpred$soldprice[housingcleanpred$type == 'multi-family home'],
              housingcleanpred$soldprice[housingcleanpred$type == 'single-family home'],
              housingcleanpred$soldprice[housingcleanpred$type == 'town house'],
              main = "Sold price density in different \ntypes of houses after the modification of NULL data",
              xlab = "Money($)",
              ylab = "Density",
              ylim = c(0,0.000007),
              position = "topright",
              topic = c("condo", 'condominium', 'multi-family home', 'single-family home', 'town house')
)



#** 6.2.2.3 scatter
#*** relevant to the soldprice
(myfavscatter(housingcleanpred$sqft, housingcleanpred$soldprice, var = housingcleanpred, color = "hotpink", word = "after the modification of NULL data"))
(myfavscatter(housingcleanpred$lotsize, housingcleanpred$soldprice, var = housingcleanpred, color = "hotpink", word = "after the modification of NULL data"))
(myfavscatter(housingcleanpred$year, housingcleanpred$soldprice, var = housingcleanpred, color = "hotpink", word = "after the modification of NULL data"))


(myfavscatter(housingcleanpred$beds, housingcleanpred$soldprice, var = housingcleanpred, color = 'blue'))
(myfavscatter(housingcleanpred$baths, housingcleanpred$soldprice, var = housingcleanpred, color = 'blue'))
(myfavscatter(housingcleanpred$size, housingcleanpred$soldprice, var = housingcleanpred, color = 'blue'))
(myfavscatter(housingcleanpred$rating, housingcleanpred$soldprice, var = housingcleanpred, color = 'blue'))
(myfavscatter(housingcleanpred$levels, housingcleanpred$soldprice, var = housingcleanpred, color = 'blue'))



#*** 6.3 correlation, linear regression, clusters
#*** 6.3.1 correlation
#* Plotting scatter plot
png(file= "Image/After_plot_key_variables_after the modification of NULL data.png", width = 900, height = 750)
plot(housingcleanpred[,c(3:7,9:12,15:17)])
dev.off()

#* change binary factor to numeric in order to do calculating
housingcleanpred$levels <- as.factor(housingcleanpred$levels )
housingcleanpred$cooling <- as.factor(housingcleanpred$cooling )
housingcleanpred$heating <- as.factor(housingcleanpred$heating )
housingcleanpred$fireplace <- as.factor(housingcleanpred$fireplace)

housingcleanpred$levels <- as.numeric(housingcleanpred$levels )
housingcleanpred$cooling <- as.numeric(housingcleanpred$cooling )
housingcleanpred$heating <- as.numeric(housingcleanpred$heating )
housingcleanpred$fireplace <- as.numeric(housingcleanpred$fireplace)


library(corrplot)
library(openxlsx)
#* Calculating correlation between key variables
correlation_after <- cor(housingcleanpred[,c(3:7,9:12,15,16,17)])






#* Plotting correlation with key variables (pie)
png(file= "Image/After_corrplot_key_variables after the modification of NULL data.png", width = 900, height = 750)
corrplot(cor(housingcleanpred[,c(3:7,9:12,15,16,17)]), method = 'pie')
dev.off()

#* Plotting correlation with key variables (number)
png(file= "Image/After_corrplot_key_variables after the modification of NULL data-2.png", width = 900, height = 750)
corrplot(cor(housingcleanpred[,c(3:7,9:12,15,16,17)]), method = "number")
dev.off()



library(GGally)
#* Plotting correlation ... using ggscatmat
png(file= "Image/After_corrplot_key_variables after the modification of NULL data-3.png", width = 900, height = 750)
ggscatmat(housingcleanpred,columns = c(3:7,9:12,15,16,17))
dev.off()


#*** 6.3.2 linear regression
m1_after <- lm (soldprice~beds + baths + sqft + lotsize + year + levels + cooling + heating + fireplace + size + rating, data = housingcleanpred)
summary(m1_after)
par(mfrow = c(2,2))
plot(m1_after)
par(mfrow = c(1,1))

#**  Calculating linear regression between soldprice^0.5 with other numeric variables
m2_after <- lm (soldprice^0.5~beds + baths + sqft + lotsize + year + levels + cooling + heating + fireplace + size + rating, data = housingcleanpred)
summary(m2_after)
par(mfrow = c(2,2))
plot(m2_after)
par(mfrow = c(1,1))


#** However, if I put remove year only, and re-run m1, the "cooling" and "heating" will have a low P-value,
#** which means the cooling + heating will cause multilinearity.
m21_after <- lm (soldprice~beds + baths + sqft + lotsize + levels + cooling + heating + fireplace + size + rating, data = housingcleanpred)
summary(m21_after)
par(mfrow = c(2,2))
plot(m21_after)
par(mfrow = c(1,1))




#**  Removing P-value >0.05 item, re-calculating linear regression with soldprice
m3_after <- lm (soldprice~beds + year + size + rating, data = housingcleanpred)
summary(m3_after)
par(mfrow = c(2,2))
plot(m3_after)
par(mfrow = c(1,1))

#**  Removing P-value >0.05 item, re-calculating linear regression between with soldprice^0.5
m4_after<- lm (soldprice^0.5~beds + year  + size + rating, data = housingcleanpred)
summary(m4_after)
par(mfrow = c(2,2))
plot(m4_after)
par(mfrow = c(1,1))


#**  Removing P-value >0.05 item, re-calculating linear regression with soldprice
m5_after <- lm (soldprice~beds + year + size + rating + type, data = housingcleanpred)
summary(m5_after)
par(mfrow = c(2,2))
plot(m5_after)
par(mfrow = c(1,1))



#**  The same as m4, adding type into calculation
m6_after <- lm (soldprice^0.5~beds + year + size + rating + type, data = housingcleanpred)
summary(m6_after)
par(mfrow = c(2,2))
plot(m6_after)
par(mfrow = c(1,1))




#*** 6.3.3 clusters
library(cluster)

set.seed(1987)
#*** plotting a scatter plot between beds, baths, sqft, and lotsize with grouping these numeric variables and color of clusters = 3
(k3means_after<-kmeans(housingcleanpred[,3:6],3))
(myfavscatter(housingcleanpred$beds, housingcleanpred$baths, var = housingcleanpred, color = k3means_after $cluster, word = 'by cluster = 3 after the modification of NULL data'))
(myfavscatter(housingcleanpred$beds, housingcleanpred$sqft, var = housingcleanpred, color = k3means_after $cluster, word = 'by cluster = 3 after the modification of NULL data'))
(myfavscatter(housingcleanpred$baths, housingcleanpred$beds, var = housingcleanpred, color = k3means_after $cluster, word = 'by cluster = 3 after the modification of NULL data'))



#*** plotting a scatter plot between m5 variables with grouping these numeric variables and color of clusters = 3
(k3means_2_after <-kmeans(housingcleanpred[,c(3,7,15:17)],3))
(myfavscatter(housingcleanpred$soldprice, housingcleanpred$sqft, var = housingcleanpred, color = k3means_2_after $cluster, word = 'by cluster = 5 after the modification of NULL data'))
(myfavscatter(housingcleanpred$soldprice, housingcleanpred$lotsize, var = housingcleanpred, color = k3means_2_after $cluster, word = 'by cluster = 3 after the modification of NULL data'))


#*** plotting a scatter plot between m5 variables with grouping these numeric variables and color of clusters = 5
(k5means_2_after <-kmeans(housingcleanpred[,c(3,7,15:17)],5))
(myfavscatter(housingcleanpred$soldprice, housingcleanpred$sqft, var = housingcleanpred, color = k5means_2_after$cluster, word = 'by cluster = 5 after the modification of NULL data'))



library(scatterplot3d)
#*** plotting scatter 3D plot by three variables: sold price, beds, year, the color of clusters = 5
png(file= "Image/Scatter plot sold price VS. beds VS. year cluster_5 after the modification of NULL data.png ", width = 900, height = 750)
scatterplot3d(housingcleanpred$soldprice, housingcleanpred$beds, housingcleanpred$year, xlab = "soldprice", ylab = "beds", zlab = "year", color = k5means_2_after$cluster, type = "p", lwd = 2, box = F, cex.axis = 1, cex.lab= 1.5, cex.main =1.5, main = "Scatter plot soldprice/beds/year cluser = 5 after the modification of NULL data ")
dev.off()


#*** Comparing with 2D plots. 
(myfavscatter(housingcleanpred$soldprice, housingcleanpred$beds, var = housingcleanpred, color = k5means_2_after$cluster, word = 'by cluster = 5-2 after the modification of NULL data'))
(myfavscatter(housingcleanpred$beds, housingcleanpred$year, var = housingcleanpred, color = k5means_2_after$cluster, word = 'by cluster = 5-2 after the modification of NULL data'))
(myfavscatter(housingcleanpred$soldprice, housingcleanpred$year, var = housingcleanpred, color = k5means_2_after$cluster, word = 'by cluster = 5-2 after the modification of NULL data'))









#*** Comparing with the real situation : soldprice, lotsize
png(file= "Image/Scatter plot soldprice VS. lotsize by grouping type after the modification of NULL data.png", width = 900, height = 750)
plot(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'condo'], housingcleanpred$lotsize[housingcleanpred$type == 'condo'], lty = 3, lwd = 3, xlim = c(0,2400000), ylim = c(0,1.5), col = 'red', pch = 5, xlab ='Soldprice', ylab = 'Lotsize(ft^2)', main ='Soldprice VS. Lotsize by different type', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'condominium'], housingcleanpred$lotsize[housingcleanpred$type == 'condominium'], lty = 3, lwd = 3, col = 'orange', pch = 5)
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'multi-family home'], housingcleanpred$lotsize[housingcleanpred$type == 'multi-family home'], lty = 3, lwd = 3, col = 'gold1', pch = 5)
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'single-family home'], housingcleanpred$lotsize[housingcleanpred$type == 'single-family home'], lty = 3, lwd = 3, col = 'limegreen', pch = 5)
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'town house'], housingcleanpred$lotsize[housingcleanpred$type == 'town house'], lty = 3, lwd = 3, col = 'cyan4', pch = 5)
legend("topleft", c("condo", "condominium", "multi-family home", "single-family home", "town house"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =3, pch = 5)
dev.off()


#*** Comparing with the real situation : soldprice, sqft
png(file= "Image/Scatter plot soldprice VS. sqft by grouping type after the modification of NULL data.png", width = 900, height = 750)
plot(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'condo'], housingcleanpred$sqft[housingcleanpred$type == 'condo'], lty = 3, lwd = 3, xlim = c(0,2400000), ylim = c(0,6000), col = 'red', pch = 5, xlab ='Soldprice', ylab = 'Sqft(ft^2)', main ='Soldprice VS. Sqft by different type', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'condominium'], housingcleanpred$sqft[housingcleanpred$type == 'condominium'], lty = 3, lwd = 3, col = 'orange', pch = 5)
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'multi-family home'], housingcleanpred$sqft[housingcleanpred$type == 'multi-family home'], lty = 3, lwd = 3, col = 'gold1', pch = 5)
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'single-family home'], housingcleanpred$sqft[housingcleanpred$type == 'single-family home'], lty = 3, lwd = 3, col = 'limegreen', pch = 5)
lines(type = 'p', housingcleanpred$soldprice[housingcleanpred$type == 'town house'], housingcleanpred$sqft[housingcleanpred$type == 'town house'], lty = 3, lwd = 3, col = 'cyan4', pch = 5)
legend("topleft", c("condo", "condominium", "multi-family home", "single-family home", "town house"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =3, pch = 5)
dev.off()



#*** Comparing with the real situation : sqft, lotsize
png(file= "Image/Scatter plot sqft VS. lotsize by grouping type after the modification of NULL data.png", width = 900, height = 750)
plot(type = 'p', housingcleanpred$sqft[housingcleanpred$type == 'condo'], housingcleanpred$lotsize[housingcleanpred$type == 'condo'], lty = 3, lwd = 3, xlim = c(0,6000), ylim = c(0,1.4), col = 'red', pch = 5, xlab ='Soldprice', ylab = 'Sqft(ft^2)', main ='Soldprice VS. Sqft by different type', cex.axis= 1.5, cex.lab= 1.5, cex.main =1.5 )
lines(type = 'p', housingcleanpred$sqft[housingcleanpred$type == 'condominium'], housingcleanpred$lotsize[housingcleanpred$type == 'condominium'], lty = 3, lwd = 3, col = 'orange', pch = 5)
lines(type = 'p', housingcleanpred$sqft[housingcleanpred$type == 'multi-family home'], housingcleanpred$lotsize[housingcleanpred$type == 'multi-family home'], lty = 3, lwd = 3, col = 'gold1', pch = 5)
lines(type = 'p', housingcleanpred$sqft[housingcleanpred$type == 'single-family home'], housingcleanpred$lotsize[housingcleanpred$type == 'single-family home'], lty = 3, lwd = 3, col = 'limegreen', pch = 5)
lines(type = 'p', housingcleanpred$sqft[housingcleanpred$type == 'town house'], housingcleanpred$lotsize[housingcleanpred$type == 'town house'], lty = 3, lwd = 3, col = 'cyan4', pch = 5)
legend("topleft", c("condo", "condominium", "multi-family home", "single-family home", "town house"), col = c("red", "orange", "gold1", "limegreen", "cyan4"), lty = 3, lwd =3, pch = 5)
dev.off()



#*** plotting another scatter 3D plot by the three variables: beds, baths, and sqft
library(car)
library(rgl)
scatter3d(housingcleanpred$soldprice, housingcleanpred$sqft, housingcleanpred$lotsize, revolutions = 1, bg.col="black")



### 7 Saving & removing variables ----

#* Saving data into a table

#* Creating a description table
sheet_name <- c("explanation", "correlation", "model1", "model2", "model21", "model3", "model4", "model5", "model6",
                "correlation_after", "model1_after", "model2_after", "model21_after", "model3_after", "model4_after", "model5_after", "model6_after")
description <- c("sheet's name detail description",
                 "beds, baths, sqft, lotsize, soldprice, size, rating correlation",
                 "lm(formula = soldprice ~ beds + baths + sqft + lotsize + year + 
    levels + cooling + heating + fireplace + size + rating, data = housingcleanNA)", 
                 "lm(formula = soldprice^0.5 ~ beds + baths + sqft + lotsize + 
    year + levels + cooling + heating + fireplace + size + rating, 
    data = housingcleanNA)",
                 "lm(formula = soldprice ~ beds + baths + sqft + lotsize + levels + 
    cooling + heating + fireplace + size + rating, data = housingcleanNA)",
                 "lm(formula = soldprice ~ beds + year + size + rating, data = housingcleanNA)",
                 "lm(formula = soldprice^0.5 ~ beds + year + size + rating, data = housingcleanNA)",
                 "lm(formula = soldprice ~ beds + year + size + rating + type, 
    data = housingcleanNA)",
                 "lm(formula = soldprice^0.5 ~ beds + year + size + rating + type, 
    data = housingcleanNA)",
                 "Correlation after the modification of NULL data",
                 "lm(formula = soldprice ~ beds + baths + sqft + lotsize + year + 
    levels + cooling + heating + fireplace + size + rating, data = housingcleanpred)",
                 "lm(formula = soldprice^0.5 ~ beds + baths + sqft + lotsize + 
    year + levels + cooling + heating + fireplace + size + rating, 
    data = housingcleanpred)",
                 "lm(formula = soldprice ~ beds + baths + sqft + lotsize + levels + 
    cooling + heating + fireplace + size + rating, data = housingcleanpred)",
                 "lm(formula = soldprice ~ beds + year + size + rating, data = housingcleanpred)",
                 "lm(formula = soldprice^0.5 ~ beds + year + size + rating, data = housingcleanpred)",
                 "lm(formula = soldprice ~ beds + year + size + rating + type, 
    data = housingcleanpred)",
                 "lm(formula = soldprice^0.5 ~ beds + year + size + rating + type, 
    data = housingcleanpred)")

detail <- cbind(sheet_name, description)


#* Saving data
sheet <- list("explanation" = detail,
              'correlation'= round(correlation, digits = 2),
              'model1'= round(summary(m1)$coefficients, digits = 2),
              'model2'= round(summary(m2)$coefficients, digits = 2),
              'model21'= round(summary(m21)$coefficients, digits = 2),
              'model3'= round(summary(m3)$coefficients, digits = 2),
              'model4'= round(summary(m4)$coefficients, digits = 2),
              'model5'= round(summary(m5)$coefficients, digits = 2),
              'model6'= round(summary(m6)$coefficients, digits = 2),
              'correlation_after'= round(correlation_after, digits = 2),
              'model1_after' = round(summary(m1_after)$coefficients, digits = 2),
              'model2_after' = round(summary(m2_after)$coefficients, digits = 2),
              'model21_after' = round(summary(m21_after)$coefficients, digits = 2),
              'model3_after' = round(summary(m3_after)$coefficients, digits = 2),
              'model4_after' = round(summary(m4_after)$coefficients, digits = 2),
              'model5_after' = round(summary(m5_after)$coefficients, digits = 2),
              'model6_after' = round(summary(m6_after)$coefficients, digits = 2)
)

write.xlsx(sheet, file = "Final project ouput excel.xlsx",  rowNames = T, overwrite = TRUE, quote = T)

#* Removing variables
remove(detail, sheet, histcolor, histword, schools, m1_ReplaceTOmode, m2_ReplaceTOmode,
       m3_ReplaceTOmode, m4_ReplaceTOmode, description, sheet_name, nacheck, Elementary, Elementary_after, Elementary_after_order,
       Elementary_order, kmeans_2, kmeans_3, kmeans_5, k3means)

remove(housing)



### 8 Follow up ----
#* Create my favorite histogram/density chart
#* Followed by one variable creating a multi-in-one histogram/density chart
#* for example: choosing housingcleanNA$type will have a five-in-one histogram/density chart
#* because it contains "condo", "condominium", "multi-family home", "single-family home", "town house" five variables


