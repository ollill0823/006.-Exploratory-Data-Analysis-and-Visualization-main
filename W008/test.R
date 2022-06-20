library(corrplot)		## install.packages("corrplot")
library(GGally)
library(scatterplot3d)

library(car)
library(rgl)

library(sf)
library(raster)
library(dplyr)		## note, dplyr gives you "mask" warnings
## this means two librarys have the same function name
## but the functions do different things
## to distinguish, use dplyr:__<<function>>__()
library(spData)		##spData gives us us_states below; use ?spData to see other available maps
library(spDataLarge)
library(tmap)

library(cluster)
library(ggplot2)
library(readxl)
library(datasets)
library(scatterplot3d)


install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")


wk8dat<-read.csv("randomdat2.csv", header = T)

summary(wk8dat)


wk8dat$isclassb <- rep(0,300)


wk8dat$cat1
wk8dat$isclassb[wk8dat$cat1 == "Class B"] <- 1
wk8dat$isclassb[wk8dat$cat1=="Class B"] <- 1

table(wk8dat$cat1,wk8dat$isclassb)

ggscatmat(wk8dat,columns = 3:8)

plot(wk8dat[,3:8])

ggscatmat()

wk8dat[,3:8]
names(wk8dat[,3:8])


m1<- lm(isclassb~var1+var2+var3+var4+var5, data = wk8dat)
summary(m1)

m2<- lm(isclassb~var1+var2+var4+var5, data = wk8dat)
summary(m2)

mlog1 <- glm(isclassb~var1+var2+var4+var5, data = wk8dat, family = binomial())

summary(mlog1)


wk8dat$isclassa <- 0

wk8dat$isclassa[wk8dat$cat1 =="Class A"] <- 1


m3 <- lm(isclassa~var1+var2+var4+var5, data = wk8dat)
summary(m3)

mlog2 <- glm(isclassa~var1+var2+var4+var5, data = wk8dat, family = binomial())

summary(mlog2)



schdat <- read.csv("schooldata.csv", header = T)



plot(density(schdat$pretest, na.rm=T))		## approximately normal
boxplot(schdat$pretest)					## no outliers
t.test(schdat$pretest~schdat$ontimegrad)


plot(density(schdat$studytime, na.rm=T))
t.test(schdat$studytime, schdat$ontimegrad)
t.test(schdat$studytime~schdat$ontimegrad)
t.test(studytime~ontimegrad, data = schdat)


table(schdat$gender, schdat$ontimegrad)
test <-table(schdat$gender, schdat$ontimegrad)
test <- as.data.frame(test)
prop.test(x = c(38,18), n = c(50,50))


test




tm_shape(us_states) + tm_fill() 
tm_shape(us_states) + tm_borders() 
tm_shape(us_states) + tm_fill() + tm_borders()
tm_shape(us_states) + tm_polygons()
tm_shape(world) + tm_fill() 
tm_shape(world) + tm_polygons()


head(USArrests)

us_states

head(USArrests)

USArrests2 <- cbind(USArrests, NAME = row.names(USArrests))
head(USArrests2)
head(us_states)

us_states2 <- merge(us_states, USArrests2, by="NAME")


tm_shape(us_states2) + tm_fill()


tm_shape(us_states2) + tm_polygons() +
  tm_symbols(col = viridis(1), border.col = "green", size = "Rape", colorNULL = 'green')

library("viridis")
tm_shape(us_states2) + tm_polygons() + 
  tm_dots(col = viridis(2), size = 1, shape = 19)




##### From the HELP function
require(graphics)

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 3))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:3, pch = 19, cex = 2)


plot(x, col = cl$withinss)


set.seed(900)
(k3iris<-kmeans(iris[,1:4],3))


(iris2 <- cbind(iris, grouping = k3iris$cluster))
table(iris2$Species, iris2$grouping)



scatterplot3d(iris$Sepal.Length,iris$Sepal.Width, iris$Petal.Length, color = k3iris$cluster, type = 'p', lwd = 5, cex.axis = 0.5, main = 'gogo')


scatterplot3d(iris$Sepal.Length, iris$Petal.Length, iris$Petal.Width, color = k3iris$cluster, type = "h", lwd = 2, box = F, cex.axis = 0.5, main = "Thick Height Bars and No Box")




########## Clustering: Hierarchical ##########
help(hclust)


hc <- hclust(dist(USArrests), "ave")
plot(hc)
plot(hc, hang = -1)
(cut1 <- cutree(hc, k = 10))
plot(cut1)



hc <- hclust(dist(USArrests)^2, "cen")
plot(hc)
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
  
  
}

colMeans(USArrests[memb == 10, , drop = FALSE])



USArrests[memb == 1]



cent2 <- NULL
for(k in 1:10){
  cent2 <- rbind(cent2, USArrests[memb == k, , drop = FALSE])
}
cent2


colMeans(USArrests[memb == 1, , drop = FALSE])



data(votes.repub)
agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agn1
plot(agn1)

op <- par(mfrow=c(2,2))
agn2 <- agnes(daisy(votes.repub), diss = TRUE, method = "complete")
plot(agn2)
## alpha = 0.625 ==> beta = -1/4  is "recommended" by some
agnS <- agnes(votes.repub, method = "flexible", par.meth = 0.625)
plot(agnS)
par(op)
