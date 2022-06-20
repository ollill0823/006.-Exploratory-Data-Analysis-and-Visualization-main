
library(car)

options(scipen = 999)


randomdat <- read.csv("randomdat.csv", header = T)

randomdat$var8 <- as.factor(randomdat$var8)
randomdat$var9 <- as.factor(randomdat$var9)
randomdat$var10 <- as.factor(randomdat$var10)

randomdat[is.na(randomdat$var9),]

table(randomdat$var9,randomdat$var10)

table(randomdat$var8,randomdat$var9)


randomdat$var9[is.na(randomdat$var9)] <- 'With XYZ'



randomdat$var8 <- as.character(randomdat$var8)

randomdat$var8 [randomdat$var8  == 'GroupC'] <- 'Group C'


randomdat$var8 <- as.factor(randomdat$var8)
summary(randomdat)


plot(randomdat[,c(1:7,11)])
plot(randomdat[,c(1:7,11)])


randomdat$var5
summary(randomdat$var5)
plot(randomdat$var3)


randomdat <- randomdat[which(randomdat$var5<800),]
randomdat800up <- randomdat[which(randomdat$var5 >= 800),]
randomdat <- randomdat[which(randomdat$var5<800),]

plot(randomdat$var1, randomdat$var2)

m1 <- lm(var2~var1, data = randomdat)
m2 <- lm(var1~var2, data = randomdat)
abline(m1)
plot(m1)

plot(randomdat$var1, randomdat$var2)
smooth <- smooth.spline(randomdat$var1,randomdat$var2, spar=1 )
lines(smooth, col = 'red', lwd =2, lty = 2)
abline(m1)



plot(randomdat$var1,randomdat$var2)
abline(m1)
abline(m2)
smooth <- smooth.spline(randomdat$var1,randomdat$var2, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)



  
randomdat$var1sq <- randomdat$var1^2



m2 <- lm(var2~var1sq, data = randomdat)
plot(randomdat$var1sq, randomdat$var2)
abline(m2)
smooth2 <- smooth.spline(randomdat$var1sq, randomdat$var2, spar = 1)
lines(smooth2, col='blue', lwd=2, lty =2)

plot(m2)


m2_no43<- lm(var2~var1sq, data = randomdat[-c(41,43),])
plot(randomdat$var1sq, randomdat$var2)
abline(m2_no43)
abline(m2, col = "blue", lty = 2)

plot(m2_no43)


m3 <- lm (var2~var1+var1sq, data = randomdat)

summary(m3)

m3 <- lm(var2~var1+ I(var1^2), data = randomdat)




var1seq <- seq(-60, 220, 10)

m3_predict <- predict(m3, list(var1 = var1seq))
  



plot(randomdat$var1, randomdat$var2)
lines(var1seq, m3_predict, col='red', lwd = 2, lth = 2)



predict(m3, list(var1 = 0))
predict(m3, list(var1 = 100))

plot(m3)


plot(randomdat$var1, randomdat$var3)
m4_smooth <- smooth.spline(randomdat$var1, randomdat$var3, spar = 1)
lines(m4_smooth, col='red', lwd = 2, lth = 2)


m4 <- lm(var3~var1 + I(var1^2) + I(var1^3), data = randomdat)
summary(m4)
m4_predict <- predict(m4, list(var1 = var1seq))

plot(randomdat$var1, randomdat$var3)
lines(m4_smooth, col='red', lwd = 2, lth = 2)
lines(var1seq, m4_predict, col='blue', lwd = 2, lth = 2)

plot(m4)

predict(m4, list(var1=65))


plot(sqrt(randomdat$var1+50), randomdat$var4)

smooth4 <- smooth.spline(sqrt(randomdat$var1+50), randomdat$var4, spar = 1)
lines(smooth4, col = 'blue', lwd = 2, lty = 2)

randomdat$var1sqrt <- sqrt(randomdat$var1+50)

## Two models, compare no transformation to sqrt transformation
m5_orig <- lm(var4 ~ var1, data=randomdat)
m5_sqrt <- lm(var4 ~ var1sqrt, data=randomdat)


m5_predict <- predict(m5_sqrt, list(var1sqrt = sqrt(var1seq+50)))

plot(sqrt(randomdat$var1+50),randomdat$var4)
lines(sqrt(var1seq+50), m5_predict, col = "blue", lwd = 3)



##??? PRACTICE!
## Conduct some sort of transformation to see how var1 predicts
## (or relates to) var5
## a. Generate plots to help you identify the appropriate transformation
## b. Generate a regression model that shows this transformation
## c. Overlay your regression line to see the fit of your model
## d. Examine diagnostic plots
## e. Based on your model, estimate what var5 would be if var1 = 100

par(mfrow = c(1,3))
plot(randomdat$var1+50, randomdat$var5)
smooth_50 <- smooth.spline(randomdat$var1+50, randomdat$var5, spar = 1)
lines(smooth_50, col = 'red', lwd = 2, lth = 3)
plot(sqrt(randomdat$var1+50), randomdat$var5)
smooth_sq50 <- smooth.spline(sqrt(randomdat$var1+50), randomdat$var5, spar = 1)
lines(smooth_sq50, col = 'red', lwd = 2, lth = 3)
plot((randomdat$var1+50)^2, randomdat$var5)
smooth_square50 <- smooth.spline((randomdat$var1+50)^2, randomdat$var5, spar = 1) 
lines(smooth_square50, col = 'red', lwd = 2, lth = 3)

randomdat$var1_zero <- randomdat$var1 +50
randomdat$var1_sqrt <- sqrt(randomdat$var1_zero)
randomdat$var1_square <- (randomdat$var1_zero)^2
m5_50 <- lm(var5~var1_zero, data = randomdat)
m5_sq <- lm(var5~var1_sqrt, data = randomdat)
m5_square <- lm(var5~var1_square, data = randomdat)

par(mfrow = c(1,1))

par(mfrow = c(1,3))
plot(m5_50)
plot(m5_sq)
plot(m5_square)




m5_50_predict <- predict(m5_50, list(var1_zero = var1seq))

plot(randomdat$var1+50, randomdat$var5)
lines(var1seq, m5_50_predict, col = 'red', lwd = 2, lth = 3)

predict(m5_50, list(var1_zero=150))


m9_lm <-lm(var11~var1+var2+var3+var4+var5+var6+var7+var8+var9+var10, data = randomdat)
step(m9_lm, direction = 'both')
summary(m9_lm)


m9 <-lm(var11~var1+var2+var4+var5+var7+var10, data = randomdat)
step(m9, direction = 'backward')
summary(m9)


m9 <- lm(var7~var1*var8, data = randomdat)
summary(m9)


cor(randomdat[,c(1:7,11)])



vocab <- read.csv("vocab.csv",header=T)

plot(vocab$education, vocab$vocabulary)

sp(vocab$education, vocab$vocabulary, jitter = list(x=2, y=2))


sp(vocab$education, vocab$vocabulary, jitter = list(x=2,y=2))
sp(vocab$education, vocab$vocabulary, jitter = list(x=2,y=2))
sp(vocab$education, vocab$vocabulary)

sunflowerplot(vocab$education, vocab$vocabulary)
smoothScatter(vocab$education, vocab$vocabulary)


dat$newvar[grep("pop", dat$genre)] <- "pop"