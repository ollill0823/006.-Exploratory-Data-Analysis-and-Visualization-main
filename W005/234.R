realestate <- read.csv("real_estate.csv", header=T)
realestate <- realestate[,c(2:8)]
colnames(realestate) <- c("X1", "X2", "X3", "X4", "X5", "X6", "Y")


model1 <- lm(formula = Y ~ X2 + X3 + X4 + X1, data = realestate)

plot(realestate[,c(1:7)])


summary(model1)
plot(model1)

library(car)
with(ToothGrowth, qqPlot(len))
ToothGrowth

qqplot(len)

with(model1, qqPlot(Y))
