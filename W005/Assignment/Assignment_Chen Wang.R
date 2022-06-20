turbine <- read.csv("wind_turbines.csv")

plot(turbine[,c(5:10)])

m1 <- lm(Project.Capacity~ Turbine.Total_Height+ Turbine.Swept_Area + Turbine.Rotor_Diameter+ Turbine.Hub_Height,
         data = turbine )

step(m1, direction = "backward")

plot(m1)

plot()

plot(turbine[,c(1:2,6:9)])

par(mfrow = c(1,3))
plot(turbine$Turbine.Hub_Height,turbine$Turbine.Swept_Area)
plot(sqrt(turbine$Turbine.Hub_Height),turbine$Turbine.Swept_Area)
plot((turbine$Turbine.Hub_Height)^2,turbine$Turbine.Swept_Area)

plot(turbine$Turbine.Hub_Height,turbine$Turbine.Swept_Area)
plot(turbine$Turbine.Hub_Height,sqrt(turbine$Turbine.Swept_Area))
plot(turbine$Turbine.Hub_Height,(turbine$Turbine.Swept_Area)^2)



plot(turbine$Turbine.Total_Height, turbine$Turbine.Hub_Height)

m1 <- lm(Turbine.Hub_Height~Turbine.Total_Height, data = turbine)
plot(m1)
summary(m1)
plot(turbine$Turbine.Total_Height, turbine$Turbine.Hub_Height)
abline(m1, col = 'blue', lwd = 2, lty =4)
smooth <- smooth.spline(turbine$Turbine.Total_Height, turbine$Turbine.Hub_Height, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)
legend (50,120, c('linear regression line', 'smooth line'), text.col = c('blue', 'red'), col = c('blue', 'red'), 
        lwd = 2, lty = c(4,2))
text(150,40, "Turbine.Hub_Height = 14.269581 + \n0.517067 * Turbine.Total_Height")

xaxisseq <- seq(0, 150, 10)
plot(sqrt(turbine$Turbine.Swept_Area), turbine$Turbine.Hub_Height)
m2_predict <- predict(m2,list(Turbine.Swept_Area=(xaxisseq)^2))
lines(xaxisseq, m2_predict, col = "blue", lwd = 3)



par(mfrow = c(1,2))
plot(turbine$Turbine.Hub_Height,turbine$Turbine.Swept_Area)
plot(turbine$Turbine.Hub_Height,sqrt(turbine$Turbine.Swept_Area))


par(mfrow = c(1,1))

par(mfrow = c(2,2))
m2 <- lm(Turbine.Hub_Height~sqrt(Turbine.Swept_Area), data = turbine)
plot(m2)
summary(m2)
xaxisseq <- seq(0, 150, 10)
plot(sqrt(turbine$Turbine.Swept_Area), turbine$Turbine.Hub_Height)
smooth2 <- smooth.spline(sqrt(turbine$Turbine.Swept_Area), turbine$Turbine.Hub_Height, spar=1)

m2_predict <- predict(m2,list(Turbine.Swept_Area=(xaxisseq)^2))
lines(xaxisseq, m2_predict, col='blue', lwd = 2, lty = 2)

legend (10,120, c('linear regression line'), text.col = c('blue'), col = c('blue'), 
        lwd = 2, lty = c(4,2))
text(80,40, "Turbine.Hub_Height = 40.036397 + \n0.479432*sqrt(Turbine.Swept_Area)")






par(mfrow = c(1,1))

m3 <- lm(Turbine.Hub_Height~Turbine.Swept_Area , data = turbine)
plot(m3)
summary(m3)
plot(turbine$Turbine.Swept_Area, turbine$Turbine.Hub_Height)
smooth3 <- smooth.spline(turbine$Turbine.Swept_Area, turbine$Turbine.Hub_Height, spar=1)
lines(smooth3, col='red', lwd = 2, lty = 2)

xaxisseq2 <- seq(0, 18000, 1000)
plot(turbine$Turbine.Swept_Area, turbine$Turbine.Hub_Height)
m3_predict <- predict(m3,list(Turbine.Swept_Area=xaxisseq2))
lines(xaxisseq2, m3_predict, col = "blue", lwd = 3)


predict(m3, list(Turbine.Swept_Area=5000))


m4 <- lm(Turbine.Hub_Height~ sqrt(Turbine.Swept_Area) + Turbine.Swept_Area + I(Turbine.Swept_Area)^2 + I(Turbine.Swept_Area)^3 , data = turbine)
plot(m4)
summary(m4)
plot(turbine$Turbine.Hub_Height, turbine$Turbine.Swept_Area)
abline(m4)


m4_5 <- lm(Turbine.Hub_Height~ sqrt(Turbine.Swept_Area) + Turbine.Swept_Area , data = turbine)
plot(m4_5)
summary(m4_5)
plot(turbine$Turbine.Hub_Height, turbine$Turbine.Swept_Area)
abline(m4_5)



plot(turbine[,c(1:2,5:8)])

m5 <- lm (Turbine.Hub_Height~Turbine.Rotor_Diameter+Turbine.Swept_Area + Turbine.Total_Height, data = turbine)
summary(m5)
plot(m5)
step(m5, direction = 'backward')



m6 <- lm (Turbine.Hub_Height~Turbine.Rotor_Diameter*Turbine.Swept_Area*Turbine.Total_Height, data = turbine)
summary(m5)
plot(m6)
step(m6, direction = 'backward')
