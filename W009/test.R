
#################################################
library(cluster)
library(ggplot2)
library(readxl)
library(datasets)
library(scatterplot3d)


########## Missing Data ##########
grades<-read_excel("MissingDataPractice.xlsx", sheet = "TestGrades")
grades<-as.data.frame(grades)
summary(grades)
grades$Gender <- as.factor(grades$Gender)


(natest <- is.na(grades$Test))
table <- table(grades$Gender, natest)
prop.table(table, 2)

table2 <- table(natest)


plot(density(grades$Physics1, na.rm = T))
boxplot(grades$Physics1)
t.test(grades$Physics1, natest)
