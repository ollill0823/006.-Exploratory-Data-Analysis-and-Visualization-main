
bear <- read.csv('bears.csv')


## Create a strip chart for each possible numeric variable 
## (there should be 7). Use jitter. Include the R code above 
## each graph in a Word Document.
stripchart(bear$age, method = 'jitter', main = 'Age', xlab = 'year',cex = 1.0)
stripchart(bear$headlength, method = 'jitter', main = 'Head length', xlab = 'inches',cex = 1.0)
stripchart(bear$headwidth, method = 'jitter', main = 'Head width', xlab = 'inches',cex = 1.0)
stripchart(bear$neckcircum, method = 'jitter', main = 'Neck circumference', xlab = 'inches',cex = 1.0)
stripchart(bear$length, method = 'jitter', main = 'Length', xlab = 'inches',cex = 1.0)
stripchart(bear$chest, method = 'jitter', main = 'Chest', xlab = 'inches',cex = 1.0)
stripchart(bear$weight, method = 'jitter', main = 'Weight', xlab = 'pounds',cex = 1.0)


## In the same Word Document, create a sorted dot chart of the following 
## (3 graphs) using gender as the row label. Include the R code 
## above each graph:

#### the length
length_order <- bear[order(bear$length),]

dotchart(length_order$length, labels = bear$gender, cex = 0.7, main = 
           'Length compare with gender', xlab = 'Length(inches)', 
         ylab = 'Gender', cex.main = 2, cex.lab = 1.5, pch = 19, 
         col = c('darkred', 'blue'))

#### the chest
chest_order <- bear[order(bear$chest),]
dotchart(chest_order$chest, labels = bear$gender, cex = 0.7, main = 
           'Chest compare with gender', xlab = 'Chest(inches)', 
         ylab = 'Gender', cex.main = 2, cex.lab = 1.5, pch = 19, 
         col = c('darkred', 'blue'))

#### the weight
weight_order <- bear[order(bear$weight),]
dotchart(weight_order$weight, labels = bear$gender, cex = 0.7, main = 
           'Weight compare with gender', xlab = 'Weight(pounds)', 
         ylab = 'Gender', cex.main = 2, cex.lab = 1.5, pch = 19, 
         col = c('darkred', 'blue'))


## In the same Word Document, create box plots for each of the 7 numeric
## variables separated by gender. There should be 7 pairs of 
## side-by-side boxplots giving you 14 in total. Include the R code 
## above each graph. Below each graph, thoroughly explain in complete 
## sentences what you observe in each of the box plots. Use summary() 
## to help obtain precise numbers.

par(mfrow = c(1,1))

boxplot(bear$age~gender, data = bear, col = c('deeppink1', 'deepskyblue3'), 
        main = 'Age group by gender', xlab = 'Gender', 
        ylab = 'Age(year)', cex.main = 2, cex.lab = 1.5)

boxplot(bear$headlength~gender, data = bear, col = c('deeppink1', 'deepskyblue3'), 
        main = 'Head length grouop by gender', xlab = 'Gender', 
        ylab = 'Head length(inches)', cex.main = 2, cex.lab = 1.5)

boxplot(bear$headwidth~gender, data = bear, col = c('deeppink1', 'deepskyblue3'), 
        main = 'Head width group by gender', xlab = 'Gender', 
        ylab = 'Head width(inches)', cex.main = 2, cex.lab = 1.5)

boxplot(bear$neckcircum~gender, data = bear, col = c('deeppink1', 'deepskyblue3'), 
        main = 'Neck circumference group by gender', xlab = 'Gender', 
        ylab = 'Neck circumference(inches)', cex.main = 1.8, cex.lab = 1.5)

boxplot(bear$length~gender, data = bear, col = c('deeppink1', 'deepskyblue3'), 
        main = 'Length group by gender', xlab = 'Gender', 
        ylab = 'Length(inches)', cex.main = 2, cex.lab = 1.5)

boxplot(bear$chest~gender, data = bear, col = c('deeppink1', 'deepskyblue3'), 
        main = 'Chest group by gender', xlab = 'Gender', 
        ylab = 'Chest(inches)', cex.main = 2, cex.lab = 1.5)

boxplot(bear$weight~gender, data = bear, col = c('deeppink1', 'deepskyblue3'), 
        main = 'Weight group by gender', xlab = 'Gender', 
        ylab = 'Weight(pounds)', cex.main = 2, cex.lab = 1.5)



bear$color <- ifelse (bear$gender == 'female', 'darkred', 'Blue' )

dotchart(length_order$length, labels = bear$gender, cex = 0.7, main = 
           'Length compare with gender', xlab = 'Length(inches)', 
         ylab = 'Gender', cex.main = 2, cex.lab = 1.5, pch = 19, 
         col = c(bear$color))

dotchart(chest_order$chest, labels = bear$gender, cex = 0.7, main = 
           'Chest compare with gender', xlab = 'Chest(inches)', 
         ylab = 'Gender', cex.main = 2, cex.lab = 1.5, pch = 19, 
         col = c(bear$color))


dotchart(weight_order$weight, labels = bear$gender, cex = 0.7, main = 
           'Weight compare with gender', xlab = 'Weight(pounds)', 
         ylab = 'Gender', cex.main = 2, cex.lab = 1.5, pch = 19, 
         col = c(bear$color))
