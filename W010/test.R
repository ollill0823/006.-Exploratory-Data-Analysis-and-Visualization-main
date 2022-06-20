#################################################
library(readxl)
## install.packages("tm")  			# text mining
## install.packages("SnowballC") 		# text stemming
## install.packages("wordcloud") 		# word-cloud generator 
## install.packages("RColorBrewer") 	# colors
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


myfun <- function(x){
  y <- x^3 + 2*x^2 + 3*x + 4
  y
}

sumsq <- function(x,y){
  sol <- x^2 + y^2
  sol
}


sumsq(3,4)

myfun(2)



hmtxt <- readLines("GormanA_TheHillWeClimb.txt")
hmcorpus <- Corpus(VectorSource((hmtxt)))
inspect(hmcorpus)

hmtxt <- gsub(',','', hmtxt)




## Matrix of words, how many words are in each line
termdocmat <- TermDocumentMatrix(newhmcorpus)
wordmat <- as.matrix(termdocmat)

## Tally up the lines
wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)
head(worddat)


termdocmat <- TermDocumentMatrix((newhmcorpus))
wordmat <- as.matrix(termdocmat)


## Tally up the lines
wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)
head(worddat)



wordmat_tally <- sort(rowSums(wordmat), decreasing = T)


rowSums(wordmat)
ColSums(wordmat)

data[,1:6]

rowMeans(data[,1:5])

colMeans(data[,1:5])
summary(data[,1:5])



wordmat
row.names(wordmat)

write.table(wordmat, file = "test.txt")
write.csv(wordmat_tally , file = "test.csv")

head(wordmat)



wordmat


wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)
head(worddat)


data.frame(wordmat)
head(data.frame(wordmat_tally))

test2 <- data.frame(wordmat_tally)
head(test2)
