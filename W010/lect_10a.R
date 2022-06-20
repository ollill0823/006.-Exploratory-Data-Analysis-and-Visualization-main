#################################################
## Exploratory Data Analysis and Visualization
## Summary: Lecture 10
##
## Agenda:
## 1. Excel
## 2. Tableau
## 3. R-Programming: Functions
## 4. Word Clouds
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


########## R-Programming: Functions ##########
myfun <- function(x){
	y <- x^3 + 2*x^2 + 3*x + 4
	y
}

myfun(2)		## x = 2 and calculate x^3 + 2*x^2 + 3*x + 4
myfun(5)


sumsq <- function(x, y){
	sol <- x^2 + y^2
}

sumsq2 <- function(x, y){
	sol <- x^2 + y^2
	sol		## this will give a printout
}

## compare	
sumsq(3,4)
sumsq2(3,4)


num <- sumsq(3,4)




myfavgraph <- function(x, y, xtitle = "HELLO", ytitle = "BYE"){
	plot(x, y, xlab = xtitle, ylab = ytitle, col = "red", pch = 21)
}

## If nothing specified, the arguments will go in order
## x is 1:10
## y is 21:30
## xtitle is "Random X"
## ytitle is "Some Y"
myfavgraph(1:10, 21:30, "Random X", "Some Y")


myfavgraph(1:10, 21:30, ytitle = "Random Y", xtitle = "Some X")

myfavgraph(3:22, 2*(4:23))		## no xtitle or ytitle specifications, so it will go with the default




myhistgraph<- function(x, y=NULL, z = NULL, title ="", cex.axis = 1,xlab = "", ylim = NULL){
	xmin <- min(x, y, z)
	xmax <- max(x, y, z)
	hist(x, main = title, breaks = seq(xmin,xmax,(xmax-xmin)/20), cex.axis = cex.axis, freq = F, xlab = xlab, col = rgb(1,0,0,0.25), xlim = c(xmin, xmax), ylim = ylim)
	dens1 <- density(x)
	lines(dens1 , lwd = 2)
	if(!is.null(y)){
		hist(y, breaks = seq(xmin,xmax,(xmax-xmin)/20), freq = F, add = T, col = rgb(0,0,1,0.25))
		dens2 <- density(y)
		lines(dens2 , lwd = 2, lty = 2, col = rgb(0,0,1,0.25))
	}
	if(!is.null(z)){
		hist(z, breaks = seq(xmin,xmax,(xmax-xmin)/20), freq = F, add = T, col = rgb(0,1,0,0.25))
		dens3 <- density(z)
		lines(dens3 , lwd = 2, lty = 3, col = rgb(0,1,0,0.25))
	}
}

## Now I don't need to keep retyping hist.... dens... lines..., I can just use myhistgraph()

myhistgraph(iris$Sepal.Length[iris$Species == "setosa"], 
	iris$Sepal.Length[iris$Species == "virginica"], 
	iris$Sepal.Length[iris$Species == "versicolor"])
myhistgraph(iris$Sepal.Width[iris$Species == "setosa"], 
	iris$Sepal.Width[iris$Species == "virginica"], 
	iris$Sepal.Width[iris$Species == "versicolor"], ylim = c(0,2))
myhistgraph(iris$Sepal.Length[iris$Species == "setosa"], 
	iris$Sepal.Length[iris$Species == "virginica"])
myhistgraph(iris$Sepal.Length[iris$Species == "setosa"], 
	z = iris$Sepal.Length[iris$Species == "virginica"])


poverty<-read_excel("poverty.xls", sheet = 1)
poverty<-as.data.frame(poverty)
poverty$gnp<-as.numeric(poverty$gnp)
myhistgraph(poverty$birthrt[poverty$region == 1], poverty$birthrt[poverty$region == 2], poverty$birthrt[poverty$region == 3])
legend("topright", c("Region 1", "Region 2", "Region 3"), 
	col=c(rgb(1,0,0,0.25), rgb(0,0,1,0.25), rgb(0,1,0,0.25)), lwd=10)





########## R-Programming: Another Handy Trick for Repeated Analyses ##########
## Run any personal functions/variables first


data <- read_excel("poverty.xls", sheet = 1)		##edit this line
data <- as.data.frame(data)
var1 <- data$lexpf
var2 <- data$lexpm
var3 <- data$birthrt
rnames <- c("Female LE", "Male LE")

jpeg('var1.jpg')
myhistgraph(var1)
dev.off()

jpeg('var2.jpg')
myhistgraph(var2)
dev.off()

jpeg('var3.jpg')
myhistgraph(var3)
dev.off()

model1 <- lm(var3~var1+var2, data=data)
write.table(cbind(c("Intercept",rnames),round(summary(model1)$coefficients, digits = 2)), sep = "\t", file = "model1.txt")
write.csv(cbind(c("Intercept",rnames),round(summary(model1)$coefficients, digits = 2)), file = "model1.csv", row.names=F, quote = T)





########## Word Clouds ##########
hmtxt<- readLines("GormanA_TheHillWeClimb.txt")
hmcorpus <- Corpus(VectorSource(hmtxt))
inspect(hmcorpus)

##### gsub(<<chr to replace>>, <<chr to replace with>>, <<text vector>>)
##### NOTE, you can use tm_map() below instead of gsub()
hmtxt<-gsub(",", "", hmtxt)
hmtxt<-gsub("-", " ", hmtxt)
hmtxt<-gsub("'", " ", hmtxt)
hmtxt<-gsub("\\.", "", hmtxt)	## Note, we'll see an error here if we use "." .?)( is R's builded function, so \\. can escape the problem)
hmtxt<-gsub("\\?", "", hmtxt)		## \\ is the escape character in R
hmtxt<-gsub("\\)", "", hmtxt)
hmtxt<-gsub("\\(", "", hmtxt)

newhmcorpus<-Corpus(VectorSource(hmtxt))
inspect(newhmcorpus)

##### In R, "somehow" and "Somehow" are not the same (capitalization matters)
##### Two ways to change everything to lower-casing
## Option 1 - We're working with the text vector
hmtxt2<-tolower(hmtxt)					## go back to original text and change everything to lower
newhmcorpus2<-Corpus(VectorSource(hmtxt2))	## create a new corpus
inspect(newhmcorpus2)



## Option 2 - We're working with the corpus

## START FROM THE TOP
hmtxt<- readLines("GormanA_TheHillWeClimb.txt")
newhmcorpus<-Corpus(VectorSource(hmtxt))
newhmcorpus <- tm_map(newhmcorpus, tolower)	## change to lowercase in the corpus directly
inspect(newhmcorpus)


## The general format of tm_map is tm_map(__<<corpus>>__, __<<function to apply>>__)
## Let's continue off of option 2 to make more minor modifications
## Note, you will get warnings, but that's okay

newhmcorpus<- tm_map(newhmcorpus, removeNumbers)
newhmcorpus<- tm_map(newhmcorpus, removeWords, c("a","it"))
newhmcorpus<- tm_map(newhmcorpus, removeWords, stopwords("english"))
newhmcorpus<- tm_map(newhmcorpus, removePunctuation)
newhmcorpus<- tm_map(newhmcorpus, stripWhitespace)
inspect(newhmcorpus)


## Matrix of words, how many words are in each line
termdocmat <- TermDocumentMatrix(newhmcorpus)
wordmat <- as.matrix(termdocmat)

## Tally up the lines
wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
## Define it's own names
worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)
head(worddat)

## rowSums(x) calculate the overall by rows
## rowMeans(x) calculate the overall by rows
## colSums(x) calculate the mean by rows
## colMeans(x) calculate the mean by rows


set.seed(1114)		## R works with pseudo-random numbers
## rot.per means how many percentage of the words are vertical or horizontal
wordcloud(words = worddat$word, freq = worddat$freq, min.freq = 2,
          max.words=100, random.order=FALSE, rot.per=0.8, 
          colors=brewer.pal(8, "Accent"))
wordcloud(words = worddat$word, freq = worddat$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(8, "Accent"))
wordcloud(words = worddat$word, freq = worddat$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Accent"))




## remove all variables
remove(hmcorpus, hmtxt, hmtxt2, myfun, newhmcorpus,newhmcorpus2, termdocmat, worddat)
remove(wordmat,wordmat_tally)


########## Word Cloud Function ##########
## If you know you're going to use the same "set" of code, turn it into a function
## You'll need to load library packages beforehand (we already did this above)
mywc <- function(file){
	txt<- readLines(file)

	## removing -, (, and ) from text vector
	txt<-gsub("-", " ", txt)
	txt<-gsub("\\)", "", txt)
	txt<-gsub("\\(", "", txt)

	corpus<-Corpus(VectorSource(txt))
	
	## cleaning corpus
	corpus<- tm_map(corpus, tolower)
	corpus<- tm_map(corpus, removeNumbers)
	corpus<- tm_map(corpus, removeWords, stopwords("english"))
	corpus<- tm_map(corpus, removePunctuation)
	corpus<- tm_map(corpus, stripWhitespace)

	## create word matrix
	termdocmat <- TermDocumentMatrix(corpus)
	wordmat <- as.matrix(termdocmat)

	## Tally up the lines
	wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
	worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)

	wordcloud(words = worddat$word, freq = worddat$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Accent"))
}


mywc("GormanA_TheHillWeClimb.txt")


## Example 2
## by default, my_colors uses this palette: brewer.pal(8, "Accent")
mywc2 <- function(file, my_colors = brewer.pal(8, "Accent")){
	txt<- readLines(file)

	## removing -, (, and ) from text vector
	txt<-gsub("-", " ", txt)
	txt<-gsub("\\)", "", txt)
	txt<-gsub("\\(", "", txt)

	corpus<-Corpus(VectorSource(txt))
	
	## cleaning corpus
	corpus<- tm_map(corpus, tolower)
	corpus<- tm_map(corpus, removeNumbers)
	corpus<- tm_map(corpus, removeWords, stopwords("english"))
	corpus<- tm_map(corpus, removePunctuation)
	corpus<- tm_map(corpus, stripWhitespace)

	## create word matrix
	termdocmat <- TermDocumentMatrix(corpus)
	wordmat <- as.matrix(termdocmat)

	## Tally up the lines
	wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
	worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)

	wordcloud(words = worddat$word, freq = worddat$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=my_colors)		### NOTE, I CHANGED COLORS TO BE MY_COLORS
}



mywc2("PoeEA_Raven.txt")
mywc2("PoeEA_Raven.txt",my_colors=brewer.pal(7,"Accent"))
mywc2("PoeEA_Raven.txt",my_colors=brewer.pal(8,"Greens"))


## Example 3
## by default, my_colors uses this palette: brewer.pal(8, "Accent")
mywc3 <- function(file, my_colors = brewer.pal(8, "Accent"), myminfreq = 1, mymaxwords = 100, myrot = 0.5){
	txt<- readLines(file)

	## removing -, (, and ) from text vector
	txt<-gsub("-", " ", txt)
	txt<-gsub("\\)", "", txt)
	txt<-gsub("\\(", "", txt)

	corpus<-Corpus(VectorSource(txt))
	
	## cleaning corpus
	corpus<- tm_map(corpus, tolower)
	corpus<- tm_map(corpus, removeNumbers)
	corpus<- tm_map(corpus, removeWords, stopwords("english"))
	corpus<- tm_map(corpus, removePunctuation)
	corpus<- tm_map(corpus, stripWhitespace)

	## create word matrix
	termdocmat <- TermDocumentMatrix(corpus)
	wordmat <- as.matrix(termdocmat)

	## Tally up the lines
	wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
	worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)

	wordcloud(words = worddat$word, freq = worddat$freq, min.freq = myminfreq,
          max.words = mymaxwords, random.order=FALSE, rot.per = myrot, 
          colors=my_colors)		### NOTE, I CHANGED MIN.FREQ. MAX.WORDS, ROT.PER, AS WELL
}

mywc3("PoeEA_Raven.txt")
mywc3("PoeEA_Raven.txt",mymaxwords = 160)
mywc3("PoeEA_Raven.txt", my_colors=brewer.pal(8,"Greens"), myminfreq = 2)
mywc3("PoeEA_Raven.txt", my_colors=brewer.pal(8,"Greens"), myminfreq = 2, myrot = 0)

mywc3("GormanA_TheHillWeClimb.txt", mymaxwords = 120)