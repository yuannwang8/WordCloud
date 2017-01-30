# Built on R v3.2.3
# Date: January 2016
# Motivation - this is an early first attempt at building a word cloud

# set working folder to folder containing your text corpus
setwd("~/your folder/your subfolder")

# libraries used
library(tm)
library(wordcloud)
library(SnowballC)

# read the data
cFile <-readLines("Text.txt")

# Text cleaning
# The following removes empty lines, then cleans text of CP1252 punctuation.
# Reference http://www.acc.umu.se/~saasha/charsets/

# remove zero length lines
cFile2 <- cFile[sapply(cFile, nchar)>0]
# replace left double quotation mark 342\200\234 with nothing
cFile2<-gsub("\342\200\234","",cFile2)
# replace right double quotation mark 342\200\235 with nothing
cFile2<-gsub("\342\200\235","",cFile2)
# replace left single quotation mark 342\200\230 with a single '
cFile2<-gsub("\342\200\230","'",cFile2)
# replace right single quotation mark 342\200\231 with a single '
cFile2<-gsub("\342\200\231","'",cFile2)
# replace ellipsis 342\200\246 with nothing
cFile2<-gsub("\342\200\246","",cFile2)

# remove all punctuation except apostrophe
cFile2<-gsub("[^[:alnum:][:space:]']", "",cFile2)
# check header
# head(cFile2[1:2])

# now let's create a Corpus
a <- Corpus(VectorSource(cFile2))
a <- tm_map(a, tolower)
# stopwords include 174 of the most common English words including contractions
a <- tm_map(a, removeWords, stopwords("english")) 
# this is a manual workaround to remove the most common words found in corpus: see v in row 62
# a <- tm_map(a, removeWords, c("said", "can", "get")) 
# remove stems. the unstemmed version has fewer words!
# a <- tm_map(a, stemDocument) 
a <- tm_map(a, removePunctuation) # to be doubly sure
a <- tm_map(a, stripWhitespace, lazy=TRUE)
a <- tm_map(a, PlainTextDocument)

## here's a simple way of generating a word cloud
wordcloud(a, max.words = 30, random.order = FALSE)

## Here's a prettier word cloud, with more control over output
# create relationship matrix
myDTM <- TermDocumentMatrix(a)
m <- as.matrix(myDTM)
v <- sort(rowSums(m), decreasing = TRUE)
# check v stats
head(v) #see the top words
length(v) #no. of words
# write.csv(as.data.frame(v), file="v.csv", row.names=TRUE)

# make pretty word cloud
library(ggplot2)
library(RColorBrewer)

# set the palette and number of colours to use
palcol <- brewer.pal(7, "Dark2") 
# create new file. remember to change the filename below for different saves
png("wordcloud1.png", width=750, height=700)
set.seed(3)
par(bg="white")
wordcloud(names(v), v, random.order=FALSE, random.color=FALSE, scale=c(8, 0.7), 
          # here we define how many words to include in the cloud: here all words appearing 50 times or more are used
          min.freq=50, max.words=Inf, 
          rot.per=0.05, colors=palcol)
title(main ="My Word Cloud", col.sub="gray")
dev.off()
