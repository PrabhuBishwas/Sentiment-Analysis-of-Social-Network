#load required libraries
library(ggplot2)
library(lubridate)
library(Scale)
library(reshape2)
#Read from chat history file
texts <- readLines("GroupChat.txt")
#load libraries to create wordcloud
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
text=texts;
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("vemparala","media"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2")) 

#sentiment analysis
#we use all packages that are used for twitter sentiment analysis
library(tm)
library(stringr)
library(syuzhet) #this library contain sentiment dictionary
library(lubridate) #provides tools that make it easier to parse and manipulate dates
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr) #dplyr provides a flexible grammar of data manipulation

#fetch sentiment words from tweets
mySentiment <- get_nrc_sentiment(texts)
head(mySentiment)
text <- cbind(texts, mySentiment)
#count the sentiment words by category
sentimentTotals <- data.frame(colSums(text[,c(2:11)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals),
                         sentimentTotals)
rownames(sentimentTotals) <- NULL
#total sentiment score of all texts
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment
Score for All Texts with Group chat") 