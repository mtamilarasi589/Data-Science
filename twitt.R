install.packages("twitteR")
install.packages("ROAuth")
library("NLP")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("twitteR")
library("ROAuth")
install.packages("ggplot2")
library(ggplot2)
install.packages("wordcloud")
library(wordcloud)
consumer_key <- 'qUHXAFnSkry4d5W1jyji8GHSH'
consumer_secret <- '96lJFBmn9tzePuWfdUI94fZJGV2lKrawZ6nx5jlFgnj5D8LQcs'
access_token <- '1277260122390069248-zFsBy1WLR4ZFDrzP0tLaqF9sKJNvaC'
access_secret <- 'TmqsOnLD08fY8kncYWo2MG0fODeyGCqqDlGI5Z3LvEFkj'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets_covid <- searchTwitter("#COVID-19 in India", n=10000,lang = "en")
covid_tweets <- twListToDF(tweets_covid)
View(covid_tweets)

setwd("F:/Assignment/Text Mining-20200627T023208Z-001/Text Mining/Assignment")

write.csv(tech_tweets, "Covid19.csv",row.names = F)

getwd()

tech <- read.csv(file.choose())
str(tech)

corpus <- tech$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus_clean<-tm_map(corpus,stripWhitespace)
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
cleanset<-tm_map(cleanset,removeWords, c("rt","india","covid","cases"))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

cleanset <- tm_map(cleanset,stripWhitespace)

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
freq.terms <- findFreqTerms(tdm, lowfreq = 100)

w <- rowSums(tdm)  
w <- subset(w, w>= 25) 
barplot(w, las = 2, col = rainbow(50))

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.

wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  20, 
          colors = rainbow(20),
          scale = c(2,0.3),
          rot.per = 0.6)


# Sentiment Analysis 



