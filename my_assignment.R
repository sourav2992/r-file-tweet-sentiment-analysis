library(twitteR)
library(syuzhet)
#install.packages("textdata")

library(textdata)
library(tidytext)

library(ROAuth)

library(wordcloud2)

library(wordcloud)

library(plyr)

library(dplyr)

library(stringr)

library(RColorBrewer)

library(ggplot2)

library(httr)

library(wordcloud)

library(RCurl)

library(syuzhet)

library(tm)

oauth_endpoint(authorize = "https://api.twitter.com/oauth",
               
               
               
               access = "https://api.twitter.com/oauth/access_token")

reqURL <- 'https://api.twitter.com/oauth/request_token'



accessURL <- 'https://api.twitter.com/oauth/access_token'



authURL <- 'https://api.twitter.com/oauth/authorize'

### Twitter Application
consumerKey="6dkT4pxUMFGm67Fy0HbViVpQc" #Replace with your consumerKey
consumerSecret="iMdpXzIpRInFt7UeWcpPpTvmJBQvnH1Toqpe7sBxTTcgzuTJrP" #Replace with your consumerSecret
accesstoken="402562017-dO2UwlNJ6fIUeA4lGLfxbno8CmA9WRpT9fsZmUYG" #Replace with your accesstoken
accesssecret="YIEUulzpRshL9xFN9SHZdM5niFLjIBn1T33FOBjHjcKXs" #Replace with your accesssecret
setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token =accesstoken, access_secret = accesssecret)

tweets = searchTwitter("@Uber",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

write.csv(tweets_dataframe,"tweets.csv")
View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)

# Step 4 --> Data modeling
pal <- brewer.pal(8,"Accent")

wordcloud(txt, min.freq = 15,  max.words = 500, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
#install.packages("textdata")

library(textdata)
library(tidytext)
get_sentiments("bing")
get_sentiments("affinn")
get_sentiments("nrc")

View(get_sentiments("affinn"))
View(get_sentiments("bing"))
View(get_sentiments("nrc"))
dev.off()
tweets = searchTwitter("@ola",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())
View(tweets_dataframe)



tweets = searchTwitter("@Uber",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

write.csv(tweets_dataframe,"tweets.csv")
View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)

# Step 4 --> Data modeling
pal <- brewer.pal(8,"Accent")

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
#install.packages("textdata")

library(textdata)
library(tidytext)
get_sentiments("bing")
get_sentiments("affinn")
get_sentiments("nrc")

View(get_sentiments("affinn"))
View(get_sentiments("bing"))
View(get_sentiments("nrc"))
dev.off()

tweets = searchTwitter("@ola",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

#write.csv(tweets_dataframe,"tweets.csv")
#View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)

# Step 4 --> Data modeling
pal <- brewer.pal(8,"Accent")

wordcloud(txt, min.freq = 15,  max.words = 500, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
#install.packages("textdata")

library(textdata)
library(tidytext)
get_sentiments("bing")
get_sentiments("affinn")
get_sentiments("nrc")

View(get_sentiments("affinn"))
View(get_sentiments("bing"))
View(get_sentiments("nrc"))
dev.off()
tweets = searchTwitter("@ola",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())
View(tweets_dataframe)



tweets = searchTwitter("#BSNL",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

#write.csv(tweets_dataframe,"tweets.csv")
#View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)

# Step 4 --> Data modeling
pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
#install.packages("textdata")

library(textdata)
library(tidytext)
get_sentiments("bing")
get_sentiments("affinn")
get_sentiments("nrc")

View(get_sentiments("affinn"))
View(get_sentiments("bing"))
View(get_sentiments("nrc"))
dev.off()


pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
#install.packages("textdata")

library(textdata)
library(tidytext)
get_sentiments("bing")
get_sentiments("affinn")
get_sentiments("nrc")

View(get_sentiments("affinn"))
View(get_sentiments("bing"))
View(get_sentiments("nrc"))
dev.off()

##################################Zomato#######################################

tweets = searchTwitter("@Zomato",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

#write.csv(tweets_dataframe,"tweets.csv")
#View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)
# Step 4 --> Data modeling
pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
#install.packages("textdata")

library(textdata)
library(tidytext)
get_sentiments("bing")
get_sentiments("affinn")
get_sentiments("nrc")

View(get_sentiments("affinn"))
View(get_sentiments("bing"))
View(get_sentiments("nrc"))
dev.off()

pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
dev.off()

##################################yatra.com#########################33
tweets = searchTwitter("#yatra.com",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

#write.csv(tweets_dataframe,"tweets.csv")
#View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)
# Step 4 --> Data modeling
pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)
############Sentiment analysis##############

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")

pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,shape=1.5,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
dev.off()

############################## cleartrip.com#############################################33
tweets = searchTwitter("#cleartrip.com",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

#write.csv(tweets_dataframe,"tweets.csv")
#View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)
# Step 4 --> Data modeling
pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)
############Sentiment analysis##############

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")

pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,shape=1.5,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
dev.off()


############################## goibibo#############################################33
tweets = searchTwitter("@goibibo",lang= "en", n=5000)
length(tweets)
tweets_dataframe <- ldply(tweets, function(t) t$toDataFrame())

#View(tweets_dataframe)

#write.csv(tweets_dataframe,"tweets.csv")
#View(tweets_dataframe)
txt = sapply(tweets_dataframe, function(x) x$getText())

txt2 = tweets_dataframe$text
#View(txt2)

txt = sapply(tweets, function(x) x$getText())
#View(txt)
####removing text##########
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", txt)

txt = gsub("http[^[:blank:]]+", "", txt)
txt


txt = gsub("@\\w+", "", txt)
txt

txt = gsub("[[:punct:]]", " ", txt)
txt

txt = gsub("[^[:alnum:]]", " ", txt)
txt

#Create the Corpus
txt = Corpus(VectorSource(txt))
txt

#convert to lower case
txt= tm_map(txt,content_transformer(tolower))
txt

# Remove all stop words
txt = tm_map(txt, removeWords, stopwords("english"))
txt
txt[[2]]$content


txt = tm_map(txt, stripWhitespace)
# Step 4 --> Data modeling
pal <- brewer.pal(12,"RdYlBu")
#wordcloud2(demoFreq, size=1.6, color='random-dark')

wordcloud(txt, min.freq = 15,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)
############Sentiment analysis##############
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")

pal <- brewer.pal(12,"RdYlBu")
wordcloud2(txt, size=1.6, color='random-dark')
wordcloud2(txt, color = "random-dark",size = 0.8, shape = "circle", backgroundColor = "white")
wordcloud(txt, min.freq = 15,shape=1.5,  max.words = 1000, width=1000, height =1000,  random.order = FALSE, color=pal)

mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
library(syuzhet)
mysentiment = get_nrc_sentiment(as.character(txt))
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
dev.off()

