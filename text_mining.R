##**********Steps to Set up authorization to connect and extract tweets********
### Setting Working Directory


library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(RCurl)
library(syuzhet)
library(tm)

# OAuth 
oauth_endpoint(authorize = "https://api.twitter.com/oauth",
               access = "https://api.twitter.com/oauth/access_token")

#connect to API
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

### Twitter Application
consumerKey="" #Replace with your consumerKey
consumerSecret="" #Replace with your consumerSecret
accesstoken="" #Replace with your accesstoken
accesssecret="" #Replace with your accesssecret

setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token =accesstoken, access_secret = accesssecret )

##****************Step 3: Find trending topics************************************

# Return data frame with name, country & woeid
# woeid - Where On Earth IDentifier - 32-bit unique identifier

trend_locations <- availableTrendLocations()

# Get Country

trend_country = subset(trend_locations, country == "India")

# Get City's woeid

city_woeid = subset(trend_country, name == "Toronto")$woeid

country_woeid = trend_country$woeid

# Get trending topics in city

trends = getTrends(woeid=2295381)

View(trends)


##****************Step 4: Perform tweets extraction and data cleaning****************

# Harvest some tweets

some_tweets = searchTwitter("#Bangalore", n=1000, lang= "en")

# Explore Tweets

length.some_tweets <- length(some_tweets)
length.some_tweets

some_tweets.df <- ldply(some_tweets, function(t) t$toDataFrame())
write.csv(some_tweets.df, "/tmp/tweets.csv")

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())

# Cleaning 1-  remove people name, RT text etc. 

some_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)

# Cleaning 2- remove html links
some_txt2 = gsub("http[^[:blank:]]+", "", some_txt1)

# Cleaning 3- remove people names

some_txt3 = gsub("@\\w+", "", some_txt2)

# Cleaning 4- remove Punctuations 

some_txt4 = gsub("[[:punct:]]", " ", some_txt3)

# Cleaning 5- remove alphanumeric words

some_txt5 = gsub("[^[:alnum:]]", " ", some_txt4)

# Exporting to Excel

write.csv(some_txt5, "/tmp/tweets1.csv")

# Creating wordcorpus and cleaning

some_txt6 <- Corpus(VectorSource(some_txt5))
#some_txt6 <- tm_map(some_txt6, removePunctuation)
some_txt6 <- tm_map(some_txt6, content_transformer(tolower))
some_txt6 <- tm_map(some_txt6, removeWords, stopwords("english"))
some_txt6 <- tm_map(some_txt6, stripWhitespace)

# Building wordcloud

pal <- brewer.pal(8,"Dark2")

wordcloud(some_txt6, min.freq = 15,  max.words = 200, width=1000, height =1000,  random.order = FALSE, color=pal)

# Sentiment Analysis

# how the function works

mysentiment <- get_nrc_sentiment(some_txt5)
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")

