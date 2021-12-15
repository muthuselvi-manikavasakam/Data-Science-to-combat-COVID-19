#1. Sentiment Analysis(Positive & Negative tone) of Online News using word cloud

#Load libraries
library("wordcloud")
library("tm")
library("quanteda")
library("dplyr")
library(cluster)
library(sparcl)
library(pacman)
library(dplyr)
library(ggplot2)

#Set default word directory
rm(list = ls())
setwd("C:\\Users\\mnith\\Documents\\2nd Sem\\Major Project\\Text Analytics")

# Extract corpus downloaded from big query into data frame - Change FirstPeriodTier_Data 
#to SecondPeriodTier_Data to extract COVID-19 current stage Online new articles
news <- read.csv("FirstPeriodTier_Data.csv", header = TRUE, sep=",",
                 stringsAsFactors = FALSE, check.names=FALSE)

#Create a new column for Label which has two values positive or negative based on the Tone values
online_news <- news %>%
  mutate(Label = if_else(news$DoCTone >=0.0, 'Postive', 'Negative'))

#Create separate Dataframe for Ireland, Italy, South Korea & US
Ireland_news <- filter(online_news, online_news$Countrycode == "EI")
Italy_news <- filter(online_news, online_news$Countrycode == "IT")
SouthKorea_news <- filter(online_news, online_news$Countrycode == "KS")
US_news <- filter(online_news, online_news$Countrycode == "US")

#Separate online positve & negative news - Change Ireland news to 
#Italy_news, SouthKorea_news & US_news to get respective country sentiment analysis
neg_words <- data.frame(sentence = filter(Ireland_news, Ireland_news$Label == "Negative"), 
                        stringsAsFactors = FALSE)
neg_words['sentiment'] <- "neg"
View(neg_words)


pos_words <- data.frame(sentence = filter(Ireland_news, Ireland_news$Label == "Postive"), 
                        stringsAsFactors = FALSE)
pos_words['sentiment'] <- "pos"
View(pos_words)


#creation of corpus and joining them
CorpusMovieReviews <- corpus(rbind(neg_words, pos_words), text_field = 'sentence.ContextualText')
summary(CorpusMovieReviews)
View(CorpusMovieReviews)

#removing the english stop words
CorpusMovieReviews <- tokens(CorpusMovieReviews, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE)
CorpusMovieReviews <- tokens_select(CorpusMovieReviews, pattern = stopwords('en'), selection = 'remove')

#creation of dfm
dfmat_CorpusMovieReviews <- dfm(CorpusMovieReviews)

#a frequency plot for top 15 words'
ggplot(textstat_frequency(dfmat_CorpusMovieReviews, n = 15),
       aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# A word-cloud of 50 most common words
set.seed(1234)
textplot_wordcloud(dfmat_CorpusMovieReviews,max_words = 50)

### Grouped word cloud
names(docvars(CorpusMovieReviews))
docvars(CorpusMovieReviews, "sentiment") <- 
  factor(ifelse(docvars(CorpusMovieReviews, "sentiment") == "neg",
                "negative words",
                "positive words"))

#plot positive & negative wordcloud - min_count varies for the 4 countries
#Ireland - 1000, Italy - 10000, South Korea - 4000 & US - 40000
dfmat_corp_senti <- dfm(CorpusMovieReviews, 
                        groups = "sentiment")
textplot_wordcloud(dfmat_corp_senti,min_size = 1,
                   max_size = 4, min_count = 40000, max_words = 80, 
                   comparison = TRUE, color = c('red', 'blue'), random.order = F)

#2. Sentiment Analysis(emotion) of Online News using Syuzhet package
#loading libraries
library('stringr')
library('readr')
library('tm')
library('SnowballC')
library('RWeka')
library('RSentiment')
library(DT)
library(dplyr)
library(ggplot2) # Data visualization
library(syuzhet)

#Read country specific news into data_1 - Change Ireland_news to 
#Italy_news, SouthKorea_news & US_news to get respective country sentiment analysis
library(tidyverse)
data_1 <- Ireland_news %>% 
  select(ContextualText)
head(data_1)

#Using the 'syuzhet' package
#Convert news into text 
text = as.character(data_1$ContextualText)
#remove punctuations
some_txt<-gsub("[[:punct:]]"," ",text)
#let's remove number (alphanumeric)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)

#visual
mysentiment<-get_nrc_sentiment((some_txt))

# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust",
           "Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red",
            "green","orange","blue","green","red")
yRange <- range(0,yAxis) + 1000
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional valence", ylab = "Score", 
        main = "Ireland Online News sentiment", sub = "Initial Stage", col = colors, 
        border = "black", ylim = yRange, xpd = F, axisnames = T, 
        cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")
colSums(mysentiment)

