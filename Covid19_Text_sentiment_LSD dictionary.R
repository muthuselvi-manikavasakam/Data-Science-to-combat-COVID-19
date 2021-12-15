# 
# remove old vectors and datasets from environment
rm(list = ls())
setwd("C:\\Datasets\\Covid19\\")

# load "readtext" package, used to load text from different sources and formats
library(readtext)
library(quanteda)

# Extract corpus downloaded from big query into data frame
news <- read.csv("gdelt_covid_news.csv", header = TRUE, sep=",",
                       stringsAsFactors = FALSE, check.names=FALSE)
colnames(news) <- c("Text_News","Date")

# Create corpus (colection of text documents) from the previously loaded data frame
corp_news <- corpus(news, text_field='Text_News')

# count number of documents within the created corpus, using ndoc function
ndoc(corp_news)

# tokenization process to break human-redable text into machine readable components
# tokens() function will segments text in the corpus into tokens (words or sentences) by word boundaries
tk_news <- tokens(corp_news)
#head(tk_news[[1]], 50)

# visual analysis detected pontuactions, which is not object of the analysis, we will remove it
tks_news <- tokens(corp_news, remove_punct = TRUE)

# visual analysis, also detected stop words (grammatical) which are not important for the analysis
# using the function stopwords() returns a pre-defined list of function words. using "en" english vocabulary
toks_news <- tokens_remove(tks_news, pattern = stopwords('en'))

# to proceed with the word frequency analysis we need to create a document-feature matrix (DFM)
# the function dfm() is used to construct a DFM from a tokens object.
dfmat_news <- dfm(toks_news)

# use the topfeatures() function to find the most frequent features, to be used in wordclouds if needed
topfeatures(dfmat_news, 15)

# in order to show both term and document frequencies into a table, use textstat_frequency() function
help(textstat_frequency)
text_freq <- textstat_frequency(dfmat_news, n=50)
#head(text_freq, 15)

# Lexicoder Sentiment Dictionary 2015
lengths(data_dictionary_LSD2015)
head(data_dictionary_LSD2015, 10)
lengths(data_dictionary_LSD2015[1:2])

# Label the sentiment of tokens according to dictionary
toks_news_lsd <- tokens_lookup(toks_news, 
                                 dictionary =  data_dictionary_LSD2015[1:2])
#head(toks_news_lsd,5)

# Create document-feature matrix from sentiments assigned from lex dictionary 
dfmat_news_lsd <- dfm(toks_news_lsd)
head(dfmat_news_lsd)

# perform a basic level type of sentiment analysis
# classify the documents based on the count of negative vs positive words
df_dict_sent <- convert(dfmat_news_lsd, to = "data.frame")
df_dict_sent$sentiment <- ifelse(df_dict_sent$negative > df_dict_sent$positive,
                                 "negative", "positive")

#export file to be further analyzed and summarized into an excel table (visual benefits)
#write.csv(df_dict_sent, "c:/Datasets/Covid19/docs_sentiments.csv", row.names=FALSE)


################################################

df_dict_sent$key <- rownames(df_dict_sent)
news$key <- rownames(news)
merged_df1 <- inner_join(df_dict_sent,news)
merged_df <- subset(merged_df1, select = c(6,4))


# Create corpus (colection of text documents) from the previously loaded data frames
corp_southkorea <- corpus(merged_df, text_field='Text_News')

# count number of documents within the created corpus, using ndoc function
ndoc(corp_southkorea)

# show first 5 documents in the created corpus
head(docvars(corp_southkorea), 5)

# view summary results of the movie corpus, showing 2 docs out of the 10662 (neg + pos dfs)
summary(corp_southkorea,2)

# tokenization process to break human-redable text into machine readable components
# tokens() function will segments text in the corpus into tokens (words or sentences) by word boundaries
tk_southkorea <- tokens(corp_southkorea)

# head funtion to visualize output from tokens function
head(tk_southkorea[[1]], 50)

# visual analysis detected pontuactions, which is not object of the analysis, we will remove it
tks_southkorea <- tokens(corp_southkorea, remove_punct = TRUE)
head(tks_movies[[1]], 50)

# visual analysis, also detected stop words (grammatical) which are not important for the analysis
# using the function stopwords() returns a pre-defined list of function words. using "en" english vocabulary
stopwords("en")
toks_southkorea <- tokens_remove(tks_southkorea, pattern = stopwords('en'))
head(toks_southkorea[[1]], 50)

# to proceed with the word frequency analysis we need to create a document-feature matrix (DFM)
# the function dfm() is used to construct a DFM from a tokens object.
dfmat_southkorea <- dfm(toks_southkorea)

dfmat_southkorea_1 <- dfm(dfmat_southkorea, 
                          groups = "sentiment")

set.seed(123)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5,col="#1F3552", cex = 0.8, font = 2, 
     "Word Cloud of 50 most commom used words, grouped by sentiment.")
textplot_wordcloud(dfmat_southkorea_1, comparison = TRUE, max_words = 50,
                   color = c("red2", "green2"),
                   random_order = FALSE, random_color = FALSE,
                   fixed_aspect = TRUE,min_size = 1.8, max_size = 3,
                   rotation = 0.25,labelcolor = "#1F3552", labeloffset = 0,
                   labelsize = 0.8)

textplot_wordcloud(dfmat_southkorea_1, comparison = TRUE, max_words = 50,
                   color = c("red2", "green2"))
