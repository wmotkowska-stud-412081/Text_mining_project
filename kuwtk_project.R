library(tidyverse)
library(tm)
library(SnowballC)

# Word Frequency measures the most frequently occurring words or concepts in a given text; will help to analyze the words or expressions Kardashians use most frequently

# Word association a form of analyzing the content of text data in search of relations between terms

# Keyword Extraction - the most relevant terms within a text, terms that summarize the contents of text in list form, keyword extraction can be used to index data to be searched and to generate word clouds (a visual representation of text data)

# Sentiment Analysis - automated process of understanding of the emotional intent of words to infer whether a section of text is positive, negative or neutral

text <- readLines("kuwtk_se19e02.txt")
text

docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
inspect(docs)

# We suspect "like" to be a stop word for Kardashians

setdiff("so", stopwords("english"))
setdiff("like", stopwords("english"))

docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs) %>% 
       as.matrix() %>% 
       rowSums() %>% 
       data.frame(word = names(.), freq = .) %>% 
       arrange(desc(freq)) %>% 
       filter(freq > 10)

dtm

# Kardashians mostly use their own stopping words in text
# docs <- tm_map(docs, removeWords, c("like", "yeah", "dont", "just", "know", "god")) 

