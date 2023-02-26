library(tidyverse)
library(tm)
library(SnowballC)
library(tidytext)
library(textdata)
library(ggplot2)
library(wordcloud)
library(reshape2)


# Show "Keeping up with the Kardashians" has 20 seasons each with many episodes.
# Our project enables to make quick conclusions on episode content if someone wants to be up-to-date but has no time to watch.

# The shows transcript can be found for example here https://subslikescript.com/series/Keeping_Up_with_the_Kardashians-1086761#google_vignette

# Word Frequency measures the most frequently occurring words or concepts in a given text; will help to analyze the words or expressions Kardashians use most frequently

kards.show.analysis <- function(name) {
    text <- readLines(name)
    text
    
    name <- str_sub(name, start = 1, end =  -5)
    
    # Cleaning the transcript
    
    docs <- Corpus(VectorSource(text))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, content_transformer(tolower))
    inspect(docs)
    
    # We suspect "like" to be a stop word for Kardashians
    
    setdiff("so", stopwords("english"))
    setdiff("like", stopwords("english"))
    
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # docs <- tm_map(docs, stemDocument)
    
    dtm <- TermDocumentMatrix(docs) %>% 
        as.matrix() %>% 
        rowSums() %>% 
        data.frame(word = names(.), freq = .) %>% 
        arrange(desc(freq)) %>% 
        filter(freq > 10)
    
    head(dtm)
    
    # Kardashians mostly use their own stopping words in text
    docs <- tm_map(docs, removeWords, c("like", "yeah", "dont", "just", "know", "god", "really", "gonna", "get", "got",      "right", "can", "cant"))
    
    dtm <- TermDocumentMatrix(docs) %>%
        as.matrix() %>%
        rowSums() %>%
        data.frame(word = names(.), freq = .) %>%
        arrange(desc(freq)) %>% 
        filter(freq > 6)
    
    dtm1 <- dtm
    
    rownames(dtm) <- NULL
    
    head(dtm, 25)
    
    # Sentiment Analysis - automated process of understanding of the emotional intent of words to infer whether a      section of text is positive, negative or neutral
    
    get_sentiments("nrc") %>% distinct(sentiment)
    
    get_sentiments("nrc") %>% 
        filter(sentiment == "joy") %>%
        inner_join(dtm)
    
    get_sentiments("nrc") %>% 
        filter(sentiment == "positive") %>%
        inner_join(dtm)
    
    get_sentiments("nrc") %>% 
        filter(sentiment == "negative") %>%
        inner_join(dtm)
    
    name
    
    # Transcript with Kardashian stopping words (like, you know)
    
    # dtm1 %>%
    #   with(wordcloud(word, freq, max.words = 25, scale = c(1, 10)))
    
    
    
    # Transcript wo Kardashian stopping words (like, you know)
    
    return(dtm)
    
    
    # Word association a form of analyzing the content of text data in search of relations between terms
    
    # Keyword Extraction - the most relevant terms within a text, terms that summarize the contents of text in list         form, keyword extraction can be used to index data to be searched and to generate word clouds (a visual         representation of text data)
    
    
}

name <- "kuwtk_se19e02.txt"
dtm <- kards.show.analysis(name)
name <- str_sub(name, start = 1, end =  -5)

sentiment <- get_sentiments("nrc") %>% 
    right_join(dtm) %>% 
    na.omit() %>% 
    group_by(sentiment) %>% 
    arrange(desc(freq)) %>% 
    ungroup()

pdf(file = paste0(name, ".pdf"),
    title = name,
    width = 15,
    height = 7)

dtm %>%
    with(wordcloud(word, freq, max.words = 25, scale = c(0.1, 3)))

ggplot(sentiment, aes(sentiment, freq, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~word, ncol = 2) +
    labs(title = "Most frequent words their usage and sentiment")

ggplot(sentiment, aes(sentiment, freq, fill = sentiment)) +
    geom_col(show.legend = FALSE)

dev.off()

# The word frequency shows that the episode include some kind of prank on one of the family members, there is some talk about coronavirus so the episode can be set in time without knowledge of airing time. We see usage of "kim", "kris" who is "mom". 

# Most of the emotions in the episode were positive anticipation and trust



# most frequent words in each sentiment 


sentiment %>%
    group_by(sentiment) %>%
    top_n(5) %>%
    ungroup() %>%
    mutate(word = reorder(word, freq)) %>%
    ggplot(aes(freq, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = NULL,
         y = NULL,
         title = "Top words contributing to each sentiment")


# Overall sentiment: (positive: +1, negative: -1, neutral: 0)


overall_sentiment <- sentiment %>% 
    mutate(sentiment = recode(sentiment, "fear" = "negative", "disgust" = "negative", "anger" = "negative","sadness"="negative", "joy"="positive","surprise"="positive","trust"="positive","anticipation"="neutral" )) %>% 
    mutate(value =
               case_when(sentiment == "negative" ~ freq*(-1), 
                         sentiment == "positive" ~ freq,
                         sentiment == "neutral" ~ 0)) %>%
    group_by(sentiment) %>% 
    summarise(sum_sentiment = sum(value),
              .groups = 'drop')%>%
    as.data.frame()

overall_sentiment

sum(overall_sentiment$sum_sentiment)

#overall sentiment of the episode is positive

### Sentiment over time in episode

kards.show.analysis.all <- function(name) {
    text <- readLines(name)
    text
    
    name <- str_sub(name, start = 1, end =  -5)
    
    # Cleaning the transcript
    
    docs <- Corpus(VectorSource(text))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, content_transformer(tolower))
    inspect(docs)
    
    
    setdiff("so", stopwords("english"))
    setdiff("like", stopwords("english"))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removeWords, c("like", "yeah", "dont", "just", "know", "god", "really", "gonna", "get", "got",      "right", "can", "cant"))
    
    
    
    dtm <- TermDocumentMatrix(docs) %>%
        as.matrix() %>% 
        rowSums() %>%
        data.frame(word = names(.), freq = .) %>%
        mutate(order = row_number())
    
    dtm1 <- dtm
    
    rownames(dtm) <- NULL
    
    head(dtm, 25)
    
    # Sentiment Analysis - automated process of understanding of the emotional intent of words to infer whether a      section of text is positive, negative or neutral
    
    get_sentiments("nrc") %>% distinct(sentiment)
    
    get_sentiments("nrc") %>% 
        filter(sentiment == "joy") %>%
        inner_join(dtm)
    
    get_sentiments("nrc") %>% 
        filter(sentiment == "positive") %>%
        inner_join(dtm)
    
    get_sentiments("nrc") %>% 
        filter(sentiment == "negative") %>%
        inner_join(dtm)
    
    name
    
    
    return(dtm)
    
    
}

name <- "kuwtk_se19e02.txt"
dtm <- kards.show.analysis.all(name)
name <- str_sub(name, start = 1, end =  -5)

sentiment.order.kept <- get_sentiments("nrc") %>% 
    right_join(dtm) %>% 
    na.omit() %>%
    ungroup()%>%
    mutate(id = row_number())

# Determine number of groups and leftover rows
n_groups <- ceiling(nrow(sentiment.order.kept) / 20)
n_leftover <- n_groups * 20 - nrow(sentiment.order.kept)

# Assign a number between 1 and n_groups+1 to each row
sentiment.order.kept <- sentiment.order.kept %>%
    mutate(group = rep(1:n_groups, each = 20)[1:nrow(sentiment.order.kept)]) %>%
    mutate(group = ifelse(row_number() > n_groups * 20 - n_leftover, n_groups + 1, group))

# Assign a number between 1 and n_groups to the leftover rows
if (n_leftover > 0) {
    sentiment.order.kept %>%
        tail(n_leftover) %>%
        mutate(group = n_groups + 1) %>%
        bind_rows(head(sentiment.order.kept, nrow(sentiment.order.kept) - n_leftover)) ->
        sentiment.order.kept
}
sentiment.order.kept%>% 
    mutate(sentiment = recode(sentiment, "fear" = "negative", "disgust" = "negative", "anger" = "negative","sadness"="negative", "joy"="positive","surprise"="positive","trust"="positive","anticipation"="neutral" )) %>% 
    group_by(sentiment, group) %>%
    summarise(num_words = n_distinct(word)) %>%
    arrange(sentiment, group)%>%
    filter(sentiment == c("positive", "negative", "neutral"))%>%
    ggplot(aes(group, num_words, colour = sentiment)) + geom_line() +
    labs(x = "time of the episode",
         y = "number of words in sentiment",
         title = "The changes in sentiment over time")


