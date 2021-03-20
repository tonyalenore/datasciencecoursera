
library(tm)
library(dplyr)
library(stringi)
library(stringr)
library(quanteda)
library(data.table)

rm(list = ls(all.names = TRUE))
setwd("~/git/datasciencecoursera/Capstone")

# blogs
blogsFileName <- "data/en_US/en_US.blogs.txt"
con <- file(blogsFileName, open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# news
newsFileName <- "data/en_US/en_US.news.txt"
con <- file(newsFileName, open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# twitter
twitterFileName <- "data/en_US/en_US.twitter.txt"
con <- file(twitterFileName, open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

rm(con, blogsFileName, newsFileName, twitterFileName)

# set seed for reproducibility
set.seed(8)

# assign sample size
sampleSize = 0.05

# sample all three data sets
sampleBlogs <- sample(blogs, length(blogs) * sampleSize, replace = FALSE)
sampleNews <- sample(news, length(news) * sampleSize, replace = FALSE)
sampleTwitter <- sample(twitter, length(twitter) * sampleSize, replace = FALSE)

# remove all non-English characters from the sampled data
sampleBlogs <- iconv(sampleBlogs, "latin1", "ASCII", sub = "")
sampleNews <- iconv(sampleNews, "latin1", "ASCII", sub = "")
sampleTwitter <- iconv(sampleTwitter, "latin1", "ASCII", sub = "")

# remove outliers such as very long and very short articles 
removeOutliers <- function(data) {
  first <- quantile(nchar(data), 0.05)
  third <- quantile(nchar(data), 0.95)
  data <- data[nchar(data) > first]
  data <- data[nchar(data) < third]
  return(data)
}

sampleBlogs <- removeOutliers(sampleBlogs)
sampleNews <- removeOutliers(sampleNews)
sampleTwitter <- removeOutliers(sampleTwitter)

# combine all three data sets into a single data set
sampleData <- c(sampleBlogs, sampleNews, sampleTwitter)

# get number of lines and words from the sample data set
sampleDataLines <- length(sampleData)
sampleDataWords <- sum(stri_count_words(sampleData))

# remove variables no longer needed to free up memory
rm(blogs, news, twitter, sampleBlogs, sampleNews, sampleTwitter)
rm(removeOutliers)

# ------------------------------------------------------------------------------
# Clean the data
# ------------------------------------------------------------------------------

# load bad words file
badWordsFile <- "data/bad-words.txt"

con <- file(badWordsFile, open = "r")
profanity <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
profanity <- iconv(profanity, "latin1", "ASCII", sub = "")
close(con)

# convert text to lowercase
sampleData <- tolower(sampleData)

# remove URL, email addresses, Twitter handles and hash tags
sampleData <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("\\S+[@]\\S+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("@[^\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("#[^\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove ordinal numbers
sampleData <- gsub("[0-9](?:st|nd|rd|th)", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove profane words
sampleData <- removeWords(sampleData, profanity)

# remove punctuation
sampleData <- gsub("[^\\p{L}'\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove punctuation (leaving ')
sampleData <- gsub("[.\\-!]", " ", sampleData, ignore.case = FALSE, perl = TRUE)

# trim leading and trailing whitespace
sampleData <- gsub("^\\s+|\\s+$", "", sampleData)
sampleData <- stripWhitespace(sampleData)

# write sample data set to disk
sampleDataFileName <- "data/en_US.sample.txt"
con <- file(sampleDataFileName, open = "w")
writeLines(sampleData, con)
close(con)

# remove variables no longer needed to free up memory
rm(badWordsFile, con, sampleDataFileName, profanity)

# ------------------------------------------------------------------------------
# Build corpus
# ------------------------------------------------------------------------------

corpus <- corpus(sampleData)

# ------------------------------------------------------------------------------
# Build n-gram frequencies
# ------------------------------------------------------------------------------

getTopThree <- function(corpus) {
  first <- !duplicated(corpus$token)
  balance <- corpus[!first,]
  first <- corpus[first,]
  second <- !duplicated(balance$token)
  balance2 <- balance[!second,]
  second <- balance[second,]
  third <- !duplicated(balance2$token)
  third <- balance2[third,]
  return(rbind(first, second, third))
}

# Generate a token frequency dataframe. Do not remove stemwords because they are
# possible candidates for next word prediction.

tokenFrequency <- function(corpus, n, rem_stopw = NULL) {
  
  toks <- tokens(corpus, remove_punct = TRUE)
  corpus <- tokens_ngrams(toks, n+1)
  print(corpus[1:10])
  
  corpus <- dfm(corpus, ngrams = n)
  corpus <- colSums(corpus)
  total <- sum(corpus)
  
  corpus <- data.frame(names(corpus),
                       corpus,
                       row.names = NULL,
                       check.rows = FALSE,
                       check.names = FALSE,
                       stringsAsFactors = FALSE
  )
  colnames(corpus) <- c("token", "n")
  
  corpus <- mutate(corpus, token = gsub("_", " ", token))
  corpus <- mutate(corpus, percent = corpus$n / total)

  if (n > 1) {
    print('n > 1')
    print(corpus$token[1:10])
    corpus$outcome <- word(corpus$token, -1) # the last word
    print(corpus$outcome[1:10])
    corpus$token <- word(corpus$token, start = 1, end = n)
  }
  setorder(corpus, -n)
  corpus <- getTopThree(corpus)
  return(corpus)
}

tokenFrequency2 <- function(corpus, n = 1, rem_stopw = NULL) {
  corpus <- dfm(corpus, ngrams = n)
  corpus <- colSums(corpus)
  total <- sum(corpus)
  corpus <- data.frame(names(corpus),
                       corpus,
                       row.names = NULL,
                       check.rows = FALSE,
                       check.names = FALSE,
                       stringsAsFactors = FALSE
  )
  colnames(corpus) <- c("token", "n")
  corpus <- mutate(corpus, token = gsub("_", " ", token))
  corpus <- mutate(corpus, percent = corpus$n / total)
  if (n > 1) {
    corpus$outcome <- word(corpus$token, -1)
    corpus$token <- word(string = corpus$token, start = 1, end = n - 1, sep = fixed(" "))
  }
  setorder(corpus, -n)
  corpus <- getTopThree(corpus)
  return(corpus)
}

# get top 3 words to initiate the next word prediction app
startWord <- word(texts(corpus), 1)  # get first word for each document
startWord <- tokenFrequency2(startWord, n = 1, NULL)  # determine most popular start words
startWordPrediction <- startWord$token[1:3]  # select top 3 words to start word prediction app
saveRDS(startWordPrediction, "data/start-word-prediction.RData")

print('bigram')
# bigram
bigram <- tokenFrequency(corpus, n = 2, NULL)
saveRDS(bigram, "data/bigram.RData")
remove(bigram)

print('trigram')
# trigram
trigram <- tokenFrequency(corpus, n = 3, NULL)
trigram <- trigram %>% filter(n > 1)
saveRDS(trigram, "data/trigram.RData")
remove(trigram)

print('quadgram')
# quadgram
quadgram <- tokenFrequency(corpus, n = 4, NULL)
quadgram <- quadgram %>% filter(n > 1)
saveRDS(quadgram, "data/quadgram.RData")
remove(quadgram)


initialPrediction <- readRDS("data/start-word-prediction.RData")
freq2ngram <- readRDS("data/bigram.RData")
freq3ngram <- readRDS("data/trigram.RData")
freq4ngram <- readRDS("data/quadgram.RData")
