# Programmer: Mark Barkell
# Purpose: Create a program allowing a piece of text to have been typed in and allowing a prediction of the next word the user may be expressing.
# This is part of the Data Sciense Capstone from John Hopkins University as hosted by Coursera.  The course advertises that the work
# could be shown to potential employeers as a sample of what the studen is able to do.  I don't know whether this script will yield the results
# I desire for the project, but am trying another techique.
library(dplyr)
library(tidytext)
library(tidyr)
library(ngram)
library(widyr)
library(tibble)
library(topicmodels)


topicSeps <- 100

fileNames <- c("final/en_US/en_US.twitter.txt",
               "final/en_US/en_US.blogs.txt",
               "final/en_US/en_US.news.txt")

GetFilesLines <- function(files) {
  filesFrame <- data.frame(stringsAsFactors = FALSE)
  for(file in files) {
    print(paste("file name being processed", file))
    lines = readLines(file, n =  500 * 1000 %/% 3)
    doc = sapply(1:length(lines), function(ll) file)
    fileFrame <- data.frame(line = lines, doc = doc, stringsAsFactors = FALSE)
    filesFrame <- rbind(filesFrame, fileFrame)
  }
  return (filesFrame)
}

fromDataLDA <- function(data)
{
  # Reference: Page 78 of Text Mining With R: A TIDY Approach.
  dtm <- (data %>% unnest_tokens(word, line) %>% count(doc, word) %>% cast_dtm(doc, word, n))
  ldda <- LDA(dtm, k = topicSeps, control = list(seed = 20180730))
  return (ldda)
}

fromDataLDABeta <- function(data) {
  d <- fromDataLDA(data)
  b <- tidy(d, matrix = "beta")
}

buildLDAOfFiles <- function(files) {
  filesFrame <- GetFilesLines(files)
  lineAndDoc <- myPreprocessLines(filesFrame)
  d <- (fromDataLDA(lineAndDoc))
  return (d)
}

buildAllLDA <- function()
{
  d <- buildLDAOfFiles(
    fileNames
  )
  return (d)
}


buildModel <- function(files) {
  filesFrame <- GetFilesLines(files)
  lineAndDoc <- myPreprocessLines(filesFrame)
  r <- linedata(lineAndDoc)
  return(r)
}

myPreprocessLine <- function (line) {
  preparedLine <- preprocess(line, remove.punct = TRUE, remove.numbers = TRUE)
  preparedLine <- gsub("[^a-z\\s]", "", preparedLine, perl = TRUE)
  preparedLine <- sub("^\\s*", "", preparedLine, perl = TRUE)
  preparedLine <- sub("\\s*$", "", preparedLine, perl = TRUE)
  preparedLine <- gsub("\\s\\s", " ", preparedLine, perl = TRUE)  
  return (preparedLine)
}

myPreprocessLines <- function (lineAndDoc) {
    s <- data.frame(stringsAsFactors = FALSE)
    for(ll in 1:(dim(lineAndDoc))[1]) {
      pLine <- myPreprocessLine(lineAndDoc[ll,"line"])
      if (nchar(pLine) > 0) {
        r <- data.frame(line = pLine, doc = lineAndDoc[ll,"doc"], stringsAsFactors = FALSE)
        s <- rbind(s, r)
        if (ll %% 10000 == 0) {
          print(paste("Read file lines", ll))
        }
      #  print(pLine)
      }
    }
  return (s)
}

linedata <- function(lineAndDoc) {
  filteredInfo <- lineAndDoc %>% mutate(linenum = row_number()) %>% unnest_tokens(word, line) %>% filter(!(word %in% stop_words$word)) 
  info <- filteredInfo %>% pairwise_count(word, linenum, sort = TRUE) %>% filter(n > 10)
  filteredInfo <- filteredInfo %>%  bind_tf_idf(word, doc, linenum)
  betas <- fromDataLDABeta(lineAndDoc)
  betas <-  betas %>% group_by(term) %>% filter(beta == max(beta))
  return (list(filteredInfo, info, betas))
}

buildTwitterModel <- function() {
  return (buildModel(c("final/en_US/en_US.twitter.txt")))
}

buildAllModel <- function() {
  model <-buildModel(c("final/en_US/en_US.twitter.txt",
                       "final/en_US/en_US.blogs.txt",
                       "final/en_US/en_US.news.txt")
                      )
  return (model)
}

predictBasedOnPrev <- function(model, txt) {
  linepart <- myPreprocessLine(txt)
  existingWords <- strsplit(linepart, " ")[[1]]
  fullExistingWords <- existingWords
  ew <- c()
  for(word in existingWords) {
    ew <- append(ew[which(!(ew %in% word))], word)
  }
  f <- 50
  start <- if (length(ew)-f < 1) { 0 } else { length(ew)-f }
  existingWords <- ew[(start):length(ew)]
  candidates <- NULL
  coee <- 1
  betas <- model[[3]]
  betas$word = betas$term
  cntsModel <- model[[2]]
  tfModels <- model[[1]]
  cur <- 1
  topicCounts <- sapply(1:topicSeps, function(it) 0)
  sentimentOfPhrase <- 0
  containsNegation <- FALSE
  sentimentDirection <- 1
  sent <- get_sentiments("afinn")
  for(wordInLine in existingWords) {
    print(paste("word in line", wordInLine))
    sentimentOfWord <- sent[which(wordInLine == sent$word), "score"]
    sentimentOfWord <- if (0 == dim(sentimentOfWord)[1]) { 0 } else { sentimentOfWord$score[[1]] }
    if (wordInLine %in% c('no', 'not', 'never', "dont", "doesnt", "didnt", "wont", "havent")) {
      containsNegation <- if (containsNegation) { FALSE } else { TRUE }
      sentimentOfPhrase <- sentimentOfPhrase * (if (containsNegation) {-1} else {1})
      sentimentDirection <- sentimentDirection * -1
    }
    sentimentOfPhrase <- sentimentOfPhrase + sentimentDirection * sentimentOfWord
    m <- (cntsModel %>% filter(item1 == wordInLine)) %>% filter(!is.na(item1)) %>% filter(!item2 %in% fullExistingWords)
    m$word <- m$item2
    tfSub <- tfModels %>% filter(word %in% m$word) %>% select(word, idf) %>% group_by(word) %>% distinct()
    tmg <- (inner_join(tfSub, m))
    topicId <- (betas[which(betas$word == wordInLine),"topic"])[[1]]
    topicCounts[topicId] = topicCounts[topicId] + 1
    g <- (inner_join(tmg, betas))
    m$c <- (coee * abs(g[,"idf"]))[,]
    m$sentiment <- as.integer( (0 * m$c) != 0)
    if (dim(m)[1] > 0) {
      #m$rowid <- sapply(cur:( (cur-1) + dim(m)[1]), function(it) paste0(it, ""))
      remove_rownames(m)
      cur <- cur + dim(m)[1] + 100
      coee <- coee * 1.1
    }
    if (is.null(candidates)) {
      candidates <- m
    } else if (dim(m)[1] > 0) {
      candidates <- union(candidates, m)
    }
  }
  usedTopics <- topicCounts[which(topicCounts >= 0)]
  ut <- data.frame(topic = 1:length(usedTopics), topiccount = usedTopics )
  slimbetas <- betas[which(betas$topic %in% usedTopics),]
  candidates <- candidates %>% filter(!item2 %in% fullExistingWords) #%>% filter(item2 %in% slimbetas$term)
  candidates <- inner_join(inner_join(candidates,betas), ut) %>% group_by(word,topic,topiccount) %>% summarise(c = sum(c))
  candidates$sentiment <- sapply(left_join(candidates, sent)$score, function(s) if (is.na(s)) {0} else {s})
  if (sentimentOfPhrase > 0) {
    candidates <- candidates %>% arrange(sentiment, c, topiccount)
  } else {
    candidates <- candidates %>% arrange(desc(sentiment),c, topiccount)
  }
  #candidates <- candidates[1:1000,]
  return (candidates)
}

# ba <- buildAllModel()