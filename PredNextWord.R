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

GetFilesLines <- function(files) {
  filesFrame <- data.frame(stringsAsFactors = FALSE)
  for(file in files) {
    print(paste("file name being processed", file))
    lines = readLines(file, n = (250000 %/% length(files)))
    doc = sapply(1:length(lines), function(ll) file)
    fileFrame <- data.frame(line = lines, doc = doc, stringsAsFactors = FALSE)
    filesFrame <- rbind(filesFrame, fileFrame)
  }
  return (filesFrame)
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
  return (list(filteredInfo, info))
}

buildTwitterModel <- function() {
  return (buildModel(c("final/en_US/en_US.twitter.txt")))
}

buildAllModel <- function() {
  return (buildModel(c("final/en_US/en_US.twitter.txt",
                       "final/en_US/en_US.blogs.txt",
                       "final/en_US/en_US.news.txt")))
}

predictBasedOnPrev <- function(model, txt) {
  linepart <- myPreprocessLine(txt)
  existingWords <- strsplit(linepart, " ")[[1]]
  fullExistingWords <- existingWords
  ew <- c()
  for(word in existingWords) {
    ew <- append(ew[which(!(ew %in% word))], word)
  }
  f <- 100
  start <- if (length(ew)-f < 1) { 0 } else { length(ew)-f }
  existingWords <- ew[(start):length(ew)]
  candidates <- NULL
  coee <- 1
  cntsModel <- model[[2]]
  tfModels <- model[[1]]
  cur <- 1
  for(wordInLine in existingWords) {
    print(paste("word in line", wordInLine))
    m <- (cntsModel %>% filter(item1 == wordInLine))[1:1000,] %>% filter(!is.na(item1)) %>% filter(!item2 %in% fullExistingWords)
    m$word <- m$item2
    tfSub <- tfModels %>% filter(word %in% m$word) %>% select(word, idf) %>% group_by(word) %>% distinct()
    g <- (inner_join(tfSub, m) %>% group_by(word)) 
    m$c <- (coee * g[,"n"] * abs(g[,"idf"]))[,]
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
  candidates <- candidates %>% filter(!item2 %in% fullExistingWords)
  candidates <- candidates %>% group_by(item2) %>% summarise(c = sum(c))
  candidates <- candidates %>% arrange(desc(c))
  candidates <- candidates[1:1000,]
  return (candidates)
}

# ba <- buildAllModel()