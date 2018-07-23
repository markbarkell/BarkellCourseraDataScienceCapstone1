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
buildModel <- function(files) {
  lines <- c()
  for(file in files) {
    lines <- append(lines,readLines(file))
  }
  return(linedata(myPreprocessLines(lines)))
}

myPreprocessLine <- function (line) {
  preparedLine <- preprocess(line, remove.punct = TRUE, remove.numbers = TRUE)
  preparedLine <- gsub("[^a-z\\s]", "", preparedLine, perl = TRUE)
  preparedLine <- sub("^\\s*", "", preparedLine, perl = TRUE)
  preparedLine <- sub("\\s*$", "", preparedLine, perl = TRUE)
  preparedLine <- gsub("\\s\\s", " ", preparedLine, perl = TRUE)  
  return (preparedLine)
}

myPreprocessLines <- function (lines) {
  s <- (sapply(lines, function(ll) { myPreprocessLine(ll) }))
  s <- s[which(nchar(s) > 0)]
  return (s)
}

linedata <- function(lines) {
  dflines <- data.frame(line = lines, foo = "", stringsAsFactors = FALSE)
  filteredInfo <- dflines %>% mutate(linenum = row_number()) %>% unnest_tokens(word, line) %>% filter(!(word %in% stop_words$word))
  info <- filteredInfo %>% pairwise_count(word, linenum, sort = TRUE)
  data <- info #info[which(info$correlation > .15 && info$correlation < .99),]
  return (data)
}

buildTwitterModel <- function() {
  return (buildModel(c("final/en_US/en_US.twitter.txt")))
}

buildAllModel <- function() {
  return (buildModel(c("final/en_US/en_US.twitter.txt",
                       "final/en_US/en_US.blogs.txt",
                       "final/en_US/en_US.news.txt")))
}

