# Purpose:  The Academic Assignment for the Data Science Specialization offered through Coursera.org by John Hopkins University
# seems to be some type of predict the next word in a series of words.  One of the first parts of this is to download the seed data.
# Apparently, the corpus is a bunch of text anonymousized from the Web.
# This Capstone project is advertised as being one that I can show off to family, friends, and employeers.   So, I look forward to the fun.
# Author: Mark Barkell
# Date Begun: 2018-02-03

# The purpose for this particular file is to look at the properies of the data
# that I would like to explore with suggestions from the course -- like
# understanding the unique properites of the n-grams used.

# Yes, I know that there is an n-gram package in CRAN, but, it takes in
# random seed values as input.  I'm not ready to worry about that yet.


library(dplyr)
library(hash)
library(sqldf)

barkellr20180203ngramgen <- function(n, words) {
  r <- vector()
  wordLen <- length(words)
  for(i in 1:wordLen) {
    cr <- ""
    cnt <- 0
    for(j in 0:n) {
      ind <- i + j
      if (ind <= wordLen) {
          w <- words[ind]
          cr <- paste0(paste0(cr, w), " ")
          cnt <- cnt + 1
          if (cnt == n) {
            r <- append(r, cr)
            break
          }
      }
    }
  }
  return (r)
}

simplisticvalidationBarkellr20180203ngramgen <- function() {
  print("should be A")
  print(barkellr20180203ngramgen(1, c("A")))
  print ("should be A, B")
  print(barkellr20180203ngramgen(1, c("A", "B")))
  print ("should be A B, B C")
  print(barkellr20180203ngramgen(2, c("A", "B", "C")))
  print("should be nada")
  print(barkellr20180203ngramgen(3, c("A", "B")))
}

#simplisticvalidationBarkellr20180203ngramgen()


urlOfCapstoneSeedData <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileNameOfCapstoneSeedData <- "Coursera-SwiftKey.zip"
if (!file.exists(fileNameOfCapstoneSeedData)) {
  download.file(urlOfCapstoneSeedData, fileNameOfCapstoneSeedData)
}
enUsBlogsPath <- "final/en_US/en_US.blogs.txt"
if (!file.exists(enUsBlogsPath)) {
  unzip(fileNameOfCapstoneSeedData)
}

enUsTwitterPath <- "final/en_US/en_US.twitter.txt"
enUsNewsPath <- "final/en_US/en_US.news.txt"

linesOfFile <- function(filename) {
  f = file(filename, open="r")
  lines <- readLines(f)
  close(f)
  return (lines)
}

blogFeed <- linesOfFile(enUsBlogsPath)
twitterFeed <- linesOfFile(enUsTwitterPath)
newsFeed <- linesOfFile(enUsNewsPath)

splity <- function(x) { 
  strsplit(paste0(x, " "), " +") 
}

grammy <- function (n, x) { 
  s <- splity(x)[[1]]
  barkellr20180203ngramgen(n, s)
}

grammy2 <- function(x) {
  grammy(2, x)
}

grammy3 <- function(x) {
  grammy(3, x)
}

twoGramsBlogFeed <- lapply(blogFeed, grammy2)
threeGramsBlogFeed <- lapply(blogFeed, grammy3)
twoGramNewsFeed <- lapply(newsFeed, grammy2)
threeGramNewsFeed <- lapply(newsFeed, grammy3)
twoGramsTwitterFeed <- lapply(twitterFeed, grammy2)
threeGramsTwitterFeed <- lapply(twitterFeed, grammy3)



