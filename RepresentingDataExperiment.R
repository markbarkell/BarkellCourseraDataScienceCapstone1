# Purpose:  The Academic Assignment for the Data Science Specialization offered through Coursera.org by John Hopkins University
# seems to be some type of predict the next word in a series of words.  One of the first parts of this is to download the seed data.
# Apparently, the corpus is a bunch of text anonymousized from the Web.
# This Capstone project is advertised as being one that I can show off to family, friends, and employeers.   So, I look forward to the fun.
# Author: Mark Barkell
# Date Begun: 2018-01-30


# The download described in the variable urlOfCapstoneSeedData has the following description
# on the course site's documentation: 
# "Content archived from heliohost.org on September 30, 2016 and retrieved via Wayback Machine on April 24, 207. https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html"
# The page of that sentence on the course site is:
# https://www.coursera.org/learn/data-science-project/supplement/4phKX/about-the-copora

library(ngram)
library(hash)

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

splitter <- function(txt, n) {
  ng <- ngram(txt, n)
  pt <- get.phrasetable(ng)
  sp <- "^(.+)\\s+([^\\s]+)\\s*$"
  ts <- "\\s+$";
  k <- sub(sp, "\\1", pt$ngram, perl = TRUE)
  vl <- sub(sp, "\\2", pt$ngram, perl = TRUE)
  k <- gsub(ts, "", k, perl = TRUE)
  vl <- sub(ts, "", vl, perl = TRUE)
  v <- mapply(function(x, y) { hash(keys = c(x), values = c(y))}, vl, pt$freq)
  h <- hash(keys = k, values = v)
  return (h)
}

markovMerge <- function(rvalue, hvalue) {
  return (rvalue)
}

buildmapping <- function() {
  r <- hash()
  for(filename in c(enUsBlogsPath, enUsNewsPath, enUsTwitterPath)) {
    for(line in readLines(filename)) {
      preparedLine <- preprocess(line, remove.punct = TRUE, remove.numbers = TRUE)
      h <- splitter(preparedLine, 1)
      for(k in h$keys) {
        if (is.null(r[[k]])) {
          r[[k]] <- h[[k]]
        }
        else
        {
          r[[k]] <- markovMerge(r[[k]], h[[k]])
        }
      }
      clear(h)
      rm(h)
    }  
  }
  return (r)
}

