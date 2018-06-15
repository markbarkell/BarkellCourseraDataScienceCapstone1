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

wordSufficientCountPattern <- function(n) {
  pattern <- paste0("([^\\s]+\\s){", n-1, ",}")
  return (pattern)
}

wordSufficientCountCheck <- function(txt, n) {
  b <- grepl(wordSufficientCountPattern(n), txt, perl = TRUE)
  return (b)
}

splitter <- function(txt, n) {
  if (is.null(txt) || txt == "") {
    txt = "!"
  }
  wordSufCntPtr <- wordSufficientCountPattern(n)
  if (!wordSufficientCountCheck(txt, n)) {
    txt = ""
    for(i in 1:n) { txt = paste(txt, "!") }
  }
  ng <- ngram(txt, n)
  pt <- get.phrasetable(ng)
  sp <- "^(.+)\\s+([^\\s]+)\\s*$"
  ts <- "\\s+$";
  k <- sub(sp, "\\1", pt$ngram, perl = TRUE)
  vl <- sub(sp, "\\2", pt$ngram, perl = TRUE)
  k <- gsub(ts, "", k, perl = TRUE)
  vl <- sub(ts, "", vl, perl = TRUE)
  v <- mapply(function(x, y) { hash(keys = c(x), values = c(y))}, vl, pt$freq)
  k[which(k  == "")] = "!"
  h <- hash(keys = k, values = v)
  return (h)
}

markovMerge <- function(rvalue, hvalues) {
  #print(paste("hvalue is of ", class(hvalues)))
  for(hvalue in sapply(c(hvalues), function(x) { x })) { 
    for(k in keys(hvalue)) {
      #print(paste("key is", k))
      hv <- hvalue[[k]]
      rv <- rvalue[[k]] 
      if (!is.null(rv)) {
        #print(paste("hv is ", hv))
        if (class(hv) == "integer" && class(rv) == "integer") {
          rvalue[[k]] <- rv + hv
        }
        else {
          print(paste("oddly hv class is ", class(hv),k))
          print(paste("oddly rv class is ", class(rv),k))
        }
      }
      else {
        if (class(hv) == "integer") {
          rvalue[[k]] <- hv
        }
        else {
          print(paste("somewhat oddly hv class is ", class(hv), k))
        }
      }
    }
  }
  return (rvalue)
}

buildmapping <- function() {
  r <- hash()
  for(filename in c(enUsBlogsPath, enUsNewsPath, enUsTwitterPath)) {
    print(paste("filename is", filename))
    linei <- 0
    for(line in readLines(filename)) {
      linei <- linei + 1
      if (linei %% 1000 == 0) {
        print(paste("processing line", linei))
        #break
      }
      preparedLine <- preprocess(line, remove.punct = TRUE, remove.numbers = TRUE)
      
      
      preparedLine <- gsub("[^a-z\\s]", "", preparedLine, perl = TRUE)
      #preparedLine <- gsub("\\b(a|the|an)\\b", " ", preparedLine, perl = TRUE)
      preparedLine <- sub("^\\s*", "", preparedLine, perl = TRUE)
      preparedLine <- sub("\\s*$", "", preparedLine, perl = TRUE)
      preparedLine <- gsub("\\s\\s", " ", preparedLine, perl = TRUE)
      #print(preparedLine)
      h <- splitter(preparedLine, 2)
      for(k in keys(h)) {
        if (is.null(r[[k]])) {
          r[[k]] <- markovMerge(hash(), h[[k]])
        }
        else
        {
          r[[k]] <- markovMerge(r[[k]], h[[k]])
        }
      }
      h <- splitter(preparedLine, 3)
      for(k in keys(h)) {
        if (is.null(r[[k]])) {
          r[[k]] <- markovMerge(hash(), h[[k]])
        }
        else
        {
          r[[k]] <- markovMerge(r[[k]], h[[k]])
        }
      }
    }  
  }
  return (r)
}

representation <- buildmapping()
save(representation, file = "image.cnt")