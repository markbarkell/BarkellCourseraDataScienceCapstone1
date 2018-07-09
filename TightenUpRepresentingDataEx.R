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

phraser <- function(txt, n) {
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
  return (names(sapply(mapply(function(x, y) { sapply(1:y, function(z) x) }, pt$ngrams, pt$freq), function(k) k)))
  
}



buildmapping <- function() {
  for(filename in c(enUsBlogsPath, enUsNewsPath, enUsTwitterPath)) {
    file2name <- paste0(filename, ".bi.raw")
    fileDes2 <- file(description = file2name, open = "w+")
    file3name <- paste0(filename, ".tri.raw")
    fileDes3 <- file(description = file3name, open = "w+")
    print(paste("filename is", filename))
    linei <- 0
    for(line in readLines(filename)) {
      linei <- linei + 1
      if (linei %% 1000 == 0) {
        print(paste("processing line", linei))
      }
      if (linei %% 1000 == 0) {
        #break
      }
      preparedLine <- preprocess(line, remove.punct = TRUE, remove.numbers = TRUE)
      preparedLine <- gsub("[^a-z\\s]", "", preparedLine, perl = TRUE)
      preparedLine <- gsub("\\b(a|the|an)\\b", " ", preparedLine, perl = TRUE)
      preparedLine <- sub("^\\s*", "", preparedLine, perl = TRUE)
      preparedLine <- sub("\\s*$", "", preparedLine, perl = TRUE)
      preparedLine <- gsub("\\s\\s", " ", preparedLine, perl = TRUE)
      preparedLine2 <- gsub("(i|you|we|they|s?he) (would|d|a?m|ll|will|have|ve|shall|be|can|is) ", "sptrv ", preparedLine, perl = TRUE)
      preparedLine2 <- gsub("(i|you|we|they|s?he) (wouldn't|would not|a?m|will not|haven't|have not|shall not|cannot|can not|isn't|is not) ", "sptrvn ", preparedLine2, perl = TRUE) 
      preparedLine3 <- gsub("(\\w+) (would|d|a?m|ll|will|have|ve|shall|be|can|is) ", "gsptrvn ", preparedLine, perl = TRUE)
      preparedLine3 <- gsub("(\\w+) (wouldn't|would not|a?m|will not|haven't|have not|shall not|cannot|can not|isn't|is not) ", "gsptrvn ", preparedLine3, perl = TRUE) 
      
      #print(preparedLine)
      p <- phraser(preparedLine, 2)
      write(p, file = fileDes2)
      p <- phraser(preparedLine2, 2)
      write(p, file = fileDes2)
      p <- phraser(preparedLine3, 2)
      write(p, file = fileDes2)
      p <- phraser(preparedLine, 3)
      write(p, file = fileDes3)
      p <- phraser(preparedLine2, 3)
      write(p, file = fileDes3)
      p <- phraser(preparedLine3, 3)
      write(p, file = fileDes3)
    }
    close(fileDes2)
    close(fileDes3)
  }
}


sort_bi_command <- function(srcFilename) {
  biCommand <- paste("cat", paste0(srcFilename, ".bi.raw"), "| grep -v ! | sort >", paste0(srcFilename, ".bi.srt"))
  return (biCommand)
}

sort_tri_command <- function(srcFilename) {
  triCommand <- paste("cat", paste0(srcFilename, ".tri.raw"), "| grep -v ! | sort >", paste0(srcFilename, ".tri.srt"))
                      return (triCommand)
}

alphabetize_raw <- function() {
  for(srcFilename in c(enUsBlogsPath, enUsNewsPath, enUsTwitterPath)) {
    biCommand <- sort_bi_command(srcFilename);
    print(biCommand)
    system(biCommand)
    triCommand <- sort_tri_command(srcFilename)
    system(triCommand)
  }
}

alphabetize_raw_fn_bi <- function(srcFilename) {
  biCommand <- sort_bi_command(srcFilename);
  print(biCommand)
  system(biCommand)
}

alphabetize_raw_fn_tri <- function(srcFilename) {
  triCommand <- sort_tri_command(srcFilename);
  print(triCommand)
  system(triCommand)
}



#buildmapping()
#alphabetize_raw()
# Just called the manual sorting for the Twitter textual data.
# cause it wasn't otherwise getting written to disk.
