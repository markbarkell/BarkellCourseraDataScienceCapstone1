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

charCountInLongestLineOfFile <- function(filename) {
  f = file(filename, open="r")
  lines = readLines(f)
  close(f)
  maxLineCount <- max(sapply(lines, function(x) {nchar(x)}))
  return (maxLineCount)
}

maxBlogChars <- charCountInLongestLineOfFile(enUsBlogsPath)
maxTwitterChars <- charCountInLongestLineOfFile(enUsNewsPath)
 maxNewsChars <- charCountInLongestLineOfFile(enUsNewsPath)

print(paste("Max character count of the file english blogs", maxBlogChars))
print(paste("Max character count of the file english news", maxNewsChars))
print(paste("Max character count of the file english twitter", maxTwitterChars))
