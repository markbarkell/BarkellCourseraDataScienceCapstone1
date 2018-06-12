DataUnderStandingPresentation
========================================================
author: Mark Barkell
date: 2018-05-08
autosize: true

========================================================

This presentation downloads data for the Data Science Capstone project.  Then, it looks at some basic descriptive statitics.  It also outlines a plan of action.  The desired goal is to be able to predict upcoming words.

========================================================

This is the code to get and extract the data for the Capstone project.


```r
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
```

===========================================================

Read the data


```r
#install.packages("ngram")
library(ngram)

blogs <- readLines("final/en_US/en_US.blogs.txt")
news <- readLines("final/en_US/en_US.news.txt")
twitter <- readLines("final/en_US/en_US.twitter.txt")
allFeeds <- ngram::concatenate(blogs, news, twitter)
ppAllFeeds <- ngram::preprocess(allFeeds, case="upper", remove.punct = TRUE)
```

