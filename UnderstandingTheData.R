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

simplisticvalidationBarkellr20180203ngramgen()