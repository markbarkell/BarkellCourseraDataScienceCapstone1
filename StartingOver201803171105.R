#install.packages("ngram")
#install.packages('hash')

library(ngram)
library(hash)

directoryPathRel <- "final/en_US/"
blogPathRel <- paste0(directoryPathRel, "en_US.blogs.txt")
newsPathRel <- paste0(directoryPathRel, "en_US.news.txt")
twitterPathRel <- paste0(directoryPathRel, "en_US.twitter.txt")

ngrammer <- function(line, ngramcount) {
  pattern <- paste0("([^ ]+ ){", ngramcount, ",}")
  if (grepl(pattern, line)) {
    ng <- ngram(line, n = ngramcount)
    return (ng)
  }
  return (NULL)
}

phraser <- function(ng) {
  if (!is.null(ng)) {
    pt <- get.phrasetable(ng)
    h <- hash(keys = pt$ngram, values = pt$freq)
    return (h)
  }
  return (NULL)
}

lastWordWithFreq <- function(ng) {
  if (!is.null(ng)) {
    
    p <- get.phrasetable(ng)
    key <- sub("^(.+)(\\s+.*)$", "\\1", p$ngram, perl = TRUE)
    word <- sub("^(.*)\\s([^\\s]+)\\s*$", "\\2", p$ngram, perl = TRUE)
    return (list(key = key, word = word, freq = p$freq ))

  }
  return (list())
}

splittaker <- function(paths, ngramcount) {
  
  h <- hash()
  
  for(lines in sapply(paths, function(p) { readLines(p)})) {
    for(line in lines) {
    
      pp <- gsub("[^A-Z\\s]", "", line, ignore.case = TRUE, perl = TRUE)
      pp <- preprocess(pp, case="upper", remove.punct=TRUE)
      
      n3 <- ngrammer(pp, 3)
      n2 <- ngrammer(pp, 2)
      n1 <- ngrammer(pp, 1)
      p1 <- phraser(n1)
      p2 <- phraser(n2)
      p3 <- phraser(n3)
      
      lf1 <- lastWordWithFreq(n1)
      lf2 <- lastWordWithFreq(n2)
      lf3 <- lastWordWithFreq(n3)
      
      h1 <- hash(keys = lf1$key, values = p1$hash)
      h2 <- hash(keys = lf2$key, values = list(word = lf2$word, freq = p2$freq, hash = p2$hash))
      h3 <- hash(keys = lf3$key, values = list(word = lf3$word, freq = p3$freq, hash = p3$hash ))
      for(h1Key in keys(h2)) {
        maxFreqOfNextWordH2 <- max(h2[h1Key]$freq)
        h1[h1Key]$value <- sapply(1:maxFreqOfNextWordH2, function(index) hash())
        for(i in 1:maxFreqOfNextWordH2) {
          h1[h1Key]$value[i] <- hash(keys = h2$values[freq == i,]$word, values = c(hash()))
          for(h2Word in h2$values[freq == i,]$word) {
            h2Key <- paste(h1Key, h2Word)
            maxFreqOfNextWordH3 <- max(h3[h2Key]$freq)
            h1[h1Key]$value[i][h2Word] <- sapply(1:maxFreqOfNextWordH3, function(index) hash())
            for(j in 1:maxFreqOfNextWordH3) {
              h1[h1Key]$value[i][h2Word] <- hash(keys = h3[h2Key]$value[freq == j,]$word, values = h3[h2Key]$values[freq == j,]$freq)
            }
          }
        }
        h[h1Key] <- h1
      }
      
    }
  }
  return (h)
}




#themodel <- splittaker(c(blogPathRel, newsPathRel, twitterPathRel), 2)


