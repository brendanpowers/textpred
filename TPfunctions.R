downloadCorpus <- function(SwiftDataDir, Swiftfile, SwFilePath,SwUrl) {
  if (!dir.exists(SwiftDataDir)) {
    dir.create(SwiftDataDir)
  }
  if (!file.exists(SwFilePath)) {
    download.file(SwUrl,SwFilePath)
  }
  if (!dir.exists(UnZipDir)) {
    dir.create(UnZipDir)
    unzip(SwFilePath, exdir = UnZipDir)
  }
}

numlineswords <- function (con) {  
##function numlineswords reads files and returns number of lines and  word count.
  readsize <- 10000
  numln <- wordct <- 0
  suppressWarnings(
    repeat {
      rline <- readLines(con,readsize)
      linesread <- length(rline) 
      if (linesread == 0) {
        break
      }
      numln <- numln+linesread
      wordct <- wordct + sum(sapply(gregexpr(
        "[[:alpha:]]+", rline), function(x) sum(x > 0)))
    }
  )
  #should return one row two columns
  cbind(numln,wordct)
}

filesum <- function(enDir, files) {
##function filesum create a data.frame with length(files) observations and 
##columns numline, wordct, and filesize.  
  for (n in 1:length(files)) {
    testcon <- file(paste0(enDir,"/", files[,n]) , open="r")
    if (n == 1) 
      filelengths <- data.frame(numlineswords(testcon))
          else
      filelengths <- rbind(filelengths,data.frame(numlineswords(testcon)))
    row.names(filelengths)[n] <- names(files)[n]
    close(testcon)
    if (n ==1)
      filessize_mb <- file.info(
        file.path(enDir,files[,n]))$size/2^20
    else
      filessize_mb <- rbind(filessize_mb, file.info(
        file.path(enDir,files[,n]))$size/2^20)
    
  }
  FilesSummary <- cbind(filelengths,filessize_mb)
  }

samplereadlines <- function(con=NUll, filelength, probs=.1,readsize = 1) {
  ##reads read size number of lines if a rndom binomial with probs, probability 
  ##is true.  Profanity is removed if it matches the expression return by 
  ##profanity.
  ## Returns linesout
  #I tried using scan to only read sampled lines (seems buggy in windows) and reading the whole file then selecting lines to keep (seems to bog down the computer.
  source("profanity.R")
  linesout <- list()
  linesread <- readLines(con,readsize, warn = FALSE)
  n<-0
  while(length(linesread) > 0 ) {
    n=n+1
    if (rbinom(1,1,prob = probs)){
      #remove profanity
      expr <- profanity()
      linesread <- gsub(expr, "", 
                        linesread, ignore.case = TRUE)
      #remove not latin charecters
      linesread <- iconv(linesread,"latin1", "ASCII", sub="") 
      
      if (length(linesout > 0)) {
        linesout <- c(linesout, linesread)
      }
      else {
        linesout <- linesread
      }
    }
    linesread <- readLines(con,readsize, warn = FALSE)
    if (n==70000) 
      break
  } 
  
  linesout
} #End samplereadlines