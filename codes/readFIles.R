readem <- function(x){
  temp <- list.files(path = x, pattern='*.csv')
  temp <- temp[order(as.numeric(gsub('\\D+|\\Patch.*','',temp)))]
  myFiles <- lapply(temp, read.csv)
  myFiles <<- myFiles
  temp <<- temp
}

#setwd() as function argument
#directory should be where csv's are stored
