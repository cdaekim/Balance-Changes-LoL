lolScraperMain <- function(patch){
  library(RSelenium)
  library(rvest)
  
  ### initialize vectors that will used throughout the program
  ### and a list to collect all the dataframes that will be produced
  childNodes <- c(3,5,6,7,8,10)
  lanes <- c("top","jungle","middle","bottom","support")
  dfList <- list()
  counter <- 1
  
  driver <- rsDriver(browser="chrome")
  driver <<- driver
  remote_driver <- driver[["client"]]
  
  
  for (i in 1:length(patch)){
    for (j in 1:length(lanes)){

      remote_driver$navigate(sprintf("https://lolalytics.com/lol/tierlist/?lane=%s&tier=all&patch=%s",lanes[j],patch[i]))
     
      Sys.sleep(5)
      
      filterBox1 <- remote_driver$findElement(using='css', value='.FilterBox_filter__1Rx76:nth-child(4) input')
      filterBox1$clickElement()
      filterBox1$clearElement()
      filterBox1$sendKeysToElement(list("0"))
      filterBox2 <- remote_driver$findElement(using='css', value= '.FilterBox_filter__1Rx76:nth-child(5) input')
      filterBox2$clickElement()
      filterBox2$clearElement()
      filterBox2$sendKeysToElement(list("0"))
      
      Sys.sleep(5)
      
      output <- remote_driver$getPageSource()[[1]] 
      listCols <- list()
      
      for(k in 1:6){
        scrappedCols <- output %>% read_html() %>% html_nodes(sprintf(".Layout_wrapper__1qGqQ div div:nth-child(%i)",childNodes[k])) %>% html_text()
        scrappedCols <- scrappedCols[nchar(scrappedCols)>1]
        listCols[[k]] <- scrappedCols
      }
      
      dfScrapped <- as.data.frame(listCols, stringsAsFactors = FALSE)
      colnames(dfScrapped) <- c("champName","laneRate","winRate","pickRate","banRate","gamesPlayed")
      colsToClean <- c(2,3,4,5)
      ### NEED TO CHANGE THIS PART
      ### makes no sense...
      ### need to change champName to be class character
      for (c in 1:5){
        if (c <= 4){
          dfScrapped[,colsToClean[c]] <- as.numeric(dfScrapped[,colsToClean[c]])
        } else {
          dfScrapped[,6] <- gsub(",","",dfScrapped[,6])
          dfScrapped[,6] <- as.numeric(dfScrapped[,6])
        }
      }
      
      dfList[[counter]] <- dfScrapped
      names(dfList)[counter] <- sprintf("Global%sPatch%sLaneAllRanks",patch[i],lanes[j])
      counter <- counter + 1
    }
  }
  driver$server$stop()
  lapply(1:length(dfList), function(csv4everyone) write.csv(dfList[[csv4everyone]], file=paste0(names(dfList)[csv4everyone], ".csv"), row.names=FALSE))
}
