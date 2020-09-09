propTestAgg <- function(listOfDfs) {
  library(rvest)

  counts <- 1
  champLoLists <- list()
  for (a in 1:length(listOfDfs)){
    link <- sprintf('https://na.leagueoflegends.com/en-us/news/game-updates/patch-10-%i-notes/', a)
    updatedChamps <- link %>% read_html() %>% html_nodes('.change-title') %>% html_text() %>%
      gsub('(\\t|\\n)','',.) %>%
      trimws(.,which='right')
    if (a==11){
      updatedChamps2 <- link %>% read_html() %>% html_nodes('.content-border~ .header-primary+ .content-border .ability-title') %>%
        html_text() %>%
        gsub('(\\t|\\n)','',.) %>%
        trimws(.,which='right')
      updatedChamps <- unique(c(updatedChamps, updatedChamps2))
    }
    champLoLists[[a]] <- updatedChamps
  }
  
  mergedDfList <- list()
  balancedDfList <- list()
  riskDfList <- list()
  compareDfList <- list()
  listOfListsCompare <- list()
  
  for (b in 1:length(listOfDfs)){
    if (b<length(listOfDfs)){
      mergedDf <- merge(x=listOfDfs[[b]][,c(1,2,3,6)], 
                        y=listOfDfs[[(b+1)]][,c(1,2,3,6)],
                        by.x='champName', by.y='champName')
      
      suffixesDf <- c('x','y')
      
      for (c in 1:length(suffixesDf)){
        mergedDf[,sprintf('winRate.%s',suffixesDf[c])] <- mergedDf[,sprintf('winRate.%s',suffixesDf[c])]*0.01
        mergedDf[,sprintf('gamesWon.%s',suffixesDf[c])] <- round(
          mergedDf[,sprintf('gamesPlayed.%s',suffixesDf[c])] * mergedDf[,sprintf('winRate.%s',suffixesDf[c])])
      }
      mergedDf <- mergedDf[(mergedDf$laneRate.x >= 7 & mergedDf$laneRate.y >= 7),]
      balancedDf <- mergedDf[(mergedDf[,1] %in% champLoLists[[b]]) | (mergedDf[,1] %in% champLoLists[[b+1]]),]
      mergedDf <- mergedDf[!(mergedDf[,1] %in% champLoLists[[b]]) & !(mergedDf[,1] %in% champLoLists[[b+1]]),]
      
      balancedDf <- balancedDf[,c(1,3,4,8,6,7,9)]
      mergedDf <- mergedDf[,c(1,3,4,8,6,7,9)]
      
      rownames(balancedDf) <- NULL
      rownames(mergedDf) <- NULL
      
      HaTests <- c('greater','less','two.sided')
      HoValues <- c('lessequal','greaterequal','equal')
      
      for (j in 1:3){
        listOfPValues <- list()
        counter <- 1
        for (k in 1:length(mergedDf[,1])){
          pValues <- prop.test(x=c(mergedDf[k,7], mergedDf[k,4]), 
                               n=c(mergedDf[k,6],mergedDf[k,3]),
                               alternative = HaTests[j],
                               correct = FALSE)
          listOfPValues[[counter]] <-  pValues[[3]]
          counter <- counter + 1
        }
        mergedDf[,sprintf('HO%s',HoValues[j])] <- round(unlist(listOfPValues),4)
      }

      mergedDfList[[b]] <- mergedDf
      balancedDfList[[b]] <- balancedDf
      
      riskDf <- subset(mergedDf, HOequal <= 0.05 | HOgreaterequal <= 0.05 | HOlessequal <= 0.05)
      rownames(riskDf) <- NULL
      
      riskEst <- list()
      riskLowerCI <- list()
      riskUpperCI <- list()
      counts <- counts + 1
      print(counts)
      
      if (length(riskDf[,1])>0){
        for (f in 1:length(riskDf[,1])){
          standardError <- sqrt((riskDf[f,2]*(1-riskDf[f,2]))/riskDf[f,3] + (riskDf[f,5]*(1-riskDf[f,5]))/riskDf[f,6])
          riskDifference <- riskDf[f,2] - riskDf[f,5]
          riskLower <- riskDifference - standardError
          riskUpper <- riskDifference + standardError
          riskEst[[f]] <- riskDifference
          riskLowerCI[[f]] <- riskLower
          riskUpperCI[[f]] <- riskUpper
        }
        riskDf$riskDifference <- round(unlist(riskEst),4)
        riskDf$riskLower <- round(unlist(riskLowerCI),4)
        riskDf$riskUpper <- round(unlist(riskUpperCI),4)
      }

      riskDfList[[b]] <- riskDf
      
      mcNemarPs <- list()
      lowerBoundLists <- list()
      upperBoundLists <- list()
      listPChi <- list()

      for (g in 1:length(balancedDf[,1])){
        compareDf <- rbind(balancedDf[g,], mergedDf[,names(balancedDf)])
        
        for (m in 1:length(compareDf[,1])){
          if (m==1){
            pre <- c(rep(1, compareDf[m,4]), rep(0, compareDf[m,3]-compareDf[m,4]))
            post <- c(rep(1, compareDf[m,7]), rep(0, compareDf[m,6]-compareDf[m,7]))
            preChi <- c(compareDf[m,4],compareDf[m,3]-compareDf[m,4])
            postChi <- c(compareDf[m,7],compareDf[m,6]-compareDf[m,7])
            } 
          else {
            pre <- c(rep(1, compareDf[1,7]), rep(0, compareDf[1,6]-compareDf[1,7]))
            post <- c(rep(1, compareDf[m,7]), rep(0, compareDf[m,6]-compareDf[m,7]))
            preChi <- c(compareDf[1,7],compareDf[1,6]-compareDf[1,7])
            postChi <- c(compareDf[m,7],compareDf[m,6]-compareDf[m,7])
          }
          #begin chi square
          chiTable <- as.table(rbind(preChi,postChi))
          chipValue <- chisq.test(chiTable, correct = FALSE)[[3]]
          listPChi[[m]] <- chipValue
          
          #begin mcnemar
          max.len <- max(length(pre), length(post))
          pre <- c(sample(pre), rep(NA, max.len - length(pre)))
          post <- c(sample(post), rep(NA, max.len - length(post)))
          contingencyTables <- table(na.omit(data.frame(pre,post)))
          contingencyTables <- contingencyTables[2:1,2:1]
          mcnemarP <- mcnemar.test(contingencyTables, correct = FALSE)[[3]]
          contingencyTables <- addmargins(contingencyTables)
          n12 <- contingencyTables[1,2]
          n21 <- contingencyTables[2,1]
          nsize <- contingencyTables[3,3]
          differenceEst <- n12/nsize - n21/nsize
          varianceEst <- abs((n12/nsize)*(1-(n12/nsize)) + (n21/nsize)*(1-n21/nsize) + 2 *(n12*n21/nsize^2))/nsize
          lowerBoundCI <- differenceEst - (1.96 * sqrt(varianceEst))
          upperBoundCI <- differenceEst + (1.96 * sqrt(varianceEst))
          mcNemarPs[[m]] <- mcnemarP
          lowerBoundLists[[m]] <- lowerBoundCI
          upperBoundLists[[m]] <- upperBoundCI
          #end mcnemar
        }
        
        compareDf$mcNemarP <- round(unlist(mcNemarPs),4)
        compareDf$lbound95 <- round(unlist(lowerBoundLists),4)
        compareDf$ubound95 <- round(unlist(upperBoundLists),4)
        compareDf$chiP <- round(unlist(listPChi),4)
        rownames(compareDf) <- NULL
        compareDfList[[g]] <- compareDf
      }
      listOfListsCompare[[b]] <- compareDfList  
      listOfListsCompare <<- listOfListsCompare
      compareDfList <<- compareDfList
      mergedDfList <<- mergedDfList
      riskDfList <<- riskDfList
      balancedDfList <<- balancedDfList
      
    }
    else{
      break
    }
  }
}
