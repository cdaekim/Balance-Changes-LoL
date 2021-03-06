library(shiny)
library(shinydashboard)
library(rvest)
library(ggplot2)
library(plyr)
library(meta)
library(metafor)
library(ggthemes)
library(tidyverse)
library(ggforce)
library(ggridges)
library(ggrepel)
library(tidyverse)
server <- function(input, output, session){
  set.seed(23)
  columnsNice <- c("Champions", 
                   "Previous Win Rates", "Previous Games Played", "Previous Games Won",
                   "Current Win Rates", "Current Games Played", "Current Games Won",
                   "HO Lesser", "HO Greater", "HO Equal")

  difficultyDf <- read.csv(file='https://raw.githubusercontent.com/cdaekim/Balance-Changes-LoL/master/csv/difficultyDf.csv')
  statsDf <- read.csv(file='https://raw.githubusercontent.com/cdaekim/Balance-Changes-LoL/master/csv/StatsClassesVariables.csv', stringsAsFactors = F)
  subclassDf <- statsDf[,c(1,20)]
  
  
  
  
  allData <- reactive({
    firstPatch <- read.csv(file= sprintf(
      'https://raw.githubusercontent.com/cdaekim/Balance-Changes-LoL/master/csv/%s/Global10.%sPatch%sLaneAllRanks.csv',
      tolower(input$lanes),input$patches,tolower(input$lanes)),
      stringsAsFactors = F
      )
    secondPatch <- read.csv(file= sprintf(
      'https://raw.githubusercontent.com/cdaekim/Balance-Changes-LoL/master/csv/%s/Global10.%sPatch%sLaneAllRanks.csv',
      tolower(input$lanes),as.character(as.numeric(input$patches)+1),tolower(input$lanes)),
      stringsAsFactors = F
    )
    mergedDf <- merge(x=firstPatch[,c(1,2,3,6)], 
                      y=secondPatch[,c(1,2,3,6)],
                      by.x='champName', by.y='champName')
    suffixesDf <- c('x','y')
    
    for (c in 1:length(suffixesDf)){
      mergedDf[,sprintf('winRate.%s',suffixesDf[c])] <- mergedDf[,sprintf('winRate.%s',suffixesDf[c])]*0.01
      mergedDf[,sprintf('gamesWon.%s',suffixesDf[c])] <- round(
        mergedDf[,sprintf('gamesPlayed.%s',suffixesDf[c])] * mergedDf[,sprintf('winRate.%s',suffixesDf[c])])
    }
    mergedDf <- mergedDf[(mergedDf$laneRate.x >= 7 & mergedDf$laneRate.y >= 7),]
    mergedDf <- mergedDf[,c(1,3,4,8,6,7,9)]
    
    mergedDf <- merge(x=mergedDf,
                      y=subclassDf,
                      by.x='champName',
                      by.y='X')
    
    rownames(mergedDf) <- NULL
    colnames(mergedDf) <- c(head(columnsNice, -3),'Subclasses')

    return(mergedDf)
  })
  myData <- reactive({
    read.csv(
      file= (
        if (input$lanes=='Bottom' & (input$different =='Merged'|input$different =='Risk'|input$different =='Balanced')){
          sprintf(
            'https://raw.githubusercontent.com/cdaekim/Balance-Changes-LoL/master/csv/%s/%s/%s%s%s.csv',
            tolower(input$lanes),tolower(input$different),input$lanes,input$different,input$patches
          )
        } else {
          sprintf(
            'https://raw.githubusercontent.com/cdaekim/Balance-Changes-LoL/master/csv/%s/%s/%s%s%s.csv',
            tolower(input$lanes),input$different,input$lanes,input$different,input$patches
          )
        }
        
      ), 
      col.names = if(input$different=='Merged'){
        columnsNice
      } else if(input$different=='Risk'){
        c(columnsNice,'Risk Difference','Lower Conf.Int','Upper Conf.Int')
      },
      stringsAsFactors = FALSE
    )
  })
  scalesData <- reactive({
    mergedDf <- merge(x=allData()[,c(1,ncol(allData()))],
                      y=difficultyDf[,1:8],
                      by.x=Champions,
                      by.y=Champions)
  })
  output$testingAll <- DT::renderDataTable({
    DT::datatable(
      allData(),
      options = list(
        autoWidth=T,
        scrollX=T
      ),
      selection = list(mode='single', selected=c(1))
      
    )
  })
  balancedChampions <- reactive({
    read.csv(
      sprintf(
        'https://raw.githubusercontent.com/cdaekim/Balance-Changes-LoL/master/csv/%s/Balanced/%sBalanced%s.csv',
        tolower(input$lanes),input$lanes,input$patches), 
      col.names = c("Champions", 
                    "Previous Win Rates", "Previous Games Played", "Previous Games Won",
                    "Current Win Rates", "Current Games Played", "Current Games Won"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$balancedChampions1 <- DT::renderDataTable({
    DT::datatable(
      balancedChampions())
  })

  output$testOutputPaste1 <- DT::renderDataTable({
   DT::datatable(
    myData(), 
    options = list(
      autoWidth=T,
      scrollX=T
      )
      )
    })
  
  output$testOutputPaste2 <- DT::renderDataTable({
    DT::datatable(
      myData(),
      options = list(
        autoWidth=T,
        scrollX=T
      ),
      selection='single'
    )
  })
  
  output$Histograms <- renderPlot({
    ggplot(data=stack(myData()[,c(2,5)]), aes(x=values, color=ind)) +
      geom_density(alpha=0.4) +
      geom_vline(data=ddply(stack(myData()[,c(2,5)]), 'ind', summarise, grp.mean=mean(values)),
                 aes(xintercept=grp.mean, color=ind), linetype='dashed') +
      scale_color_manual(values = c('purple','orange')) +
      labs(x='Win Rates') +
      guides(color=guide_legend(title='Legend'))
  })
  output$subGroups <- renderPlot({
    
    metaProps <- metaprop(event=allData()[,7],
                          n=allData()[,6],
                          data=allData(),
                          sm='PRAW',
                          byvar=allData()[,8],
                          studlab = allData()[,1],
                          comb.fixed = T,
                          comb.random = F)
    metaProps1 <- metaprop(event=allData()[,4],
                          n=allData()[,3],
                          data=allData(),
                          sm='PRAW',
                          byvar=allData()[,8],
                          studlab = allData()[,1],
                          comb.fixed = T,
                          comb.random = F)
    
    metaPropsAgg <- data.frame(Subclasses = metaProps$bylevs,
                               props = metaProps$TE.fixed.w,
                               sError = metaProps$seTE.fixed.w)
    metaPropsAgg1 <- data.frame(Subclasses = metaProps1$bylevs,
                               props = metaProps1$TE.fixed.w,
                               sError = metaProps1$seTE.fixed.w)
    
    runningDfList <- list()
    for (g in 1:length(metaPropsAgg[,1])){
      runningDf <- data.frame(Subclass = rep(metaPropsAgg[g,1],10), 
                              norms = rnorm(10, metaPropsAgg[g,2], metaPropsAgg[g,3]),
                              Patch = rep('Patch 2', 10)
                              )
      runningDfList[[g]] <- runningDf
    }
    
    runningDfList1 <- list()
    for (gg in 1:length(metaPropsAgg1[,1])){
      runningDf <- data.frame(Subclass = rep(metaPropsAgg1[gg,1],10), 
                              norms = rnorm(10, metaPropsAgg1[gg,2], metaPropsAgg1[gg,3]),
                              Patch = rep('Patch 1', 10)
      )
      runningDfList1[[gg]] <- runningDf
    }
    
    aggDfs <- data.table::rbindlist(c(runningDfList, runningDfList1))
    
    colnames(aggDfs) <- c('Subclass', 'norms','Patch')
    ggplot(aggDfs, aes(x=norms, y=Subclass, fill=paste(Subclass, Patch))) + 
      stat_density_ridges(quantile_lines=T, quantiles = c(0.025,0.975), alpha=0.8,scale=5, rel_min_height=0.001, color='white') +
      xlab('Win Rates') +
      scale_fill_cyclical(
        breaks = c("Marksmen Patch 1", "Marksmen Patch 2"),
        labels = c(`Marksmen Patch 1` = "Previous Patch", `Marksmen Patch 2` = "Current Patch"),
        values = c("#605ca8", "#f39c12"),
        name = "Legend", guide = "legend"
      ) +
      theme_ridges()

  })
  observe({
    selRow <- input$testingAll_rows_selected
    classPicked <- allData()[selRow,8]
    champPicked <- allData()[selRow,1]
    mapping <-list('Base HP'=2,
                   'HP Regeneration'=4,
                   'Base MP' = 6,
                   'MP Regeneration' = 8,
                   'Base AD' = 10,
                   'Base Attack Speed' = 12,
                   'Base Armor' = 14,
                   'Base MR' =16)
    WinsPlayedShare <- function(vectorsAllData){
      vectorsPatch <- c('Played', 'Won')
      #aggregate all played games grouped by subclasses
      winsDf <- aggregate(allData()[,vectorsAllData[1]]~allData()[,8], allData(),sum)
      winsDf$group <- rep('Game Shares', length(winsDf[,1]))
      #aggregate all won games grouped by subclasses
      winsDf1 <- aggregate(allData()[,vectorsAllData[2]]~allData()[,8], allData(),sum)
      winsDf1[,3] <- exp(round(winsDf1[,2]/winsDf[,2] * 100,2))/10^21
      winsDf[,2] <- round(winsDf[,2]/sum(winsDf[,2])*100,2)
      winsDf1$group <- rep('Win Rates', length(winsDf1[,1]))
      
      winsDf <- merge(winsDf[,1:3], winsDf1[,c(1,3:4)], by.x=names(winsDf)[1], by.y=names(winsDf1)[1])
      names(winsDf) <- c('Subclass','Games','Group1','Win Rates','Group2')
      winsDf <- reshape(winsDf, direction='long',
                        varying = list(c('Games','Win Rates'),
                                       c('Group1', 'Group2')),
                        v.names = c('Percentages','Group'))
      
      winsDf <- winsDf[,-c(2,5)]
      
      winsDf$Group <- as.factor(winsDf$Group)
      winsDf <- winsDf %>% arrange(Group, Percentages)
      
      empty_bar <- 4
      to_add <- data.frame(matrix(NA, empty_bar*nlevels(winsDf$Group), ncol(winsDf)))
      colnames(to_add) <- colnames(winsDf)
      to_add$Group <- rep(levels(winsDf$Group), each=empty_bar)
      winsDf <- rbind(winsDf, to_add)
      winsDf <- winsDf %>% arrange(Group)
      winsDf$id <- seq(1, nrow(winsDf))
      
      label_data <- winsDf
      num_bar <- nrow(label_data)
      angle <- 90-360*(label_data$id-0.5)/num_bar
      label_data$hjust <- ifelse(angle < -90, 1, 0)
      label_data$angle <- ifelse(angle < -90, angle+180, angle)
      
      base_data <- winsDf %>%
        dplyr::group_by(Group) %>%
        dplyr::summarize(start=min(id), end=max(id)-empty_bar) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(title=mean(c(start,end)))
      
      ggplot(winsDf, aes(x=as.factor(id), y=Percentages, fill=Group)) +
        geom_bar(stat='identity',alpha=0.5) +
        scale_fill_manual(values = c("#f39c12","#605ca8")) +
        ylim(-100,120) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-4,5), "cm")
        ) +
        coord_polar() +
        geom_text(data=label_data,
                  aes(x=id, y=Percentages+10, label=Subclass, hjust=hjust),
                  color="black", fontface="bold",alpha=0.6, size=2.5,
                  angle= label_data$angle, inherit.aes = FALSE) +
        geom_segment(data=base_data,
                     aes(x = start, y = -5, xend = end, yend = -5),
                     color = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )+
        geom_text(data=base_data,
                  aes(x = title, y = -18, label=Group),
                  hjust=c(1,0), color = "black", alpha=0.8, size=4,
                  fontface="bold", inherit.aes = FALSE)
      
    }

    output$gameShares <- renderPlot({
      WinsPlayedShare(c(3,4))
    })

    output$testShares <- renderPlot({
      WinsPlayedShare(c(6,7))
    })

    output$champDist <- renderPlot({

      means <- c(allData()[selRow,2],allData()[selRow,5])
      sdevs <- c(sqrt(allData()[selRow,2] * (1 - allData()[selRow,2]) /
                        allData()[selRow,3]),
                 sqrt(allData()[selRow,5] * (1 - allData()[selRow,5]) /
                        allData()[selRow,6])
                 )
      normDfs <- cbind.data.frame(Patches = c('Previous Patch','Current Patch'), means, sdevs)
      
      runningDfList1 <- list()
      for (y in 1:length(normDfs[,1])){
        runningDf1 <- data.frame(Patches = rep(normDfs[y,1],100), 
                                norms <- rnorm(100, normDfs[y,2], normDfs[y,3]))
        runningDfList1[[y]] <- runningDf1
      }
      aggDfs1 <- data.table::rbindlist(runningDfList1)
      colnames(aggDfs1) <- c('Patches', 'norms')
      ggplot(aggDfs1, aes(x=norms, y=Patches, fill=Patches)) + 
        stat_density_ridges(quantile_lines=T, quantiles = c(0.025,0.975), alpha=0.2,scale=5, rel_min_height=0.001) +
        scale_fill_manual(values = c("#f39c12","#605ca8")) +
        xlab('Win Rates') +
        theme_ridges()
  
    })
    
    getBaseStats <- function(dfStats){
      statList <- list()
      cumStatsList <- list()
      for (abc in 1:length(dfStats[,1])){
        for (def in 1:18){
          if (def == 1){
            g <- dfStats[abc,as.numeric(input$basestats)]
          } else {
            g <- g + dfStats[abc,as.numeric(input$basestats)+1]
          }
          statList[[def]] <- g
        }
        cumStatsList[[abc]] <- statList
      }
      names(cumStatsList) <- names(mapping[mapping==as.numeric(input$basestats)])
      return(cumStatsList)
    }
    
    output$BaseStats <- renderPlot({
      baseStats <- statsDf[(statsDf[,1]==champPicked) | (statsDf[,20]==classPicked) | (statsDf[,21]==classPicked),]
      baseStats <- baseStats[complete.cases(baseStats),]
      listStats <- getBaseStats(baseStats)
      plotStats <- data.frame(champions = rep(baseStats[,1], each=18), 
                              level=rep(1:18, length(baseStats[,1])), 
                              values = unlist(listStats),
                              stringsAsFactors = F)
      plotStats <- plotStats %>% mutate(highlight=ifelse(champions==champPicked,'Chosen','Other'),
                                        status = ifelse(level %in% c(1:6), 'Early',
                                                        ifelse(level %in% c(7:12), 'Mid', 'Late')))
      plotStats$status <- factor(plotStats$status, levels=c('Early','Mid','Late'))

      ggplot(plotStats, aes(x=level, y=values, group=champions,color=highlight, size=highlight, alpha=highlight)) +
        geom_rect(data=data.frame(status='Early'), 
                  fill='#ffddd2', alpha=0.4, inherit.aes=F, 
                  xmin=1,xmax=6,ymin=-Inf, ymax=Inf)+
        geom_rect(data=data.frame(status='Mid'), 
                  fill='#feccc8', alpha=0.4, inherit.aes=F, 
                  xmin=6,xmax=12,ymin=-Inf, ymax=Inf)+
        geom_rect(data=data.frame(status='Late'), 
                  fill='#feb4c3', alpha=0.4, inherit.aes=F, 
                  xmin=12,xmax=18,ymin=-Inf, ymax=Inf)+
        geom_line() +
        facet_wrap(~status, scale='free')+
        scale_color_manual(values = c('red','lightgrey'),
                           name='Champions',
                           breaks=c('Chosen','Other'),
                           labels=c(champPicked, 'Others')) +
        scale_size_manual(values=c(2,1), guide=F) +
        scale_alpha_manual(values=c(1,0.5), guide=F) +
        theme_minimal() +
        ylab(names(listStats)[1])
      
      
    })
  })
  
  output$winRatePatch1 <- renderInfoBox({
    infoBox(
      'Avg Win Rates(%)', renderText(sprintf('%.2f',mean(allData()[,2])*100)),fill=T, color='purple'
      )
  })
  output$totalGames1 <- renderInfoBox({
    infoBox(
      'Total Games Analyzed', sum(allData()[,3]), fill=T, color='purple', icon=icon('globe')
      )
  })
  output$winRatePatch2 <- renderInfoBox({
    infoBox(
      'Avg Win Rates(%)', renderText(sprintf('%.2f',mean(allData()[,5])*100)),fill=T, color='yellow'
      )
  })
  output$totalGames2 <- renderInfoBox({
    infoBox(
      'Total Games Analyzed', sum(allData()[,6]), fill=T, color='yellow', icon=icon('globe')
      )
  })
  output$numChamps1 <- renderInfoBox({
   infoBox( 
     HTML(paste('Champs With',br(),'Lowered Win Rates')), sum(myData()[,9]<0.05), color='red',fill=T,icon=icon('angle-double-down')
     )
  })
  output$numChamps2 <- renderInfoBox({
    infoBox( 
      HTML(paste('Champs With',br(),'Increased Win Rates')), sum(myData()[,8]<0.05), color='green',fill=T,icon=icon('angle-double-up')
    )
  })
  output$patchValue1 <- renderValueBox({
    valueBox(
      paste0('10.', input$patches), 'Patch', color='purple'
    )
  })
  output$patchValue2 <- renderValueBox({
    valueBox(
      paste0('10.', as.numeric(input$patches)+1), 'Patch',color='orange'
    )
  })
}