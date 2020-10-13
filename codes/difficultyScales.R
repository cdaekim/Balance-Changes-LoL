library(rvest)
link <- 'https://leagueoflegends.fandom.com/wiki/List_of_champions/Ratings'

#name
Champions <- link %>% read_html() %>% html_nodes('td > .link-internal+ a') %>% html_text()
#phys/ability damage
BasicAbilityScale <- link %>% read_html() %>% html_nodes('.champion_style span:nth-child(2)') %>% html_attr(.,name='title')

columnsScrapped <- c(4,5,6,7,8,11)
columnsNames <- c('Damage', 'Toughness', 'Control', 'Mobility', 'Utility', 'Difficulty')
myList <- list()

for (i in 1:length(columnsScrapped)){
  myList[[i]] <- link %>% read_html() %>% html_nodes(sprintf('td:nth-child(%s)', columnsScrapped[i])) %>% html_text()
}

names(myList) <- columnsNames

difficultyDf <- data.frame(Champions=Champions, BasicAbilityScale, myList)
difficultyDf[,1] <- as.character(difficultyDf[,1])
difficultyDf[,2]<-as.numeric(as.character(difficultyDf[,2]))

difficultyDf[85,1] <- 'Nunu'
write.csv(difficultyDf, file='difficultyDf.csv', row.names = F)
