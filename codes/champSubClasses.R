library(RSelenium)
library(rvest)

driver <- rsDriver[[browser='chrome']]
remote_driver <- driver[['client']]
remote_driver$navigate('https://www.mobafire.com/league-of-legends/tier-list/all-champion-classes-which-playstyle-do-you-like-2374')
mobafire <- remote_driver$getPageSource()[[1]]
driver$server$stop()

champClasses <- mobafire %>% 
  read_html() %>% html_nodes('#tier__bottom h3 , #tier__bottom li') %>%
  html_attr('champ')
delimitedChamps <- split(champClasses[!is.na(champClasses)], 
                         cumsum(is.na(champClasses))[!is.na(champClasses)]
                         )
champSubClasses <- mobafire %>%
  read_html() %>% html_nodes('#tier__bottom h3') %>%
  html_text()

filteredWords <- c('Tier', as.character(c(1:13)))

filteringWords <- function(x) {
  x <- unlist(strsplit(x, " "))
  x <- x[!x %in% filteredWords]
  paste(
    paste(toupper(substring(x, 1,1)), substring(x,2),
          sep="", collapse=" "),
    collapse= " ")
}
names(delimitedChamps) <- sapply(champSubClasses, filteringWords)
champsSubclasses <- stack(setNames(delimitedChamps, names(delimitedChamps)))
champsSubclasses[122,1] <- 'Nunu'
write.csv(champsSubclasses, 'champsSubclasses.csv')
