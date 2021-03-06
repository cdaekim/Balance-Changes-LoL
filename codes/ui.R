library(shiny)
library(shinydashboard)
library(DT)

header <- dashboardHeader(
    title = 'Statistical Testing for Champion Balance Changes - League of Legends',
    titleWidth = 800
)

sidebar <- dashboardSidebar(disable=T)

body <- dashboardBody(
  fluidRow(
    column(width=12,
           box(
             title='Filtering Options',
             width=NULL,
             column(width=6,
               selectInput(
               inputId = 'lanes',
               label = 'Lanes',
               choices = c(
                 'Top' = 'Top',
                 'Jungle' = 'Jungle',
                 'Middle' = 'Middle',
                 'Bottom' = 'Bottom',
                 'Support' = 'Support'
               ),
               selected = 'Top'
             )),
             column(width=6,
             selectInput(
               inputId = 'patches',
               label = 'Patches',
               choices = c(
                 'Patch 10.1 + 10.2' = 1,
                 'Patch 10.2 + 10.3' = 2,
                 'Patch 10.3 + 10.4' = 3,
                 'Patch 10.4 + 10.5' = 4,
                 'Patch 10.5 + 10.6' = 5,
                 'Patch 10.6 + 10.7' = 6,
                 'Patch 10.7 + 10.8' = 7,
                 'Patch 10.8 + 10.9' = 8,
                 'Patch 10.9 + 10.10' = 9,
                 'Patch 10.10 + 10.11' = 10,
                 'Patch 10.11 + 10.12' = 11,
                 'Patch 10.12 + 10.13' = 12,
                 'Patch 10.13 + 10.14' = 13
               ),
               selected = 1
             )
           ))
    )
  ),
  fluidRow(
    box(
      width=12,
      title = 'Aggregate Statistics for Lane and Patches',
      fluidRow(
        valueBoxOutput('patchValue1', width=4),
        infoBoxOutput('winRatePatch1',width=4),
        infoBoxOutput('totalGames1',width=4)
      ),
      fluidRow(
        valueBoxOutput('patchValue2', width=4),
        infoBoxOutput('winRatePatch2',width=4),
        infoBoxOutput('totalGames2',width=4)
      )
    )
  ),
  fluidRow(
    column(width=12,
      box(
          width=NULL,
          title="All Lane Champions",
          DT::dataTableOutput('testingAll')
          )
      )
  ),
  
  fluidRow(
    box(
      width=12,
      title = 'Charts',
      fluidRow(
        box(
          width=12,
          title="Distributions of Selected Champion's Current and Previous Patch Win Rates in the Chosen Lane",
          plotOutput('champDist')
        )
      ),
      fluidRow(
        box(
          width=12,
          title='Champion Stat Options',
          checkboxGroupInput(
            inputId = 'basestats',
            label = 'Base Stats',
            choices = list('Base HP'=2,
                           'HP Regeneration'=4,
                           'Base MP' = 6,
                           'MP Regeneration' = 8,
                           'Base AD' = 10,
                           'Base Attack Speed' = 12,
                           'Base Armor' = 14,
                           'Base MR' =16
            ),
            selected= 2,
            inline=T
          )
        )
      ),
      fluidRow(
        box(
          width=12,
          title='Base Stats',
          plotOutput('BaseStats')
        )
      ),
      fluidRow(
        box(
          width=6,
          title='Previous Patch: Relationship between Game Shares and Win Rates',
          plotOutput('gameShares')
        ),
        box(
          width=6,
          title='Current Patch: Relationship between Game Shares and Win Rates',
          plotOutput('testShares')
        )
      ),
      fluidRow(
        box(
          width=12,
          title='Win Rates of Subclasses in Chosen Lane',
          plotOutput('subGroups')
        )
      )
    )
  ),
  fluidRow(
    box(width=12,
      infoBoxOutput('numChamps2',width=6),
      infoBoxOutput('numChamps1',width=6)
    )
  ),
  fluidRow(
        column(width=12,
               tabBox(
                 title="Dataset of Unbalanced Champions' Win Rates and p-values from Z-Testing",
                 id = 'different',
                 width=NULL,
                 tabPanel(title='Unbalanced',
                          value='Merged',
                          DT::dataTableOutput('testOutputPaste1')
                          ),
                 tabPanel(title='Risk', 
                          DT::dataTableOutput('testOutputPaste2')),
                 tabPanel('Balanced',
                          DT::dataTableOutput('balancedChampions1')),
                 tabPanel('Comparison')
                 
                 )
               )
        )
)


ui <- dashboardPage(header, sidebar, body)