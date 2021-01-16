library(shiny)
library(GGally)
library(devtools) 
library(tidyverse)
library(lubridate)
install_github("ramnathv/rCharts")
library(rCharts)
library(fmsb)
library(plotly)

stats = read.csv("/Users/hugo/Documents/upm/cuatrimestre1/BigData/projects/DV2/nba_stats/stats.csv", 
                 header = TRUE)

players <- (stats$PLAYER_NAME)
tmp = as.factor(unique(players))

# Define UI ----
ui <- fluidPage(
  headerPanel("Data Visualization Project"),
  tabsetPanel(type = 'tabs',
    tabPanel("Evolution of players",
      sidebarLayout(
        sidebarPanel(
          img(src = "nba.png", height = 100, width = 200, style="display: block; margin-left: auto; margin-right: auto;", style="text-align: center;"),
          p(""),
          helpText("See how players of the NBA have changed along the years."),
          selectInput(inputId = "playersInput", 
                      label = "Select a player",
                      choices = levels(tmp),
                      selected = levels(tmp)[1], 
                      #multiple = TRUE
          ),
          selectInput("registerData","Choose a variable to display", 
                      choices = list("Points", "Assists",
                                  "Blocks", "Rebounds"),
                      selected = "Points"
          ),
          sliderInput("slider", h3("Range of seasons:"), min = 2003, max = 2020, 
                      value = c(min,max)
          )
        ),
        mainPanel(
          verticalLayout(
            plotlyOutput("lineChart1"),
            splitLayout(
              plotlyOutput("lineChart2"),
              plotlyOutput("lineChart3")
            )
          )
        )
      )
    ),
    tabPanel("Compare Players",
      sidebarLayout(
        sidebarPanel(
          img(src = "nba.png", height = 100, width = 200, style="display: block; margin-left: auto; margin-right: auto;", style="text-align: center;"),
          p(""),
          helpText("See how players of the NBA have changed along the years."),
          selectizeInput("playersInput","Player", h3("Choose a player"), 
                        selected = NULL, 
                        #multiple = TRUE
          ),
          selectInput("registerData","Choose a variable to display", 
                      choices = list("Points", "Assists",
                                    "Blocks", "Rebounds"),
                      selected = "Points"
          ),
          sliderInput("slider", h3("Range of seasons:"), min = 2010, max = 2020, 
                      value = c(min,max)
          )
        ),
        mainPanel(
          
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$lineChart1 <- renderPlotly({
    player_selected = input$playersInput
    minimum <- input$slider[1]
    maximum <- input$slider[2]
    lines_data <- subset(stats, PLAYER_NAME == player_selected & SEASON >= minimum & SEASON <= maximum)
    #lines_data <- subset(stats, PLAYER_NAME == player_selected)
    
    register <- switch(input$registerData,
                       "Points" = lines_data$PTS,
                       "Assists" = lines_data$AST,
                       "Blocks" = lines_data$BLK,
                       "Rebounds" = lines_data$REB)
    
    if(is_empty(register)){
      text = paste(toString(player_selected),"wasn't playing in the NBA at that season/s")
      ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = text) + 
        theme_void()
    }else{
      #ggplot(lines_data, aes(x = 1:length(register), y = register)) +
      #     geom_line(aes(colour = "red")) +
      #     geom_point() + 
      #     labs(x = "Games", y = input$registerData)
      plot_ly(lines_data, x = 1:length(register), y = register, type = 'scatter', mode = 'lines+markers')
      
    }
  })
  output$lineChart2 <- renderPlotly({
    player_selected = input$playersInput
    minimum <- input$slider[1]
    maximum <- input$slider[2]
    lines_data <- subset(stats, PLAYER_NAME == player_selected & SEASON >= minimum & SEASON <= maximum)
    register <- switch(input$registerData,
                       "Points" = cbind(lines_data$PTS,lines_data$SEASON),
                       "Assists" = cbind(lines_data$AST,lines_data$SEASON),
                       "Blocks" = cbind(lines_data$BLK,lines_data$SEASON),
                       "Rebounds" = cbind(lines_data$REB,lines_data$SEASON))
    register = data.frame(register)
    register = na.omit(register)
    char_columns <- sapply(register, is.character)             
    register[ , char_columns] <- as.data.frame(   
      apply(register[ , char_columns], 2, as.numeric))
    
    seasons = as.factor(register$X2)
    tmp1 <- array()
    tmp2 <- array()
    for (i in levels(seasons)){
      tmp0 = filter(register, X2 == i)
      tmp1 = append(tmp1,round(sum(tmp0$X1)/length(tmp0$X1),2))
      tmp2 = append(tmp2, i)
    } 
    medias <- cbind(tmp2,tmp1)
    medias = data.frame(medias)
    medias = na.omit(medias)
    char_columns <- sapply(medias, is.character)             
    medias[ , char_columns] <- as.data.frame(   
      apply(medias[ , char_columns], 2, as.numeric))
    
    medias$tmp2 <- as.factor(medias$tmp2)

    #ggplot(medias, aes(x = medias$tmp2, y = medias$tmp1)) +
    #  geom_bar(stat = "identity", color="darkblue", fill="white") +
    #  labs(x = "Season", y = input$registerData)
    plot_ly(lines_data, x = tmp2, y = tmp1, type = "bar", color = ~tmp2)
  })
  output$lineChart3 <- renderPlotly({
    player_selected = input$playersInput
    lines_data <- subset(stats, PLAYER_NAME == player_selected)
    lines_data = select(lines_data, PTS, AST, REB, STL, BLK)
    medias <- apply(lines_data, 2, mean)
    medias = data.frame(t(medias))
    medias_glob = select(stats, PTS, AST, REB, STL, BLK)
    medias_glob <- apply(medias_glob, 2, mean)
    medias_glob = data.frame(t(medias_glob))
    plot_ly(type = 'parcoords',
            dimensions = list(
              list(range = c(0,max(round(medias$PTS+2),round(medias_glob$PTS+2))),
                   constraintrange = c(0,1),
                   label = 'Points', values = c(medias$PTS,medias_glob$PTS)),
              list(range = c(0,max(round(medias$AST+2),round(medias_glob$AST+2))),
                   label = 'Assists', values = c(medias$AST,medias_glob$AST)),
              list(range = c(0,max(round(medias$BLK+2),round(medias_glob$BLK+2))),
                   label = 'Blocks', values = c(medias$BLK,medias_glob$BLK)),
              list(range = c(0,max(round(medias$REB+2),round(medias_glob$REB+2))),
                   label = 'Rebounds', values = c(medias$REB,medias_glob$REB)),
              list(range = c(0,max(round(medias$STL+2),round(medias_glob$STL+2))),
                   label = 'Steals', values = c(medias$STL,medias_glob$STL))
            )
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)