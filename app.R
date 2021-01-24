library(shiny)
library(GGally)
library(devtools) 
library(tidyverse)
library(lubridate)
library(rCharts)
library(fmsb)
library(plotly)
library(factoextra)
library(quantreg)


stats = read.csv("nba_stats/stats.csv", 
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
              ),tabPanel("Compare Players",
                         sidebarLayout(
                           sidebarPanel(
                             img(src = "nba.png", height = 100, width = 200, style="display: block; margin-left: auto; margin-right: auto;", style="text-align: center;"),
                             p(""),
                             helpText("Compare the different players of the NBA along the years"),
                             selectInput(inputId = "playersInput1", 
                                         label = "Select player 1",
                                         choices = levels(tmp),
                                         selected = "James Harden"
                             ),
                             selectInput(inputId = "playersInput2", 
                                         label = "Select player 2",
                                         choices = levels(tmp),
                                         selected = "Russell Westbrook"
                             ),
                             selectInput("registerData1","Choose a variable to display", 
                                         choices = list("Points", "Assists",
                                                        "Blocks", "Rebounds"),
                                         selected = "Points"
                             ),
                             selectInput("registerData2","Choose a variable to display", 
                                         choices = list("Field Goal %",
                                                        "Field Goal 3p %"),
                                         selected = "Field Goal %"
                             )
                           ),
                           mainPanel(
                             verticalLayout(
                               plotlyOutput("lineChart4"),
                               splitLayout(
                                 plotlyOutput("lineChart5"),
                                 plotlyOutput("lineChart6")
                               )
                             )
                           )
                         )
              ),
              tabPanel("Clustering",
                       sidebarLayout(
                         sidebarPanel(
                           img(src = "nba.png", height = 100, width = 200, style="display: block; margin-left: auto; margin-right: auto;", style="text-align: center;"),
                           p(""),
                           helpText("Clustering to identify positions and players."),
                           selectInput("registerData3","Choose X variable to display", 
                                       choices = list("Points", "Assists",
                                                      "Blocks", "Rebounds", 
                                                      "Free throws", "Steals"),
                                       selected = "Points"
                           ),
                           selectInput("registerData4","Choose Y variable to display", 
                                       choices = list("Points", "Assists",
                                                      "Blocks", "Rebounds",
                                                      "Free throws", "Steals"),
                                       selected = "Assists"
                           )
                         ),
                         mainPanel(
                           verticalLayout(
                             plotOutput("lineChart7"),
                             splitLayout(
                               plotOutput("lineChart8"),
                               plotOutput("lineChart9")
                             )
                           )
                         )
                       )
              )
  )
);

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
      
      plot_ly(lines_data, x = 1:length(register), y = register, type = 'scatter',
              mode = 'lines+markers',
              text = paste(input$registerData, ": ", register,
                           "<br>Match: ", lines_data$GAME_DATE_EST),
              hoverinfo = 'text') %>% layout(title = "Evolution of the player", 
                                             xaxis = list(title = "Date"),
                                             yaxis = list(title = input$registerData))
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
    plot_ly(lines_data, x = tmp2, y = tmp1, type = "bar", color = ~tmp2) %>% layout(xaxis = list(title = "Season"),yaxis = list(title = input$registerData))
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
  output$lineChart4 <- renderPlotly({
    player_selected1 = input$playersInput1
    player_selected2 = input$playersInput2
    lines_data <- subset(stats, PLAYER_NAME == player_selected1 | PLAYER_NAME  == player_selected2)
    lines_data <- lines_data %>%
      group_by(SEASON, PLAYER_NAME) %>%
      summarise(PTS=(mean(PTS)),AST=(mean(AST)),REB=(mean(REB)),STL=(mean(STL)),BLK=(mean(BLK)))
    register <- switch(input$registerData1,
                       "Points" = lines_data$PTS,
                       "Assists" = lines_data$AST,
                       "Blocks" = lines_data$BLK,
                       "Rebounds" = lines_data$REB)
    p <- plot_ly(x = lines_data$SEASON,
                 y = register,
                 type = 'scatter',
                 mode = 'lines',
                 color = lines_data$PLAYER_NAME,
                 text = paste(input$registerData1, ": ", round(register,1),
                              "<br>Season: ", lines_data$SEASON,
                              "<br>Player name: ", lines_data$PLAYER_NAME),
                 hoverinfo = 'text')
    p <- p %>% layout(
      title = "Player comparison", 
      xaxis = list(title = 'Season'),
      yaxis = list(title = input$registerData1)
    )
    p
  })
  output$lineChart5 <- renderPlotly({
    player_selected1 = input$playersInput1
    player_selected2 = input$playersInput2
    lines_data <- subset(stats, PLAYER_NAME == player_selected1 | PLAYER_NAME  == player_selected2)
    lines_data <- lines_data %>%
      group_by(SEASON, PLAYER_NAME) %>%
      summarise(FG3M=sum(FG3M),FG3A=sum(FG3A),FGM=sum(FGM),FGA=sum(FGA))
    lines_data$FG3_PCT <- (lines_data$FG3M/lines_data$FG3A)*100
    lines_data$FG_PCT <- (lines_data$FGM/lines_data$FGA)*100
    register <- switch(input$registerData2,
                       "Field Goal %" = lines_data$FG_PCT,
                       "Field Goal 3p %" = lines_data$FG3_PCT)
    p <- plot_ly(
      y = lines_data$SEASON,
      x = register,
      type = 'bar',
      orientation = 'h',
      color = lines_data$PLAYER_NAME,
      hoverinfo = 'text',
      hovertext = paste(input$registerData2,": ", round(register,1),
                        "<br>Season: ", lines_data$SEASON,
                        "<br>Player name: ", lines_data$PLAYER_NAME)
    )
    
    p <- p %>% layout(
      yaxis = list(title = 'Season'),
      xaxis = list(title = input$registerData2),
      barmode = 'stack'
    )
    p
  })
  output$lineChart6 <- renderPlotly({
    player_selected1 = input$playersInput1
    player_selected2 = input$playersInput2
    lines_data <- subset(stats, PLAYER_NAME == player_selected1 | PLAYER_NAME  == player_selected2)
    lines_data <- lines_data %>%
      group_by(SEASON, PLAYER_NAME) %>%
      summarise(PLUS_MINUS=(mean(PLUS_MINUS)))
    p <- plot_ly(
      x = lines_data$PLUS_MINUS,
      y = lines_data$SEASON,
      type = 'bar',
      orientation = 'h',
      color = lines_data$PLAYER_NAME,
      text = paste("Plus minus: ", round(lines_data$PLUS_MINUS,0),
                   "<br>Season: ", lines_data$SEASON,
                   "<br>Player name: ", lines_data$PLAYER_NAME),
      hoverinfo = 'text'
    )
    
    p <- p %>% layout(
      yaxis = list(title = 'Season'),
      xaxis = list(title = 'Plus minus')
    )
    p
  })
  output$lineChart7 <- renderPlot({
    lines_data <- subset(stats)
    lines_data <- lines_data %>%group_by(PLAYER_NAME) %>% summarise(PTS=(mean(PTS)),FTM=(mean(FTM)),FGM=(mean(FGM)),AST=(mean(AST)),REB=(mean(REB)),STL=(mean(STL)),BLK=(mean(BLK)))
    lines_data2 <- lines_data[,-1]
    
    XVariable <- switch(input$registerData3,
                        "Points" = lines_data$PTS,
                        "Assists" = lines_data$AST,
                        "Blocks" = lines_data$BLK,
                        "Rebounds" = lines_data$REB,
                        "Free throws" = lines_data$FTM,
                        "Field Goals made" = lines_data$FGM,
                        "Steals" = lines_data$STL)
    
    YVariable <- switch(input$registerData4,
                        "Points" = lines_data$PTS,
                        "Assists" = lines_data$AST,
                        "Blocks" = lines_data$BLK,
                        "Rebounds" = lines_data$REB,
                        "Free throws" = lines_data$FTM,
                        "Field Goals made" = lines_data$FGM,
                        "Steals" = lines_data$STL)
    
    print(class(XVariable))
    print(class(YVariable))
    
    k1 <- kmeans(lines_data2, centers=5)
    plot1 <- ggplot(lines_data, aes(XVariable, YVariable, color = k1$cluster)) + geom_point() + theme(legend.position = "top") + labs(x=input$registerData3, y = input$registerData4)
    plot1 + scale_color_gradient(low = "blue", high = "red")
    
  })
  output$lineChart8 <- renderPlot({
    lines_data <- subset(stats)
    lines_data <- lines_data %>%group_by(PLAYER_NAME) %>% summarise(PTS=(mean(PTS)),FTM=(mean(FTM)),FG3M=(mean(FG3M)),FGM=(mean(FGM)),AST=(mean(AST)),REB=(mean(REB)),STL=(mean(STL)),BLK=(mean(BLK)))
    lines_data2 <- lines_data[,-1]
    k2 <- kmeans(lines_data2, centers=5)
    fviz_cluster(k2, data=lines_data2)
  })
  output$lineChart9 <- renderPlot({
    lines_data <- subset(stats)
    lines_data <- lines_data %>%group_by(PLAYER_NAME) %>% summarise(PTS=(mean(PTS)),FTM=(mean(FTM)),FG3M=(mean(FG3M)),FGM=(mean(FGM)),AST=(mean(AST)),REB=(mean(REB)),STL=(mean(STL)),BLK=(mean(BLK)))
    lines_data2 <- lines_data[,-1]
    res <- hcut(lines_data2, k = 5, stand = TRUE)
    fviz_dend(res, rect = TRUE, cex = 0.5)
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)
