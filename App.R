rm(list = ls())
library(shiny)
library(scales)
library(plyr)
library(ggplot2)
library(readxl)
library(stringr)

shinyApp(
  ui = tagList(
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "NBA charts using R Shiny",
      tabPanel("Best Player", plotOutput("plot1")),
      tabPanel("Best Scoring", plotOutput("plot2")),
      tabPanel("James Harden Ways of Scoring", plotOutput("plot3")),
      tabPanel("Top Games rating",
               sidebarPanel(
                 radioButtons("seasontype1", "",
                              c("Regular Season"="s", "Playoffs"="p"))
               ),
               mainPanel(
                 plotOutput("plot4")
               )
      ),
      tabPanel("PER rating", 
               sidebarPanel(
                 radioButtons("seasontype2", "",
                              c("Regular Season"="s", "Playoffs"="p"))
               ),
               mainPanel(
                 plotOutput("plot5")
               )
      )
    )
  ),
  
  server = function(input, output) {
    
    GameScoreRegular <- read_xlsx("Game_score_regular.xlsx")
    GameScorePlayoffs <- read_xlsx("Game_score_playoffs.xlsx")
    PER_season <- read_xlsx("PER_season.xlsx")
    PER_playoffs <- read_xlsx("PER_playoffs.xlsx")
    MVP_VOTES <- read.csv(file="MVP_VOTES.csv", header=TRUE, sep=",")
    Points_per_100 <- read.csv(file="Points_per100.csv", header=TRUE, sep=",")
    Season_stats <- read_xlsx("Season_stats_total.xlsx")
    
    
    stats_wsc <- Season_stats[Season_stats$Pos=='SG'| Season_stats$Pos=='PG',]
    stats_wsc <- stats_wsc[1:10,]
    
    Points_in_the_paint <- c(862, 688, 816, 626, 386, 778, 586, 640, 442, 422)
    stats_wsc$PITP <- Points_in_the_paint
    stats_wsc$Paint_ratio <- stats_wsc$PITP/stats_wsc$`2PA`
    
    freq <- table(GameScoreRegular$Player)
    freq <- freq[order(freq, decreasing = TRUE)]
    freq <- data.frame(freq)
    names(freq)[1] <- "Players"
    names(freq)[2] <- "Games"
    GameScoreRegular <- freq
    
    freq <- table(GameScorePlayoffs$Player)
    freq <- freq[order(freq, decreasing = TRUE)]
    freq <- data.frame(freq)
    names(freq)[1] <- "Players"
    names(freq)[2] <- "Games"
    GameScorePlayoffs <- freq
    
    freq <- table(PER_season$Player)
    freq <- freq[order(freq, decreasing = TRUE)]
    freq <- data.frame(freq)
    names(freq)[1] <- "Players"
    names(freq)[2] <- "Seasons"
    PER_season <- freq
    
    freq <- table(PER_playoffs$Player)
    freq <- freq[order(freq, decreasing = TRUE)]
    freq <- data.frame(freq)
    names(freq)[1] <- "Players"
    names(freq)[2] <- "Seasons"
    PER_playoffs <- freq
    
    g <- reactive({
      
      switch(input$seasontype1,
             s = GameScoreRegular,
             p = GameScorePlayoffs
      )
    })
    
    per <- reactive({
      
      switch(input$seasontype2,
             s = PER_season,
             p = PER_playoffs
      )
    })
    
    output$plot1 <- renderPlot({
      
      g_genre <- ggplot(data = MVP_VOTES, aes(x = Player, y = Votes, fill = Player));
      g_genre + geom_boxplot() + xlab("Player") + ylab("Votes Points") + ggtitle("Distribution of votes for best players")
      
    })
    
    output$plot2 <- renderPlot({
      p <- ggplot(data = Points_per_100[1:10,], aes(x=reorder(Player, PTS_PER), y=PTS_PER)) 
      p +  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) + ggtitle("MOST POINTS PER 100 POSSESSIONS IN NBA HISTORY - REGULAR SEASON") + scale_y_continuous(limits=c(40,50),oob = rescale_none) + coord_flip() +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank()) + geom_text(aes(label=PTS_PER), vjust = 0.4, nudge_y = 0.2)
    })
    
    output$plot3 <- renderPlot({
      
      p <- ggplot(stats_wsc, aes(x=stats_wsc$`3PA`, y=stats_wsc$'FTA', size = stats_wsc$Paint_ratio, label = Player))
      p + geom_point()+ 
        geom_text(size = 4, vjust = 0, nudge_y = 17)+ 
        scale_size(range = c(.01, 10), name="2pts (in the paint)")+
        xlab("3pts attempts")+ 
        ylab("Free throw attempts")+
        ggtitle("James Harden - ways of scoring")
    })
    
    output$plot4 <- renderPlot({
      
      g <- ggplot(data = g()[1:12,], aes(x=Players, y=Games)) 
      g +  geom_bar(stat="identity") + ggtitle("Number of games in top 100 best performances. Regular Season")
      
    })
  
    output$plot5 <- renderPlot({
      
      g <- ggplot(data = per()[1:12,], aes(x=Players, y=Seasons)) 
      g +  geom_bar(stat="identity") + ggtitle("Number of PER in top 100 for each player. Regular Season")
      
    })
  }
)
