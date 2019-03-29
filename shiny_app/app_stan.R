#
# This is a Shiny web application. You can run the application by clicking
# the "Run App" button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rstan)
source("parseWeb.R")
source("util_stan.R")
source("today_pred.R")

load_latestg()

bteams <- read.csv("app_data/bracket_teams.csv", stringsAsFactors = F, header = F)$V1
teams <- union(unique(g$team1), unique(g$team2)) %>% unique() %>% str_sort()


load("app_data/param_190324.RData")

its <- 100000

# Define UI for application that draws a histogram
ui <- navbarPage("NCAA Men's Basketball 2019",
                 
  tabPanel("Today's Games",
           tableOutput("today")
           ),
   
   
   tabPanel("Matchup",
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        "Model date: 19/03/24",
        selectInput(
          "team1", "Select Team 1", choices= bteams, selected="Virginia"
        ),
        selectInput(
          "team2", "Select Team 2", choices= bteams, selected="Gonzaga"
        ))
      ,
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("matchup"),
        plotOutput("scorediff"),
        plotOutput("score")
        
      )
   )
   ),
    tabPanel("Writeup",
      includeHTML("writeup.html")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  mcm <- reactive({
    withProgress(message = "Simulating games...", value = .5, {
    t1 <- which(input$team1==teams)
    t2 <- which(input$team2==teams)
    print(paste("Teams:",input$team1,":",input$team2))
    print(t1)
    print(t2)
    #print(paste(t1,":",list(ofmu=param$ofmu[t1],ofsd=param$ofsd[t1],dfmu=param$dfmu[t1],dfsd=param$dfsd[t1]),
      #    t2,":",list(ofmu=param$ofmu[t2],ofsd=param$ofsd[t2],dfmu=param$dfmu[t2],dfsd=param$dfsd[t2])))
    #print(teams)
    mc_matchup(list(ofmu=param$ofmu[t1],ofsd=param$ofsd[t1],dfmu=param$dfmu[t1],dfsd=param$dfsd[t1]),
                      list(ofmu=param$ofmu[t2],ofsd=param$ofsd[t2],dfmu=param$dfmu[t2],dfsd=param$dfsd[t2]),
                      iter=its)
    })
  })
  
  df <- reactive({
    data.frame(team=c(rep(input$team1,its), rep(input$team2,its)),
                     points=c(mcm()$s1,mcm()$s2))
  })
  
  dif <- reactive({
    data.frame(points=mcm()$s1-mcm()$s2)
  })
   
   output$matchup <- renderTable({
     
     p1 <- sum(mcm()$s1>mcm()$s2)/its
     
     
     data.frame(team=c(input$team1, input$team2),
                win_prob=c(p1,1-p1),
                score=c(mean(mcm()$s1),mean(mcm()$s2)))
     
   })
   
   output$score <- renderPlot({
     withProgress(message = "Generating point distribution...", value = 0.5, {
     ggplot() +
       geom_histogram(data=df()[df()$team==input$team1,],  aes(x=points, fill=team, color=team),alpha=0.3, binwidth=2)+
       geom_histogram(data=df()[df()$team==input$team2,],  aes(x=points, fill=team, color=team),alpha=0.3, binwidth=2)+
       scale_color_manual(name="team", values=c("red", "blue"))  +
       labs(title="Point Distributions") +
       theme_bw()
     })
   })
   
   output$scorediff <- renderPlot({
     withProgress(message = "Generating point spread...", value = 0.5, {
     ggplot(dif(), aes(x=points)) + geom_histogram(binwidth=2) +
       scale_fill_manual(name="team", values=c("red", "blue"), labels=c(input$team1, input$team2))  +
       labs(title=paste("Point Spread Distribution:",input$team1,"-",input$team2)) +
       theme_bw()
     })
   })
   
   output$today <- renderTable({
     withProgress(message = "Fetching games...", value = .5, {
       pred_today(teams,param, its)
       })
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

