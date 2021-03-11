ui <- navbarPage("NCAA Men's Basketball 2019",
                 
  tabPanel("Today's Games",
           tableOutput("today")
           ),
   
   
   tabPanel("Matchup",
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        paste0("Model date: ", paramdt),
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
      includeHTML("markdown/writeup.html")
    )
)
