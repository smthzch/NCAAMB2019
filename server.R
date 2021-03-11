#set number monte carlo iterations
its <- 100000

server <- function(input, output) {
  
  mcm <- reactive({
    withProgress(message = "Simulating games...", value = .5, {
      t1 <- which(input$team1==teams)
      t2 <- which(input$team2==teams)
      # print(paste("Teams:",input$team1,":",input$team2))
      # print(t1)
      # print(t2)
      # print(paste(t1,":",list(ofmu=param$ofmu[t1],ofsd=param$ofsd[t1],dfmu=param$dfmu[t1],dfsd=param$dfsd[t1]),
      #     t2,":",list(ofmu=param$ofmu[t2],ofsd=param$ofsd[t2],dfmu=param$dfmu[t2],dfsd=param$dfsd[t2])))
      # print(teams)
      mc_matchup(list(ofmu=param$ofmu[t1], ofsd=param$ofsd[t1], dfmu=param$dfmu[t1], dfsd=param$dfsd[t1]),
                 list(ofmu=param$ofmu[t2], ofsd=param$ofsd[t2], dfmu=param$dfmu[t2], dfsd=param$dfsd[t2]),
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
      preds <- pred_today(teams, param, its)
      if(nrow(preds)>0){
        preds
      }else{
        data.frame(Message="No games today.")
      }
    })
  })
}