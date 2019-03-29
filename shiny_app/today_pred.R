source("parseWeb.R")
source("util_stan.R")


pred_today <- function(teams, param, its=100000){
  tg <- get_games(strftime(Sys.Date(),"%Y/%m/%d"),strftime(Sys.Date(),"%Y/%m/%d"))
  tg$Predicted_Score1 <- 0
  tg$Predicted_Score2 <- 0
  tg$Prob_Team1_Win <- 0
  for(i in 1:nrow(tg)){
    t1 <- which(tg[i,"team1"]==teams)
    t2 <- which(tg[i,"team2"]==teams)
    
    scrs <- mc_matchup(list(ofmu=param$ofmu[t1],ofsd=param$ofsd[t1],dfmu=param$dfmu[t1],dfsd=param$dfsd[t1]),
               list(ofmu=param$ofmu[t2],ofsd=param$ofsd[t2],dfmu=param$dfmu[t2],dfsd=param$dfsd[t2]),
               iter=its)
    
    p1 <- sum(scrs$s1>scrs$s2)/its
    
    
    tg[i,]$Predicted_Score1 <- round(mean(scrs$s1),1)
    tg[i,]$Predicted_Score2 <- round(mean(scrs$s2),1)
    tg[i,]$Prob_Team1_Win <- round(p1,2)
  }
  tg %>% dplyr::select(-score1, -score2, -date)
}


