library(rvest)
library(lubridate)

create_dirs <- function(){
  dir.create("data/games", recursive = T)
  dir.create("data/fits", recursive = T)
  dir.create("data/meta", recursive = T)
  dir.create("data/parameters", recursive = T)
}

read_page <- function(u, d){
  outdf <- data.frame(matrix(nrow=0,ncol=5), stringsAsFactors = F) %>% set_names(c("team1","team2","score1","score2","date"))
  html <- read_html(u)
  nodes <- html %>% 
    html_nodes(".gamePod-game-teams") %>% 
    map_dfr(function(nd){
      tms <- nd %>% html_nodes("li")
      t1 <- tms[[1]] %>% html_node(".gamePod-game-team-name") %>% html_text() %>% as.character()
      s1 <- tms[[1]] %>% html_node(".gamePod-game-team-score") %>% html_text() %>% as.numeric()
      t2 <- tms[[2]] %>% html_node(".gamePod-game-team-name") %>% html_text() %>% as.character()
      s2 <- tms[[2]] %>% html_node(".gamePod-game-team-score") %>% html_text() %>% as.numeric()
      if(t1!="" & t2!="" & !is.na(s1) & !is.na(s2) & s1>0 & s2>0) {
        return(data.frame("team1"=t1,"team2"=t2,"score1"=s1,"score2"=s2, "date"=d, stringsAsFactors = F))
      }
    })
}

get_games <- function(st, ed){
  tryCatch(
    {
      games <- seq.Date(ymd(st), ymd(ed), by=1) %>% 
        map_dfr(function(d){
          dte <- strftime(d, "%Y/%m/%d")
          print(paste("Getting date:",dte))
          ur <- paste0("https://www.ncaa.com/scoreboard/basketball-men/d1/", dte)
          read_page(ur, d)
        })
    },
    error=function(er){print(paste("Failed on:",er))}
  )
  print("Got games.")
  games
}

get_newgames <- function(g, ed=NA){
  if(is.na(ed)){
    ed = today() - days(1)
  }else{
    ed = ymd(ed)
  }
  stdt <- g$date %>% max() + days(1)
  if(stdt>ed) return(g)
  g_ <- get_games(st=stdt, ed=ed)
  g <- rbind(g, g_)
  g
}

save_games <- function(g){
  write_csv(g, "data/games/scores.csv")
}

load_games <- function(){
  read_csv("data/games/scores.csv")
}
