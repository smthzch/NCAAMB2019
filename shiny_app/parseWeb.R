library(rvest)

read_page <- function(u, d){
  outdf <- data.frame(matrix(nrow=0,ncol=5), stringsAsFactors = F) %>% set_names(c("team1","team2","score1","score2","date"))
  html <- read_html(u)
  html %>% html_nodes(".gamePod-game-teams")
  nodes <- html %>% html_nodes(".gamePod-game-teams")
  for(nd in nodes){
    tms <- nd %>% html_nodes("li")
    t1 <- tms[[1]] %>% html_node(".gamePod-game-team-name") %>% html_text() %>% as.character()
    s1 <- tms[[1]] %>% html_node(".gamePod-game-team-score") %>% html_text() %>% as.numeric()
    t2 <- tms[[2]] %>% html_node(".gamePod-game-team-name") %>% html_text() %>% as.character()
    s2 <- tms[[2]] %>% html_node(".gamePod-game-team-score") %>% html_text() %>% as.numeric()
    if(t1!="" & t2!="") {
      outdf <- rbind(outdf, data.frame("team1"=t1,"team2"=t2,"score1"=s1,"score2"=s2, "date"=d, stringsAsFactors = F))
    }
  }
  outdf
}

get_games <- function(st="2018/11/06", ed="2019/03/19"){
  games <- data.frame(matrix(nrow=0,ncol=5), stringsAsFactors = F) %>% set_names(c("team1","team2","score1","score2","date"))
  tryCatch({
    for(d in seq.Date(as.Date(st), as.Date(ed), by=1)){
      dte <- strftime(as.Date(d, origin="1970-01-01"), "%Y/%m/%d")
      print(paste("Getting date:",dte))
      ur <- paste("https://www.ncaa.com/scoreboard/basketball-men/d1/", dte, sep="")
      games <- rbind(games, read_page(ur, dte))
    }
  },
  error=function(er){print(paste("Failed on:",dte))})
  games
}

get_newgames <- function(g){
  currdt <- strftime(Sys.Date(),"%Y/%m/%d")
  
  gmfiles <- list.files("game_data")
  mxdt <- as.Date(0, origin="1970-01-01")
  for(gf in gmfiles){
    fdt <- gf %>% parse_number() %>% as.character() %>% strptime(format="%y%m%d")
    if(fdt>mxdt) mxdt <- fdt
  }
  mxdt <- strftime(mxdt + (60*60*24), "%Y/%m/%d")
  
  g_ <- get_games(st=mxdt, ed=currdt)
  g <- rbind(g, g_)
  gt <- strftime(Sys.Date(), "%y%m%d")
  save(g, file=paste("game_data/games_",gt,".RData", sep=""))
  g
}

load_latestg <- function(){
  gmfiles <- list.files("game_data")
  mxdt <- as.Date(0, origin="1970-01-01")
  for(gf in gmfiles){
    fdt <- gf %>% parse_number() %>% as.character() %>% strptime(format="%y%m%d")
    if(fdt>mxdt) mxdt <- fdt
  }
  mxdt <- strftime(mxdt, "%y%m%d")
  load(paste("game_data/games_",mxdt,".RData", sep=""), .GlobalEnv)
}