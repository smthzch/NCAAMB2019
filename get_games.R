library(tidyverse)
library(lubridate)

source("R/parse_web.R")

#create dirs if not exist
if(!dir.exists("data")){
  create_dirs()
}

#get initial games
st <- "2021-11-9"
ed <- "2022-03-10"
if(!file.exists("data/games/scores.csv")){
  g <- get_games(st=st, ed=ed) %>%
    mutate(
      score1 = as.numeric(score1),
      score2 = as.numeric(score2)
    )
  save_games(g)
}else{#load last loaded games
  g <- load_games()
}
#get new games
g <- get_newgames(g) %>%
  mutate(
    score1 = as.numeric(score1),
    score2 = as.numeric(score2)
  )
g$team1 <- stringr::str_replace_all(g$team1, "'", "")
g$team2 <- stringr::str_replace_all(g$team2, "'", "")

save_games(g)
