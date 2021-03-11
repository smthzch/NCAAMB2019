library(tidyverse)
library(lubridate)

source("R/parse_web.R")

#create dirs if not exist
if(!dir.exists("data")){
  create_dirs()
}

#get initial games
st <- "2020-11-25"
ed <- "2020-12-26"
if(!file.exists("data/games/scores.csv")){
  g <- get_games(st=st, ed=ed)
  save_games(g)
}else{#load last loaded games
  g <- load_games()
}
#get new games
g <- get_newgames(g)
save_games(g)
