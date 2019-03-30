library(tidyverse)
library(rvest)
library(tidygraph)
library(ggraph)

source("utils/parseWeb.R")

#load last loaded games
load_latestg()
#get new games
g_ <- get_newgames(g)
#reload newest games into object 'g'
load_latestg()

