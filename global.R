library(shiny)
library(tidyverse)

source("R/parse_web.R")
source("R/util_stan.R")
source("R/today_pred.R")

#load games
g <- load_games()

#load list of teams
bteams <- read.csv("data/meta/bracket_teams.csv", stringsAsFactors = F, header = F)$V1 %>% str_trim()
teams <- union(unique(g$team1), unique(g$team2)) %>% unique() %>% str_sort()

#load latest model parameters
mxdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%y%m%d")
paramdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%Y-%m-%d") #for display on ui
load(paste0("data/parameters/param_", mxdt, ".RData"))