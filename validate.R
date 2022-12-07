library(verification)
library(Metrics)
library(tidyverse)
source("utils/parseWeb.R")
source("utils/util_stan.R")

validate <- function(param, st = "2022-03-14", ed = "2022-04-05"){
  games <- load_games() |> filter(date >= st & date <= ed)
  teams <- union(unique(games$team1), unique(games$team2)) %>% unique() %>% str_sort()
  games$p1 <- 0
  games$t1w <- F
  games$correct <- F
  its <- 10000
  for(i in 1:nrow(games)){
    print(paste("Sim game:",i))
    t1 <- games[["team1"]][i]
    t2 <- games[["team2"]][i]
    pred <- matchup(t1, t2, param, teams, its = its)
    p1 <- sum(pred$s1>pred$s2)/its
    games[i,"p1"] <- p1
    games[i,"t1w"] <- games[i,"score1"] > games[i,"score2"]
    games[i,"ps1"] <- mean(pred$s1)
    games[i,"ps2"] <- mean(pred$s2)
    s1 <- games[i,"score1"]
    s2 <- games[i,"score2"]
    correct <- ifelse(s1>s2&p1>.5,T,ifelse(s1<s2&p1<.5,T,F))
    games[i,"correct"] <- correct
  }
  games
}

load("data/parameters/pois_sum_220313.RData")
vld <- validate(param, st = "2022-03-14", ed = "2022-04-05")
sum(vld$correct)/nrow(vld)
reliability.plot(verify(vld$t1w, vld$p1))


validate <- function(team_dist, st = "2022-03-14", ed = "2022-04-05"){
  games <- load_games() |> filter(date >= st & date <= ed)
  teams <- union(unique(games$team1), unique(games$team2)) %>% unique() %>% str_sort()
  games$p1 <- 0
  games$t1w <- F
  games$correct <- F
  its <- 1000000
  for(i in 1:nrow(games)){
    print(paste("Sim game:",i))
    t1 <- games[["team1"]][i]
    t2 <- games[["team2"]][i]
    p1 <- team_dist[[t1]]
    p2 <- team_dist[[t2]]
    p1w <- p1 / (p1 + p2)
    games[i,"p1"] <- p1w
    s1 <- games[i,"score1"]
    s2 <- games[i,"score2"]
    t1w <- s1 > s2
    games[i,"t1w"] <- t1w
    games[i,"correct"] <- t1w == (p1 > p2)
  }
  games
}

load("data/markov/team_dist.Rdata")
vld <- validate(team_dist, st = "2022-03-14", ed = "2022-04-05")
sum(vld$correct)/nrow(vld)
reliability.plot(verify(vld$t1w, vld$p1))
