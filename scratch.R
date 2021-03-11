library(tidyverse)
library(lubridate)

source("R/parse_web.R")
source("R/today_pred.R")
source("R/util_stan.R")

#load games
g <- load_games()

#load list of teams
bteams <- read.csv("data/meta/bracket_teams.csv", stringsAsFactors = F, header = F)$V1 %>% str_trim()
teams <- union(unique(g$team1), unique(g$team2)) %>% unique() %>% str_sort()


####################################
#poisson product model lambda = exp(of*df)
#load latest model parameters
mxdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%y%m%d")
paramdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%Y-%m-%d") #for display on ui
load(paste0("data/parameters/pois_prod_", mxdt, ".RData"), .GlobalEnv)

#posterior predictive
g.pred <- g %>% 
  mutate(
    s_hat = map2(team1, team2, function(t1, t2){
      matchup.pois(t1, t2, param, teams, 100, method="prod")$s1 %>% mean()
    })
  ) %>% 
  unnest(s_hat) %>% 
  mutate(s_hat = unlist(s_hat))

g.pred %>% ggplot() +
  geom_point(aes(x=score1, y=s_hat)) +
  geom_abline(slope=1) +
  theme(aspect.ratio = 1)
  
g.pred %>% ggplot() +
  geom_point(aes(x=score1, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=date, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=team1, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=team2, y=s_hat - score1))

cor(g.pred$score1, g.pred$s_hat)
sqrt(mean((g.pred$s_hat - g.pred$score1)^2))
mean(abs(g.pred$s_hat - g.pred$score1))

####################################
#poisson sum model lambda = exp(of - df)
#load latest model parameters
mxdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%y%m%d")
paramdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%Y-%m-%d") #for display on ui
load(paste0("data/parameters/pois_prod_", mxdt, ".RData"), .GlobalEnv)

#posterior predictive
g.pred <- g %>% 
  mutate(
    s_hat = map2(team1, team2, function(t1, t2){
      matchup.pois(t1, t2, param, teams, 100, method="prod")$s1 %>% mean()
    })
  ) %>% 
  unnest(s_hat) %>% 
  mutate(s_hat = unlist(s_hat))

g.pred %>% ggplot() +
  geom_point(aes(x=score1, y=s_hat)) +
  geom_abline(slope=1) +
  theme(aspect.ratio = 1)

g.pred %>% ggplot() +
  geom_point(aes(x=score1, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=date, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=team1, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=team2, y=s_hat - score1))

cor(g.pred$score1, g.pred$s_hat)
sqrt(mean((g.pred$s_hat - g.pred$score1)^2))
mean(abs(g.pred$s_hat - g.pred$score1))
###################################
#normal model
#load latest model parameters
mxdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%y%m%d")
paramdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%Y-%m-%d") #for display on ui
load(paste0("data/parameters/norm_", mxdt, ".RData"), .GlobalEnv)

#posterior predictive
g.pred <- g %>% 
  mutate(
    s_hat = map2(team1, team2, function(t1, t2){
      matchup.norm(t1, t2, param, teams, 100)$s1 %>% mean()
    })
  ) %>% 
  unnest(s_hat) %>% 
  mutate(s_hat = unlist(s_hat))

g.pred %>% ggplot() +
  geom_point(aes(x=score1, y=s_hat)) +
  geom_abline(slope=1) +
  theme(aspect.ratio = 1)

g.pred %>% ggplot() +
  geom_point(aes(x=score1, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=date, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=team1, y=s_hat - score1))

g.pred %>% ggplot() +
  geom_point(aes(x=team2, y=s_hat - score1))

cor(g.pred$score1, g.pred$s_hat)

sqrt(mean((g.pred$s_hat - g.pred$score1)^2))
mean(abs(g.pred$s_hat - g.pred$score1))
