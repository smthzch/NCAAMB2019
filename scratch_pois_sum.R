library(tidyverse)
library(lubridate)

source("R/parse_web.R")
source("R/today_pred.R")
source("R/util_stan.R")

#load games
g <- load_games()

#load list of teams
bracket0 <- read.csv("data/meta/bracket_2021.csv", stringsAsFactors = F)
bteams <- bracket0$team %>% str_trim()
teams <- union(unique(g$team1), unique(g$team2)) %>% unique() %>% str_sort()

####################################
#poisson sum model lambda = exp(of - df)
#load latest model parameters
mxdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%y%m%d")
paramdt <- list.files("data/parameters") %>% parse_number() %>% ymd() %>% max() %>% strftime("%Y-%m-%d") #for display on ui
load(paste0("data/parameters/pois_sum_", mxdt, ".RData"), .GlobalEnv)

#posterior predictive
# g.pred <- g %>%
#   mutate(
#     s_hat = map2(team1, team2, function(t1, t2){
#       matchup.pois(t1, t2, param, teams, 100, method="sum")$s1 %>% mean()
#     })
#   ) %>%
#   unnest(s_hat) %>%
#   mutate(s_hat = unlist(s_hat))
# 
# g.pred %>% ggplot() +
#   geom_point(aes(x=score1, y=s_hat)) +
#   geom_abline(slope=1) +
#   theme(aspect.ratio = 1)
# 
# g.pred %>% ggplot() +
#   geom_point(aes(x=score1, y=s_hat - score1))
# 
# g.pred %>% ggplot() +
#   geom_point(aes(x=date, y=s_hat - score1))
# 
# g.pred %>% ggplot() +
#   geom_point(aes(x=team1, y=s_hat - score1))
# 
# g.pred %>% ggplot() +
#   geom_point(aes(x=team2, y=s_hat - score1))
# 
# cor(g.pred$score1, g.pred$s_hat)
# sqrt(mean((g.pred$s_hat - g.pred$score1)^2))
# mean(abs(g.pred$s_hat - g.pred$score1))

#solve bracket
iters <- 100000

#first 4
bracket1 <- bracket0 %>% 
  filter(round==0) %>% 
  group_by(conference, seed) %>% 
  nest() %>% 
  mutate(
    team = map_chr(data, function(x){
      t1 <- x$team[[1]]
      t2 <- x$team[[2]]
      mc <- matchup.pois(t1, t2, param, teams, iters, method="sum")
      s1w <- sum(mc$s1>mc$s2)
      s2w <- sum(mc$s2>mc$s1)
      if(s1w==s2w) print(paste("Tie between:",t1,"&",t2))
      ifelse(s1w>s2w, t1, t2)
    }),
    round = 1
  ) %>% 
  dplyr::select(-data) %>% 
  ungroup() %>% 
  rbind(bracket0)

#round 1
border <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
o1a <- border[seq(1,16,2)]
o1b <- border[seq(2,16,2)]
orders <- map2(o1a, o1b, function(x, y) c(x,y))

bracket2 <- bracket1 %>% 
  filter(round==1) %>% 
  #determine which game each team plays in
  mutate(
    game = map_dbl(seed, function(x){
      map_lgl(orders, function(y){
        x %in% y
      }) %>% 
        which()
    })
  ) %>% 
  #perform matchups
  group_by(conference, game) %>% 
  nest() %>% 
  mutate(
    gdata = map(data, function(x){
      t1 <- x$team[[1]]
      t2 <- x$team[[2]]
      mc <- matchup.pois(t1, t2, param, teams, iters, method="sum")
      s1 <- mean(mc$s1)
      s2 <- mean(mc$s2)
      s1w <- sum(mc$s1>mc$s2)
      s2w <- sum(mc$s2>mc$s1)
      if(s1w==s2w) print(paste("Tie between:",t1,"&",t2))
      list(t1=t1, t2=t2, s1=s1, s2=s2, winner=ifelse(s1w>s2w, t1, t2))
    }),
    team = map_chr(gdata, function(x) x$winner),
    team1 = map_chr(gdata, function(x) x$t1),
    team2 = map_chr(gdata, function(x) x$t2),
    s1 = map_chr(gdata, function(x) x$s1),
    s2 = map_chr(gdata, function(x) x$s2),
    round = 2
  ) %>% 
  dplyr::select(-data, -gdata) %>% 
  ungroup()

#round 2
o1a <- seq(1,8,2)
o1b <- seq(2,8,2)
orders <- map2(o1a, o1b, function(x, y) c(x,y))

bracket3 <- bracket2 %>% 
  filter(round==2) %>% 
  #determine which game each team plays in
  mutate(
    game = map_dbl(game, function(x){
      map_lgl(orders, function(y){
        x %in% y
      }) %>% 
        which()
    })
  ) %>% 
  #perform matchups
  group_by(conference, game) %>% 
  nest() %>% 
  mutate(
    gdata = map(data, function(x){
      t1 <- x$team[[1]]
      t2 <- x$team[[2]]
      mc <- matchup.pois(t1, t2, param, teams, iters, method="sum")
      s1 <- mean(mc$s1)
      s2 <- mean(mc$s2)
      s1w <- sum(mc$s1>mc$s2)
      s2w <- sum(mc$s2>mc$s1)
      if(s1w==s2w) print(paste("Tie between:",t1,"&",t2))
      list(t1=t1, t2=t2, s1=s1, s2=s2, winner=ifelse(s1w>s2w, t1, t2))
    }),
    team = map_chr(gdata, function(x) x$winner),
    team1 = map_chr(gdata, function(x) x$t1),
    team2 = map_chr(gdata, function(x) x$t2),
    s1 = map_chr(gdata, function(x) x$s1),
    s2 = map_chr(gdata, function(x) x$s2),
    round = 3
  ) %>% 
  dplyr::select(-data, -gdata) %>% 
  ungroup()

#round 3
o1a <- seq(1,4,2)
o1b <- seq(2,4,2)
orders <- map2(o1a, o1b, function(x, y) c(x,y))

bracket4 <- bracket3 %>% 
  filter(round==3) %>% 
  #determine which game each team plays in
  mutate(
    game = map_dbl(game, function(x){
      map_lgl(orders, function(y){
        x %in% y
      }) %>% 
        which()
    })
  ) %>% 
  #perform matchups
  group_by(conference, game) %>% 
  nest() %>% 
  mutate(
    gdata = map(data, function(x){
      t1 <- x$team[[1]]
      t2 <- x$team[[2]]
      mc <- matchup.pois(t1, t2, param, teams, iters, method="sum")
      s1 <- mean(mc$s1)
      s2 <- mean(mc$s2)
      s1w <- sum(mc$s1>mc$s2)
      s2w <- sum(mc$s2>mc$s1)
      if(s1w==s2w) print(paste("Tie between:",t1,"&",t2))
      list(t1=t1, t2=t2, s1=s1, s2=s2, winner=ifelse(s1w>s2w, t1, t2))
    }),
    team = map_chr(gdata, function(x) x$winner),
    team1 = map_chr(gdata, function(x) x$t1),
    team2 = map_chr(gdata, function(x) x$t2),
    s1 = map_chr(gdata, function(x) x$s1),
    s2 = map_chr(gdata, function(x) x$s2),
    round = 4
  ) %>% 
  dplyr::select(-data, -gdata) %>% 
  ungroup()

#round 4
o1a <- seq(1,2,2)
o1b <- seq(2,2,2)
orders <- map2(o1a, o1b, function(x, y) c(x,y))

bracket5 <- bracket4 %>% 
  filter(round==4) %>% 
  #determine which game each team plays in
  mutate(
    game = map_dbl(game, function(x){
      map_lgl(orders, function(y){
        x %in% y
      }) %>% 
        which()
    })
  ) %>% 
  #perform matchups
  group_by(conference, game) %>% 
  nest() %>% 
  mutate(
    gdata = map(data, function(x){
      t1 <- x$team[[1]]
      t2 <- x$team[[2]]
      mc <- matchup.pois(t1, t2, param, teams, iters, method="sum")
      s1 <- mean(mc$s1)
      s2 <- mean(mc$s2)
      s1w <- sum(mc$s1>mc$s2)
      s2w <- sum(mc$s2>mc$s1)
      if(s1w==s2w) print(paste("Tie between:",t1,"&",t2))
      list(t1=t1, t2=t2, s1=s1, s2=s2, winner=ifelse(s1w>s2w, t1, t2))
    }),
    team = map_chr(gdata, function(x) x$winner),
    team1 = map_chr(gdata, function(x) x$t1),
    team2 = map_chr(gdata, function(x) x$t2),
    s1 = map_chr(gdata, function(x) x$s1),
    s2 = map_chr(gdata, function(x) x$s2),
    round = 5
  ) %>% 
  dplyr::select(-data, -gdata) %>% 
  ungroup()

#semifinals
orders <- list(c("east", "west"), c("south", "midwest"))

bracket6 <- bracket5 %>% 
  filter(round==5) %>% 
  #determine which game each team plays in
  mutate(
    game = map_dbl(conference, function(x){
      map_lgl(orders, function(y){
        x %in% y
      }) %>% 
        which()
    })
  ) %>% 
  #perform matchups
  group_by(game) %>% 
  nest() %>% 
  mutate(
    gdata = map(data, function(x){
      t1 <- x$team[[1]]
      t2 <- x$team[[2]]
      mc <- matchup.pois(t1, t2, param, teams, iters, method="sum")
      s1 <- mean(mc$s1)
      s2 <- mean(mc$s2)
      s1w <- sum(mc$s1>mc$s2)
      s2w <- sum(mc$s2>mc$s1)
      if(s1w==s2w) print(paste("Tie between:",t1,"&",t2))
      list(t1=t1, t2=t2, s1=s1, s2=s2, winner=ifelse(s1w>s2w, t1, t2))
    }),
    team = map_chr(gdata, function(x) x$winner),
    team1 = map_chr(gdata, function(x) x$t1),
    team2 = map_chr(gdata, function(x) x$t2),
    s1 = map_chr(gdata, function(x) x$s1),
    s2 = map_chr(gdata, function(x) x$s2),
    round = 6
  ) %>% 
  dplyr::select(-data, -gdata) %>% 
  ungroup()

#finals
bracket7 <- bracket6 %>% 
  filter(round==6) %>% 
  #perform matchups
  group_by(round) %>% 
  nest() %>% 
  mutate(
    gdata = map(data, function(x){
      t1 <- x$team[[1]]
      t2 <- x$team[[2]]
      mc <- matchup.pois(t1, t2, param, teams, iters, method="sum")
      s1 <- mean(mc$s1)
      s2 <- mean(mc$s2)
      s1w <- sum(mc$s1>mc$s2)
      s2w <- sum(mc$s2>mc$s1)
      if(s1w==s2w) print(paste("Tie between:",t1,"&",t2))
      list(t1=t1, t2=t2, s1=s1, s2=s2, winner=ifelse(s1w>s2w, t1, t2))
    }),
    team = map_chr(gdata, function(x) x$winner),
    team1 = map_chr(gdata, function(x) x$t1),
    team2 = map_chr(gdata, function(x) x$t2),
    s1 = map_chr(gdata, function(x) x$s1),
    s2 = map_chr(gdata, function(x) x$s2),
    round = 7
  ) %>% 
  dplyr::select(-data, -gdata) %>% 
  ungroup()

fmc <- matchup.pois("App State", "Norfolk St.", param, teams, 1000000, method="sum")
mean(fmc$s1>fmc$s2)
mean(fmc$s1)
mean(fmc$s2)
hist(fmc$s1)
hist(fmc$s2)
hist(fmc$s1-fmc$s2)
mean(fmc$s1-fmc$s2)
sum(fmc$s1>=fmc$s2)/iters
fmcd <- data.frame(fmc) %>% pivot_longer(everything(),names_to="team", values_to="score")
fmcd %>% ggplot() +
  geom_histogram(aes(x=score, fill=team), position="identity", alpha=0.5)
