library(verification)
source("utils/parseWeb.R")
source("utils/util_stan.R")

validate <- function(param, st="2019/03/19", ed="2019/03/22"){
  load_latestg()
  teams <- union(unique(g$team1), unique(g$team2)) %>% unique() %>% sort()
  games <- data.frame(matrix(nrow=0,ncol=5), stringsAsFactors = F) %>% set_names(c("team1","team2","score1","score2","date"))
  for(d in seq.Date(as.Date(st), as.Date(ed), by=1)){
    dte <- strftime(as.Date(d, origin="1970-01-01"), "%Y/%m/%d")
    print(paste("Getting date:",dte))
    ur <- paste("https://www.ncaa.com/scoreboard/basketball-men/d1/", dte, sep="")
    games <- rbind(games, read_page(ur, dte))
  }
  games$p1 <- 0
  games$t1w <- F
  games$correct <- F
  its <- 1000000
  for(i in 1:nrow(games)){
    print(paste("Sim game:",i))
    t1 <- which(teams==games[i,"team1"])
    t2 <- which(teams==games[i,"team2"])
    pred <- mc_matchup(list(ofmu=param$ofmu[t1],ofsd=param$ofsd[t1],dfmu=param$dfmu[t1],dfsd=param$dfsd[t1]),
               list(ofmu=param$ofmu[t2],ofsd=param$ofsd[t2],dfmu=param$dfmu[t2],dfsd=param$dfsd[t2]),
               iter=its)
    p1 <- sum(pred$s1>pred$s2)/its
    games[i,"p1"] <- p1
    games[i,"t1w"] <- games[i,"score1"] > games[i,"score2"]
    s1 <- games[i,"score1"]
    s2 <- games[i,"score2"]
    correct <- ifelse(s1>s2&p1>.5,T,ifelse(s1<s2&p1<.5,T,F))
    games[i,"correct"] <- correct
  }
  games
}

update_validate <- function(){
  
}

#just a running go of it
#03/22
vld <- validate(param)
save(vld, file="vali_data/vld_190322.RData")
sum(vld$correct)/nrow(vld)

reliability.plot(verify(vld$t1w, vld$p1))

load("vali_data/vld_190322.RData")
vld <- rbind(vld, validate(param, st="2019/03/23",ed="2019/03/23"))
save(vld, file="vali_data/vld_190323.RData")
sum(vld$correct)/nrow(vld)
reliability.plot(verify(vld$t1w, vld$p1))

load("app_data/param_190323.RData")
load("vali_data/vld_190323.RData")
vld <- rbind(vld, validate(param, st="2019/03/24",ed="2019/03/24"))
save(vld, file="vali_data/vld_190324.RData")
sum(vld$correct)/nrow(vld)
reliability.plot(verify(vld$t1w, vld$p1))

load("app_data/param_190324.RData")
load("vali_data/vld_190324.RData")
vld <- rbind(vld, validate(param, st="2019/03/25",ed="2019/03/25"))
save(vld, file="vali_data/vld_190325.RData")
sum(vld$correct)/nrow(vld)
reliability.plot(verify(vld$t1w, vld$p1))
