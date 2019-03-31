library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("utils/parseWeb.R")
source("utils/util_stan.R")

bteams <- read.csv("app_data/bracket_teams.csv", stringsAsFactors = F, header = F)$V1
load_latestg()

#data structure for model.stan
teams <- union(unique(g$team1), unique(g$team2)) %>% unique() %>% str_sort()
nteams <- length(teams)
N <- nrow(g)
t1 <- g$team1 %>% factor(levels=teams) %>% as.numeric()
t2 <- g$team2 %>% factor(levels=teams) %>% as.numeric()
p1 <- g$score1
p2 <- g$score2


stan_data <- list("T"=nteams,
                  N=N,
                  tid1=t1,
                  tid2=t2,
                  pt1=p1,
                  pt2=p2)

#fit model
fit <- stan(file="stan_model/model.stan", data=stan_data, chains=4, iter=10000)
save(fit, file = "fit_data/stan_fit_190331_4_10000.RData")

#check rhat and neff
print(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))
traceplot(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))

#extract parameter samples
para <- extract(fit)
#save(para, file="app_data/para_190329_4_10000.RData")

#summarize posterior
param <- get_posterior(para)
save(param, file="app_data/param_190331.RData")
