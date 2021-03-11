library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("R/parse_web.R")
source("R/util_stan.R")

#bteams <- read.csv("data/meta/bracket_teams.csv", stringsAsFactors = F, header = F)$V1 %>% str_trim()
g <- load_games()
gdt <- g$date %>% max() %>% strftime("%y%m%d")

#data structure for model.stan
teams <- union(unique(g$team1), unique(g$team2)) %>% unique() %>% str_sort()
stan_data <- list("T" = length(teams),
                  N = nrow(g),
                  tid1 = g$team1 %>% factor(levels=teams) %>% as.numeric(),
                  tid2 = g$team2 %>% factor(levels=teams) %>% as.numeric(),
                  pt1 = g$score1,
                  pt2 = g$score2)

##########################################
#fit poisson mixture model
model <- stan_model("stan/gamma_poisson.stan")
fit <- sampling(model, data=stan_data, chains=1, iter=10000)
#save(fit, file = paste0("data/fits/poisson_", gdt, ".RData"))
    
#check rhat and neff
#print(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))
#traceplot(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))
pairs(fit, pars=c("ofsd", "dfsd"))
pairs(fit, pars=c("of[1,103]","df[103]","of[2,103]"))
pairs(fit, pars=c("of[1,103]","df[103]","of[1,291]","df[291]"))

#extract parameter samples
para <- extract(fit)
#save(para, file=paste0("data/parameters/para_", gdt, ".RData"))

#summarize posterior
param <- get_posterior.pois(para)
save(param, file=paste0("data/parameters/pois_prod_", gdt, ".RData"))

##########################################
#fit poisson prod model
model <- stan_model("stan/poisson_prod.stan")
fit <- sampling(model, data=stan_data, chains=1, iter=4000)
#save(fit, file = paste0("data/fits/poisson_", gdt, ".RData"))

#check rhat and neff
#print(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))
#traceplot(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))
pairs(fit, pars=c("ofsd", "dfsd"))
pairs(fit, pars=c("of[103]","df[103]","of[291]","df[291]"))

#extract parameter samples
para <- extract(fit)
#save(para, file=paste0("data/parameters/para_", gdt, ".RData"))

#summarize posterior
param <- get_posterior.pois(para)
save(param, file=paste0("data/parameters/pois_prod_", gdt, ".RData"))

##########################################
#fit poisson sum model
model <- stan_model("stan/poisson_sum.stan")
fit <- sampling(model, data=stan_data, chains=4, iter=4000)
#save(fit, file = paste0("data/fits/poisson_", gdt, ".RData"))

#check rhat and neff
#print(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))
#traceplot(fit, pars=c("ofsd","dfsd","of[1]","df[1]","of[10]","df[10]"))
pairs(fit, pars=c("ofsd", "dfsd"))
pairs(fit, pars=c("of[103]","df[103]","of[291]","df[291]"))

#extract parameter samples
para <- extract(fit)
#save(para, file=paste0("data/parameters/para_", gdt, ".RData"))

#summarize posterior
param <- get_posterior.pois(para)
save(param, file=paste0("data/parameters/pois_sum_", gdt, ".RData"))


