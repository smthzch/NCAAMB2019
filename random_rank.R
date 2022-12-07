library(tidyverse)
source("R/parse_web.R")

games <- load_games() |> filter(date < "2022-03-14")

games$team1 <- stringr::str_replace_all(games$team1, "'", "")
games$team2 <- stringr::str_replace_all(games$team2, "'", "")

teams <- as.factor(sort(union(games$team1, games$team2)))
games$team1 <- factor(games$team1, levels = levels(teams))
games$team2 <- factor(games$team2, levels = levels(teams))
N <- length(teams)

played <- matrix(0, nrow = N, ncol = N)
wins <- matrix(0, nrow = N, ncol = N)
losses <- matrix(0, nrow = N, ncol = N)
scores <- matrix(1, nrow = N, ncol = N)

for(i in 1:nrow(games)){
  t1 <- games$team1[i]
  t2 <- games$team2[i]
  s1 <- games$score1[i]
  s2 <- games$score2[i]
  
  w1 <- 1 * (s1 > s2)
  w2 <- 1 * (s2 > s1)
  
  played[t1, t2] <- played[t1, t2] + 1
  played[t2, t1] <- played[t2, t1] + 1
  wins[t1, t2] <- wins[t1, t2] + w1
  wins[t2, t1] <- wins[t2, t1] + w2
  losses[t1, t2] <- losses[t1, t2] + w2
  losses[t2, t1] <- losses[t2, t1] + w1
  scores[t1, t2] <- scores[t1, t2] * s2 
  scores[t2, t1] <- scores[t2, t1] * s1
}

# markov chain
mpow <- function(x, n){
  y <- x
  for(i in 1:(n-1)){
    y <- y %*% x
  }
  y
}

trans <- losses
diag(trans) <- rowSums(wins)
trans <- sweep(trans, 1, rowSums(trans), FUN = "/")
statmat <- mpow(trans, 100)

# keep everything reachable from gonzaga
gz <- which(teams == "Gonzaga")
to_keep <-which(statmat[gz,] > 0)

teams <- as.factor(as.character(teams)[to_keep])
wins <- wins[to_keep, to_keep]
losses <- losses[to_keep, to_keep]


fit_markov <- function(wins, losses, eps){
  N <- nrow(wins)
  trans <- losses
  diag(trans) <- rowSums(wins)
  trans <- trans + eps
  trans <- sweep(trans, 1, rowSums(trans), FUN = "/")
  A <- trans - diag(N)
  
  solve(cbind(A[,-N], rep(1,N)))[N,]  
}

predict_odds <- function(pi, t1, t2){
  if(is.character(t1)){
    t1 <- which(teams == t1)
    t2 <- which(teams == t2)
  }
  pi[t1] / pi[t2]
}

stationary <- fit_markov(wins, losses, 3e-3)
z_ix <- order(stationary)
teams[z_ix]

hist(stationary[stationary>0])

team_dist <- purrr::map(
  stationary,
  ~ .
) %>% setNames(teams)

save(team_dist, file = "data/markov/team_dist.Rdata")

sort(unlist(team_dist))

t1 <- which(teams == "Monmouth")
t2 <- which(teams == "Saint Peters")
stationary[t1] / stationary[t2]

team_dist[[t1]]
team_dist[["Seton Hall"]] / team_dist[["TCU"]]

predict_odds(stationary, "Monmouth", "Saint Peters")

#transition rate matrix
norm_played <- played#[to_keep, to_keep]
norm_played[norm_played==0] <- 1


rates <- scores ^ (1/norm_played)#[to_keep,to_keep] ^ (1/norm_played)
rates[rates==1] <- 0
diag(rates) <- -rowSums(rates) 
rowSums(rates)

A <- rates
A[,ncol(A)] <- 1
stationary <- solve(A)[nrow(A),]

team_dist <- purrr::map(
  stationary,
  ~ .
) %>% setNames(teams)

save(team_dist, file = "data/markov/team_dist_rate.Rdata")

# 10 fold
n_folds <- 30
folds <- sample(n_folds, nrow(games), replace = TRUE)

eps <- c(7.5e-4, 1e-3, 2e-3, 3e-3, 5e-3) # for markov model
eps <- c(1e-3, 1e-1, 1, 10)
eps <- c(1e-4, , 1e-1, 1)
entropies <- purrr::map_dbl(
  eps,
  ~{
    entropy <- 0
    for(f in 1:n_folds){
      played <- matrix(0, nrow = N, ncol = N)
      wins <- matrix(0, nrow = N, ncol = N)
      losses <- matrix(0, nrow = N, ncol = N)
      scores <- matrix(1, nrow = N, ncol = N)
      
      games_ <- games[folds != f,]
      for(i in 1:nrow(games_)){
        t1 <- games_$team1[i]
        t2 <- games_$team2[i]
        s1 <- games_$score1[i]
        s2 <- games_$score2[i]
        
        w1 <- 1 * (s1 > s2)
        w2 <- 1 * (s2 > s1)
        
        played[t1, t2] <- played[t1, t2] + 1
        played[t2, t1] <- played[t2, t1] + 1
        wins[t1, t2] <- wins[t1, t2] + w1
        wins[t2, t1] <- wins[t2, t1] + w2
        losses[t1, t2] <- losses[t1, t2] + w2
        losses[t2, t1] <- losses[t2, t1] + w1
        
        scores[t1, t2] <- scores[t1, t2] * s2 
        scores[t2, t1] <- scores[t2, t1] * s1
      }  
      #wins <- wins[to_keep, to_keep]
      #losses <- losses[to_keep, to_keep]
      #stationary <- fit_markov(wins, losses, .x)
      
      #transition rate matrix
      norm_played <- played
      norm_played[norm_played==0] <- 1
      
      
      rates <- scores ^ (1/norm_played)
      rates[rates==1] <- .x
      diag(rates) <- -rowSums(rates) + .x 
      rowSums(rates)
      
      A <- rates
      A[,ncol(A)] <- 1
      stationary <- solve(A)[nrow(A),]
      
      games_ <- games[folds == f,]
      
      for(i in 1:nrow(games_)){
        t1 <- as.character(games_$team1[i])
        t2 <- as.character(games_$team2[i])
        s1 <- games_$score1[i]
        s2 <- games_$score2[i]
        
        w1 <- 1 * (s1 > s2)
        w2 <- 1 * (s2 > s1)
        
        if(t1 %in% teams && t2 %in% teams){
          o <- predict_odds(stationary, t1, t2)
          p <- o / (1 + o)
          
          entropy <- entropy - log(p) * w1 - log(1 - p) * w2   
        }

      }
    }
    
    entropy    
  }
)

entropies
plot(entropies ~ eps,type="b")


#
rt <- matrix(c(
  0, 50, 0,
  50, 0, 50,
  0, 75, 0
), byrow = TRUE, ncol=3)


