get_posterior <- function(p){
  ofmu <- colMeans(p$of)
  ofsd <- sapply(1:length(teams), function(i) sd(p$of[,i]))
  dfmu <- colMeans(p$df)
  dfsd <- sapply(1:length(teams), function(i) sd(p$df[,i]))
  list(ofmu=ofmu,
       ofsd=ofsd,
       dfmu=dfmu,
       dfsd=dfsd)
}

rank_stan <- function(para, teams, bteams, its=1000){
  ti <- which(teams %in% bteams)
  ofmu <- colMeans(para$of)
  ofsd <- sapply(1:length(teams), function(i) sd(para$of[,i]))
  dfmu <- colMeans(para$df)
  dfsd <- sapply(1:length(teams), function(i) sd(para$df[,i]))
  
  matchups_prob <- matrix(0, nrow=length(teams), ncol=length(teams))
  matchups_score <- matrix(0, nrow=length(teams), ncol=length(teams))
  #its <- 1000
  for(i in ti){
    for(j in ti){
      mcm <- mc_matchup(list(ofmu=ofmu[i],ofsd=ofsd[i],dfmu=dfmu[i],dfsd=dfsd[i]),
                        list(ofmu=ofmu[j],ofsd=ofsd[j],dfmu=dfmu[j],dfsd=dfsd[j]),
                        iter=its)
      matchups_prob[i,j] <- sum(mcm$s1>mcm$s2)/its
      matchups_score[i,j] <- mean(mcm$s1)
    }
  }
  matchups_prob <- matchups_prob[ti,ti]
  colnames(matchups_prob) <- bteams
  rownames(matchups_prob) <- bteams
  matchups_score <- matchups_score[ti,ti]
  colnames(matchups_score) <- bteams
  rownames(matchups_score) <- bteams
  
  list(matchups_prob, matchups_score)
}

mc_matchup <- function(t1, t2, iter=1000){
  of1 <- rnorm(iter, t1$ofmu, t1$ofsd)
  df1 <- rnorm(iter, t1$dfmu, t1$dfsd)
  of2 <- rnorm(iter, t2$ofmu, t2$ofsd)
  df2 <- rnorm(iter, t2$dfmu, t2$dfsd)
  
  s1 <- rpois(iter, exp(of1 - df2))
  s2 <- rpois(iter, exp(of2 - df1))
  list(s1=s1, s2=s2)
}

matchup <- function(t1n, t2n, param, teams, its=10000){
  
  t1 <- which(t1n==teams)
  t2 <- which(t2n==teams)
  
  mc_matchup(list(ofmu=param$ofmu[t1],ofsd=param$ofsd[t1],dfmu=param$dfmu[t1],dfsd=param$dfsd[t1]),
             list(ofmu=param$ofmu[t2],ofsd=param$ofsd[t2],dfmu=param$dfmu[t2],dfsd=param$dfsd[t2]),
             iter=its)
  
}
