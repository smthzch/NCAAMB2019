get_posterior.pois <- function(p){
  ofmu <- colMeans(p$of)
  ofsd <- sapply(1:length(teams), function(i) sd(p$of[,i]))
  dfmu <- colMeans(p$df)
  dfsd <- sapply(1:length(teams), function(i) sd(p$df[,i]))
  list(ofmu=ofmu,
       ofsd=ofsd,
       dfmu=dfmu,
       dfsd=dfsd)
}

get_posterior.norm <- function(p){
  ofmu <- colMeans(p$of)
  ofsd <- sapply(1:length(teams), function(i) sd(p$of[,i]))
  dfmu <- colMeans(p$df)
  dfsd <- sapply(1:length(teams), function(i) sd(p$df[,i]))
  phimu <- colMeans(p$phi)
  phisd <- sapply(1:length(teams), function(i) sd(p$phi[,i]))
  list(ofmu=ofmu,
       ofsd=ofsd,
       dfmu=dfmu,
       dfsd=dfsd,
       phimu=phimu,
       phisd=phisd)
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

mc_matchup.pois_sum <- function(t1, t2, iter=1000){
  of1 <- rnorm(iter, t1$ofmu, t1$ofsd)
  df1 <- rnorm(iter, t1$dfmu, t1$dfsd)
  of2 <- rnorm(iter, t2$ofmu, t2$ofsd)
  df2 <- rnorm(iter, t2$dfmu, t2$dfsd)
  
  s1 <- rpois(iter, exp(of1 - df2))
  s2 <- rpois(iter, exp(of2 - df1))
  list(s1=s1, s2=s2)
}

mc_matchup.pois_prod <- function(t1, t2, iter=1000){
  of1 <- rnorm(iter, t1$ofmu, t1$ofsd)
  df1 <- rnorm(iter, t1$dfmu, t1$dfsd)
  of2 <- rnorm(iter, t2$ofmu, t2$ofsd)
  df2 <- rnorm(iter, t2$dfmu, t2$dfsd)
  
  s1 <- rpois(iter, exp(of1 * df2))
  s2 <- rpois(iter, exp(of2 * df1))
  list(s1=s1, s2=s2)
}

mc_matchup.norm <- function(t1, t2, iter=1000){
  of1 <- rnorm(iter, t1$ofmu, t1$ofsd)
  df1 <- rnorm(iter, t1$dfmu, t1$dfsd)
  phi1 <- rnorm(iter, t1$phimu, t1$phisd)
  of2 <- rnorm(iter, t2$ofmu, t2$ofsd)
  df2 <- rnorm(iter, t2$dfmu, t2$dfsd)
  phi2 <- rnorm(iter, t2$phimu, t2$phisd)
  #phis <- rnorm(iter, phi$mu, phi$sd)
  
  s1 <- rnorm(iter, exp(of1 - df2), phi1)
  s2 <- rnorm(iter, exp(of2 - df1), phi2)
  list(s1=s1, s2=s2)
}

matchup.pois <- function(t1n, t2n, param, teams, its=10000, method="sum"){
  if(!method %in% c("sum", "prod")){
    stop("method must be one of: [sum, prod]")
  }
  t1 <- which(t1n==teams)
  t2 <- which(t2n==teams)
  
  if(method=="sum"){
    mc_matchup.pois_sum(list(ofmu=param$ofmu[t1], ofsd=param$ofsd[t1], dfmu=param$dfmu[t1], dfsd=param$dfsd[t1]),
               list(ofmu=param$ofmu[t2], ofsd=param$ofsd[t2], dfmu=param$dfmu[t2], dfsd=param$dfsd[t2]),
               iter=its)
  }else if(method=="prod"){
    mc_matchup.pois_prod(list(ofmu=param$ofmu[t1], ofsd=param$ofsd[t1], dfmu=param$dfmu[t1], dfsd=param$dfsd[t1]),
               list(ofmu=param$ofmu[t2], ofsd=param$ofsd[t2], dfmu=param$dfmu[t2], dfsd=param$dfsd[t2]),
               iter=its)
  }
  
}

matchup.norm <- function(t1n, t2n, param, teams, its=10000){
  t1 <- which(t1n==teams)
  t2 <- which(t2n==teams)
  
  mc_matchup.norm(list(ofmu=param$ofmu[t1], ofsd=param$ofsd[t1], dfmu=param$dfmu[t1], dfsd=param$dfsd[t1], phimu=param$phimu[t1], phisd=param$phisd[t1]),
             list(ofmu=param$ofmu[t2], ofsd=param$ofsd[t2], dfmu=param$dfmu[t2], dfsd=param$dfsd[t2], phimu=param$phimu[t2], phisd=param$phisd[t2]),
             #list(mu=param$phimu, sd=param$phisd),
             iter=its)
  
}
