data{
    int T; //number teams
    int N; //number of games
    int tid1[N]; //team id of team 1 of game n
    int tid2[N]; //team id of team 2 of game n
    real pt1[N]; // pt for team 1
    real pt2[N]; // pt for team 2
}
parameters{
    vector<lower=0>[T] of;
    vector<lower=0>[T] df;
    real<lower=0> phi;
    
    real<lower=0> ofsd;
    real<lower=0> dfsd;
    real<lower=0> phisd;
}
model{
    ofsd ~ lognormal(0, 1);
    dfsd ~ lognormal(0, 1);
    phisd ~ lognormal(1, 1);
    
    of ~ normal(0, ofsd);
    df ~ normal(0, dfsd);
    phi ~ double_exponential(1, phisd);
    
    for(n in 1:N){
        pt1[n] ~ normal(exp(of[tid1[n]] - df[tid2[n]]), phi);
        pt2[n] ~ normal(exp(of[tid2[n]] - df[tid1[n]]), phi);
    }
}
