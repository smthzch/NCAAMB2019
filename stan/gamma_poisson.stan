data{
    int T; //number teams
    int N; //number of games
    int tid1[N]; //team id of team 1 of game n
    int tid2[N]; //team id of team 2 of game n
    int pt1[N]; // pt for team 1
    int pt2[N]; // pt for team 2
}
parameters{
    real<lower=0> ofa[T];
    real<lower=0> ofb[T];
    real<lower=0> dfa[T];
    real<lower=0> dfb[T];
    matrix<lower=0>[N, 2] of;
    matrix<lower=0>[N, 2] df;
}
model{
    ofa ~ lognormal(0, 0.1);
    ofb ~ lognormal(0, 0.1);
    dfa ~ lognormal(0, 0.1);
    dfb ~ lognormal(0, 0.1);
    
    for(n in 1:N){
        of[n, 1] ~ gamma(ofa[tid1[n]], ofb[tid1[n]]);
        of[n, 2] ~ gamma(ofa[tid2[n]], ofb[tid2[n]]);
        df[n, 1] ~ gamma(dfa[tid1[n]], dfb[tid1[n]]);
        df[n, 2] ~ gamma(dfa[tid2[n]], dfb[tid2[n]]);
        pt1[n] ~ poisson(of[n, 1] / df[n, 2]);
        pt2[n] ~ poisson(of[n, 2] / df[n, 1]);
    }
}
