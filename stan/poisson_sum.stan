data{
    int T; //number teams
    int N; //number of games
    int tid1[N]; //team id of team 1 of game n
    int tid2[N]; //team id of team 2 of game n
    int pt1[N]; // pt for team 1
    int pt2[N]; // pt for team 2
}
parameters{
    real<lower=0> ofsd;
    real<lower=0> dfsd;
    vector<lower=0>[T] of;
    vector<lower=0>[T] df;
}
model{
    ofsd ~ normal(0, 10);
    dfsd ~ normal(0, 10);
    of ~ normal(0, ofsd);
    df ~ normal(0, dfsd);
    
    for(n in 1:N){
        pt1[n] ~ poisson_log(of[tid1[n]] - df[tid2[n]]);
        pt2[n] ~ poisson_log(of[tid2[n]] - df[tid1[n]]);
    }
}
