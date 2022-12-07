functions{
    real conmax_lpdf(real y, real lam1, real lam2){
        
    }
}
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
    ofsd ~ lognormal(0, 1);
    dfsd ~ lognormal(0, 1);
    of ~ lognormal(0, ofsd);
    df ~ lognormal(0, dfsd);
    
    for(n in 1:N){
        pt1[n] ~ poisson_log(of[tid1[n]] - df[tid2[n]]);
        pt2[n] ~ poisson_log(of[tid2[n]] - df[tid1[n]]);
    }
}
