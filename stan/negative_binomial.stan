data{
    int T; //number teams
    int N; //number of games
    int tid1[N]; //team id of team 1 of game n
    int tid2[N]; //team id of team 2 of game n
    int pt1[N]; // pt for team 1
    int pt2[N]; // pt for team 2
}
parameters{
    vector<lower=0>[T] of_eta;
    vector<lower=0>[T] phi;
    vector<lower=0>[T] df_eta;
    
    real<lower=0> of_etasd;
    real<lower=0> df_etasd;
    real<lower=0> phisd;
}
model{
    of_etasd ~ inv_gamma(1,1);
    df_etasd ~ inv_gamma(1,1);
    phisd ~ inv_gamma(1,1);
    
    of_eta ~ normal(0, of_etasd);
    df_eta ~ normal(0, df_etasd);
    phi ~ normal(0, phisd);
    
    for(n in 1:N){
        pt1[n] ~ neg_binomial_2_log(of_eta[tid1[n]] - df_eta[tid2[n]], phi[tid1[n]]);
        pt2[n] ~ neg_binomial_2_log(of_eta[tid2[n]] - df_eta[tid1[n]], phi[tid2[n]]);
    }
}
