data{
    int T; //number teams
    int N; //number of games
    int tid1[N]; //team id of team 1 of game n
    int tid2[N]; //team id of team 2 of game n
    int pt1[N]; // pt for team 1
    int pt2[N]; // pt for team 2
}
parameters{
    //hyperparameters
    real<lower=0> ofsd1;
    real<lower=0> ofsd2;
    real<lower=0> dfsd;
    //parameters of interest
    vector<lower=0>[T] of[2]; //team level offensive parameters
    vector<lower=0>[T] df; //team level defensive parameter
    simplex[2] pis[T]; //team level offensive mixing
}
model{
    vector[2] alpha;
    alpha[1] = 10;
    alpha[2] = 1;
    
    of[1] ~ normal(0, ofsd1);
    of[2] ~ normal(0, ofsd2);
    df ~ normal(0, dfsd);
    
    for(t in 1:T){
        pis[t] ~ dirichlet(alpha);
    }
    
    for(n in 1:N){
        vector[2] tosum;
        tosum[1] = log(pis[tid1[n]][1]) + poisson_log_lpmf(pt1[n] | of[1][tid1[n]] - df[tid2[n]]);
        tosum[2] = log(pis[tid1[n]][2]) + poisson_log_lpmf(pt1[n] | of[2][tid1[n]] - df[tid2[n]]);
        target += log_sum_exp(tosum);
        tosum[1] = log(pis[tid2[n]][1]) + poisson_log_lpmf(pt2[n] | of[1][tid2[n]] - df[tid1[n]]);
        tosum[2] = log(pis[tid2[n]][2]) + poisson_log_lpmf(pt2[n] | of[2][tid2[n]] - df[tid1[n]]);
        target += log_sum_exp(tosum);
    }
}
