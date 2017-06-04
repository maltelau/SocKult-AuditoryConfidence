data{
    // number of data points, groups, and vowels
    int<lower=1> N;
    int<lower=1> N_group;
    int<lower=1> N_vowel;
    
    // binomial data for the two psychometric functions
    int<lower=0> k_ind[N];
    int<lower=0> k_gro[N];
    int<lower=0> n_ind[N];
    int<lower=0> n_gro[N];
    
    // input data
    int group[N];
    int vowel[N];
    vector[N] intensity;
    
    // collective benefit model
    vector[N_group] synergy_l;
    
}

parameters{
    
    // random effects
    vector[N_group] ia_group;
    vector[N_group] ib_group;
    vector[N_group] ga_group;
    
    vector[N_vowel] ia_vowel;
    vector[N_vowel] ib_vowel;
    vector[N_vowel] ga_vowel;
    vector[N_vowel] gb_vowel;
    
    // main effects
    real ga;
    real gb;
    real ia;
    real ib;
    
    // =========================================================
        // parameters for the collective benefit model
    real<lower=0> collective_benefit[N_group];
    real<lower=0> collective_sigma;
    real col_a;
    real col_b;
    
}

transformed parameters{
    vector[N_group] gb_group;
    vector<lower=0,upper=1>[N] gb_group_theta;
    
    for (i in 1:N) {
        // collective benefit = inv_logit(dyad slope) / inv_logit(individual slope)
        // isolate for the group effect of the dyad slope
        gb_group_theta[i] = inv_logit(ib + ib_vowel[vowel[i]] + ib_group[group[i]]) * collective_benefit[group[i]];
        gb_group[group[i]] = logit(gb_group_theta[i]) - (gb + gb_vowel[vowel[i]]);
    }
}


model{
    // intermediate parameters for the psychometric curves
    vector[N] gtheta;
    vector[N] gA;
    vector[N] gB;
    
    vector[N] itheta;
    vector[N] iA;
    vector[N] iB;
    
    // intermediate parameter for the collective benefit model
    vector[N] collective_mu;
    
    // priors
    ia_group ~ normal(0,1);
    ib_group ~ normal(0,1);
    ga_group ~ normal(0,1);
    
    ia_vowel ~ normal(0,1);
    ib_vowel ~ normal(0,1);
    ga_vowel ~ normal(0,1);
    gb_vowel ~ normal(0,1);
    
    ia ~ normal(0, 1);
    ib ~ normal(0, 1);
    ga ~ normal(0, 1);
    gb ~ normal(0, 1);
    
    // psychometric function for both individuals and groups
    gA = ga + ga_vowel[vowel] + ga_group[group]; 
    gB = gb + gb_vowel[vowel] + gb_group[group];
    
    iA = ia + ia_vowel[vowel] + ia_group[group]; 
    iB = ib + ib_vowel[vowel] + ib_group[group];
    
    for (i in 1:N){
        // theta is not actually a rate parameter since I'm using binomial_logit
        gtheta[i] =  gA[i] + gB[i] * intensity[i];
        itheta[i] =  iA[i] + iB[i] * intensity[i];
    }
        
        k_gro ~ binomial_logit(n_gro, gtheta);
        k_ind ~ binomial_logit(n_ind, itheta);
        
        // here comes the collective benefit stuff
        
        
        for (k in 1:N_group) {
        collective_mu[k] = col_a + col_b * synergy_l[k];
        collective_benefit[k] ~ normal(collective_mu[k], collective_sigma);
        }
        
        col_a ~ normal(1,1);
        col_b ~ normal(0,1);
        collective_sigma ~ cauchy(0,2);
        
}
    
