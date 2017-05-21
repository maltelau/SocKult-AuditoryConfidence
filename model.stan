//modell
data{
    // number of data points, groups, and vowels
    int<lower=1> N;
    int<lower=1> N_group;
    int<lower=1> N_vowel;
    
    // binomial data for the two psychometric functions
    int k_ind[N];
    int k_gro[N];
    int n_ind[N];
    int n_gro[N];
    
    // input data
    int group[N];
    int intensity[N];
    int vowel[N];
    
    
    // collective benefit model
    // real local_confidence[N];
    
}


parameters{
    
    // less important note: I tried two different parametrisations of the random slope-intercept covariance,
    // and couldn't get the cholesky decomposition to work 
    // ==================================
    // Option 1) specify var-covar matrices manually
    vector[2] gv_group[N_group];
    vector[2] gv_vowel[N_vowel];
    vector[2] iv_group[N_group];
    vector[2] iv_vowel[N_vowel];
    
    corr_matrix[2] gRho_group;
    corr_matrix[2] gRho_vowel;
    corr_matrix[2] iRho_group;
    corr_matrix[2] iRho_vowel;
    
    
    
    // =========================================
    // Option 2: cholesky decomposition
    // cholesky_factor_corr[2] gL_group;
    // vector<lower=0>[2] gcsigma_group;
    // cholesky_factor_corr[2] gL_vowel;
    // vector<lower=0>[2] gcsigma_vowel;
    // 
    // cholesky_factor_corr[2] iL_group;
    // vector<lower=0>[2] icsigma_group;
    // cholesky_factor_corr[2] iL_vowel;
    // vector<lower=0>[2] icsigma_vowel;

    
    // scale parameters for the random effects
    vector<lower=0>[2] gsigma_group;
    vector<lower=0>[2] gsigma_vowel;
    vector<lower=0>[2] isigma_group;
    vector<lower=0>[2] isigma_vowel;
    
    // main effects
    real ga;
    real gb;
    real ia;
    real ib;
    
    
    // =========================================================
    // parameters for the collective benefit model
    // real collective_mu;
    // real<lower=0> collective_sigma;
    // 
    real col_a;
    // real col_b_confidence;
    

}
transformed parameters{
    real collective_benefit[N];
    for ( i in 1:N ) {
        collective_benefit[i] = inv_logit(
            gb + gv_vowel[vowel[i],2] * gsigma_vowel[2] + gv_group[group[i],2] * gsigma_group[2])
        / inv_logit(
            gb + iv_vowel[vowel[i],2] * isigma_vowel[2] + iv_group[group[i],2] * isigma_group[2]);
        // collective_benefit[i] = inv_logit(gb) - inv_logit(ib);
    }
    
}


model{
    
    // logit(theta) = A + B * data
    vector[N] gtheta;
    vector[N] gA;
    vector[N] gB;
    
    vector[N] itheta;
    vector[N] iA;
    vector[N] iB;
    
    real collective_mu[N];
    
    ia ~ normal(0, 1);
    ib ~ normal(0, 1);
    ga ~ normal(0, 1);
    gb ~ normal(0, 1);
    
    
    // scale of the random effects
    gsigma_group ~ cauchy( 0 , 2 );
    gsigma_vowel ~ cauchy( 0 , 2 );
    isigma_group ~ cauchy( 0 , 2 );
    isigma_vowel ~ cauchy( 0 , 2 );
    
    
    
    // ==================================
    // 1) specify var-covar matrices manually
    // correlations of the random effects
    gv_group ~ multi_normal(rep_vector(0,2), gRho_group);
    gv_vowel ~ multi_normal(rep_vector(0,2), gRho_vowel);
    iv_group ~ multi_normal(rep_vector(0,2), iRho_group);
    iv_vowel ~ multi_normal(rep_vector(0,2), iRho_vowel);
    
    
    gRho_group ~ lkj_corr( 4 );
    gRho_vowel ~ lkj_corr( 4 );
    iRho_group ~ lkj_corr( 4 );
    iRho_vowel ~ lkj_corr( 4 );
    
    
    
    // =========================================
    // 2: cholesky decomposition
    // cholesky stuff - gives me high Rhat and constant L=1
    // gv_group ~ multi_normal_cholesky( rep_vector(0,2) ,  diag_pre_multiply(gcsigma_group, gL_group));
    // gv_vowel ~ multi_normal_cholesky( rep_vector(0,2) ,  diag_pre_multiply(gcsigma_vowel, gL_vowel));
    // gcsigma_vowel ~ cauchy(0,2);
    // gcsigma_group ~ cauchy(0,2);
    // gL_group ~ lkj_corr_cholesky(4);
    // gL_vowel ~ lkj_corr_cholesky(4);
    // 
    // iv_group ~ multi_normal_cholesky( rep_vector(0,2) ,  diag_pre_multiply(icsigma_group, iL_group));
    // iv_vowel ~ multi_normal_cholesky( rep_vector(0,2) ,  diag_pre_multiply(icsigma_vowel, iL_vowel));
    // icsigma_vowel ~ cauchy(0,2);
    // icsigma_group ~ cauchy(0,2);
    // iL_group ~ lkj_corr_cholesky(4);
    // iL_vowel ~ lkj_corr_cholesky(4);

    
    
    
    
    // psychometric function for both individuals and groups
    for ( i in 1:N ) {
        gA[i] = ga + gv_vowel[vowel[i],1] * gsigma_vowel[1] + gv_group[group[i],1] * gsigma_group[1]; 
        gB[i] = gb + gv_vowel[vowel[i],2] * gsigma_vowel[2] + gv_group[group[i],2] * gsigma_group[2];
        gtheta[i] =  gA[i] + gB[i] * intensity[i];
        
        iA[i] = ia + iv_vowel[vowel[i],1] * isigma_vowel[1] + iv_group[group[i],1] * isigma_group[1]; 
        iB[i] = gb + iv_vowel[vowel[i],2] * isigma_vowel[2] + iv_group[group[i],2] * isigma_group[2];
        itheta[i] =  iA[i] + iB[i] * intensity[i];
    }
    k_gro ~ binomial_logit(n_gro, gtheta);
    k_ind ~ binomial_logit(n_ind, itheta);
    
    
    
    // here comes the collective benefit stuff
    // not sure if I should do something something with a jacobian b/c of sampling a transformed parameter
    
    for ( i in 1:N ) {
        // collective_benefit[i] ~ normal(1, 1);
    
    // sends rhat through the roof:
        collective_mu[i] = col_a;// + col_b_confidence * local_confidence[i];
    }
    
    
    collective_benefit ~ normal(collective_mu, 1);
    // 
    col_a ~ normal(1,10);
    // col_b_confidence ~ normal(0,10);
    // collective_sigma ~ cauchy(0,2);
    
    
    
// generated quantities{
}
//     // =========================================
//     // 2: cholesky decomposition
//     // transform cholesky stuff back to var-covar matrixes
//     matrix[2,2] gO_group;
//     matrix[2,2] gSigma_group;
//     matrix[2,2] gO_vowel;
//     matrix[2,2] gSigma_vowel;
//     matrix[2,2] iO_group;
//     matrix[2,2] iSigma_group;
//     matrix[2,2] iO_vowel;
//     matrix[2,2] iSigma_vowel;
//     
//     gO_group = multiply_lower_tri_self_transpose(gL_group);
//     gSigma_group = quad_form_diag(gO_group, gcsigma_group); 
//     gO_vowel = multiply_lower_tri_self_transpose(gL_vowel);
//     gSigma_vowel = quad_form_diag(gO_vowel, gcsigma_vowel); 
//     
//     iO_group = multiply_lower_tri_self_transpose(iL_group);
//     iSigma_group = quad_form_diag(iO_group, icsigma_group); 
//     iO_vowel = multiply_lower_tri_self_transpose(iL_vowel);
//     iSigma_vowel = quad_form_diag(iO_vowel, icsigma_vowel); 
// }
