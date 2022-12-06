// Multivariate hierarchical  

data {
  int<lower = 1> All;               // Number of observations 
  int<lower=0> N;                   // Number of groups, in our case number of individuals 
  int<lower=1> K;                   // num ind predictors, intercept + 5 predictors so 6 
  int<lower=1> group_inds[All];     // Indicator which observation belongs to which individual
  matrix[All, K] x;                 // matrix of predictors
  vector[All] y;                    // Vector of observations.

}
parameters {
  vector[K] beta_p[N];              // ind participant intercept and slope coefficients by group
  vector<lower=0>[K] sigma_p;       // sd for intercept and slope
  vector[K] beta;                   // intercept and slope hyper-priors
  corr_matrix[K] Omega;             // correlation matrix
  real<lower=0> sigma;              // population sigma
}
model {
  vector[All] mu;
  
// priors
  //beta ~ normal(0, 1);             // This is what I don't get 
  beta[1] ~ normal(0.5, 1)T[0,];     // mean intercept  
  beta[2] ~ normal(0, 0.3);          // mean beta, 0.09 is variance  
  beta[3] ~ normal(0, 0.3);          // mean delta  
  beta[4] ~ normal(0, 0.3);          // mean delta 
  beta[5] ~ normal(0, 0.3);          // mean delta 
  beta[6] ~ normal(0, 0.3);          // mean delta 
  Omega ~ lkj_corr(1);               // correlation 
  //sigma_p ~ exponential(1);  // This is just from an example 
  target += normal_lpdf(sigma_p[1] | 0, 1) - normal_lcdf(0 | 0, 1);    // variance beta 
  target += normal_lpdf(sigma_p[2] | 0, 1) - normal_lcdf(0 | 0, 1);    // variance delta 
  target += normal_lpdf(sigma_p[3] | 0, 1) - normal_lcdf(0 | 0, 1);    // variance delta 
  target += normal_lpdf(sigma_p[4] | 0, 1) - normal_lcdf(0 | 0, 1);    // variance delta 
  target += normal_lpdf(sigma_p[5] | 0, 1) - normal_lcdf(0 | 0, 1);    // variance delta 
  target += normal_lpdf(sigma_p[6] | 0, 1) - normal_lcdf(0 | 0, 1);    // variance delta 
  sigma ~ inv_gamma(3, 0.7);         // variance 
  beta_p ~ multi_normal(beta, quad_form_diag(Omega, sqrt(sigma_p)));
  
  
  // likelihood
  for(i in 1:All) {
    mu[i] = x[i] * (beta_p[group_inds[i]]); // * is matrix multiplication in this context
  }
  
  y ~ normal(mu, sqrt(sigma));
  
}



