//
// Multilevel with correlated random effects 
// Index values and observations.
data {
  int<lower = 1> All;               // Number of observations 
  int<lower = 1> N;                 // Number of participants 
  vector[All] y;                    // Vector of observations.
  int<lower=1> group_inds[All];     // Indicator which observation belongs to which individual 
  int<lower=1> K;                   // num ind predictors, intercept + 5 predictors so 6 
  matrix[All, K] x;                 // matrix of predictors
  
  vector[All] side;                 //Beta side, either x= 1/2 or -1/2 
  vector[All] dif1;                // Delta digit 8 and 7, either 1/2, -1/2 or 0 
  vector[All] dif2;                // Delta digit 7 and 6, either 1/2, -1/2 or 0 
  vector[All] dif3;                // Delta digit 4 and 3, either 1/2, -1/2 or 0 
  vector[All] dif4;                // Delta digit 3 and 2, either 1/2, -1/2 or 0 
  real a;                           // Prior: mean for mu, mu is the mean of gamma 
  real b;                           // Prior: variance for mu, mu is the mean of gamma  
  real a2;                         // Prior: mean for mean side            
  real b2;                         // Prior variance for mean side 
  real c;                          // Prior: mean for sigma, sigma is the variance of observations  
  real d;                          // Prior: variance for sigma, sigma is the variance of observations  
  real dd;                         // Prior: mean for g, g is the variance of theta 
  real f;                          // Prior: variance for g, g is the variance of theta 
  real dd2;                       // Prior: mean for prior variance side, real if continuous variable 
  real f2;                        // Prior: variance for prior variance side 
  real a3;                        // Prior: mean for mean prior digit 
  real b3;                        // Prior: variance for mean prior digit/sd 
  real dd3;                       // Prior: mean for variance prior digit 
  real f3;                        // Prior: variance for variance prior digit 
  

}

// Parameters and hyperparameters.
parameters {
  vector<lower=0>[K] s2_p;       // sd for intercept and slope
  vector[K] beta;                   // population intercept and slope (hyper-priors)
  real<lower=0> sigma;              // population standard deviation
  cholesky_factor_corr[K] Lcorr;    // Cholesky factor for the correlation matrix
  vector[K] beta_p_raw[N];               // stand-in variables for non-centered parameterization. non-centered parameterization was needed to avoid divergent transitions
  }
  
transformed parameters {
  vector[K] beta_p[N];              // ind participant intercept and slope coefficients by group
  matrix[K,K] L =  diag_pre_multiply(s2_p, Lcorr);
  real<lower=0> s2;              // population variance

  for(i in 1:N) {
    beta_p[i] = beta + L * beta_p_raw[i];  // implies theta ~ Multivariate-Normal(beta, diag_pre_multiply(sigma,Lcorr))
  }
  
  s2 = sigma^2;
}

// Hierarchical regression.
model {
  for(i in 1:N){
    beta_p_raw[i] ~ std_normal();                    //  ensures non-centered parameterization
  }
  
  target += normal_lpdf(beta[1]| a, sqrt(b))-normal_lccdf(0.0 | a, sqrt(b));             // mean of intercept (gamma), b = standard deviation , sqrt(b) is sd but since 1 not necessary to sqrt()
  target += inv_gamma_lpdf(s2_p[1] | dd, f);            // tau is g, g is the variance of intercept (gamma)   
  
  target += normal_lpdf(beta[2] | a2, sqrt(b2));            // mean for side (beta), sqrt(b2) but since 1 not necessary 
  target += inv_gamma_lpdf(s2_p[2] | dd2, f2);         // variance for side (beta)
  
  target += normal_lpdf(beta[3] | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(s2_p[3]| dd3, f3);         // variance for digit (delta's)  

  target += normal_lpdf(beta[4] | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(s2_p[4]| dd3, f3);         // variance for digit (delta's)  

  target += normal_lpdf(beta[5] | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(s2_p[5]| dd3, f3);         // variance for digit (delta's)  

  target += normal_lpdf(beta[6] | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(s2_p[6]| dd3, f3);         // variance for digit (delta's)  
  
 // target += normal_lpdf(beta[1]| a, sqrt(b))-normal_lccdf(0.0 | a, b);             // mean of intercept (gamma), b = standard deviation , sqrt(b) is sd but since 1 not necessary to sqrt()
 // target += normal_lpdf(sigma_p[1] | dd, f);            // sd of intercept (gamma)   
  
 // target += normal_lpdf(beta[2] | a2, b2);            // mean for side (beta), sqrt(b2) but since 1 not necessary 
 // target += normal_lpdf(sigma_p[2] | dd2, f2);         // sd for side (beta)
  
 // target += normal_lpdf(beta[3] | a3, b3);            // mean for digit (delta's) 
 // target += normal_lpdf(sigma_p[3]| dd3, f3);         // sd for digit (delta's)  

 // target += normal_lpdf(beta[4] | a3, b3);            // mean for digit (delta's) 
 // target += normal_lpdf(sigma_p[4]| dd3, f3);         // sd for digit (delta's)  

 // target += normal_lpdf(beta[5] | a3, b3);            // mean for digit (delta's) 
 // target += normal_lpdf(sigma_p[5]| dd3, f3);         // sd for digit (delta's)  

 // target += normal_lpdf(beta[6] | a3, b3);            // mean for digit (delta's) 
 // target += normal_lpdf(sigma_p[6]| dd3, f3);         // sd for digit (delta's)  
  
  Lcorr ~ lkj_corr_cholesky(1);                     // prior on correlations between effects of interest
  //target += lkj_corr_lpdf(Omega | 1);          // stan suggest to use lkj_cor_cholesky_lpdf, I don't know what the difference is exactly 
  
  // Prior.
  target += inv_gamma_lpdf(s2 | c, d);         // variance of observations (y)  
  // target += normal_lpdf(sigma | c, d);         // sd of observations (y)  

  
  {
    vector[All] x_beta_All;
    for (n in 1:All){
      x_beta_All[n] = x[n, ] * beta_p[group_inds[n]];
      target += normal_lpdf(y[n] | x_beta_All[n], sigma);
    }
  }

}

generated quantities {
  matrix[K,K] Omega;
  Omega = multiply_lower_tri_self_transpose(Lcorr);   // creates correlation matrix
}
