// Log model scenario 3  
// Index values and observations.
data {
  int<lower = 1> All;               // Number of observations 
  int<lower = 1> N;                 // Number of groups 
  vector[All] y;                    // Vector of observations.
  int<lower=1> group_inds[All];     // Indicator which observation belongs to which individual 
  real a;                           // Prior: mean for mu, mu is the mean of gamma 
  real b;                           // Prior: variance for mu, mu is the mean of gamma  
  real a2;                         // Prior: mean for mean side            
  real b2;                         // Prior variance for mean side 
  real c;                          // Prior: mean for sigma, sigma is the variance of observations  
  real d;                          // Prior: variance for sigma, sigma is the variance of observations  
  real dd;                         // Prior: mean for g, g is the variance of theta 
  real f;                          // Prior: variance for g, g is the variance of theta 
  real a3;                        // Prior: mean for mean prior digit 
  real b3;                        // Prior: variance for mean prior digit/sd 
}

// Parameters and hyperparameters.
parameters {
  real mu;                        // Mean of the population model.
  real<lower = 0> g;            // Variance of the population model.
  real<lower = 0> sigma;          // Variance of the likelihood.
   vector[N] gammai;              // intercept 

}

// Hierarchical regression.
model {
 
  
  // Hyperpriors.
  target += normal_lpdf(mu| a, sqrt(b));             // mean of intercept (gamma), b = standard deviation , sqrt(b) is sd but since 1 not necessary to sqrt()
  target += inv_gamma_lpdf(g | dd, f);            // tau is g, g is the variance of intercept (gamma)   
  

  // Prior.
  target += inv_gamma_lpdf(sigma | c, d);         // variance of observations (y)  

  // Population model and likelihood.
  target += normal_lpdf(gammai | mu, sqrt(g));          // Intercept (gamma)
  
  
  //betaii 
  target += lognormal_lpdf(y | gammai[group_inds], sqrt(sigma)); // does not work

}




