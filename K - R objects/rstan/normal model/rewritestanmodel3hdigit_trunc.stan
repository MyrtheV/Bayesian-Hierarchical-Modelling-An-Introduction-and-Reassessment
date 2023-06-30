// Rewrite: multilevel example thesis saved as multileveltutorial in gibb sampler folder 
// Rewrite: multilevel example thesis saved as multileveltutorial in gibb sampler folder 
// Index values and observations.
data {
  int<lower = 1> All;               // Number of observations 
  int<lower = 1> N;                 // Number of groups 
  vector[All] y;                    // Vector of observations.
  int<lower=1> group_inds[All];     // Indicator which observation belongs to which individual 
  //vector[All] side;                 //Beta side, either x= 1/2 or -1/2 
  vector[All] dif1;                // Delta digit 8 and 7, either 1/2, -1/2 or 0 
  vector[All] dif2;                // Delta digit 7 and 6, either 1/2, -1/2 or 0 
  vector[All] dif3;                // Delta digit 4 and 3, either 1/2, -1/2 or 0 
  vector[All] dif4;                // Delta digit 3 and 2, either 1/2, -1/2 or 0 
  real a;                           // Prior: mean for mu, mu is the mean of gamma 
  int b;                           // Prior: variance for mu, mu is the mean of gamma  
  //int a2;                         // Prior: mean for mean side            
  //real b2;                         // Prior variance for mean side 
  int c;                          // Prior: mean for sigma, sigma is the variance of observations  
  real d;                          // Prior: variance for sigma, sigma is the variance of observations  
  int dd;                         // Prior: mean for g, g is the variance of theta 
  real f;                          // Prior: variance for g, g is the variance of theta 
  //real dd2;                       // Prior: mean for prior variance side, real if continuous variable 
  //real f2;                        // Prior: variance for prior variance side 
  int a3;                        // Prior: mean for mean prior digit 
  real b3;                        // Prior: variance for mean prior digit/sd 
  int dd3;                       // Prior: mean for variance prior digit 
  real f3;                        // Prior: variance for variance prior digit 
}

// Parameters and hyperparameters.
parameters {
  real mu;                        // Mean of the population model.
  //real mu2;                       // mean population model side 
  real mu3;                       // mean of population model digit 
  real mu4;                       // mean of population model digit 
  real mu5;                       // mean of population model digit 
  real mu6;                       // mean of population model digit 
  real<lower = 0> g;            // Variance of the population model.
  //real<lower=0> g2;              // Variance population model side 
  real<lower=0> g3;               // Variance population model digit 
  real<lower=0> g4;               // Variance population model digit 
  real<lower=0> g5;               // Variance population model digit 
  real<lower=0> g6;               // Variance population model digit 
  real<lower = 0> sigma;          // Variance of the likelihood.
   vector[N] gammai;              // intercept 
   //vector[N] betai;               // side effect 
   vector[N] delta1;              // Digit effect difference 8 and 7 
   vector[N] delta2;              // Digit effect difference 7 and 6 
   vector[N] delta3;              // Digit effect difference 4 and 3 
   vector[N] delta4;              // Digit effect difference 3 and 2 
}

// Hierarchical regression.
model {
 
  
  // Hyperpriors.
  target += normal_lpdf(mu| a, sqrt(b))-normal_lccdf(0.0 | a, sqrt(b));             // mean of intercept (gamma), b = standard deviation , sqrt(b) is sd but since 1 not necessary to sqrt()
  target += inv_gamma_lpdf(g | dd, f);            // tau is g, g is the variance of intercept (gamma)   
  
  //target += normal_lpdf(mu2 | a2, sqrt(b2));            // mean for side (beta), sqrt(b2) but since 1 not necessary 
  //target += inv_gamma_lpdf(g2 | dd2, f2);         // variance for side (beta)
  
  target += normal_lpdf(mu3 | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(g3| dd3, f3);         // variance for digit (delta's)  

  target += normal_lpdf(mu4 | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(g4| dd3, f3);         // variance for digit (delta's)  

  target += normal_lpdf(mu5 | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(g5| dd3, f3);         // variance for digit (delta's)  

  target += normal_lpdf(mu6 | a3, sqrt(b3));            // mean for digit (delta's) 
  target += inv_gamma_lpdf(g6| dd3, f3);         // variance for digit (delta's)  
  
  // Prior.
  target += inv_gamma_lpdf(sigma | c, d);         // variance of observations (y)  

  // Population model and likelihood.
  target += normal_lpdf(gammai | mu, sqrt(g));          // Intercept (gamma)
  //target += normal_lpdf(betai | mu2, sqrt(g2));         // Side effect (beta)
  target += normal_lpdf(delta1 | mu3, sqrt(g3));       // Digit effect difference 8 and 7 (delta1)
  target += normal_lpdf(delta2 | mu4, sqrt(g4));       // Digit effect difference 7 and 6 (delta2)
  target += normal_lpdf(delta3 | mu5, sqrt(g5));       // Digit effect difference 4 and 3 (delta3)
  target += normal_lpdf(delta4 | mu6, sqrt(g6));       // Digit effect difference 3 and 2 (delta4)

  
  //betaii 
  target += normal_lpdf(y | gammai[group_inds] + dif1 .* delta1[group_inds] + dif2 .* delta2[group_inds] + dif3 .* delta3[group_inds] + dif4 .* delta4[group_inds], sqrt(sigma)); // does not work

}


