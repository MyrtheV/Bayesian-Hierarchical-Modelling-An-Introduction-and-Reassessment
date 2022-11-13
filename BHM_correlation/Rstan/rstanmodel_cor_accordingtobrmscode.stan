//
// Stan code from brms model 

// generated with brms 2.16.3
functions {
 /* compute correlated group-level effects
  * Args: 
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns: 
  *   matrix of scaled group-level effects
  */ 
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_a;  // number of population-level effects
  matrix[N, K_a] X_a;  // population-level design matrix
  int<lower=1> K_b;  // number of population-level effects
  matrix[N, K_b] X_b;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_a_1;
  vector[N] Z_1_a_2;
  vector[N] Z_1_a_3;
  vector[N] Z_1_a_4;
  vector[N] Z_1_a_5;
  int<lower=1> NC_1;  // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_b_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_a] b_a;  // population-level effects
  vector<lower=0>[K_b] b_b;  // population-level effects
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_a_1;
  vector[N_1] r_1_a_2;
  vector[N_1] r_1_a_3;
  vector[N_1] r_1_a_4;
  vector[N_1] r_1_a_5;
  vector[N_2] r_2_b_1;  // actual group-level effects
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_a_1 = r_1[, 1];
  r_1_a_2 = r_1[, 2];
  r_1_a_3 = r_1[, 3];
  r_1_a_4 = r_1[, 4];
  r_1_a_5 = r_1[, 5];
  r_2_b_1 = (sd_2[1] * (z_2[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_a = X_a * b_a;
    // initialize linear predictor term
    vector[N] nlp_b = X_b * b_b;
    // initialize non-linear predictor term
    vector[N] mu;
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_a[n] += r_1_a_1[J_1[n]] * Z_1_a_1[n] + r_1_a_2[J_1[n]] * Z_1_a_2[n] + r_1_a_3[J_1[n]] * Z_1_a_3[n] + r_1_a_4[J_1[n]] * Z_1_a_4[n] + r_1_a_5[J_1[n]] * Z_1_a_5[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_b[n] += r_2_b_1[J_2[n]] * Z_2_b_1[n];
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = nlp_a[n] + nlp_b[n];
    }
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += normal_lpdf(b_a[1] | 0,0.3);
  target += normal_lpdf(b_a[2] | 0,0.3);
  target += normal_lpdf(b_a[3] | 0,0.3);
  target += normal_lpdf(b_a[4] | 0,0.3);
  target += normal_lpdf(b_a[5] | 0,0.3);
  target += normal_lpdf(b_b | 0.5,1)
    - 1 * normal_lccdf(0 | 0.5,1);
  target += inv_gamma_lpdf(sigma | 13.8, 6.3);
  target += inv_gamma_lpdf(sd_1[1] | 13.8,5.3);
  target += inv_gamma_lpdf(sd_1[2] | 13.8,5.3);
  target += inv_gamma_lpdf(sd_1[3] | 13.8,5.3);
  target += inv_gamma_lpdf(sd_1[4] | 13.8,5.3);
  target += inv_gamma_lpdf(sd_1[5] | 13.8,5.3);
  target += std_normal_lpdf(to_vector(z_1));
  target += lkj_corr_cholesky_lpdf(L_1 | 1);
  target += inv_gamma_lpdf(sd_2[1] | 13.8, 6.3);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
}
> 

