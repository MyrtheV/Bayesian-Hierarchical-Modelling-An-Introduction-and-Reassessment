# New brms fit correlation

library(brms)

################################################################################
## 1. Load data 
################################################################################
indat=read.table(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all'))
colnames(indat)=c('sub','block','trial','stim','resp','rt','error')

## Cleaning the data according to criteria discussed in paper 
clean=function()
{
  indat=read.table(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all'))
  colnames(indat)=c('sub','block','trial','stim','resp','rt','error')
  
  bad1=indat$sub%in%c(34,43)
  bad2=indat$rt<250 | indat$rt>2000
  bad3=indat$err==1
  bad4=indat$block==0 & indat$trial<20
  bad5=indat$trial==0
  
  bad=bad1 | bad2 | bad3 |bad4 |bad5
  dat=indat[!bad,]
  return(dat)
}

indat1 <- clean()  # Final dataset 

## Data add necessary information 
### Add participant number 
rank(unique(indat1$sub))
sub <- rep(1:N, each = J)
true.theta[sub]

# create variable inter for the intercept gamma 
indat1$inter <- rep(1, nrow(indat1))

for (j in 1:nrow(indat1)){
  if (j == 1) {
    indat1$sub1[j] <- 1}
  else if (indat1$sub[j] == indat1$sub[j-1]) {
    indat1$sub1[j] <- indat1$sub1[j-1]}
  else indat1$sub1[j] <- indat1$sub1[j-1] + 1
}

### Add variable with information of the side of the digit to the list, for parameter beta    
# j = condition, if j > 5 than x = -1/2, if j < 5 than x = 1/2 
# indat1$stim 0 = 2, 1=3, 2=4, 3=6, 4=7, 5=8 
# so if bigger than 2 than 1/2, else than -1/2 
for (j in 1:nrow(indat1)){
  if (indat1$stim[j] < 3) {
    indat1$side[j] <- 1/2}
  else indat1$side[j] <- -1/2
}

### Add variable with information about difference between digits to the list, for parameters delta's 
#### Difference 8 and 7 
for (j in 1:nrow(indat1)){ 
  if (indat1$stim[j] == 5) {  # 5 = 8 , reference: https://github.com/PerceptionCognitionLab/data0/blob/master/lexDec-dist5/ld5.txt
    indat1$dif1[j] <- 0}
  else if (indat1$stim[j] == 4) {
    indat1$dif1[j] <- 1}
  else indat1$dif1[j] <- 0
}

#### Difference 7 and 6 
for (j in 1:nrow(indat1)){ 
  if (indat1$stim[j] == 4) {  # 4 = 7 , reference: https://github.com/PerceptionCognitionLab/data0/blob/master/lexDec-dist5/ld5.txt
    indat1$dif2[j] <- 0}
  else if (indat1$stim[j] == 3) {  # 3 = 6 
    indat1$dif2[j] <- 1}
  else indat1$dif2[j] <- 0
}

#### Difference 4 and 3 
for (j in 1:nrow(indat1)){ 
  if (indat1$stim[j] == 1) {  # 1 = 3, 2 = 4 , reference: https://github.com/PerceptionCognitionLab/data0/blob/master/lexDec-dist5/ld5.txt
    indat1$dif3[j] <- 0}
  else if (indat1$stim[j] == 2) {
    indat1$dif3[j] <- 1}
  else indat1$dif3[j] <- 0
}

#### Difference 3 and 2 
for (j in 1:nrow(indat1)){ 
  if (indat1$stim[j] == 0) {  # 0 = 2, 1 = 3 , reference: https://github.com/PerceptionCognitionLab/data0/blob/master/lexDec-dist5/ld5.txt
    indat1$dif4[j] <- 0}
  else if (indat1$stim[j] == 1) {
    indat1$dif4[j] <- 1}
  else indat1$dif4[j] <- 0
}

### Response time from miliseconds to seconds 
indat1$rt2 <- indat1$rt/1000

# Priors 
a <- 0.5  # mean for mu, mu is the mean of theta  
b <- 1  # sd for mu 
c <- 13.8 # mean for sigma, not rounded 13.75134
d <- 6.3 # sd for sigma, sigma is the sd of observations, not rounded 6.265999  
e <- 13.8 # mean for g, g is the sd of theta, not rounded 13.75134   
f <- 6.3  # sd for g, not rounded 6.265999 

#### Priors for beta 
a2 <- 0  # mean for mean of beta
b2 <- 0.3  # sd for mean of beta, used to be 1 
e2 <- 13.8 # mean for sd of beta, not rounded 13.75134
f2 <- 5.3 # sd for sd of beta, not rounded 5.295736 

#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.3  # sd for mean of delta, 0.5, 0.3  
dd3 <- 13.8  # mean for sd of delta, not rounded 13.75134
f3 <- 5.3   # sd for sd of delta, not rounded 5.295736


priorsmodel9_cor <- c(set_prior("normal(0.5,1)", class = "b", lb = 0, nlpar = "b"),  
                  set_prior("inv_gamma(13.8, 6.3)", class = "sd", coef="inter", group = "sub1", nlpar = "b"), 
                  set_prior("normal(0,0.3)", class = "b", coef="side",  nlpar = "a"),  
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="side", group = "sub1",  nlpar = "a"), 
                  set_prior("normal(0,0.3)", class = "b", coef="dif1",  nlpar = "a"), 
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif1", group = "sub1",  nlpar = "a"),
                  set_prior("normal(0,0.3)", class = "b", coef="dif2",  nlpar = "a"),  
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif2", group = "sub1",  nlpar = "a"),
                  set_prior("normal(0,0.3)", class = "b", coef="dif3",  nlpar = "a"), 
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif3", group = "sub1",  nlpar = "a"),
                  set_prior("normal(0,0.3)", class = "b", coef="dif4",  nlpar = "a"),  
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif4", group = "sub1",  nlpar = "a"),
                  set_prior("inv_gamma(13.8, 6.3)", class = "sigma"))

# Fit Model 
bform_cor <- brms::bf(rt2 ~ a + b, nl = TRUE, 
                  lf(a ~ 0 + side + dif1 + dif2 + dif3 + dif4 + (0 + side + dif1 + dif2 + dif3 + dif4 | sub1), center = TRUE),  
                  lf(b ~ 0 + inter + (0 + inter | sub1), cmc = FALSE)
)

# Adjust 'myPath' to your directory 
brmmodel9_new21102022 <- brm(formula = bform_cor,         # Model formula 
                             data = indat1,              # Data frame with variables + dependent variable 
                             family = gaussian(),        # Response distribution 
                             prior = priorsmodel9_cor,       # Priors 
                             # sample_prior = TRUE, need to add this for model comparison  
                             warmup = 1000,              # Iterations used for warmup 
                             iter = 4000,                # Total iterations per chain 
                             chains = 4,                 # Number of chains 
                             core = 4,                   # Cores for parallel estimation 
                             #control = list(adapt_delta = 0.95),  # Control sampler's behavior, this avoided the problem of convergement 
                             # Finally we can save the model fit to a file 
                             file = "/myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R code/BRMSmodelfit19_21102022")

# Warning messages brmmodel9_new21102022:
#   1: The largest R-hat is 1.74, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 2: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 3: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 

# prior_summary(brmmodel9_new21102022)
# prior class  coef group resp dpar nlpar     bound       source
# (flat)     b                           a                default
# normal(0,0.3)     b  dif1                     a                   user
# normal(0,0.3)     b  dif2                     a                   user
# normal(0,0.3)     b  dif3                     a                   user
# normal(0,0.3)     b  dif4                     a                   user
# normal(0,0.3)     b  side                     a                   user
# normal(0.5,1)     b                           b <lower=0>         user
# normal(0.5,1)     b inter                     b           (vectorized)
# lkj_corr_cholesky(1)     L                                            default
# lkj_corr_cholesky(1)     L        sub1                           (vectorized)
# student_t(3, 0, 2.5)    sd                           a                default
# student_t(3, 0, 2.5)    sd                           b                default
# student_t(3, 0, 2.5)    sd        sub1               a           (vectorized)
# inv_gamma(13.8,5.3)    sd  dif1  sub1               a                   user
# inv_gamma(13.8,5.3)    sd  dif2  sub1               a                   user
# inv_gamma(13.8,5.3)    sd  dif3  sub1               a                   user
# inv_gamma(13.8,5.3)    sd  dif4  sub1               a                   user
# inv_gamma(13.8,5.3)    sd  side  sub1               a                   user
# student_t(3, 0, 2.5)    sd        sub1               b           (vectorized)
# inv_gamma(13.8, 6.3)    sd inter  sub1               b                   user
# inv_gamma(13.8, 6.3) sigma                                               user


# Stan code (stancode(brmmodel9_new21102022))
# // generated with brms 2.16.3
# functions {
#   /* compute correlated group-level effects
#   * Args: 
#     *   z: matrix of unscaled group-level effects
#   *   SD: vector of standard deviation parameters
#   *   L: cholesky factor correlation matrix
#   * Returns: 
#     *   matrix of scaled group-level effects
#   */ 
#     matrix scale_r_cor(matrix z, vector SD, matrix L) {
#       // r is stored in another dimension order than z
#       return transpose(diag_pre_multiply(SD, L) * z);
#     }
# }
# data {
#   int<lower=1> N;  // total number of observations
#   vector[N] Y;  // response variable
#   int<lower=1> K_a;  // number of population-level effects
#   matrix[N, K_a] X_a;  // population-level design matrix
#   int<lower=1> K_b;  // number of population-level effects
#   matrix[N, K_b] X_b;  // population-level design matrix
#   // data for group-level effects of ID 1
#   int<lower=1> N_1;  // number of grouping levels
#   int<lower=1> M_1;  // number of coefficients per level
#   int<lower=1> J_1[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_1_a_1;
#   vector[N] Z_1_a_2;
#   vector[N] Z_1_a_3;
#   vector[N] Z_1_a_4;
#   vector[N] Z_1_a_5;
#   int<lower=1> NC_1;  // number of group-level correlations
#   // data for group-level effects of ID 2
#   int<lower=1> N_2;  // number of grouping levels
#   int<lower=1> M_2;  // number of coefficients per level
#   int<lower=1> J_2[N];  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_2_b_1;
#   int prior_only;  // should the likelihood be ignored?
# }
# transformed data {
# }
# parameters {
#   vector[K_a] b_a;  // population-level effects
#   vector<lower=0>[K_b] b_b;  // population-level effects
#   real<lower=0> sigma;  // dispersion parameter
#   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#   matrix[M_1, N_1] z_1;  // standardized group-level effects
#   cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
#   vector<lower=0>[M_2] sd_2;  // group-level standard deviations
#   vector[N_2] z_2[M_2];  // standardized group-level effects
# }
# transformed parameters {
#   matrix[N_1, M_1] r_1;  // actual group-level effects
#   // using vectors speeds up indexing in loops
#   vector[N_1] r_1_a_1;
#   vector[N_1] r_1_a_2;
#   vector[N_1] r_1_a_3;
#   vector[N_1] r_1_a_4;
#   vector[N_1] r_1_a_5;
#   vector[N_2] r_2_b_1;  // actual group-level effects
#   // compute actual group-level effects
#   r_1 = scale_r_cor(z_1, sd_1, L_1);
#   r_1_a_1 = r_1[, 1];
#   r_1_a_2 = r_1[, 2];
#   r_1_a_3 = r_1[, 3];
#   r_1_a_4 = r_1[, 4];
#   r_1_a_5 = r_1[, 5];
#   r_2_b_1 = (sd_2[1] * (z_2[1]));
# }
# model {
#   // likelihood including constants
#   if (!prior_only) {
#     // initialize linear predictor term
#     vector[N] nlp_a = X_a * b_a;
#     // initialize linear predictor term
#     vector[N] nlp_b = X_b * b_b;
#     // initialize non-linear predictor term
#     vector[N] mu;
#     for (n in 1:N) {
#       // add more terms to the linear predictor
#       nlp_a[n] += r_1_a_1[J_1[n]] * Z_1_a_1[n] + r_1_a_2[J_1[n]] * Z_1_a_2[n] + r_1_a_3[J_1[n]] * Z_1_a_3[n] + r_1_a_4[J_1[n]] * Z_1_a_4[n] + r_1_a_5[J_1[n]] * Z_1_a_5[n];
#     }
#     for (n in 1:N) {
#       // add more terms to the linear predictor
#       nlp_b[n] += r_2_b_1[J_2[n]] * Z_2_b_1[n];
#     }
#     for (n in 1:N) {
#       // compute non-linear predictor values
#       mu[n] = nlp_a[n] + nlp_b[n];
#     }
#     target += normal_lpdf(Y | mu, sigma);
#   }
#   // priors including constants
#   target += normal_lpdf(b_a[1] | 0,0.3);
#   target += normal_lpdf(b_a[2] | 0,0.3);
#   target += normal_lpdf(b_a[3] | 0,0.3);
#   target += normal_lpdf(b_a[4] | 0,0.3);
#   target += normal_lpdf(b_a[5] | 0,0.3);
#   target += normal_lpdf(b_b | 0.5,1)
#   - 1 * normal_lccdf(0 | 0.5,1);
#   target += inv_gamma_lpdf(sigma | 13.8, 6.3);
#   target += inv_gamma_lpdf(sd_1[1] | 13.8,5.3);
#   target += inv_gamma_lpdf(sd_1[2] | 13.8,5.3);
#   target += inv_gamma_lpdf(sd_1[3] | 13.8,5.3);
#   target += inv_gamma_lpdf(sd_1[4] | 13.8,5.3);
#   target += inv_gamma_lpdf(sd_1[5] | 13.8,5.3);
#   target += std_normal_lpdf(to_vector(z_1));
#   target += lkj_corr_cholesky_lpdf(L_1 | 1);
#   target += inv_gamma_lpdf(sd_2[1] | 13.8, 6.3);
#   target += std_normal_lpdf(z_2[1]);
# }
# generated quantities {
#   // compute group-level correlations
#   corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
#   vector<lower=-1,upper=1>[NC_1] cor_1;
#   // extract upper diagonal of correlation matrix
#   for (k in 1:M_1) {
#     for (j in 1:(k - 1)) {
#       cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
#     }
#   }
# }




# Adjust below 
# For BF 
brmmodel8_new_bf_03012022 <- brm(formula = bform,         # Model formula 
                                 data = indat1,              # Data frame with variables + dependent variable 
                                 family = gaussian(),        # Response distribution 
                                 prior = priorsmodel8,       # Priors 
                                 sample_prior = TRUE,        # need to add this for model comparison  
                                 warmup = 1000,              # Iterations used for warmup 
                                 iter = 6000,                # Total iterations per chain 
                                 chains = 4,                 # Number of chains 
                                 core = 4,                   # Cores for parallel estimation 
                                 #control = list(adapt_delta = 0.95),  # Control sampler's behavior, this avoided the problem of convergement 
                                 # Finally we can save the model fit to a file 
                                 file = "/myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R code/BRMSmodelfit18_BF_03012022")







