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
                  set_prior("inv_gamma(13.8, 6.3)", class = "sigma"), 
                  set_prior("lkj_corr_cholesky(1)", class = "L"))

# Fit Model 
bform_cor <- brms::bf(rt2 ~ a + b, nl = TRUE, 
                  lf(a ~ 0 + side + dif1 + dif2 + dif3 + dif4 + (0 + side + dif1 + dif2 + dif3 + dif4 | sub1), center = TRUE),  
                  lf(b ~ 0 + inter + (0 + inter | sub1), cmc = FALSE)
)

# Adjust 'myPath' to your directory 
brmmodel9_new28062023 <- brm(formula = bform_cor,         # Model formula 
                             data = indat1,              # Data frame with variables + dependent variable 
                             family = gaussian(),        # Response distribution 
                             prior = priorsmodel9_cor,       # Priors 
                             warmup = 2000,              # Iterations used for warmup 
                             iter = 6000,                # Total iterations per chain 
                             chains = 4,                 # Number of chains 
                             core = 4                   # Cores for parallel estimation 
                             )


# For BF 
brmmodel9_new_bf_28062023 <- brm(formula = bform_cor,         # Model formula 
                             data = indat1,              # Data frame with variables + dependent variable 
                             family = gaussian(),        # Response distribution 
                             prior = priorsmodel9_cor,       # Priors 
                             warmup = 2000,              # Iterations used for warmup 
                             iter = 6000,                # Total iterations per chain 
                             chains = 4,                 # Number of chains 
                             core = 4,                   # Cores for parallel estimation 
                             save_pars = save_pars(TRUE))






