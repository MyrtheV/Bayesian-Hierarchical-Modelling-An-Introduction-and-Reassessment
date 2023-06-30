# non linear hierarchical model in brms 

formula3 <- rt2 ~ 1 + side + dif1 + dif2 + dif3 + dif4 +   # General effects 
  (1 + side + dif1 + dif2 + dif3 + dif4 || sub1)

indat=read.table(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all'))
colnames(indat)=c('sub','block','trial','stim','resp','rt','error')

## Cleaning the data according to criteria discussed in paper 
# (code retrieved from Julia Haaf:
# https://github.com/PerceptionAndCognitionLab/bf-order/blob/public/papers/submission/R-scripts/ld5.R)
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

### Response time from milliseconds to seconds 
indat1$rt2 <- indat1$rt/1000

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


## Data add necessary information 
### Add participant number 
for (j in 1:nrow(indat1)){
  if (j == 1) {
    indat1$sub1[j] <- 1}
  else if (indat1$sub[j] == indat1$sub[j-1]) {
    indat1$sub1[j] <- indat1$sub1[j-1]}
  else indat1$sub1[j] <- indat1$sub1[j-1] + 1
}


priorsmodel7log <- c(set_prior("normal(-0.5,1)", class = "Intercept"),  
                  set_prior("inv_gamma(13.75136,4.102063)", class = "sd", coef="Intercept", group = "sub1"), 
                  set_prior("normal(0,0.07071068)", class = "b", coef="side"),  
                  set_prior("inv_gamma(13.75208,0.7489703)", class = "sd", coef="side", group = "sub1"), 
                  set_prior("normal(0,0.07071068)", class = "b", coef="dif1"), 
                  set_prior("inv_gamma(13.75208,0.7489703)", class = "sd", coef="dif1", group = "sub1"),
                  set_prior("normal(0,0.07071068)", class = "b", coef="dif2"),  
                  set_prior("inv_gamma(13.75208,0.7489703)", class = "sd", coef="dif2", group = "sub1"),
                  set_prior("normal(0,0.07071068)", class = "b", coef="dif3"), 
                  set_prior("inv_gamma(13.75208,0.7489703)", class = "sd", coef="dif3", group = "sub1"),
                  set_prior("normal(0,0.07071068)", class = "b", coef="dif4"),  
                  set_prior("inv_gamma(13.75208,0.7489703)", class = "sd", coef="dif4", group = "sub1"),
                  set_prior("inv_gamma(13.75136, 4.102063)", class = "sigma"))

# Adjust 'myPath' below to your directory 
brmmodel8log13012022 <- brm(formula = formula3,         # Model formula 
                 data = indat1,              # Data frame with variables + dependent variable 
                 family = lognormal(),        # Response distribution 
                 prior = priorsmodel7log,       # Priors 
                 # sample_prior = TRUE, need to add this for model comparison  
                 warmup = 1000,              # Iterations used for warmup 
                 iter = 4000,                # Total iterations per chain 
                 chains = 4,                 # Number of chains 
                 core = 4,                   # Cores for parallel estimation 
                 #control = list(adapt_delta = 0.95),  # Control sampler's behavior, this avoided the problem of convergement 
                 # Finally we can save the model fit to a file 
                 file = "/myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R code/BRMSmodelfit18_13012022")

brmmodel8log26052022_bf <- brm(formula = formula3,         # Model formula 
                            data = indat1,              # Data frame with variables + dependent variable 
                            family = lognormal(),        # Response distribution 
                            prior = priorsmodel7log,       # Priors 
                            sample_prior = TRUE,    # need to add this for model comparison  
                            warmup = 1000,              # Iterations used for warmup 
                            iter = 4000,                # Total iterations per chain 
                            chains = 4,                 # Number of chains 
                            core = 4,                   # Cores for parallel estimation 
                            #control = list(adapt_delta = 0.95),  # Control sampler's behavior, this avoided the problem of convergement 
                            # Finally we can save the model fit to a file 
                            file = "myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R code/BRMSmodelfit18_26052022_bf")


# No warnings 


