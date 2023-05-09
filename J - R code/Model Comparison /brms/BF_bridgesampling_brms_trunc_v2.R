# BF bridge-sampling brms 

# https://discourse.mc-stan.org/t/bayes-factor-using-brms/4469
# http://paul-buerkner.github.io/brms/reference/bayes_factor.brmsfit.html 


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
sub <- rep(1:N, each = J)

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
a <- 0  # mean for mu, mu is the mean of theta  
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

## Full model 
priorsmodel1 <- c(set_prior("normal(0.5,1)", class = "b", lb = 0, nlpar = "b"),  
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


## Digit model 
priorsmodel2 <- c(set_prior("normal(0.5,1)", class = "b", lb = 0, nlpar = "b"),  
                  set_prior("inv_gamma(13.8, 6.3)", class = "sd", coef="inter", group = "sub1", nlpar = "b"), 
                  set_prior("normal(0,0.3)", class = "b", coef="dif1",  nlpar = "a"), 
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif1", group = "sub1",  nlpar = "a"),
                  set_prior("normal(0,0.3)", class = "b", coef="dif2",  nlpar = "a"),  
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif2", group = "sub1",  nlpar = "a"),
                  set_prior("normal(0,0.3)", class = "b", coef="dif3",  nlpar = "a"), 
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif3", group = "sub1",  nlpar = "a"),
                  set_prior("normal(0,0.3)", class = "b", coef="dif4",  nlpar = "a"),  
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="dif4", group = "sub1",  nlpar = "a"),
                  set_prior("inv_gamma(13.8, 6.3)", class = "sigma"))


## Side model 
priorsmodel3 <- c(set_prior("normal(0.5,1)", class = "b", lb = 0, nlpar = "b"),  
                  set_prior("inv_gamma(13.8, 6.3)", class = "sd", coef="inter", group = "sub1", nlpar = "b"), 
                  set_prior("normal(0,0.3)", class = "b", coef="side",  nlpar = "a"),  
                  set_prior("inv_gamma(13.8,5.3)", class = "sd", coef="side", group = "sub1",  nlpar = "a"), 
                  set_prior("inv_gamma(13.8, 6.3)", class = "sigma"))

## Null model 
priorsmodel4 <- c(set_prior("normal(0.5,1)", class = "b", lb = 0, nlpar = "b"),  
                  set_prior("inv_gamma(13.8, 6.3)", class = "sd", coef="inter", group = "sub1", nlpar = "b"), 
                  set_prior("inv_gamma(13.8, 6.3)", class = "sigma"))


# Fit Models 
## Full model
formula1 <- brms::bf(rt2 ~ a + b, nl = TRUE, 
                  lf(a ~ 0 + side + dif1 + dif2 + dif3 + dif4 + (0 + side + dif1 + dif2 + dif3 + dif4 || sub1), center = TRUE),  
                  lf(b ~ 0 + inter + (0 + inter || sub1), cmc = FALSE)
)
## Digit model 
formula2 <- brms::bf(rt2 ~ a + b, nl = TRUE, 
                     lf(a ~ 0 + dif1 + dif2 + dif3 + dif4 + (0 + dif1 + dif2 + dif3 + dif4 || sub1), center = TRUE),  
                     lf(b ~ 0 + inter + (0 + inter || sub1), cmc = FALSE)
)
## Side model 
formula3 <- brms::bf(rt2 ~ a + b, nl = TRUE, 
                     lf(a ~ 0 + side + (0 + side || sub1), center = TRUE),  
                     lf(b ~ 0 + inter + (0 + inter || sub1), cmc = FALSE)
)

## Null model 
# formula4 <- rt2 ~ 1 + (1 || sub1)
formula4 <- brms::bf(rt2 ~ b, nl = TRUE, 
                     lf(b ~ 0 + inter + (0 + inter || sub1), cmc = FALSE)
)

# Adjust 
bridesamplinghierbrms <- function(rep) {
  library(bridgesampling)
  
  bfbs1 <- list()
  rep1 <- rep 
  ESS_full <- list()
  ESS_digit <- list()
  ESS_side <- list()
  ESS_null <- list()
  
  for (i in 1:rep1){
    # Fit models 
    # Fit models 
    ## Full model
    print("full model")
    fit1 <- brm(formula = formula1,         # Model formula 
                data = indat1,              # Data frame with variables + dependent variable 
                family = gaussian(),        # Response distribution 
                prior = priorsmodel1,       # Priors 
                # sample_prior = TRUE, need to add this for model comparison  
                warmup = 1000,              # Iterations used for warmup 
                iter = 8000,                # Total iterations per chain 
                chains = 4,                 # Number of chains 
                core = 4,                   # Cores for parallel estimation 
                control = list(adapt_delta = 0.97, max_treedepth = 15),  # Control sampler's behavior, this avoided the problem of convergement 
                # Finally we can save the model fit to a file 
                save_pars = save_pars(all = TRUE))
    
    ## Digit model 
    print("digit model")
    fit2 <- brm(formula = formula2,         # Model formula 
                data = indat1,              # Data frame with variables + dependent variable 
                family = gaussian(),        # Response distribution 
                prior = priorsmodel2,       # Priors 
                # sample_prior = TRUE, need to add this for model comparison  
                warmup = 1000,              # Iterations used for warmup 
                iter = 8000,                # Total iterations per chain 
                chains = 4,                 # Number of chains 
                core = 4,                   # Cores for parallel estimation 
                control = list(adapt_delta = 0.97, max_treedepth = 15),  # Control sampler's behavior, this avoided the problem of convergement 
                # Finally we can save the model fit to a file 
                save_pars = save_pars(all = TRUE))
    
    ## Side model 
    print("side model")
    fit3 <- brm(formula = formula3,         # Model formula 
                data = indat1,              # Data frame with variables + dependent variable 
                family = gaussian(),        # Response distribution 
                prior = priorsmodel3,       # Priors 
                # sample_prior = TRUE, need to add this for model comparison  
                warmup = 1000,              # Iterations used for warmup 
                iter = 8000,                # Total iterations per chain 
                chains = 4,                 # Number of chains 
                core = 4,                   # Cores for parallel estimation 
                control = list(adapt_delta = 0.97, max_treedepth = 15),  # Control sampler's behavior, this avoided the problem of convergement 
                # Finally we can save the model fit to a file 
                save_pars = save_pars(all = TRUE))
    
    ## Null model 
    print("null model")
    fit4 <- brm(formula = formula4,         # Model formula 
                data = indat1,              # Data frame with variables + dependent variable 
                family = gaussian(),        # Response distribution 
                prior = priorsmodel4,       # Priors 
                # sample_prior = TRUE, need to add this for model comparison  
                warmup = 1000,              # Iterations used for warmup 
                iter = 12000,                # Total iterations per chain 
                chains = 4,                 # Number of chains 
                core = 4,                   # Cores for parallel estimation 
                control = list(adapt_delta = 0.97, max_treedepth = 15),  # Control sampler's behavior, this avoided the problem of convergement 
                # Finally we can save the model fit to a file 
                save_pars = save_pars(all = TRUE))
    
    bfbs <- matrix(NA, nrow = 4, ncol = 1)
    rownames(bfbs) <- c("Fit1", "Fit2", "Fit3", "Fit4") 
    colnames(bfbs) <- "Fit1" 
    bfbs["Fit4", "Fit1"] <- NA
    bfbs["Fit1", "Fit1"] <- bayes_factor(fit1, fit4)[1]$bf  # compare to null 
    bfbs["Fit2", "Fit1"] <- bayes_factor(fit1, fit3)[1]$bf  # compare to side 
    bfbs["Fit3", "Fit1"] <- bayes_factor(fit1, fit2)[1]$bf  # compare to digit 
    bfbs1[[i]] <- bfbs
    
    # ESS 
    ESS_full[[i]] <- effective_sample(fit1)
    ESS_digit[[i]] <- effective_sample(fit2)
    ESS_side[[i]] <- effective_sample(fit3)
    ESS_null[[i]] <- effective_sample(fit4)
  }
  
  # Combine data 
  bfbs2 <- as.data.frame(ldply(bfbs1))
  bfbs2$model <- rep(c("Null model", "Side model", "Digit model", "Full model"), rep1)
  
  # Output
  bfbs3 <- list()
  bfbs3$data <- bfbs2  # columns represent evidence in favor of that model 
  bfbs3$ESS_full <- ESS_full
  bfbs3$ESS_digit <- ESS_digit
  bfbs3$ESS_side <- ESS_side
  bfbs3$ESS_null <- ESS_null
    
  return(bfbs3)  # Returns dataset with Bayes factors and plot for full model 
  
}

bssresults_brms_15032022_1rep <- bridesamplinghierbrms(rep = 1)
# Combine 
bssresults_brms_15032022_1rep$data
bssresults_brms_11032022_10rep$data[c(17:20),]
bssresults_brms_15032022_10rep <- rbind(bssresults_brms_11032022_10rep$data, bssresults_brms_15032022_1rep$data)
bssresults_brms_15032022_10rep <- bssresults_brms_15032022_10rep[-c(17:20), ]
write.csv(bssresults_brms_15032022_10rep, "bssresults_brms_15032022_10rep.csv")

bssresults$data
ggplot(bssresults$data[which(bssresults$data$model == "Null model" | bssresults$data$model == "Side model" |bssresults$data$model == "Digit model"),], 
       aes(x = model, y = Fit4)) + 
  geom_jitter() + 
  ylab("Bayes factor") + 
  xlab("Model") 