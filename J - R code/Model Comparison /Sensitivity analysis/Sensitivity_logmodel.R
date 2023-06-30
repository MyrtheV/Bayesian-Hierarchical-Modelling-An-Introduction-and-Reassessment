# Sensitivity analysis 
# Based on rstan results 
library(plyr)
library(rstan)
library(ggplot2)

# Preparations 
# Load datareal 
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

## Data into list and fill the list with necessary information 
datareal <- list(N = length(unique(indat1$sub)))
datareal$All <- nrow(indat1)  # number of total observations, adding to the list for the analysis  

### Indicating which observation belongs to which person 
rank(unique(indat1$sub))
sub <- rep(1:N, each = J)
true.theta[sub]

for (j in 1:nrow(indat1)){
  if (j == 1) {
    indat1$sub1[j] <- 1}
  else if (indat1$sub[j] == indat1$sub[j-1]) {
    indat1$sub1[j] <- indat1$sub1[j-1]}
  else indat1$sub1[j] <- indat1$sub1[j-1] + 1
}


datareal$group_inds <- indat1$sub1  # Add variable with indicator person to the list used for the analysi
datareal$y <- indat1$rt/1000  # rt in seconds instead of miliseconds, added to the list for analysis  

### Add variable with information of the side of the digit to the list, for parameter beta    
# j = condition, if j > 5 than x = -1/2, if j < 5 than x = 1/2 
# indat1$stim 0 = 2, 1=3, 2=4, 3=6, 4=7, 5=8 
# so if bigger than 2 than 1/2, else than -1/2 
for (j in 1:nrow(indat1)){
  if (indat1$stim[j] < 3) {
    datareal$side[j] <- 1/2}
  else datareal$side[j] <- -1/2
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

datareal$dif1 <- indat1$dif1
datareal$dif2 <- indat1$dif2
datareal$dif3 <- indat1$dif3 
datareal$dif4 <- indat1$dif4 

bridesamplinghier_trunc_bfnull_log <- function(rep) {
  library(bridgesampling)
  
  bfbs1 <- list()
  rep1 <- rep 
  
  for (i in 1:rep1){
    # Fit models 
    # Null model
    fit1 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/logstanmodelH0.stan", data = datareal, 
                 iter = 4000, chains = 4,  warmup = 1000, cores = 4)
    
    # Side model 
    
    fit2 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/logstanmodelHside.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Digit model 
    
    fit3 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/logstanmodelHdigit.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Full model 
    fit4 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/myLogModel.stan", data = datareal, 
                 iter = 4000, chains = 4,
                 # , control = list(max_treedepth = 15, adapt_delta = 0.95)
                 warmup = 1000, cores = 4)
    
    
    # Bridge sampling   
    bs1 <- bridge_sampler(fit1)
    bs2 <- bridge_sampler(fit2)
    bs3 <- bridge_sampler(fit3)
    bs4 <- bridge_sampler(fit4)
    
    bfbs <- matrix(NA, nrow = 4, ncol = 1)
    rownames(bfbs) <- c("Fit1", "Fit2", "Fit3", "Fit4") 
    colnames(bfbs) <- "Fit1" 
    bfbs["Fit1", "Fit1"] <- NA
    bfbs["Fit2", "Fit1"] <- bridgesampling::bf(bs1, bs2)[1]$bf
    bfbs["Fit3", "Fit1"] <- bridgesampling::bf(bs1, bs3)[1]$bf
    bfbs["Fit4", "Fit1"] <- bridgesampling::bf(bs1, bs4)[1]$bf
    bfbs1[[i]] <- bfbs
  }
  
  # Combine data 
  bfbs2 <- as.data.frame(ldply(bfbs1))
  bfbs2$model <- rep(c("Null model", "Side model", "Digit model", "Full model"), rep1)
  
  # Output
  bfbs3 <- list()
  bfbs3$data <- bfbs2  # columns represent evidence in favor of that model 
  bfbs3$plot <- ggplot(bfbs2[which(bfbs2$model == "Null model" | bfbs2$data$model == "Side model" |  bfbs2$data$model == "Digit model"),], 
                       aes(x = model, y = Fit4)) + 
    geom_jitter() + 
    ylab("Bayes factor") + 
    xlab("Model") 
  return(bfbs3)  # Returns dataset with Bayes factors and plot for full model 
  
}

##################################################################################################
# Scenario 1 
##################################################################################################
# Load results rstan from model comparison - we take the first repetition but we could take any 
a <- -0.5  # mean for mu, mu is the mean of theta  
b <- 1  # variance for mu 
c <- 3  # mean for sigma 
d <- 0.3  # variance for sigma, sigma is the variance of observations  
e <- 3 # mean for g, g is the variance of theta   
f <- 0.3  # variance for g 

#### Priors for beta 
a2 <- 0  # mean for mean of beta
b2 <- 0.005  # variance for mean of beta, used to be 1 
e2 <- 3  # mean for variance of beta, used to be .7, adjusted 
f2 <- 0.01  # variance for variance of beta, used to be .2., adjusted 

#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.005  # sd for mean of delta, 0.5, 0.3  
dd3 <- 3 # mean for variance of delta, adjusted
f3 <- 0.01   # variance for variance of delta, adjusted 


datareal$a <- a 
datareal$b <- b 
datareal$c <- c 
datareal$d <- d 
datareal$dd <- e 
datareal$f <- f
datareal$a2 <- a2 
datareal$b2 <- b2
datareal$dd2 <- e2 
datareal$f2 <- f2
datareal$a3 <- a3 
datareal$b3 <- b3 
datareal$dd3 <- dd3 
datareal$f3 <- f3 

# Get BF's 
bfs_scenario1_log <- bridesamplinghier_trunc_bfnull_log(rep = 1) 
# write.csv(bsf_scenario1$data, "bsf_scenario1.csv")

# In case you don't want to run the full code, you can load the results used in the study 
bfs_scenario1_log <- read.csv("bfs_scenario1_log.csv", header = TRUE)


##################################################################################################
# Scenario 3 - no individual effects side an digit effects 
##################################################################################################
# New function - because the stan files are different now 
bridesamplinghier_bfnull_log2 <- function(rep) {
  library(bridgesampling)
  
  bfbs1 <- list()
  rep1 <- rep 
  
  for (i in 1:rep1){
    # Fit models 
    # Null model
    fit1 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/rstan_anova_null_log.stan", data = datareal, 
                 iter = 4000, chains = 4,  warmup = 1000, cores = 4)
    
    # Side model 
    
    fit2 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/rstan_anova_side_log.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Digit model 
    
    fit3 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/rstan_anova_digit_log.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Full model 
    fit4 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/rstan_anova_full_log.stan", data = datareal, 
                 iter = 4000, chains = 4,
                 # , control = list(max_treedepth = 15, adapt_delta = 0.95)
                 warmup = 1000, cores = 4)
    
    
    # Bridge sampling   
    bs1 <- bridge_sampler(fit1)
    bs2 <- bridge_sampler(fit2)
    bs3 <- bridge_sampler(fit3)
    bs4 <- bridge_sampler(fit4)
    
    bfbs <- matrix(NA, nrow = 4, ncol = 1)
    rownames(bfbs) <- c("Fit1", "Fit2", "Fit3", "Fit4") 
    colnames(bfbs) <- "Fit1" 
    bfbs["Fit1", "Fit1"] <- NA
    bfbs["Fit2", "Fit1"] <- bridgesampling::bf(bs1, bs2)[1]$bf
    bfbs["Fit3", "Fit1"] <- bridgesampling::bf(bs1, bs3)[1]$bf
    bfbs["Fit4", "Fit1"] <- bridgesampling::bf(bs1, bs4)[1]$bf
    bfbs1[[i]] <- bfbs
  }
  
  # Combine data 
  bfbs2 <- as.data.frame(ldply(bfbs1))
  bfbs2$model <- rep(c("Null model", "Side model", "Digit model", "Full model"), rep1)
  
  # Output
  bfbs3 <- list()
  bfbs3$data <- bfbs2  # columns represent evidence in favor of that model 
  bfbs3$plot <- ggplot(bfbs2[which(bfbs2$model == "Null model" | bfbs2$data$model == "Side model" |  bfbs2$data$model == "Digit model"),], 
                       aes(x = model, y = Fit4)) + 
    geom_jitter() + 
    ylab("Bayes factor") + 
    xlab("Model") 
  return(bfbs3)  # Returns dataset with Bayes factors and plot for full model 
  
}

bfs_scenario3_log <- bridesamplinghier_bfnull_log2(rep = 1) 

bfs_scenario1_log <- bfs_scenario1_log$data
bfs_scenario3_log <- bfs_scenario3_log$data

##################################################################################################
# BF compare all models 
##################################################################################################
# Compute the sum of all models against full model 
sumBFs_log <- sum(1, 
              1/bfs_scenario1_log[3, 1],  1/bfs_scenario3_log[3, 1],  # Digit vs null model 
              1/bfs_scenario1_log[2, 1], 1/bfs_scenario3_log[2, 1],  # Side vs null model 
              1/bfs_scenario1_log[4, 1], 1/bfs_scenario3_log[4, 1])   # Full vs null model 


# Digit 
Post_M0.5_log <- (1/bfs_scenario1_log[3, 1])/sumBFs
Post_A0.5_log <- (1/bfs_scenario3_log[3, 1])/sumBFs

# Side 
Post_MS0.5_log <- (1/bfs_scenario1_log[2, 1])/sumBFs
Post_AS0.5_log <- (1/bfs_scenario3_log[2, 1])/sumBFs

# Full 
Post_MF0.5_log <- (1/bfs_scenario1_log[4, 1])/sumBFs
Post_AF0.5_log <- (1/bfs_scenario3_log[4, 1])/sumBFs

# Null 
Post_00_log <- 1/sumBFs

# version paper 
Postmat_log <- matrix(c(Post_M0.5_log,
                        Post_A0.5_log,
                        Post_MS0.5_log,
                        Post_AS0.5_log,
                        Post_MF0.5_log,
                        Post_AF0.5_log), 
                  3, 2, byrow = TRUE)

rownames(Postmat_log) <- c("Digit", "Side", "Full")
colnames(Postmat_log) <- c("Scenario 1", "Scenario 3")

# Save results 
sensbfs_log_results <- list(allmodelsagainstnull = Postmat_log, 
                        nullagainstnull = Post_00_log)

saveRDS(sensbfs_log_results, "sensbfs_log_results.rds")
