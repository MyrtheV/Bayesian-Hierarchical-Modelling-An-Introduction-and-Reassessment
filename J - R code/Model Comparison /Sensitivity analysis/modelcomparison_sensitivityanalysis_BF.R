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

bridesamplinghier_trunc_bfnull <- function(rep) {
  library(bridgesampling)
  
  bfbs1 <- list()
  rep1 <- rep 
  
  for (i in 1:rep1){
    # Fit models 
    # Null model
    fit1 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rewritestanmodel3h0_trunc.stan", data = datareal, 
                 iter = 4000, chains = 4,  warmup = 1000, cores = 4)
    
    # Side model 
    
    fit2 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rewritestanmodel3hside_trunc.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Digit model 
    
    fit3 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rewritestanmodel3hdigit_trunc.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Full model 
    fit4 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rewritestanmodel3_trunc.stan", data = datareal, 
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
a <- 0.5  # mean for mu, mu is the mean of theta  
b <- 1  # variance for mu 
c <- 3  # mean for sigma 
d <- .7  # variance for sigma, sigma is the variance of observations  
e <- 3  # mean for g, g is the variance of theta   
f <- .7  # variance for g 

#### Priors for beta 
a2 <- 0  # mean for mean of beta
b2 <- 0.09  # variance for mean of beta, used to be 1 
e2 <- 3  # mean for variance of beta, used to be .7 
f2 <- 0.5  # variance for variance of beta, used to be .2. 


#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.09  # sd for mean of delta, 0.5, 0.3  
dd3 <- 3  # mean for variance of delta 
f3 <- 0.5   # variance for variance of delta 


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
bfs_scenario1 <- bridesamplinghier_trunc_bfnull(rep = 1) 
# write.csv(bsf_scenario1$data, "bsf_scenario1.csv")

# In case you don't want to run the full code, you can load the results used in the study 
bfs_scenario1 <- read.csv("bsf_scenario1.csv", header = TRUE)

##################################################################################################
# Scenario 2 - smaller priors on individual differences 
##################################################################################################
# Priors 
### Set the priors and add them to the list 
a <- 0.5  # mean for mu, mu is the mean of theta  
b <- 1  # variance for mu 
c <- 3  # mean for sigma 
d <- .7  # variance for sigma, sigma is the variance of observations  
e <- 3  # mean for g, g is the variance of theta   
f <- .7  # variance for g 

#### Priors for beta 
a2 <- 0  # mean for mean of beta
b2 <- 0.09  # variance for mean of beta, used to be 1 
e2 <- 3  # mean for variance of beta, used to be .7 
f2 <- 0.05  # variance for variance of beta, used to be .2. 


#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.09  # sd for mean of delta, 0.5, 0.3  
dd3 <- 3  # mean for variance of delta 
f3 <- 0.05   # variance for variance of delta 


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
bfs_scenario2 <- bridesamplinghier_trunc_bfnull(rep = 1)  
# write.csv(bfs_scenario2$data, "bfs_scenario2.csv")

# In case you don't want to run the full code, you can load the results used in the study 
bfs_scenario2 <- read.csv("bfs_scenario2.csv", header = TRUE)

##################################################################################################
# Scenario 3 - no individual effects side an digit effects 
##################################################################################################
# New function - because the stan files are different now 
bridesamplinghier_trunc_bfnull2 <- function(rep) {
  library(bridgesampling)
  
  bfbs1 <- list()
  rep1 <- rep 
  
  for (i in 1:rep1){
    # Fit models 
    # Null model
    fit1 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rstan_anova_null_trunc.stan", data = datareal, 
                 iter = 4000, chains = 4,  warmup = 1000, cores = 4)
    
    # Side model 
    
    fit2 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rstan_anova_side_trunc.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Digit model 
    
    fit3 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rstan_anova_digit_trunc.stan", data = datareal, 
                 iter = 4000, chains = 4, warmup = 1000, cores = 4)
    
    # Full model 
    fit4 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rstan_anova_trunc.stan", data = datareal, 
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

# Set prior back to the ones in scenario 1 
# Load results rstan from model comparison - we take the first repetition but we could take any 
a <- 0.5  # mean for mu, mu is the mean of theta  
b <- 1  # variance for mu 
c <- 3  # mean for sigma 
d <- .7  # variance for sigma, sigma is the variance of observations  
e <- 3  # mean for g, g is the variance of theta   
f <- .7  # variance for g 

#### Priors for beta 
a2 <- 0  # mean for mean of beta
b2 <- 0.09  # variance for mean of beta, used to be 1 
e2 <- 3  # mean for variance of beta, used to be .7 
f2 <- 0.5  # variance for variance of beta, used to be .2. 


#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.09  # sd for mean of delta, 0.5, 0.3  
dd3 <- 3  # mean for variance of delta 
f3 <- 0.5   # variance for variance of delta 


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

bfs_scenario3 <- bridesamplinghier_trunc_bfnull2(rep = 1) 

##################################################################################################
# BF compare all models 
##################################################################################################
# Compute the sum of all models against full model 
sumBFs <- sum(1, 
              1/bfs_scenario2[3, 1], 1/bfs_scenario1[3, 1], 1/bfs_scenario3[3, 1],  # Digit vs null model 
              1/bfs_scenario2[2, 1], 1/bfs_scenario1[2, 1], 1/bfs_scenario3[2, 1],  # Side vs null model 
              1/bfs_scenario2[4, 1], 1/bfs_scenario1[4, 1], 1/bfs_scenario3[4, 1])   # Full vs null model 


# Digit 
Post_M0.05 <- (1/bfs_scenario2[3, 1])/sumBFs  # digit versus null  
Post_M0.5 <- (1/bfs_scenario1[3, 1])/sumBFs
Post_A0.5 <- (1/bfs_scenario3[3, 1])/sumBFs

# Side 
Post_MS0.05 <- (1/bfs_scenario2[2, 1])/sumBFs  # side versus null  
Post_MS0.5 <- (1/bfs_scenario1[2, 1])/sumBFs
Post_AS0.5 <- (1/bfs_scenario3[2, 1])/sumBFs

# Full 
Post_MF0.05 <- (1/bfs_scenario2[4, 1])/sumBFs  # full versus null 
Post_MF0.5 <- (1/bfs_scenario1[4, 1])/sumBFs
Post_AF0.5 <- (1/bfs_scenario3[4, 1])/sumBFs

# Null 
Post_00 <- 1/sumBFs

# version paper 
Postmat <- matrix(c(Post_M0.5,  Post_M0.05, Post_A0.5, 
                    Post_MS0.5, Post_MS0.05, Post_AS0.5, 
                    Post_MF0.5, Post_MF0.05, Post_AF0.5), 
                  3, 3, byrow = TRUE)

rownames(Postmat) <- c("Digit", "Side", "Full")
colnames(Postmat) <- c("Scenario 1", "Scenario 2", "Scenario 3")

# Save results 
sensbfs_results <- list(allmodelsagainstnull = Postmat, 
                        nullagainstnull = Post_00)

saveRDS(sensbfs_results, "sensbfs_results.rds")
