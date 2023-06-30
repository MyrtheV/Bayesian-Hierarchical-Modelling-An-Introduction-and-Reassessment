# Prior predictions figure 
library(truncnorm)
library(crch)
library(LaplacesDemon)
library(ggplot2)
library(ggbeeswarm)
library(gridExtra)

###################################################################################################
# Preparation simulation
###################################################################################################

# Without for loop 
set.seed(123)   

Ipp <- 10  # number of participants  
Jpp <- 6   # conditions: digit 2, 3, 4, 6, 7, and 8 
Kpp <- 60  # 60 observations per condition per person 

exppp <- rep(1:Ipp, each = Jpp * Kpp)  # which row belongs to which participant 
condpp <- rep(0:5, Ipp, each = Kpp)    # to which condition row belongs  
subpp <- rep(1:Kpp, Ipp * Jpp)         # to which observation per condition per parcticipant belongs row 

# add parameters x, u, v, w, z 
xjpp <- NA 
ujpp <- NA 
vjpp <- NA 
wjpp <- NA 
zjpp <- NA 
# side effect, greater or smaller than 5  
for (j in 1:length(condpp)){  # 3 = 6
  if (condpp[j] < 3) {
    xjpp[j] <- 1/2}
  else xjpp[j] <- -1/2
}

### Add variable with information about difference between digits, for parameters delta's 
#### Difference 8 and 7 
for (j in 1:length(condpp)){ 
  if (condpp[j] == 5) {  # 5 = 8 , 
    ujpp[j] <- 0}
  else if (condpp[j] == 4) {
    ujpp[j] <- 1}
  else ujpp[j] <- 0
}

# Difference 7 and 6 
for (j in 1:length(condpp)){ 
  if (condpp[j] == 4) {  # 4 = 7, 3 = 6  
    vjpp[j] <- 0}
  else if (condpp[j] == 3) {
    vjpp[j] <- 1}
  else vjpp[j] <- 0
}

# Difference 4 and 3 
for (j in 1:length(condpp)){ 
  if (condpp[j] == 1) {  # 1 = 3, 2 = 4 
    wjpp[j] <- 0}
  else if (condpp[j] == 2) {
    wjpp[j] <- 1}
  else wjpp[j] <- 0
}

# Difference 3 and 2 
for (j in 1:length(condpp)){ 
  if (condpp[j] == 0) {  # 0 = 2, 1 = 3
    zjpp[j] <- 0}
  else if (condpp[j] == 1) {
    zjpp[j] <- 1}
  else zjpp[j] <- 0
}

###################################################################################################
# Normal model 

###################################################################################################

###################################################################################################
# BRMS default priors 
###################################################################################################
# Simulation 
datalistpp3brms <- list()
delta1pp2 <- list()
gammaipp2 <- list()
betaipp2 <- list()
sigmapp2 <- c()
for (i in 1:1000){
  # Hyperpriors: These are not individual specific right? So only sample 1? 
  mugammappbrms <- rst(1, nu = 3, mu = 1, sigma = 10)
  vargammappbrms <- rtt(1, df = 3, location = 0, scale = 10, left = 0, right = Inf)
  mubetappbrms <- rnorm(1, 0, 1)
  varbetappbrms <- rtt(1, df = 3, location = 0, scale = 10, left = 0, right = Inf)
  mudeltappbrms <- rnorm(1, 0, 1)
  vardeltappbrms <- rtt(1, df = 3, location = 0, scale = 10, left = 0, right = Inf)
  
  # Priors: for each person sample prior 1 time 
  gammaippbrms <- rnorm(Ipp, mugammappbrms, vargammappbrms)
  betaippbrms <-  rnorm(Ipp, mubetappbrms, varbetappbrms)
  delta1ppbrms <- rnorm(Ipp, mudeltappbrms, vardeltappbrms)
  delta2ppbrms <- rnorm(Ipp, mudeltappbrms, vardeltappbrms)
  delta3ppbrms <- rnorm(Ipp, mudeltappbrms, vardeltappbrms)
  delta4ppbrms <- rnorm(Ipp, mudeltappbrms, vardeltappbrms)
  
  sigmappbrms <- rtt(Ipp, df = 3, location = 0, scale = 10, left = 0, right = Inf)  # Only 1 because not individual specific? 
  
  # Or start for loop here? Or do we want to vary persons as well? 
  # data
  Yppbrms <- rnorm(Ipp*Jpp*Kpp, gammaippbrms[exppp] + xjpp * betaippbrms[exppp] + 
                     ujpp * delta1ppbrms[exppp] + vjpp * delta2ppbrms[exppp] +
                     wjpp * delta3ppbrms[exppp] + zjpp * delta4ppbrms[exppp],
                   sigmappbrms)
  datartppbrms <- data.frame(exppp, condpp, subpp, repetition = rep(i, length(exppp)), gammaippbrms[exppp], betaippbrms[exppp], delta1ppbrms[exppp], delta2ppbrms[exppp], delta3ppbrms[exppp], delta4ppbrms[exppp], Yppbrms)
  datalistpp3brms[[i]] <- datartppbrms
} 

# Add datasets under each other 
bigdatapp3brms <- do.call(rbind, datalistpp3brms)


########### Create figure function 

## Or plot mean rt per condition per trial 
priorpredfigurecreate <- function(bigdatapp3brms){
  plotsimpp <- aggregate(bigdatapp3brms$Ypp, list(bigdatapp3brms$condpp, bigdatapp3brms$repetition), mean)
  meanrtsimpp <- aggregate(bigdatapp3brms$Ypp, list(bigdatapp3brms$condpp), mean)  # mean RT per condition 
  
  # Mean difference per digits per trial and side effect (mean(2, 3, 4) - mean (6, 7, 8))
  plotsimpp <- aggregate(bigdatapp3brms$Ypp, list(bigdatapp3brms$condpp, bigdatapp3brms$repetition), mean)
  # meanrtsimpp <- aggregate(bigdatapp3$Ypp, list(bigdatapp3$condpp), mean)
  
  sideapp3 <- NA 
  diff1app3 <- NA   # length 1000 
  diff2app3 <- NA 
  diff3app3 <- NA
  diff4app3 <- NA 
  repapp3 <- NA 
  
  # Group.2 is the trial number (1000 in total per condition)
  for (i in 1:length(unique(plotsimpp$Group.2))){
    repapp3[i] <- i 
    sideapp3[i] <- ((plotsimpp$x[plotsimpp$Group.1==0 & plotsimpp$Group.2 == i] + plotsimpp$x[plotsimpp$Group.1==1 & plotsimpp$Group.2 == i] + plotsimpp$x[plotsimpp$Group.1==2 & plotsimpp$Group.2 == i]) - 
                   (plotsimpp$x[plotsimpp$Group.1==3 & plotsimpp$Group.2 == i] + plotsimpp$x[plotsimpp$Group.1==4 & plotsimpp$Group.2 == i] + plotsimpp$x[plotsimpp$Group.1==5 & plotsimpp$Group.2 == i]))
    diff1app3[i] <- (plotsimpp$x[plotsimpp$Group.1==4 & plotsimpp$Group.2 == i]- plotsimpp$x[plotsimpp$Group.1==5 & plotsimpp$Group.2 == i])  # 4 = 7 and 5 = 8 
    diff2app3[i] <- (plotsimpp$x[plotsimpp$Group.1==3 & plotsimpp$Group.2 == i]- plotsimpp$x[plotsimpp$Group.1==5 & plotsimpp$Group.2 == i])  # 3 = 6, 4 = 7
    
    diff3app3[i] <- (plotsimpp$x[plotsimpp$Group.1==2 & plotsimpp$Group.2 == i]- plotsimpp$x[plotsimpp$Group.1==0 & plotsimpp$Group.2 == i])  # 2 = 4, 1 = 3 
    
    diff4app3[i] <- (plotsimpp$x[plotsimpp$Group.1==1 & plotsimpp$Group.2 == i]- plotsimpp$x[plotsimpp$Group.1==0 & plotsimpp$Group.2 == i])  # 1 = 3, 0 = 2 
    
  }
  #diffdatapp3 <- data.frame(repapp3, diff1app3, diff2app3, diff3app3, diff4app3)  # length 1000 
  diffdatapp33 <- data.frame(trial = rep(repapp3, 5), condition = rep(1:5, each = length(repapp3)), difference =  c(sideapp3, diff4app3, diff3app3, diff2app3, diff1app3))
  
  # difference aggregated 
  diffmeandatapp33 <- data.frame(condition = 1:5, 
                                 difference =  c(((meanrtsimpp$x[meanrtsimpp$Group.1==0] + meanrtsimpp$x[meanrtsimpp$Group.1==1] + meanrtsimpp$x[meanrtsimpp$Group.1==2])-(meanrtsimpp$x[meanrtsimpp$Group.1==3] + meanrtsimpp$x[meanrtsimpp$Group.1==4] + meanrtsimpp$x[meanrtsimpp$Group.1==5])), 
                                                 (meanrtsimpp$x[meanrtsimpp$Group.1==1] - meanrtsimpp$x[meanrtsimpp$Group.1==0]), 
                                                 (meanrtsimpp$x[meanrtsimpp$Group.1==2] - meanrtsimpp$x[meanrtsimpp$Group.1==1]), 
                                                 (meanrtsimpp$x[meanrtsimpp$Group.1==3] - meanrtsimpp$x[meanrtsimpp$Group.1==4]), 
                                                 (meanrtsimpp$x[meanrtsimpp$Group.1==4] - meanrtsimpp$x[meanrtsimpp$Group.1==5])))
  
  
  # Old figure 
  diffrtplotsimpp <- ggplot(diffdatapp33, aes(y = difference, x = as.factor(condition))) + 
    geom_line(aes(group = trial), alpha = .014) + 
    geom_point(data = diffdatapp33, aes(group = trial), alpha = 0.014) + 
    geom_line(data = diffmeandatapp33, aes(group = 1), color='blue') + 
    geom_point(data = diffmeandatapp33, aes(group = 1), color='blue') + 
    labs(x = "Digits", y = "Response Time Difference (Seconds)") + 
    theme_bw() + 
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) + 
    scale_x_discrete(breaks = 1:5, labels = c("side", "3-2", "4-3", "6-7", "7-8"))   # + ylim(-0.1,0.1) then you see the difference better but missing a lot of trials 
    theme_set(theme_apa(base_size = 9))
  
  priorpredictionplot1 <- diffrtplotsimpp + ylim(-15,15)
  
  # Try new figure 
  
  # Create violin plot 
  priorpredictionplotnew_1 <- ggplot(diffdatapp33, aes(y = difference, x = as.factor(condition))) + 
    geom_violin(width = 1, alpha = 0.1) + 
    geom_quasirandom(alpha = 0.014, width = 0.2, dodge.width=1) + 
    geom_point(data = diffmeandatapp33, aes(group = 1), color='blue') + 
    labs(x = "Effects", y = "Response Time Difference (Seconds)") + 
    theme_bw() + 
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank()) + 
    scale_x_discrete(breaks = 1:5, labels = c("Side", "3-2", "4-2", "6-8", "7-8"))   # + ylim(-0.1,0.1) then you see the difference better but missing a lot of trials 
  
  return(priorpredictionplotnew_1)
} 

########### End figure function 

# Create figure 
priorpredictionplotnew_1 <- priorpredfigurecreate(bigdatapp3brms)

#################################################################################################
# Final priors for normal model 
#################################################################################################
# Simulation 

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
f2 <- 0.5  # variance for variance of beta, used to be .2. 

#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.09  # sd for mean of delta, 0.5, 0.3  
dd3 <- 3  # mean for variance of delta 
f3 <- 0.5   # variance for variance of delta 

datalistpp3 <- list()
for (i in 1:1000){
  # Hyperpriors: These are not individual specific right? So only sample 1? 
  mugammapp <- rtruncnorm(1, a = 0, mean = 0.5, sd = sqrt(1))
  vargammapp <- rinvgamma(1, e, f)
  mubetapp <- rnorm(1, a2, sqrt(b2))
  varbetapp <- rinvgamma(1, e2, f2)
  mudeltapp <- rnorm(1, a3, sqrt(b3))
  vardeltapp <- rinvgamma(1, dd3, f3)
  
  # Priors: for each person sample prior 1 time 
  gammaipp <- rnorm(Ipp, mugammapp, sqrt(vargammapp))
  betaipp <-  rnorm(Ipp, mubetapp, sqrt(varbetapp))
  delta1pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta2pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta3pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta4pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  
  sigmapp <- rinvgamma(1, c, d)  # Only 1 because not individual specific? 
  
  # Or start for loop here? Or do we want to vary persons as well? 
  # data
  Ypp <- rnorm(Ipp*Jpp*Kpp, gammaipp[exppp] + xjpp * betaipp[exppp] + 
                 ujpp * delta1pp[exppp] + vjpp * delta2pp[exppp] +
                 wjpp * delta3pp[exppp] + zjpp * delta4pp[exppp],
               sqrt(sigmapp))
  datartpp <- data.frame(exppp, condpp, subpp, repetition = rep(i, length(exppp)), gammaipp[exppp], betaipp[exppp], delta1pp[exppp], delta2pp[exppp], delta3pp[exppp], delta4pp[exppp], Ypp)
  datalistpp3[[i]] <- datartpp
} 

# Add datasets under each other 
bigdatapp3_rstan <- do.call(rbind, datalistpp3)  

# Create figure 
priorpredictionplotnew_2 <- priorpredfigurecreate(bigdatapp3_rstan)

###################################################################################################
# Log-normal model 

###################################################################################################

#################################################################################################
# Priors lognormal using priors normal 
#################################################################################################
# Simulation 

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
f2 <- 0.5  # variance for variance of beta, used to be .2. 

#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.09  # sd for mean of delta, 0.5, 0.3  
dd3 <- 3  # mean for variance of delta 
f3 <- 0.5   # variance for variance of delta  

datalistpp3 <- list()
delta1pp2 <- list()
gammaipp2 <- list()
betaipp2 <- list()
sigmapp2 <- c()
for (i in 1:1000){  # with 10000 my computer cannot handle it 
  # Hyperpriors: These are not individual specific right? So only sample 1? 
  mugammapp <- rnorm(1, 0.5, sqrt(1))
  vargammapp <- rinvgamma(1, e, f)
  mubetapp <- rnorm(1, a2, sqrt(b2))
  varbetapp <- rinvgamma(1, e2, f2)
  mudeltapp <- rnorm(1, a3, sqrt(b3))
  vardeltapp <- rinvgamma(1, dd3, f3)
  
  # Priors: for each person sample prior 1 time 
  gammaipp <- rnorm(Ipp, mugammapp, sqrt(vargammapp))
  betaipp <-  rnorm(Ipp, mubetapp, sqrt(varbetapp))
  delta1pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta2pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta3pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta4pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  
  sigmapp <- rinvgamma(1, c, d)  # Only 1 because not individual specific? 
  
  # Or start for loop here? Or do we want to vary persons as well? 
  # data
  Ypp <- rlnorm(Ipp*Jpp*Kpp, gammaipp[exppp] + xjpp * betaipp[exppp] + 
                  ujpp * delta1pp[exppp] + vjpp * delta2pp[exppp] +
                  wjpp * delta3pp[exppp] + zjpp * delta4pp[exppp],
                sqrt(sigmapp))
  gammaipp2[[i]] <- gammaipp
  betaipp2[[i]] <- betaipp
  delta1pp2[[i]] <- delta1pp
  sigmapp2[i] <- sigmapp
  datartpp <- data.frame(exppp, condpp, subpp, repetition = rep(i, length(exppp)), gammaipp[exppp], betaipp[exppp], delta1pp[exppp], delta2pp[exppp], delta3pp[exppp], delta4pp[exppp], Ypp)
  datalistpp3[[i]] <- datartpp
} 

# Add datasets under each other 
bigdatapp3 <- do.call(rbind, datalistpp3)

# Create figure 
priorpredictionplotnew_3 <- priorpredfigurecreate(bigdatapp3)

#################################################################################################
# Final priors lognormal 
#################################################################################################
# Simulation 

### Set the priors and add them to the list 
a <- -0.5  # mean for mu, mu is the mean of theta  
b <- 1  # variance for mu 
c <- 3  # mean for sigma 
d <- .3  # variance for sigma, sigma is the variance of observations  
e <- 3  # mean for g, g is the variance of theta   
f <- .3  # variance for g 

#### Priors for beta 
a2 <- 0  # mean for mean of beta
b2 <- 0.005  # sd for mean of beta, used to be 1 
e2 <- 3  # mean for sd of beta, used to be .7 
f2 <- 0.01  # sd for sd of beta, used to be .2. 

#### Prior for deltas, same for every delta 
a3 <- 0  # mean for mean of delta 
b3 <- 0.005  # sd for mean of delta, 0.005 pretty good, 0,01 worse 
dd3 <- 3  # mean for sd of delta 
f3 <- 0.01  # sd for sd of delta, 0.01 good, 0.05 worse  

datalistpp3 <- list()
delta1pp2 <- list()
gammaipp2 <- list()
betaipp2 <- list()
sigmapp2 <- c()
for (i in 1:1000){  # with 10000 my computer cannot handle it 
  # Hyperpriors: These are not individual specific right? So only sample 1? 
  mugammapp <- rnorm(1, -0.5, sqrt(1))
  vargammapp <- rinvgamma(1, e, f)
  mubetapp <- rnorm(1, a2, sqrt(b2))
  varbetapp <- rinvgamma(1, e2, f2)
  mudeltapp <- rnorm(1, a3, sqrt(b3))
  vardeltapp <- rinvgamma(1, dd3, f3)
  
  # Priors: for each person sample prior 1 time 
  gammaipp <- rnorm(Ipp, mugammapp, sqrt(vargammapp))
  betaipp <-  rnorm(Ipp, mubetapp, sqrt(varbetapp))
  delta1pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta2pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta3pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  delta4pp <- rnorm(Ipp, mudeltapp, sqrt(vardeltapp))
  
  sigmapp <- rinvgamma(1, c, d)  # Only 1 because not individual specific? 
  
  # Or start for loop here? Or do we want to vary persons as well? 
  # data
  Ypp <- rlnorm(Ipp*Jpp*Kpp, gammaipp[exppp] + xjpp * betaipp[exppp] + 
                  ujpp * delta1pp[exppp] + vjpp * delta2pp[exppp] +
                  wjpp * delta3pp[exppp] + zjpp * delta4pp[exppp],
                sqrt(sigmapp))
  gammaipp2[[i]] <- gammaipp
  betaipp2[[i]] <- betaipp
  delta1pp2[[i]] <- delta1pp
  sigmapp2[i] <- sigmapp
  datartpp <- data.frame(exppp, condpp, subpp, repetition = rep(i, length(exppp)), gammaipp[exppp], betaipp[exppp], delta1pp[exppp], delta2pp[exppp], delta3pp[exppp], delta4pp[exppp], Ypp)
  datalistpp3[[i]] <- datartpp
} 

# Add datasets under each other 
bigdatapp3 <- do.call(rbind, datalistpp3)

priorpredictionplotnew_4 <- priorpredfigurecreate(bigdatapp3)

#################################################################################################
# Figure in manuscript 
#################################################################################################
# Adjust plots 
priorpredictionplotnew_1 <- priorpredictionplotnew_1 + xlab("") + labs(subtitle = "A. Initial priors for normal model") + ylim(-20, 20)
priorpredictionplotnew_2 <- priorpredictionplotnew_2 + ylab("") + xlab("") + labs(subtitle = "B. Final priors for normal model") + ylim(-4, 4)
priorpredictionplotnew_3 <- priorpredictionplotnew_3 + labs(subtitle = "C. Initial priors for log-normal model") + ylim(-20, 20)
priorpredictionplotnew_4 <- priorpredictionplotnew_4 + ylab("") + labs(subtitle = "D. Final priors for log-normal model") + ylim(-4, 4)

grid.arrange(priorpredictionplotnew_1,
             priorpredictionplotnew_2,
             priorpredictionplotnew_3,
             priorpredictionplotnew_4, 
             nrow = 2, ncol = 2)

# Save figure as png 
priorpredictionfigure_v2 <- arrangeGrob(priorpredictionplotnew_1,
                                        priorpredictionplotnew_2,
                                        priorpredictionplotnew_3,
                                        priorpredictionplotnew_4, 
                                        nrow = 2, ncol = 2)

ggsave(file = "priorpredfigure_v2.png", priorpredictionfigure_v2, width = 11, height = 8)



#################################################################################################
# Split in two figures 
#################################################################################################
# Figure 1: normal model 
priorpredictionplot1 <- priorpredictionplot1 + labs(subtitle = "A")
priorpredictionplot2 <- priorpredictionplot2 + ylab("") + labs(subtitle = "B")

grid.arrange(priorpredictionplot1, priorpredictionplot2, nrow = 1, ncol = 2)

# Save figure as png 
priorpredictionfigure_v2_normal <- arrangeGrob(priorpredictionplot1, priorpredictionplot2, nrow = 1, ncol = 2)
ggsave(file = "priorpredfigure_normal_v2.png", priorpredictionfigure_v2_normal, width = 11, height = 4)

# Figure 2: log-normal model 
priorpredictionplot4 <- priorpredictionplot4 + labs(subtitle = "A")
priorpredictionplot3 <- priorpredictionplot3 + ylab("") + labs(subtitle = "B")

grid.arrange(priorpredictionplot4, priorpredictionplot3, nrow = 1, ncol = 2)

# Save figure as png 
priorpredictionfigure_v2_lognormal <- arrangeGrob(priorpredictionplot4, priorpredictionplot3, nrow = 1, ncol = 2)
ggsave(file = "Fig5_priorpredfigure_lognormal_v2.png", priorpredictionfigure_v2_lognormal, width = 11, height = 4)

