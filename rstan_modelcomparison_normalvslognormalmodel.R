# Normal vs log normal BF comparison 

# Two digit models scenario 3 (without individual differences) 


normal_fit3 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/linear model/rstan_anova_digit_trunc.stan", data = datareal, 
             iter = 4000, chains = 4, warmup = 1000, cores = 4)

saveRDS(normal_fit3, "modelcomparison_normal_fit3.stan")

lognormal_fit3 <- stan(file = "/Users/myrtheveenman/Documents/GitHub/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/rstan_anova_digit_log.stan", data = datareal, 
             iter = 4000, chains = 4, warmup = 1000, cores = 4)

saveRDS(lognormal_fit3, "modelcomparison_normal_fit3.stan")


bs_normal3 <- bridge_sampler(normal_fit3)
bs_lognormal3 <- bridge_sampler(lognormal_fit3)

bridgesampling::bf(bs_normal3, bs_lognormal3)[1]$bf
# Estimated Bayes factor in favor of bs_normal3 over bs_lognormal3: 0.00000 
