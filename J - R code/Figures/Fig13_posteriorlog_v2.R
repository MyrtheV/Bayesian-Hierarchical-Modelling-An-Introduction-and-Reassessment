# Figure 13
# Posterior distribution parameters - lognormal model 
# For rstan the output is saved in hier_logmodelc_adj13012022 (adjust "myPath" below to your directory)
hier_logmodelc_adj13012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/log-normal model/hier_logmodelc_adj13012022.rds")

# For brms the output is saved in object brmmodel8log13012022 (this is without saving the prior samples)
brmmodel8log13012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/brms/log-normal/brmmodel8log13012022.rds")  


# Used this link: https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html 
library(tidybayes)
library(bayesplot)
library(brms)
library(rstan)

posteriorlogfunc <- function(hier_logmodelc_adj30032021, brmmodel8log02042021){
  post8 <- as.array(brmmodel8log02042021)
  dimnames(post8)
  color_scheme_set("orange")
  pairs8 <- mcmc_areas(
    post8, 
    pars = c("b_side", "b_dif1", "b_dif2", "b_dif3", "b_dif4"),
    prob = 0.95, # 95% intervals
    prob_outer = 0.99, # 99%
    point_est = "mean"
  )
  # scale_x_continuous(breaks=c(1,2,3,4), labels=c(expression(~delta[~7-8]), expression(~delta[~6-7]), expression(~delta[~4-3]), expression(~delta[~3-2]))) +
  
  pairs82 <- pairs8 + 
    theme_apa() + 
    ggtitle("Brms") + 
    xlim(-0.035, 0.085) + 
    scale_x_continuous(breaks = c(-0.025, 0, 0.025, 0.05, 0.075, 0.1)) + 
    scale_y_discrete(labels= c( 
      expression(~mu[~beta]), 
      expression(~mu[~delta[~7]]), 
      expression(~mu[~delta[~6]]), 
      expression(~mu[~delta[~4]]), 
      expression(~mu[~delta[~3]]))) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), 
          axis.line.y = element_blank())
  
  # Rstan 
  postc <- as.array(hier_logmodelc_adj30032021)
  dimnames(postc)
  color_scheme_set("green")
  pairsc <- mcmc_areas(
    postc, 
    pars = c("mu2", "mu3", "mu4", "mu5", "mu6"),
    prob = 0.95, # 95% intervals
    prob_outer = 0.99, # 99%
    point_est = "mean"
  )
  theme_set(bayesplot::theme_default())
  
  pairsc2 <- pairsc + 
    ggtitle("Rstan") + 
    scale_x_continuous(breaks = c(-0.025, 0, 0.025, 0.05, 0.075, 0.1)) + 
    theme_apa() + 
    scale_y_discrete(labels= c(
      expression(~mu[~beta]), 
      expression(~mu[~delta[~7]]), 
      expression(~mu[~delta[~6]]), 
      expression(~mu[~delta[~4]]), 
      expression(~mu[~delta[~3]])))
  
  # Plot together 
  library(gridExtra)
  grid.arrange(pairsc2, pairs82, nrow = 1, ncol = 2)
  
  # Save figure as png 
  posteriormuplot <- arrangeGrob(pairsc2, pairs82, nrow = 1, ncol = 2, widths = c(1.05/2, 0.95/2))
  library(grid)
  ggsave(file = "Fig13_posteriormulog_v2.png", posteriormuplot, width = 8, height = 4)
}


posteriorlogfunc(hier_logmodelc_adj13012022, brmmodel8log13012022)
