# Figure 15 

# Load data 
# For rstan the output is saved in hier_logmodelc_adj13012022 (adjust "myPath" below to your directory)
hier_logmodelc_adj13012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/K - R objects/rstan/log-normal model/hier_logmodelc_adj13012022.rds")

# For brms the output is saved in object brmmodel8log13012022 (this is without saving the prior samples)
brmmodel8log13012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/K - R objects/brms/log-normal/brmmodel8log13012022.rds")  

library(brms)
library(rstan)

# Plot results using https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
# Install packages for plot 
packages <- c("plyr", "lattice", "ggplot2", "dplyr", "readr", "rmarkdown","Rmisc")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github('erocoar/gghalves')

library("plyr")
library("lattice")
library("ggplot2")
library("dplyr")
library("readr")
library("rmarkdown")
library("Rmisc")
library("devtools")
library("gghalves")

lognormaldicfindestfunc_onlyrstan <- function(hier_logmodelc_adj30032021, brmmodel8log02042021){
  # Rstan 
  parameter_m7 <- rep(c("Delta 1", "Delta 2", "Delta 3", "Delta 4"), each = 52 )
  
  # Obtain all individual estimates delta 
  hier_modeld <- hier_logmodelc_adj30032021
  delta1_summary <- summary(hier_modeld, pars = c("delta1"), probs = c(0.1, 0.9))$summary
  
  delta2_summary <- summary(hier_modeld, pars = c("delta2"), probs = c(0.1, 0.9))$summary
  
  delta3_summary <- summary(hier_modeld, pars = c("delta3"), probs = c(0.1, 0.9))$summary
  
  delta4_summary <- summary(hier_modeld, pars = c("delta4"), probs = c(0.1, 0.9))$summary
  
  parest_m7 <- rbind(delta1_summary, delta2_summary, delta3_summary, delta4_summary)
  datafr_m7 <- data.frame(parameter_m7, parest_m7)
  
  
  # Plot 
  set.seed(321)
  datafr_m7$x <- rep(c(1, 2, 3, 4), each = 52)
  datafr_m7$xj <- jitter(datafr_m7$x, amount = 0.09)
  datafr_m7$id <- rep(c(1:52), 4)
  
  plotmodelest_m7log <- ggplot(data=datafr_m7, aes(y=mean)) +
    
    #Add geom_() objects
    geom_point(data = datafr_m7 %>% filter(x=="1"), aes(x=xj), color = 'dodgerblue', size = 1.5,
               alpha = .6) +
    geom_point(data = datafr_m7 %>% filter(x=="2"), aes(x=xj), color = 'darkgreen', size = 1.5,
               alpha = .6) +
    geom_point(data = datafr_m7 %>% filter(x=="3"), aes(x=xj), color = 'darkorange', size = 1.5, 
               alpha = .6) +
    geom_point(data = datafr_m7 %>% filter(x=="4"), aes(x=xj), color = 'gold2', size = 1.5, 
               alpha = .6) +
    
    geom_line(aes(x=xj, group=id), color = 'lightgray', alpha = .3) +
    
    geom_line(data=data.frame(x=-1:4.5,y=0), aes(x = x, y = y), linetype = "dashed", size = .3) + 
    
    geom_half_violin(
      data = datafr_m7 %>% filter(x=="1"),aes(x = x, y = mean), position = position_nudge(x = 3.2), 
      side = "r", fill = 'dodgerblue', alpha = .5, color = "dodgerblue", trim = TRUE) +
    
    geom_half_violin(
      data = datafr_m7 %>% filter(x=="2"),aes(x = x, y = mean), position = position_nudge(x = 2.2), 
      side = "r", fill = "darkgreen", alpha = .5, color = "darkgreen", trim = TRUE) +
    
    geom_half_violin(
      data = datafr_m7 %>% filter(x=="3"),aes(x = x, y = mean), position = position_nudge(x = 1.2), 
      side = "r", fill = "darkorange", alpha = .5, color = "darkorange", trim = TRUE) +
    
    geom_half_violin(
      data = datafr_m7 %>% filter(x=="4"),aes(x = x, y = mean), position = position_nudge(x = 0.2), 
      side = "r", fill = "darkorange", alpha = .5, color = "gold2", trim = TRUE) +
    
    #Define additional settings
    scale_x_continuous(breaks=c(1,2,3,4), labels=c(expression(~delta[~7]), expression(~delta[~6]), expression(~delta[~4]), expression(~delta[~3]))) +
    xlab("Parameter") + ylab("Value") +
    ggtitle('') +
    theme_classic() + 
    coord_cartesian(xlim = c(0.75, 5))
  
  parameter_m7 <- rep(c("Delta 1", "Delta 2", "Delta 3", "Delta 4"), each = 52 )
  
  
  inddiflogplot<- arrangeGrob(plotmodelest_m7log, nrow = 1, ncol = 1)
  ggsave("Fig15_inddiflogplot_onlyrstan_v2.png", inddiflogplot, width = 5, height = 4)
} 
lognormaldicfindestfunc_onlyrstan(hier_logmodelc_adj13012022)

