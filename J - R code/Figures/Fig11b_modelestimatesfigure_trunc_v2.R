# Model estimates figure 
# Load fits 
# For rstan the output is saved in hier_modelc_adj_07012022 (adjust "myPath" below to your directory)
hier_modelc_adj_07012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/normal model/hier_modelc_adj_07012022.rds")

# For brms the output is saved in object brmmodel8_new03012022 (this is without saving the prior samples)
brmmodel8_new03012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/brms/Posterior/brmmodel8_new03012022.rds")  

library(rstan)
library(brms)

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

indvestviolin <- function(hier_modelc_adj_v2, brmmodel8_v2){
  # Rstan 
  ## Delta's 
  ## Create dataframe 
  parameter_m7 <- rep(c("Delta 1", "Delta 2", "Delta 3", "Delta 4"), each = 52 )
  
  ### Obtain all individual estimates delta 
  hier_modeld <- hier_modelc_adj_v2
  delta1_summary <- summary(hier_modeld, pars = c("delta1"), probs = c(0.1, 0.9))$summary
  
  delta2_summary <- summary(hier_modeld, pars = c("delta2"), probs = c(0.1, 0.9))$summary
  
  delta3_summary <- summary(hier_modeld, pars = c("delta3"), probs = c(0.1, 0.9))$summary
  
  delta4_summary <- summary(hier_modeld, pars = c("delta4"), probs = c(0.1, 0.9))$summary
  
  parest_m7 <- rbind(delta1_summary, delta2_summary, delta3_summary, delta4_summary)
  datafr_m7 <- data.frame(parameter_m7, parest_m7)
  
  ## Plot 
  set.seed(321)
  datafr_m7$x <- rep(c(1, 2, 3, 4), each = 52)
  datafr_m7$xj <- jitter(datafr_m7$x, amount = 0.09)
  datafr_m7$id <- rep(c(1:52), 4)
  
  plotmodelest_m7 <- ggplot(data=datafr_m7, aes(y=mean)) +
    
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
      side = "r", fill = "gold2", alpha = .5, color = "gold2", trim = TRUE) +
    
    #Define additional settings
    scale_x_continuous(breaks=c(1,2,3,4), labels=c(expression(~delta[~7]), expression(~delta[~6]), expression(~delta[~4]), expression(~delta[~3]))) +
    xlab("Parameter") + ylab("Value") +
    ggtitle('Rstan') +
    theme_classic() + 
    coord_cartesian(xlim = c(0.75, 5), ylim = c(-0.06, 0.15))
  
  # Brms
  ## Create data.frame 
  
  ### Delta's individual estimates 
  estimatesbrms <- coef(brmmodel8_v2)$sub1
  #### Delta 1 - difference 8 and 7 
  del1estimatesbrms <- estimatesbrms[,,"a_dif1"]
  
  #### Delta 2 - difference 7 and 6 
  del2estimatesbrms <- estimatesbrms[,,"a_dif2"]
  
  #### Delta 3 - difference 4 and 3
  del3estimatesbrms <- estimatesbrms[,,"a_dif3"]
  
  #### Delta 4 - difference 3 and 2 
  del4estimatesbrms <- estimatesbrms[,,"a_dif4"]
  
  #### create dataframe 
  parameter_m7 <- rep(c("Delta 1", "Delta 2", "Delta 3", "Delta 4"), each = 52 )
  
  #### gamma_summary2 not using for plot 
  #### beta_summary2 not using for plot 
  parest_m7brms <- rbind(del1estimatesbrms[,1], del2estimatesbrms[,1], del3estimatesbrms[,1], del4estimatesbrms[,1])
  parest_m7brms2 <- t(parest_m7brms)
  parest_m7brms3 <- as.data.frame(parest_m7brms2)
  parest_m7brms4 <- c(parest_m7brms3$V1, parest_m7brms3$V2, parest_m7brms3$V3, parest_m7brms3$V4)
  datafr_m7brms <- data.frame(parameter_m7, parest_m7brms4)
  
  # Plot 
  set.seed(321)
  datafr_m7brms$x <- rep(c(1, 2, 3, 4), each = 52)
  datafr_m7brms$xj <- jitter(datafr_m7$x, amount = 0.09)
  datafr_m7brms$id <- rep(c(1:52), 4)
  
  plotmodelest_m7brms <- ggplot(data=datafr_m7brms, aes(y=parest_m7brms4)) +
    
    #Add geom_() objects
    geom_point(data = datafr_m7brms %>% filter(x=="1"), aes(x=xj), color = 'dodgerblue', size = 1.5,
               alpha = .6) +
    geom_point(data = datafr_m7brms %>% filter(x=="2"), aes(x=xj), color = 'darkgreen', size = 1.5,
               alpha = .6) +
    geom_point(data = datafr_m7brms %>% filter(x=="3"), aes(x=xj), color = 'darkorange', size = 1.5, 
               alpha = .6) +
    geom_point(data = datafr_m7brms %>% filter(x=="4"), aes(x=xj), color = 'gold2', size = 1.5, 
               alpha = .6) +
    
    geom_line(aes(x=xj, group=id), color = 'lightgray', alpha = .3) +
    
    geom_line(data=data.frame(x=-1:4.5,y=0), aes(x = x, y = y), linetype = "dashed", size = .3) + 
    
    geom_half_violin(
      data = datafr_m7brms %>% filter(x=="1"),aes(x = x, y = parest_m7brms4), position = position_nudge(x = 3.2), 
      side = "r", fill = 'dodgerblue', alpha = .5, color = "dodgerblue", trim = TRUE) +
    
    geom_half_violin(
      data = datafr_m7brms %>% filter(x=="2"),aes(x = x, y = parest_m7brms4), position = position_nudge(x = 2.2), 
      side = "r", fill = "darkgreen", alpha = .5, color = "darkgreen", trim = TRUE) +
    
    geom_half_violin(
      data = datafr_m7brms %>% filter(x=="3"),aes(x = x, y = parest_m7brms4), position = position_nudge(x = 1.2), 
      side = "r", fill = "darkorange", alpha = .5, color = "darkorange", trim = TRUE) +
    
    geom_half_violin(
      data = datafr_m7brms %>% filter(x=="4"),aes(x = x, y = parest_m7brms4), position = position_nudge(x = 0.2), 
      side = "r", fill = "gold2", alpha = .5, color = "gold2", trim = TRUE) +
    
    #Define additional settings
    scale_x_continuous(breaks=c(1,2,3,4), labels=c(expression(~delta[~7]), expression(~delta[~6]), expression(~delta[~4]), expression(~delta[~3]))) +
    xlab("Parameter") + ylab("") +
    ggtitle('Brms') +
    theme_classic() + 
    coord_cartesian(xlim = c(0.75, 5), ylim = c(-0.06, 0.15))
  
  
  # Plot together 
  library(gridExtra)
  grid.arrange(plotmodelest_m7, plotmodelest_m7brms, nrow = 1, ncol = 2)
  
  # Save figure as png 
  modelestimatefigure2 <- arrangeGrob(plotmodelest_m7, plotmodelest_m7brms, nrow = 1, ncol = 2)
  ggsave(file = "Fig11b_modelestimatefigure2_v2.png", modelestimatefigure2, width = 11, height = 4)
}

indvestviolin(hier_modelc_adj_07012022, brmmodel8_new03012022)
