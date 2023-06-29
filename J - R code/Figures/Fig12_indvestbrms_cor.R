# Brms ind estimates for model with cor between indv estimates 
# For brms the output is saved in brmmodel9_new28062023.rds (adjust "myPath" below to your directory)
brmmodel9_new28062023 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/K - R objects/brms/normal model/correlation/brmmodel9_new28062023.rds")
  
  

fig12_brmscor <- function(model_fit){
  ### Delta's individual estimates 
  estimatesbrms <- coef(model_fit)$sub1
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
  datafr_m7brms$xj <- jitter(datafr_m7brms$x, amount = 0.09)
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
    xlab("Parameter") + ylab("Value") +
    ggtitle('Brms') +
    theme_classic() + 
    coord_cartesian(xlim = c(0.75, 5), ylim = c(-0.1, 0.17))
  
  #return(plotmodelest_m7brms)
  ggsave(file = "Fig12_modelestimatefigure2_onlybrms_cor.png", plotmodelest_m7brms, width = 5, height = 4)
} 

fig12_brmscor(brmmodel9_new28062023)

  
  