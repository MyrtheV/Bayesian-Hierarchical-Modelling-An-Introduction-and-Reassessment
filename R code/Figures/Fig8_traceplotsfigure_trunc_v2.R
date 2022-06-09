# Traceplots dif 2 (difference between 6 and 7) + rhat
# Based on output each package of symbolic distance effect model 
# For rstan the output is saved in hier_modelc_adj_07012022 (adjust "myPath" below to your directory)
hier_modelc_adj_07012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/rstan/normal model/hier_modelc_adj_07012022.rds")

# For brms the output is saved in object brmmodel8_new03012022 (this is without saving the prior samples)
brmmodel8_new03012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/brms/Posterior/brmmodel8_new03012022.rds")  

tracerhatneff <- function(hier_modelc_adj_v2, brmmodel8_v2){
  #Rstan
  library(rstan)
  trdif2rstan <- plot(hier_modelc_adj_v2, plotfun = "trace", pars = c("mu4"))  # rstan 
  # plot(hier_modelc_adj, plotfun = "trace", pars = c("mu4"), inc_warmup = TRUE)  # show warmup -- > ugly plot 
  
  trdif2rstan <- trdif2rstan + labs(color='Chain') 
  
  # Extract legend 
  library(cowplot)
  legendtrace <- cowplot::get_legend(trdif2rstan)
  library(grid)
  grid.draw(legendtrace)
  
  # Obtain colors from rstanplot 
  unique(ggplot_build(trdif2rstan)$data[[1]]$colour)
  # "#E66101" "#998EC3" "#542788" "#F1A340"
  
  # Final rstan plot 
  trdif2rstan2 <- trdif2rstan + ylab(expression(~mu[~delta[~6]])) + 
    theme(axis.title.y=element_text(angle = 0, 
                                    vjust = 0.5, 
                                    hjust = -0.5, 
                                    margin = margin(0, 0.7, 0, 0, "cm"), size = 14), 
          legend.position = "none", 
          axis.line.x  = element_line(size = 0.6), 
          axis.line.y  = element_line(size = 0.6), 
          plot.margin = unit(c(0.3,0.1,0.3,0.05),"cm"), 
          plot.subtitle = element_text(size = 10.5), 
          axis.ticks = element_line(size = 0.6), 
          axis.text=element_text(size=8.5), 
          axis.ticks.length=unit(.1, "cm")) + 
    labs(subtitle = "A: Rstan") + 
    ylim(c(0.01, 0.07)) + 
    xlab("")
  
  trdif2rstan2 <- trdif2rstan2 + xlab("Iteration") + theme(axis.title.x=element_text(face = "plain", size = 11.5))
  
  # Brms 
  # I think warmup already deleted, but then have to adjust labels x axis 
  library(brms)
  trdif2brms <- stanplot(brmmodel8_v2, pars = "b_a_dif2", type = "trace")  # brms 
  # If want to set x axis: xlim(c(1000, 4000)) 
  # trdif2brms + scale_x_continuous(breaks=c(0,500,1000,1500, 2000, 2500, 3000), labels=c(1000, 1500, 2000, 2500, 3000, 3500, 4000)) + theme(axis.line.x = element_line(color="black"),
  
  # Final plot brms                                                                                                                                          axis.line.y = element_line(color="black"))
  trdif2brms2 <- trdif2brms + 
    scale_x_continuous(breaks=c(0,1000,2000, 3000, 4000, 5000), 
                       labels=c(1000, 2000, 3000, 4000, 5000, 6000)) + 
    theme_classic() +              
    theme(axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"), 
          legend.position = "none",  
          plot.margin = unit(c(0.3,0.1,0.4,0),"cm")) + 
    scale_color_manual(values=c("#E66101", "#998EC3", "#542788", "#F1A340")) + 
    ylab("") + 
    xlab("Iteration") + 
    labs(subtitle = "B: Brms") + 
    ylim(c(0.01, 0.07)) + 
    scale_y_continuous(limits = c(0.01, 0.07), 
                       breaks = c(0.02, 0.04, 0.06), 
                       labels = c(0.02, 0.04, 0.06)) 
  
  # Rhat and neff (number of effective samples) figure 
  # stanplot(brmmodel8, pars = "b_dif2", type = "neff_hist")
  
  # Obtain rhat and neff rstan 
  rhatrstan <- summary(hier_modelc_adj_v2)$summary[,"Rhat"]
  fixedrhatrstan <- rhatrstan[1:13]
  neffrstan <- summary(hier_modelc_adj_v2)$summary[,"n_eff"]
  fixedneffrstan <- neffrstan[1:13]
  
  # Obtain rhat and neff brms 
  library(bayestestR)
  rhatbrms <- rhat(brmmodel8_v2)
  fixedrhatbrms <- c(rhatbrms[6], rhatbrms[1:5], rhatbrms[12], rhatbrms[7:11], rhatbrms[13])
  # neffbrms <- effective_sample(brmmodel8_v2, effects = "all")  # lower number of parameters (324 instead of 326 (know lp (and prior it seems) is missing, but other I don't know))
  # results somewhat different from summary 
  # neff_ratio(brmmodel8_v2) * 12000  # 326 
  which(rhatbrms > 1.001)  # seems to be problem in the delta 
  #neffbrms[which(neffbrms < 12000)]  # seems to be problems in intercept, and sd_sub1_dif3 (so one participant) 
  
  # Combine rhat into one data.frame 
  rhatrstanbrms <- t(rbind(rhatrstan, rhatbrms))
  rhatrstanbrms2 <- as.data.frame(rhatrstanbrms)
  rhatrstanbrms3 <- c(rhatrstanbrms2$rhatrstan, rhatrstanbrms2$rhatbrms)
  rstanrhatneff <- data.frame()
  
  
  rstanrhatneff <- data.frame(package = rep(c("Rstan", "Brms"), each = 326), 
                              rhat = rhatrstanbrms3)
  
  # remove lp and fixed effects 
  # rhat 
  rhatrstan2 <- rhatrstan[-c(1:13, 326)]
  rhatbrms2 <- c(rhatbrms[274:325], rhatbrms[-c(1:13, 274:326)])
  rhatrstanbrms4 <- t(rbind(rhatrstan2, rhatbrms2))
  rhatrstanbrms5 <- as.data.frame(rhatrstanbrms4)
  rhatrstanbrms6 <- c(rhatrstanbrms5$rhatrstan, rhatrstanbrms5$rhatbrms)
  rstanrhatneff_v2 <- data.frame(package = rep(c("Rstan", "Brms"), each = 312), 
                                 parameter = rep(c("Gamma", "Beta", "Delta", "Delta", 
                                                   "Delta", "Delta", "Gamma", "Beta", 
                                                   "Delta", "Delta", "Delta", "Delta"), 
                                                 each = 52), 
                                 rhat = rhatrstanbrms6)
  
  rstanrhatneff_v2
  ## fixed effects 
  deltameanrrstan <- mean(fixedrhatrstan[3:6])
  deltameanrrbrms <- mean(fixedrhatbrms[3:6])
  
  library(ggbeeswarm)
  # Create violin plot 
  figrhatnew <- ggplot(rstanrhatneff_v2, aes(x = factor(parameter, level = c("Gamma", "Beta", "Delta")), y = rhat, colour = package)) + 
    geom_violin(width = 1) + 
    geom_quasirandom(alpha = 0.1, width = 0.2, dodge.width=1) + 
    geom_point(aes(x=0.75, y= fixedrhatbrms[1]), shape = 8, colour="darkorange", size = 2) + 
    geom_point(aes(x=1.25, y= fixedrhatrstan[1]), shape = 8, colour="darkgreen", size = 2) + 
    geom_point(aes(x=1.75, y= fixedrhatbrms[2]), shape = 8, colour="darkorange", size = 2) + 
    geom_point(aes(x=2.25, y= fixedrhatrstan[2]), shape = 8, colour="darkgreen", size = 2) + 
    geom_point(aes(x=2.75, y= deltameanrrbrms ), shape = 8, colour="darkorange", size = 2) + 
    geom_point(aes(x=3.25, y= deltameanrrstan), shape = 8, colour="darkgreen", size = 2) + 
    xlab("Parameter") + ylab(expression(hat(R))) +
    scale_colour_manual(values = c("darkorange", "darkgreen")) + 
    theme_classic() + 
    theme(legend.position = "none") + 
    scale_x_discrete(labels= c( 
      expression(~gamma), 
      expression(~beta), 
      expression(~delta))) + 
    labs(subtitle = "C") + 
    theme(axis.title.y = element_text(angle = 0, 
                                      vjust = 0.5, 
                                      hjust = -0.5, 
                                      margin = margin(0, 1.1, 0, 0, "cm")))
  
  ## Separate delta - does not give more information 
  rstanrhatneff_v3 <- data.frame(package = rep(c("Rstan", "Brms"), each = 312), 
                                 parameter = rep(c("Gamma", "Beta", "Delta1", "Delta2", 
                                                   "Delta3", "Delta4", "Gamma", "Beta", 
                                                   "Delta1", "Delta2", "Delta3", "Delta4"), 
                                                 each = 52), 
                                 rhat = rhatrstanbrms6)
  
  figrhatall <- ggplot(rstanrhatneff_v3, aes(x = factor(parameter, level = c("Gamma", "Beta", "Delta1", "Delta2", "Delta3", "Delta4")), y = rhat, colour = package)) + 
    geom_violin(width = 1) + 
    geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
    geom_point(aes(x=0.75, y= fixedrhatbrms[1]), colour="black", size = 0.7) + 
    geom_point(aes(x=1.25, y= fixedrhatrstan[1]), colour="black", size = 0.7) + 
    geom_point(aes(x=1.75, y= fixedrhatbrms[2]), colour="black", size = 0.7) + 
    geom_point(aes(x=2.25, y= fixedrhatrstan[2]), colour="black", size = 0.7) + 
    geom_point(aes(x=2.75, y= fixedrhatbrms[3]), colour="black", size = 0.7) + 
    geom_point(aes(x=3.25, y= fixedrhatrstan[3]), colour="black", size = 0.7) + 
    geom_point(aes(x=3.75, y= fixedrhatbrms[4]), colour="black", size = 0.7) + 
    geom_point(aes(x=4.25, y= fixedrhatrstan[4]), colour="black", size = 0.7) +
    geom_point(aes(x=4.75, y= fixedrhatbrms[5]), colour="black", size = 0.7) + 
    geom_point(aes(x=5.25, y= fixedrhatrstan[5]), colour="black", size = 0.7) +
    geom_point(aes(x=5.75, y= fixedrhatbrms[6]), colour="black", size = 0.7) + 
    geom_point(aes(x=6.25, y= fixedrhatrstan[6]), colour="black", size = 0.7) +
    xlab("Parameter") + ylab(expression(hat(R))) +
    scale_colour_manual(values = c("darkorange", "darkgreen")) + 
    theme(legend.position = "none") + 
    scale_x_discrete(labels= c( 
      expression(~gamma), 
      expression(~beta), 
      expression(~delta[~7-8]), 
      expression(~delta[~6-7]), 
      expression(~delta[~4-3]), 
      expression(~delta[~3-2]))) + 
    labs(subtitle = "C") + 
    theme(plot.margin=unit(c(0,0,0,0.6),"cm")) + 
    theme(axis.line = element_line(size = 0.5)) +
    theme(axis.title.y = element_text(angle = 0, 
                                      vjust = 0.5, 
                                      hjust = -0.5, 
                                      margin = margin(0, 0.7, 0, 0, "cm"), size = 14))
  
  # geom_beeswarm(dodge.width=0.5)
  # geom_violin(width = 1.3) + 
  # ylim(0.997, 1.006) + 
  # geom_jitter(aes(colour = package), alpha = 0.2, width = .02) + 
  # geom_quasirandom()
  # geom_jitter(aes(colour = package, fill = package), position = position_jitter(1))  
  # geom_sina()
  
  # neff
  ## brms 
  neffbrms <- mcmc_plot(brmmodel8_v2, type = "neff_hist")$data$value * 12000
  fixedneffbrms <- neffbrms[which(mcmc_plot(brmmodel8_v2, type = "neff_hist")$data$parameter%in%c("b_b_inter", "b_a_side", "b_a_dif1", "b_a_dif2", "b_a_dif3", "b_a_dif4", "sd_sub1__b_inter", "sd_sub1__a_side", "sd_sub1__a_dif1", "sd_sub1__a_dif2", "sd_sub1__a_dif3", "sd_sub1__a_dif4", "sigma"))]
  
  
  
  neffbrmsratio <- (mcmc_plot(brmmodel8_v2, type = "neff_hist")$data$value)
  # neffbrmsratio2 <- neffbrmsratio[order(neffbrmsratio[["parameter"]]),]
  
  neffbrmsratio_value <- mcmc_plot(brmmodel8_v2, type = "neff_hist")$data$value
  neffbrmsratio_para <- mcmc_plot(brmmodel8_v2, type = "neff_hist")$data$parameter
  neffbrmsratio_comb <- data.frame(parameter = neffbrmsratio_para, value = neffbrmsratio_value)
  neffbrmsratio_comb[grep("r_sub1__b", neffbrmsratio_comb$parameter),]$value
  neffbrmsratio_comb[grep(",dif1]", neffbrmsratio_comb$parameter),]$value
  
  fixedneffbrms2 <- c(neffbrmsratio_comb[grep("b_inter", neffbrmsratio_comb$parameter),]$value[1],
                      neffbrmsratio_comb[grep("b_a_side", neffbrmsratio_comb$parameter),]$value[1],
                      neffbrmsratio_comb[grep("b_a_dif1", neffbrmsratio_comb$parameter),]$value[1],
                      neffbrmsratio_comb[grep("b_a_dif2", neffbrmsratio_comb$parameter),]$value[1],
                      neffbrmsratio_comb[grep("b_a_dif3", neffbrmsratio_comb$parameter),]$value[1],
                      neffbrmsratio_comb[grep("b_a_dif4", neffbrmsratio_comb$parameter),]$value[1])

  # fixedneffbrms2 <- c(neffbrmsratio2[grep("b_inter", neffbrmsratio2$parameter),]$value[1],
  #                     neffbrmsratio2[grep("a_side", neffbrmsratio2$parameter),]$value[1],
  #                     neffbrmsratio2[grep("a_dif1", neffbrmsratio2$parameter),]$value[1],
  #                     neffbrmsratio2[grep("a_dif2", neffbrmsratio2$parameter),]$value[1],
  #                     neffbrmsratio2[grep("a_dif3", neffbrmsratio2$parameter),]$value[1],
  #                     neffbrmsratio2[grep("a_dif4", neffbrmsratio2$parameter),]$value[1])
  
  fixedneffbrms3 <- fixedneffbrms2 * 12000
  fixedneffbrmsmean <- mean(fixedneffbrms3[3:6])
  fixedneffbrms4 <- fixedneffbrms2 * 12000
  
  fixedneffrstanmean <- mean(fixedneffrstan[3:6])
  
  neffbrmsratio3 <- c(neffbrmsratio_comb[grep(",inter]", neffbrmsratio_comb$parameter),]$value,
                      neffbrmsratio_comb[grep(",side]", neffbrmsratio_comb$parameter),]$value,
                      neffbrmsratio_comb[grep(",dif1]", neffbrmsratio_comb$parameter),]$value,
                      neffbrmsratio_comb[grep(",dif2]", neffbrmsratio_comb$parameter),]$value,
                      neffbrmsratio_comb[grep(",dif3]", neffbrmsratio_comb$parameter),]$value,
                      neffbrmsratio_comb[grep(",dif4]", neffbrmsratio_comb$parameter),]$value)
  neffbrmsratio_comb
  
  # neffbrmsratio3 <- c(neffbrmsratio2[grep(",b_inter", neffbrmsratio2$parameter),]$value,
  #                     neffbrmsratio2[grep(",a_side", neffbrmsratio2$parameter),]$value,
  #                     neffbrmsratio2[grep(",a_dif1", neffbrmsratio2$parameter),]$value,
  #                     neffbrmsratio2[grep(",a_dif2", neffbrmsratio2$parameter),]$value,
  #                     neffbrmsratio2[grep(",a_dif3", neffbrmsratio2$parameter),]$value,
  #                     neffbrmsratio2[grep(",a_dif4", neffbrmsratio2$parameter),]$value)
  # 
  
  neffbrms2 <- neffbrmsratio3  * 12000
  
  # Combine neff into one data.frame 
  neffrstan2 <- data.frame(neff = neffrstan)
  neffrstan3 <- neffrstan2$neff[-c(1:13, 326)]
  # neffNA <- rep(NA, 2)
  # neffbrms2 <- c(neffbrms$ESS, neffNA)  # now same length 
  neffrstanbrms <- data.frame(package = rep(c("Rstan", "Brms"), each = 326), 
                              neff = c(neffrstan2$neff, neffbrms)) 
  
  neffrstanbrms2 <- data.frame(package = rep(c("Rstan", "Brms"), each = 312), 
                               parameter = rep(c("Gamma", "Beta", "Delta", "Delta", 
                                                 "Delta", "Delta", "Gamma", "Beta", 
                                                 "Delta", "Delta", "Delta", "Delta"), each = 52),
                               neff = c(neffrstan3, neffbrms2)) 
  figneffnew <- ggplot(neffrstanbrms2, aes(x = factor(parameter, level = c("Gamma", "Beta", "Delta")), y = neff, colour = package)) + 
    geom_violin(width = 1) + 
    geom_quasirandom(alpha = 0.1, width = 0.2, dodge.width=1) + 
    geom_point(aes(x=0.75, y= fixedneffbrms4[1]), shape = 8, colour="darkorange", size = 2) + 
    geom_point(aes(x=1.25, y= fixedneffrstan[1]), shape = 8, colour="darkgreen", size = 2) + 
    geom_point(aes(x=1.75, y= fixedneffbrms4[2]), shape = 8, colour="darkorange", size = 2) + 
    geom_point(aes(x=2.25, y= fixedneffrstan[2]), shape = 8, colour="darkgreen", size = 2) + 
    geom_point(aes(x=2.75, y= fixedneffbrmsmean), shape = 8, colour="darkorange", size = 2) + 
    geom_point(aes(x=3.25, y= fixedneffrstanmean), shape = 8, colour="darkgreen", size = 2) + 
    xlab("Parameter") + ylab("Number of effective samples") +
    scale_colour_manual(values = c("darkorange", "darkgreen")) + 
    theme_classic() + 
    theme(legend.position = "none") + 
    scale_x_discrete(labels= c( 
      expression(~gamma), 
      expression(~beta), 
      expression(~delta))) + 
    labs(subtitle = "D") + 
    geom_hline(yintercept = 12000, linetype = "dashed", color = "darkgreen", size = .3) + 
    geom_hline(yintercept = 20000, linetype = "dashed", color = "darkorange", size = .3)
  
  
  
  ## Separate by delta 
  neffrstanbrms3 <- data.frame(package = rep(c("Rstan", "Brms"), each = 312), 
                               parameter = rep(c("Gamma", "Beta", "Delta1", "Delta2", 
                                                 "Delta3", "Delta4", "Gamma", "Beta", 
                                                 "Delta1", "Delta2", "Delta3", "Delta4"), each = 52),
                               neff = c(neffrstan3, neffbrms2)) 
  
  figneffall <- ggplot(neffrstanbrms3, aes(x = factor(parameter, level = c("Gamma", "Beta", "Delta1", "Delta2", "Delta3", "Delta4")), 
                                           y = neff, colour = package)) + 
    geom_violin(width = 1) + 
    geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
    geom_point(aes(x=0.75, y= fixedneffbrms4[1]), colour="black", size = 0.7) + 
    geom_point(aes(x=1.25, y= fixedneffrstan[1]), colour="black", size = 0.7) + 
    geom_point(aes(x=1.75, y= fixedneffbrms4[2]), colour="black", size = 0.7) + 
    geom_point(aes(x=2.25, y= fixedneffrstan[2]), colour="black", size = 0.7) + 
    geom_point(aes(x=2.75, y= fixedneffbrms4[3]), colour="black", size = 0.7) + 
    geom_point(aes(x=3.25, y= fixedneffrstan[3]), colour="black", size = 0.7) + 
    geom_point(aes(x=3.75, y= fixedneffbrms4[4]), colour="black", size = 0.7) + 
    geom_point(aes(x=4.25, y= fixedneffrstan[4]), colour="black", size = 0.7) +
    geom_point(aes(x=4.75, y= fixedneffbrms4[5]), colour="black", size = 0.7) + 
    geom_point(aes(x=5.25, y= fixedneffrstan[5]), colour="black", size = 0.7) +
    geom_point(aes(x=5.75, y= fixedneffbrms4[6]), colour="black", size = 0.7) + 
    geom_point(aes(x=6.25, y= fixedneffrstan[6]), colour="black", size = 0.7) +
    xlab("Parameter") + ylab("Number of effective samples") +
    scale_colour_manual(values = c("darkorange", "darkgreen")) + 
    theme(legend.position = "none") + 
    theme(axis.line = element_line(size = 0.5)) + 
    theme(plot.margin=unit(c(0,0,0,0),"cm")) + 
    scale_x_discrete(labels= c( 
      expression(~gamma), 
      expression(~beta), 
      expression(~delta[~7-8]), 
      expression(~delta[~6-7]), 
      expression(~delta[~4-3]), 
      expression(~delta[~3-2]))) + 
    labs(subtitle = "D") + 
    geom_hline(yintercept = 12000, linetype = "dashed", color = "grey", size = .3)
  
  # Boxplot 
  ## rhat 
  figrhatallbox <- ggplot(rstanrhatneff_v3, aes(x = factor(parameter, level = c("Gamma", "Beta", "Delta1", "Delta2", "Delta3", "Delta4")), y = rhat, colour = package, fill = package)) + 
    geom_boxplot(width = 1, alpha = 0.2) + 
    # geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
    geom_point(aes(x=0.75, y= fixedrhatbrms[1]), size = 2, colour="darkorange") + 
    geom_point(aes(x=1.25, y= fixedrhatrstan[1]), size = 2, colour="darkgreen") + 
    geom_point(aes(x=1.75, y= fixedrhatbrms[2]), size = 2, colour="darkorange") + 
    geom_point(aes(x=2.25, y= fixedrhatrstan[2]), size = 2, colour="darkgreen") + 
    geom_point(aes(x=2.75, y= fixedrhatbrms[3]), size = 2, colour="darkorange") + 
    geom_point(aes(x=3.25, y= fixedrhatrstan[3]), size = 2, colour="darkgreen") + 
    geom_point(aes(x=3.75, y= fixedrhatbrms[4]), size = 2, colour="darkorange") + 
    geom_point(aes(x=4.25, y= fixedrhatrstan[4]), size = 2, colour="darkgreen") +
    geom_point(aes(x=4.75, y= fixedrhatbrms[5]), size = 2, colour="darkorange") + 
    geom_point(aes(x=5.25, y= fixedrhatrstan[5]), size = 2, colour="darkgreen") +
    geom_point(aes(x=5.75, y= fixedrhatbrms[6]), size = 2, colour="darkorange") + 
    geom_point(aes(x=6.25, y= fixedrhatrstan[6]), size = 2, colour="darkgreen") +
    xlab("Parameter") + ylab(expression(hat(R))) +
    scale_colour_manual(values = c("darkorange", "darkgreen")) + 
    theme(legend.position = "none") + 
    scale_x_discrete(labels= c( 
      expression(~gamma), 
      expression(~beta), 
      expression(~delta[~7]), 
      expression(~delta[~6]), 
      expression(~delta[~4]), 
      expression(~delta[~3]))) + 
    labs(subtitle = "C") + 
    theme(plot.margin=unit(c(0,0,0,0.6),"cm")) + 
    theme(axis.line = element_line(size = 0.5)) +
    theme(axis.title.y = element_text(angle = 0, 
                                      vjust = 0.5, 
                                      hjust = -0.5, 
                                      margin = margin(0, 0.7, 0, 0, "cm"), size = 14))
  
  ## neff 
  figneffallbox <- ggplot(neffrstanbrms3, aes(x = factor(parameter, level = c("Gamma", "Beta", "Delta1", "Delta2", "Delta3", "Delta4")), 
                                              y = neff, colour = package, fill = package)) + 
    geom_boxplot(width = 1, alpha = 0.2) + 
    # geom_quasirandom(alpha = 0.2, width = 0.2, dodge.width=1) + 
    geom_point(aes(x=0.75, y= fixedneffbrms4[1]), size = 2, colour="darkorange") + 
    geom_point(aes(x=1.25, y= fixedneffrstan[1]), size = 2, colour="darkgreen") + 
    geom_point(aes(x=1.75, y= fixedneffbrms4[2]), size = 2, colour="darkorange") + 
    geom_point(aes(x=2.25, y= fixedneffrstan[2]), size = 2, colour="darkgreen") + 
    geom_point(aes(x=2.75, y= fixedneffbrms4[3]), size = 2, colour="darkorange") + 
    geom_point(aes(x=3.25, y= fixedneffrstan[3]), size = 2, colour="darkgreen") + 
    geom_point(aes(x=3.75, y= fixedneffbrms4[4]), size = 2, colour="darkorange") + 
    geom_point(aes(x=4.25, y= fixedneffrstan[4]), size = 2, colour="darkgreen") +
    geom_point(aes(x=4.75, y= fixedneffbrms4[5]), size = 2, colour="darkorange") + 
    geom_point(aes(x=5.25, y= fixedneffrstan[5]), size = 2, colour="darkgreen") +
    geom_point(aes(x=5.75, y= fixedneffbrms4[6]), size = 2, colour="darkorange") + 
    geom_point(aes(x=6.25, y= fixedneffrstan[6]), size = 2, colour="darkgreen") +
    xlab("Parameter") + ylab("Number of effective samples") +
    scale_colour_manual(values = c("darkorange", "darkgreen")) + 
    scale_fill_manual(values = c("darkorange", "darkgreen")) + 
    theme(legend.position = "none") + 
    theme(axis.line = element_line(size = 0.5)) + 
    theme(plot.margin=unit(c(0,0,0,0),"cm")) + 
    scale_x_discrete(labels= c( 
      expression(~gamma), 
      expression(~beta), 
      expression(~delta[~7]), 
      expression(~delta[~6]), 
      expression(~delta[~4]), 
      expression(~delta[~3]))) + 
    labs(subtitle = "D") + 
    geom_hline(yintercept = 12000, linetype = "dashed", color = "grey", size = .3)
  
  
  # Figures - old 
  # Rhat histogram 
  rhatplot <- ggplot(rstanrhatneff, aes(rhat, fill = package)) + 
    geom_histogram(binwidth = 0.0001, alpha = 0.5, position = "identity") + 
    xlim(0.9989, 1.025) + 
    scale_fill_manual(values = c("darkorange", "darkgreen")) + 
    xlab(expression(hat(R))) + 
    ylab("Frequency") + 
    theme_classic() + 
    labs(subtitle = "C") + 
    theme(legend.position = "none") + 
    theme(plot.margin=unit(c(0,0,0,1.5),"cm"))
  
  # Neff histogram 
  neffplot <- ggplot(neffrstanbrms, aes(neff, fill = package)) + 
    geom_histogram(binwidth = 100, alpha = 0.5, position = "identity") + 
    scale_fill_manual(values = c("darkorange", "darkgreen")) + 
    scale_color_manual(values = c("darkorange", "darkgreen")) + 
    xlab("Number of effective samples") + 
    ylab("") + 
    theme_classic() + 
    guides(fill=guide_legend(title="Package")) + 
    labs(subtitle = "D") + 
    geom_vline(xintercept = 12000, linetype = "dashed", color = "grey", size = .3)
  
  # Plot together 
  # grid.arrange(rhatplot, neffplot, nrow = 1, ncol = 2)
  
  # Save figure as png 
  # rhatneffplot <- arrangeGrob(rhatplot, neffplot, nrow = 1, ncol = 2, widths = c(0.45/1, 0.55/1))
  # ggsave(file = "rhatneffplot.png", rhatneffplot, width = 11, height = 4)
  
  legendrhatneff <- cowplot::get_legend(neffplot)
  neffplot <- neffplot + theme(legend.position = "none") 
  
  # Plot all three figures + legend 
  library(gridExtra)
  # grid.arrange(trdif2rstan2, trdif2brms2, legendtrace, nrow = 1, ncol = 3)
  
  # Save figure as png 
  traceplots3p <- arrangeGrob(trdif2rstan2, trdif2brms2, legendtrace, figrhatnew, figneffnew, legendrhatneff, nrow = 2, ncol = 3, widths = c(0.95/3, 0.8/3, 0.25/3))
  ggsave(file = "Fig8_traceplots3pnew_v2.png", traceplots3p, width = 11, height = 8)
} 

# y scale 0 to 0.08 
# BF 8000 - 20000 - select middel 1000 

tracerhatneff(hier_modelc_adj_07012022, brmmodel8_new03012022)
