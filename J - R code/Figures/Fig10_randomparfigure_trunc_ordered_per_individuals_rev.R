# Individual effects from Rstan 
# Order on participant 
# For rstan the output is saved in hier_modelc_adj_07012022 (adjust "myPath" below to your directory)
hier_modelc_adj_07012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/K - R objects/rstan/normal model/hier_modelc_adj_07012022.rds")

# For brms the output is saved in object brmmodel8_new03012022 (this is without saving the prior samples)
brmmodel8_new03012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/K - R objects/brms/Posterior/brmmodel8_new03012022.rds")  

library(rstan)
library(brms)
library(gridExtra)

indeffplot_orderedind <- function(hier_modelc_adj_v2, brmmodel8_v2){
  
  # Only gamma's 
  gamnamesstan <- paste("gammai", "[",1:52,"]", sep="")
  gamstan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( gamnamesstan))$summary)
  gamstanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu"))$summary)
  gamstan$parameters <- c(gamnamesstan)
  
  for (i in 1:nrow(gamstan)){  # 1 if CI contains zero, 0 otherwise 
    if (gamstan$`2.5%`[i]*gamstan$`97.5%`[i] < 0) {
      gamstan$zero2[i] <- "pink"
    } else (gamstan$zero2[i] <- "blue") 
  }
  
  
  gamstan2 <- gamstan[order(gamstan$mean),]
  gamstan$order <- c(1:52)
  
  gammarandomplot <- ggplot(gamstan, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = "blue"), color = "#00BFC4") + 
    geom_point() + 
    geom_hline(yintercept = gamstanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~gamma[~i]), y = "Estimation") + 
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  # geom_hline(yintercept = 0) + because nothing zero 
  
  # Beta  
  # for rstan you get estimates for parameters, so not  deviation so don't have to add them 
  betnamesstan <- paste("betai", "[",1:52,"]", sep="")
  betstan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( betnamesstan))$summary)
  betstanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu2"))$summary)
  betstan$parameters <- c(betnamesstan)
  
  for (i in 1:nrow(betstan)){  # 1 if CI contains zero, 0 otherwise 
    if (betstan$`2.5%`[i]*betstan$`97.5%`[i] < 0) {
      betstan$zero2[i] <- "blue"
    } else (betstan$zero2[i] <- "pink") 
  }
  
  
  betstan2 <- betstan[order(betstan$mean),]
  betstan$order <- c(1:52)
  
  betarandomplot <- ggplot(betstan, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = betstanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~beta[~"i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta1 
  del1namesstan <- paste("delta1", "[",1:52,"]", sep="")
  delt1stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del1namesstan))$summary)
  del1stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu3"))$summary)
  delt1stan$parameters <- c(del1namesstan)
  
  for (i in 1:nrow(delt1stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt1stan$`2.5%`[i]*delt1stan$`97.5%`[i] < 0) {
      delt1stan$zero2[i] <- "blue"
    } else (delt1stan$zero2[i] <- "pink") 
  }
  
  
  delt1stan2 <- delt1stan[order(delt1stan$mean),]
  delt1stan$order <- c(1:52)
  
  delta1randomplot <- ggplot(delt1stan, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del1stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"7i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta 2 
  del2namesstan <- paste("delta2", "[",1:52,"]", sep="")
  delt2stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del2namesstan))$summary)
  del2stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu4"))$summary)
  delt2stan$parameters <- c(del2namesstan)
  
  for (i in 1:nrow(delt2stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt2stan$`2.5%`[i]*delt2stan$`97.5%`[i] < 0) {
      delt2stan$zero2[i] <- "blue"
    } else (delt2stan$zero2[i] <- "pink") 
  }
  
  delt2stan2 <- delt2stan[order(delt2stan$mean),]
  delt2stan$order <- c(1:52)
  
  delta2randomplot <- ggplot(delt2stan, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del2stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"6i"]), y = "Estimation") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta 3 
  del3namesstan <- paste("delta3", "[",1:52,"]", sep="")
  delt3stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del3namesstan))$summary)
  del3stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu5"))$summary)
  delt3stan$parameters <- c(del3namesstan)
  
  for (i in 1:nrow(delt3stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt3stan$`2.5%`[i]*delt3stan$`97.5%`[i] < 0) {
      delt3stan$zero2[i] <- "blue"
    } else (delt3stan$zero2[i] <- "pink") 
  }
  
  delt3stan2 <- delt3stan[order(delt3stan$mean),]
  delt3stan$order <- c(1:52)
  
  delta3randomplot <- ggplot(delt3stan, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del3stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"4i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta 4 
  del4namesstan <- paste("delta4", "[",1:52,"]", sep="")
  delt4stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del4namesstan))$summary)
  del4stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu6"))$summary)
  delt4stan$parameters <- c(del4namesstan)
  
  for (i in 1:nrow(delt4stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt4stan$`2.5%`[i]*delt4stan$`97.5%`[i] < 0) {
      delt4stan$zero2[i] <- "blue"
    } else (delt4stan$zero2[i] <- "pink") 
  }
  
  
  delt4stan2 <- delt4stan[order(delt4stan$mean),]
  delt4stan$order <- c(1:52)
  
  delta4randomplot <- ggplot(delt4stan, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del4stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"3i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Plot together 
  grid.arrange(gammarandomplot, betarandomplot, delta1randomplot, delta2randomplot, delta3randomplot, delta4randomplot, nrow = 2, ncol = 3)
  
  # Save figure as png 
  randomparplot <- arrangeGrob(gammarandomplot, betarandomplot, delta1randomplot, delta2randomplot, delta3randomplot, delta4randomplot, nrow = 2, ncol = 3, widths = c(1, 1, 1), heights = c(1, 1))
  ggsave(file = "randompar_v2.png", randomparplot, width = 9, height = 6)
  
  
  # Brms 
  ## Gamma 
  brmsgammar <- coef(brmmodel8_v2)$sub1[,,"b_inter"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsgammar <- as.data.frame(brmsgammar)
  fixbremsgammar <- fixef(brmmodel8_v2)["b_inter","Estimate"]
  
  brmsgammar2 <- brmsgammar[order(brmsgammar$Estimate),]
  brmsgammar$order <- c(1:52)
  
  for (i in 1:nrow(brmsgammar)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsgammar$Q2.5[i]*brmsgammar$Q97.5[i] < 0) {
      brmsgammar$zero2[i] <- "#F8766D"
    } else (brmsgammar$zero2[i] <- "#00BFC4") 
  }
  
  gammarandomplotbrms <- ggplot(brmsgammar, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsgammar$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~gamma[~i]), y = "") + 
    geom_hline(yintercept = fixbremsgammar, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),)
  
  # Beta 
  summary(brmmodel8_v2)[17]$random$sub1[3]
  brmsbetar <- coef(brmmodel8_v2)$sub1[,,"a_side"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsbetar <- as.data.frame(brmsbetar)
  fixbremsbetar <- fixef(brmmodel8_v2)["a_side","Estimate"]
  
  brmsbetar2 <- brmsbetar[order(brmsbetar$Estimate),]
  brmsbetar$order <- c(1:52)
  
  for (i in 1:nrow(brmsbetar)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsbetar$Q2.5[i]*brmsbetar$Q97.5[i] < 0) {
      brmsbetar$zero2[i] <- "#F8766D"
    } else (brmsbetar$zero2[i] <- "#00BFC4") 
  }
  
  betarandomplotbrms <- ggplot(brmsbetar, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsbetar$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~beta[~"i"]), y = "") + 
    geom_hline(yintercept = fixbremsbetar, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 1 
  brmsdel1r <- coef(brmmodel8_v2)$sub1[,,"a_dif1"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel1r <- as.data.frame(brmsdel1r)
  fixbremsdif2r <- fixef(brmmodel8_v2)["a_dif1","Estimate"]
  
  brmsdel1r2 <- brmsdel1r[order(brmsdel1r$Estimate),]
  brmsdel1r$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel1r)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel1r$Q2.5[i]*brmsdel1r$Q97.5[i] < 0) {
      brmsdel1r$zero2[i] <- "#F8766D"
    } else (brmsdel1r$zero2[i] <- "#00BFC4") 
  }
  
  dif1randomplotbrms <- ggplot(brmsdel1r, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel1r$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"7i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif2r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 2 
  brmsdel2r <- coef(brmmodel8_v2)$sub1[,,"a_dif2"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel2r <- as.data.frame(brmsdel2r)
  fixbremsdif22r <- fixef(brmmodel8_v2)["a_dif2","Estimate"]
  
  brmsdel2r2 <- brmsdel2r[order(brmsdel2r$Estimate),]
  brmsdel2r$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel2r)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel2r$Q2.5[i]*brmsdel2r$Q97.5[i] < 0) {
      brmsdel2r$zero2[i] <- "#F8766D"
    } else (brmsdel2r$zero2[i] <- "#00BFC4") 
  }
  
  dif2randomplotbrms <- ggplot(brmsdel2r, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel2r$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"6i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif22r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 3 
  brmsdel3r <- coef(brmmodel8_v2)$sub1[,,"a_dif3"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel3r <- as.data.frame(brmsdel3r)
  fixbremsdif3r <- fixef(brmmodel8_v2)["a_dif3","Estimate"]
  
  brmsdel3r2 <- brmsdel3r[order(brmsdel3r$Estimate),]
  brmsdel3r$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel3r)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel3r$Q2.5[i]*brmsdel3r$Q97.5[i] < 0) {
      brmsdel3r$zero2[i] <- "#F8766D"
    } else (brmsdel3r$zero2[i] <- "#00BFC4") 
  }
  
  dif3randomplotbrms <- ggplot(brmsdel3r, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel3r$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"4i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif3r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 4 
  brmsdel4r <- coef(brmmodel8_v2)$sub1[,,"a_dif4"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel4r <- as.data.frame(brmsdel4r)
  fixbremsdif4r <- fixef(brmmodel8_v2)["a_dif4","Estimate"]
  
  brmsdel4r2 <- brmsdel4r[order(brmsdel4r$Estimate),]
  brmsdel4r$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel4r)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel4r$Q2.5[i]*brmsdel4r$Q97.5[i] < 0) {
      brmsdel4r$zero2[i] <- "#F8766D"
    } else (brmsdel4r$zero2[i] <- "#00BFC4") 
  }
  
  dif4randomplotbrms <- ggplot(brmsdel4r, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel4r$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"3i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif4r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Plot together 
  grid.arrange(gammarandomplotbrms, betarandomplotbrms, dif1randomplotbrms, dif2randomplotbrms, dif3randomplotbrms, dif4randomplotbrms, nrow = 2, ncol = 3)
  
  # Save figure as png 
  randomparplotbrms <- arrangeGrob(gammarandomplotbrms, betarandomplotbrms, dif1randomplotbrms, dif2randomplotbrms, dif3randomplotbrms, dif4randomplotbrms, nrow = 2, ncol = 3, widths = c(1, 1, 1), heights = c(1, 1))
  ggsave(file = "Fig10_randomparbrms_v2.png", randomparplotbrms, width = 9, height = 6)
} 

indeffplot_orderedind(hier_modelc_adj_07012022, brmmodel8_new03012022)


indeffplot_orderedind2 <- function(hier_modelc_adj_v2, brmmodel8_v2){
  
  # Only gamma's 
  gamnamesstan <- paste("gammai", "[",1:52,"]", sep="")
  gamstan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( gamnamesstan))$summary)
  gamstanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu"))$summary)
  gamstan$parameters <- c(gamnamesstan)
  
  for (i in 1:nrow(gamstan)){  # 1 if CI contains zero, 0 otherwise 
    if (gamstan$`2.5%`[i]*gamstan$`97.5%`[i] < 0) {
      gamstan$zero2[i] <- "pink"
    } else (gamstan$zero2[i] <- "blue") 
  }
  
  
  gamstan2 <- gamstan[order(gamstan$mean),]
  gamstan2$order <- c(1:52)
  
  gammarandomplot <- ggplot(gamstan2, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = "blue"), color = "#00BFC4") + 
    geom_point() + 
    geom_hline(yintercept = gamstanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~gamma[~i]), y = "Estimation") + 
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  # geom_hline(yintercept = 0) + because nothing zero 
  
  # Beta  
  # for rstan you get estimates for parameters, so not  deviation so don't have to add them 
  betnamesstan <- paste("betai", "[",1:52,"]", sep="")
  betstan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( betnamesstan))$summary)
  betstanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu2"))$summary)
  betstan$parameters <- c(betnamesstan)
  
  for (i in 1:nrow(betstan)){  # 1 if CI contains zero, 0 otherwise 
    if (betstan$`2.5%`[i]*betstan$`97.5%`[i] < 0) {
      betstan$zero2[i] <- "blue"
    } else (betstan$zero2[i] <- "pink") 
  }
  
  
  betstan2 <- betstan[order(gamstan$mean),]
  betstan2$order <- c(1:52)
  
  betarandomplot <- ggplot(betstan2, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = betstanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~beta[~"i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta1 
  del1namesstan <- paste("delta1", "[",1:52,"]", sep="")
  delt1stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del1namesstan))$summary)
  del1stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu3"))$summary)
  delt1stan$parameters <- c(del1namesstan)
  
  for (i in 1:nrow(delt1stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt1stan$`2.5%`[i]*delt1stan$`97.5%`[i] < 0) {
      delt1stan$zero2[i] <- "blue"
    } else (delt1stan$zero2[i] <- "pink") 
  }
  
  
  delt1stan2 <- delt1stan[order(gamstan$mean),]
  delt1stan2$order <- c(1:52)
  
  delta1randomplot <- ggplot(delt1stan2, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del1stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"7i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta 2 
  del2namesstan <- paste("delta2", "[",1:52,"]", sep="")
  delt2stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del2namesstan))$summary)
  del2stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu4"))$summary)
  delt2stan$parameters <- c(del2namesstan)
  
  for (i in 1:nrow(delt2stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt2stan$`2.5%`[i]*delt2stan$`97.5%`[i] < 0) {
      delt2stan$zero2[i] <- "blue"
    } else (delt2stan$zero2[i] <- "pink") 
  }
  
  delt2stan2 <- delt2stan[order(gamstan$mean),]
  delt2stan2$order <- c(1:52)
  
  delta2randomplot <- ggplot(delt2stan2, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del2stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"6i"]), y = "Estimation") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta 3 
  del3namesstan <- paste("delta3", "[",1:52,"]", sep="")
  delt3stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del3namesstan))$summary)
  del3stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu5"))$summary)
  delt3stan$parameters <- c(del3namesstan)
  
  for (i in 1:nrow(delt3stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt3stan$`2.5%`[i]*delt3stan$`97.5%`[i] < 0) {
      delt3stan$zero2[i] <- "blue"
    } else (delt3stan$zero2[i] <- "pink") 
  }
  
  delt3stan2 <- delt3stan[order(gamstan$mean),]
  delt3stan2$order <- c(1:52)
  
  delta3randomplot <- ggplot(delt3stan2, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del3stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"4i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  
  # Delta 4 
  del4namesstan <- paste("delta4", "[",1:52,"]", sep="")
  delt4stan <- as.data.frame(summary(hier_modelc_adj_v2, pars = c( del4namesstan))$summary)
  del4stanmu <- as.data.frame(summary(hier_modelc_adj_v2, pars = c("mu6"))$summary)
  delt4stan$parameters <- c(del4namesstan)
  
  for (i in 1:nrow(delt4stan)){  # 1 if CI contains zero, 0 otherwise 
    if (delt4stan$`2.5%`[i]*delt4stan$`97.5%`[i] < 0) {
      delt4stan$zero2[i] <- "blue"
    } else (delt4stan$zero2[i] <- "pink") 
  }
  
  
  delt4stan2 <- delt4stan[order(gamstan$mean),]
  delt4stan2$order <- c(1:52)
  
  delta4randomplot <- ggplot(delt4stan2, aes(x = order, y = mean, ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_linerange(aes(color = zero2)) + 
    geom_point() + 
    geom_hline(yintercept = del4stanmu$mean, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"3i"]), y = "") + 
    geom_hline(yintercept = 0, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Plot together 
  grid.arrange(gammarandomplot, betarandomplot, delta1randomplot, delta2randomplot, delta3randomplot, delta4randomplot, nrow = 2, ncol = 3)
  
  # Save figure as png 
  randomparplot <- arrangeGrob(gammarandomplot, betarandomplot, delta1randomplot, delta2randomplot, delta3randomplot, delta4randomplot, nrow = 2, ncol = 3, widths = c(1, 1, 1), heights = c(1, 1))
  ggsave(file = "randompar_v2.png", randomparplot, width = 9, height = 6)
  
  
  # Brms 
  ## Gamma 
  brmsgammar <- coef(brmmodel8_v2)$sub1[,,"b_inter"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsgammar <- as.data.frame(brmsgammar)
  fixbremsgammar <- fixef(brmmodel8_v2)["b_inter","Estimate"]
  
  brmsgammar2 <- brmsgammar[order(brmsgammar$Estimate),]
  brmsgammar2$order <- c(1:52)
  
  for (i in 1:nrow(brmsgammar2)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsgammar2$Q2.5[i]*brmsgammar2$Q97.5[i] < 0) {
      brmsgammar2$zero2[i] <- "#F8766D"
    } else (brmsgammar2$zero2[i] <- "#00BFC4") 
  }
  
  gammarandomplotbrms <- ggplot(brmsgammar2, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsgammar2$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~gamma[~i]), y = "") + 
    geom_hline(yintercept = fixbremsgammar, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),)
  
  # Beta 
  summary(brmmodel8_v2)[17]$random$sub1[3]
  brmsbetar <- coef(brmmodel8_v2)$sub1[,,"a_side"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsbetar <- as.data.frame(brmsbetar)
  fixbremsbetar <- fixef(brmmodel8_v2)["a_side","Estimate"]
  
  brmsbetar2 <- brmsbetar[order(brmsgammar$Estimate),]
  brmsbetar2$order <- c(1:52)
  
  for (i in 1:nrow(brmsbetar2)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsbetar2$Q2.5[i]*brmsbetar2$Q97.5[i] < 0) {
      brmsbetar2$zero2[i] <- "#F8766D"
    } else (brmsbetar2$zero2[i] <- "#00BFC4") 
  }
  
  betarandomplotbrms <- ggplot(brmsbetar2, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsbetar2$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~beta[~"i"]), y = "") + 
    geom_hline(yintercept = fixbremsbetar, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 1 
  brmsdel1r <- coef(brmmodel8_v2)$sub1[,,"a_dif1"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel1r <- as.data.frame(brmsdel1r)
  fixbremsdif2r <- fixef(brmmodel8_v2)["a_dif1","Estimate"]
  
  brmsdel1r2 <- brmsdel1r[order(brmsgammar$Estimate),]
  brmsdel1r2$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel1r2)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel1r2$Q2.5[i]*brmsdel1r2$Q97.5[i] < 0) {
      brmsdel1r2$zero2[i] <- "#F8766D"
    } else (brmsdel1r2$zero2[i] <- "#00BFC4") 
  }
  
  dif1randomplotbrms <- ggplot(brmsdel1r2, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel1r2$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"7i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif2r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 2 
  brmsdel2r <- coef(brmmodel8_v2)$sub1[,,"a_dif2"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel2r <- as.data.frame(brmsdel2r)
  fixbremsdif22r <- fixef(brmmodel8_v2)["a_dif2","Estimate"]
  
  brmsdel2r2 <- brmsdel2r[order(brmsgammar$Estimate),]
  brmsdel2r2$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel2r2)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel2r2$Q2.5[i]*brmsdel2r2$Q97.5[i] < 0) {
      brmsdel2r2$zero2[i] <- "#F8766D"
    } else (brmsdel2r2$zero2[i] <- "#00BFC4") 
  }
  
  dif2randomplotbrms <- ggplot(brmsdel2r2, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel2r2$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"6i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif22r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 3 
  brmsdel3r <- coef(brmmodel8_v2)$sub1[,,"a_dif3"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel3r <- as.data.frame(brmsdel3r)
  fixbremsdif3r <- fixef(brmmodel8_v2)["a_dif3","Estimate"]
  
  brmsdel3r2 <- brmsdel3r[order(brmsgammar$Estimate),]
  brmsdel3r2$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel3r2)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel3r2$Q2.5[i]*brmsdel3r2$Q97.5[i] < 0) {
      brmsdel3r2$zero2[i] <- "#F8766D"
    } else (brmsdel3r2$zero2[i] <- "#00BFC4") 
  }
  
  dif3randomplotbrms <- ggplot(brmsdel3r2, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel3r2$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"4i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif3r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Delta 4 
  brmsdel4r <- coef(brmmodel8_v2)$sub1[,,"a_dif4"][,c("Estimate", "Q2.5", "Q97.5")]
  brmsdel4r <- as.data.frame(brmsdel4r)
  fixbremsdif4r <- fixef(brmmodel8_v2)["a_dif4","Estimate"]
  
  brmsdel4r2 <- brmsdel4r[order(brmsgammar$Estimate),]
  brmsdel4r2$order <- c(1:52)
  
  for (i in 1:nrow(brmsdel4r2)){  # 1 if CI contains zero, 0 otherwise 
    if (brmsdel4r2$Q2.5[i]*brmsdel4r2$Q97.5[i] < 0) {
      brmsdel4r2$zero2[i] <- "#F8766D"
    } else (brmsdel4r2$zero2[i] <- "#00BFC4") 
  }
  
  dif4randomplotbrms <- ggplot(brmsdel4r2, aes(x = order, y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
    geom_linerange(color = brmsdel4r2$zero2) + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) + 
    labs(title = expression(~delta[~"3i"]), y = "") + 
    geom_hline(yintercept = fixbremsdif4r, color = "red", alpha = .3) +  
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),) 
  
  # Plot together 
  grid.arrange(gammarandomplotbrms, betarandomplotbrms, dif1randomplotbrms, dif2randomplotbrms, dif3randomplotbrms, dif4randomplotbrms, nrow = 2, ncol = 3)
  
  # Save figure as png 
  randomparplotbrms <- arrangeGrob(gammarandomplotbrms, betarandomplotbrms, dif1randomplotbrms, dif2randomplotbrms, dif3randomplotbrms, dif4randomplotbrms, nrow = 2, ncol = 3, widths = c(1, 1, 1), heights = c(1, 1))
  ggsave(file = "Fig10_randomparbrms_v2.png", randomparplotbrms, width = 9, height = 6)
} 

indeffplot_orderedind2(hier_modelc_adj_07012022, brmmodel8_new03012022)


