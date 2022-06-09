# Load packages
library(ggridges)
library(brms)

# Load brms output 
# For brms the output is saved in object brmmodel8_new_bf_03012022 (this is without saving the prior samples)
brmmodel8_new_bf_03012022 <- readRDS("myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/brms/BF/brmmodel8_new_bf_03012022.rds") 

savagedickeyfigure <- function(brmmodel8bf){
  # Obtain Savage-Dickey ratio 
  # https://vuorre.netlify.app/post/2017/03/21/bayes-factors-with-brms/ 
  # Beta 
  b1brms <- hypothesis(brmmodel8bf, "a_side = 0")  # Evid.Ratio is Bayes factor BF01, have to set sample_prior = TRUE when fitting model 
  b1brms$hypothesis$Evid.Ratio 
  plotsdratiob1 <- plot(b1brms,plot = F, theme = theme_get())[[1]] 
  plotsdratiob1 + xlim(-0.07, 0.07) 
  
  # Plot function 
  savdicratioplot <- function(plotsdratio){  # put in plot.hypothesisbrms object 
    plotsratio3data <- plotsdratio$data
    
    density(plotsratio3data$values[plotsratio3data$Type == "Posterior"])
    xpriorsd <- c(density(plotsratio3data$values[plotsratio3data$Type == "Posterior"])[1]$x, density(plotsratio3data$values[plotsratio3data$Type == "Prior"])[1]$x)  # x 
    ypriorsd <- c(density(plotsratio3data$values[plotsratio3data$Type == "Posterior"])[2]$y, density(plotsratio3data$values[plotsratio3data$Type == "Prior"])[2]$y)  # y 
    xypriorsd <- data.frame(x = xpriorsd, y = ypriorsd, Type = rep(c("Posterior", "Prior"), each = 512))
    maxprior <- max(density(plotsratio3data$values[plotsratio3data$Type == "Prior"])[2]$y)  
    
    
    sdrdif3plot2 <- ggplot(xypriorsd, aes(x, y, color = Type, fill = Type)) + 
      geom_line() + 
      geom_area(position = "identity", alpha = 0.3) + 
      geom_point(aes(x = 0 , y = maxprior), size = 1) + 
      xlim(-0.05, 0.07) + 
      ylab("Density") + 
      xlab("Value") + 
      ggtitle(expression(~delta[~"3-2,i"])) + 
      scale_fill_manual(values = c("darkorange", "darkgreen")) + 
      scale_color_manual(values = c("darkorange", "darkgreen")) + 
      theme_classic() + 
      theme(legend.position = "none") 
    
    return(sdrdif3plot2)
    
  }
  
  sdrbetplot <- savdicratioplot(plotsdratiob1) 
  sdrbetplot <- sdrbetplot + ggtitle(expression(~mu[~beta])) + xlim(-0.045,0.1) + geom_point(aes(x = 0, y = 25), color = "darkorange", size = 1)
  sdrbetplot <- sdrbetplot + 
    geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 25), size=0.1, color = "black", linetype = "dashed") + 
    annotate("text", x = c(0.03, 0.048), xend = 0.03, y = 10, yend = 10.4, label = c(expression(paste("BF"["01"], " = ")), round(b1brms$hypothesis$Evid.Ratio, 2)), size = 3) 
  
  
  # Dif 1 
  h1brms <- hypothesis(brmmodel8bf, "a_dif1 = 0")  # Evid.Ratio is Bayes factor BF01, have to set sample_prior = TRUE when fitting model 
  plotsdratio1 <- plot(h1brms,plot = F, theme = theme_get())[[1]] 
  plotsdratio1 + xlim(-0.07, 0.07) 
  
  sdrdif1plot <- savdicratioplot(plotsdratio1) 
  sdrdif1plot <- sdrdif1plot + 
    ggtitle(expression(~mu[~delta[~"7-8"]])) + xlim(-0.05,0.1) + 
    geom_point(aes(x = 0, y = 18.5), color = "darkorange", size = 1) + 
    geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 19), size=0.1, color = "black", linetype = "dashed") + 
    annotate("text", x = c(0.035, 0.052), xend = 0.03, y = 10, yend = 10.4, label = c(expression(paste("BF"["01"], " = ")), round(h1brms$hypothesis$Evid.Ratio, 2)), size = 3) + 
    ylab("")
  
  
  
  # Dif 2 
  h2brms <- hypothesis(brmmodel8bf, "a_dif2 = 0")  # Evid.Ratio is Bayes factor BF01, have to set sample_prior = TRUE when fitting model 
  plotsdratio2 <- plot(h2brms,plot = F, theme = theme_get())[[1]] 
  plotsdratio2 + xlim(-0.07, 0.07) 
  
  sdrdif2plot <- savdicratioplot(plotsdratio2) 
  sdrdif2plot <- sdrdif2plot + 
    ggtitle(expression(~mu[~delta[~"6-7"]])) + 
    xlim(-0.05,0.1) + 
    geom_point(aes(x = 0, y = 0), color = "darkorange", size = 1) + 
    geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 0), size=0.1, color = "black", linetype = "dashed") + 
    annotate("text", x = c(-0.01, 0.009), xend = 0.03, y = 10, yend = 10.4, label = c(expression(paste("BF"["01"], " = ")), round(h2brms$hypothesis$Evid.Ratio, 2)), size = 3) 
  
  
  
  # Dif 3 
  h3brms <- hypothesis(brmmodel8bf, "a_dif3 = 0")  # Evid.Ratio is Bayes factor BF01, have to set sample_prior = TRUE when fitting model 
  plotsdratio3 <- plot(h3brms,plot = F, theme = theme_get())[[1]] 
  plotsdratio3 + xlim(-0.08, 0.08) 
  
  # Get data from plot 
  plotsratio3data <- plotsdratio3$data
  
  sdrdif3plot <- savdicratioplot(plotsdratio3) 
  sdrdif3plot <- sdrdif3plot + 
    ggtitle(expression(~mu[~delta[~"4-3"]])) + 
    xlim(-0.05,0.1) + 
    geom_point(aes(x = 0, y = 0), color = "darkorange", size = 1) + 
    geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 0), size=0.1, color = "black", linetype = "dashed") + 
    annotate("text", x = c(-0.01, 0.009), xend = 0.03, y = 10, yend = 10.4, label = c(expression(paste("BF"["01"], " = ")), round(h3brms$hypothesis$Evid.Ratio, 2)), size = 3) + 
    ylab("")
  
  
  
  # Get density from data 
  # Dif 4 
  h4brms <- hypothesis(brmmodel8bf, "a_dif4 = 0")  # Evid.Ratio is Bayes factor BF01, have to set sample_prior = TRUE when fitting model 
  plotsdratio4 <- plot(h4brms,plot = F, theme = theme_get())[[1]] 
  plotsratio4data <- plotsdratio4$data
  #density(plotsratio4data$values[plotsratio4data$Type == "Posterior"])
  xpriorsd <- c(density(plotsratio4data$values[plotsratio4data$Type == "Posterior"])[1]$x, density(plotsratio4data$values[plotsratio4data$Type == "Prior"])[1]$x)  # x 
  ypriorsd <- c(density(plotsratio4data$values[plotsratio4data$Type == "Posterior"])[2]$y, density(plotsratio4data$values[plotsratio4data$Type == "Prior"])[2]$y)  # y 
  xypriorsd <- data.frame(x = xpriorsd, y = ypriorsd, Type = rep(c("Posterior", "Prior"), each = 512))
  max(density(plotsratio4data$values[plotsratio4data$Type == "Prior"])[2]$y)  # 1.343088
  
  # Self made plot with dots  
  sdrdif4plot2 <- ggplot(xypriorsd, aes(x, y, color = Type, fill = Type)) + 
    geom_line() + 
    geom_area(position = "identity", alpha = 0.3) + 
    geom_point(aes(x = 0 , y = 1.343088), size = 1) + 
    geom_point(aes(x = 0, y = 0), color = "darkorange", size = 1) + 
    xlim(-0.05, 0.1) + 
    ylab("Density") + 
    xlab("Value") + 
    ggtitle(expression(~delta[~"3-2,i"])) + 
    scale_fill_manual(values = c("darkorange", "darkgreen")) + 
    scale_color_manual(values = c("darkorange", "darkgreen")) + 
    guides(color = guide_legend(override.aes = list(linetype = 0, shape = NA)))
  
  # Dif 4 
  h4brms <- hypothesis(brmmodel8bf, "a_dif4 = 0")  # Evid.Ratio is Bayes factor BF01, have to set sample_prior = TRUE when fitting model 
  plotsdratio4 <- plot(h4brms,plot = F, theme = theme_get())[[1]] 
  
  # Get data from plot 
  plotsratio4data <- plotsdratio4$data
  
  # Get density from data 
  density(plotsratio4data$values[plotsratio4data$Type == "Posterior"])
  xpriorsd <- c(density(plotsratio4data$values[plotsratio4data$Type == "Posterior"])[1]$x, density(plotsratio4data$values[plotsratio4data$Type == "Prior"])[1]$x)  # x 
  ypriorsd <- c(density(plotsratio4data$values[plotsratio4data$Type == "Posterior"])[2]$y, density(plotsratio4data$values[plotsratio4data$Type == "Prior"])[2]$y)  # y 
  xypriorsd <- data.frame(x = xpriorsd, y = ypriorsd, Type = rep(c("Posterior", "Prior"), each = 512))
  max(density(plotsratio4data$values[plotsratio4data$Type == "Prior"])[2]$y)  # 1.343088
  
  # Self made plot with dots  
  sdrdif4plot2 <- ggplot(xypriorsd, aes(x, y, color = Type, fill = Type)) + 
    geom_line() + 
    geom_area(position = "identity", alpha = 0.3) + 
    geom_point(aes(x = 0 , y = 1.343088), size = 1) + 
    geom_point(aes(x = 0, y = 8), color = "darkorange", size = 1) + 
    xlim(-0.05, 0.1) + 
    ylab("Density") + 
    xlab("Value") + 
    theme_classic() + 
    ggtitle(expression(~mu[~delta[~"3-2"]])) + 
    scale_fill_manual(values = c("darkorange", "darkgreen")) + 
    scale_color_manual(values = c("darkorange", "darkgreen")) + 
    geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 6.5), size=0.1, color = "black", linetype = "dashed") + 
    annotate("text", x = c(-0.01, 0.007), xend = 0.03, y = 10, yend = 10.4, label = c(expression(paste("BF"["01"], " = ")), round(h4brms$hypothesis$Evid.Ratio, 2)), size = 3) + 
    guides(color = guide_legend(override.aes = list(linetype = 0, shape = NA))) 
  
  # Extract legend 
  library(cowplot)
  legendsdr <- cowplot::get_legend(sdrdif4plot2)
  library(grid)
  grid.draw(legendsdr)
  
  sdrdif4plot2 <- sdrdif4plot2 + theme(legend.position = "none") + ylab("")
  
  # using the provided data (similar to provided plot) 
  ggplot(plotsratio4data, aes(x = values, fill = Type)) + geom_density() + 
    geom_density_ridges(rel_min_height = 0.01)
  
  # the provided plot 
  sdrdif4plot <- plotsdratio4 + 
    xlim(-0.07, 0.07) + 
    geom_point(aes(x=0, y=0), colour="blue") + 
    geom_point(aes(x = 0 , y = 6.22), colour = "green") + 
    geom_vline(xintercept = 0) + 
    geom_point(data = plotsratio4data, y = plotsratio4data$values, x = 0, color="red")
  
  
  # Plot all together 
  grid.arrange(sdrbetplot, sdrdif1plot, legendsdr, sdrdif2plot, sdrdif3plot, sdrdif4plot2, nrow = 2, ncol = 3)
  
  # Save figure as png 
  sdrplotall <- arrangeGrob(sdrbetplot, sdrdif1plot, legendsdr, sdrdif2plot, sdrdif3plot, sdrdif4plot2, nrow = 2, ncol = 3)
  ggsave(file = "sdrplotall.png", sdrplotall, width = 11, height = 6)
  
  sdrbetplot2 <- sdrbetplot + xlim(-0.08, 0.1)
  sdrbetplot2 <- arrangeGrob(sdrbetplot2, legendsdr, nrow = 1, ncol = 2, widths = c(0.8, 0.2)) 
  ggsave(file = "Fig16_sdrplotside_v2.png", sdrbetplot2, width = 6, height = 4)
} 

savagedickeyfigure(brmmodel8_new_bf_03012022) 
