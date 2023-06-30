# Figure distributions
library(ggplot2)

x <- seq(-3, 3, by=.1)
narrow <- dnorm(x, 0, .1)
#plot(x, narrow, type = "l")
wide <- dnorm(x, 0, 10)
#plot(x, wide)

pwide <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 10)) + ylab("Density") +
  scale_y_continuous(breaks = NULL) 

pnarrow <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("Density") +
  scale_y_continuous(breaks = NULL)

ptogether <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 4), linetype = 2) + ylab("Density") +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1))

parametersnormaldistrplot <- c("Mean = 0, SD = 4" = "#D95F02", "Mean = 0, SD = 1" = "#1B9E77", "Mean = 1, SD = 0.5" = "#E7298A")

normaldistriplot <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = 4), linetype = 2, geom = "area", alpha = 0.3, position = "identity", aes(color = "Mean = 0, SD = 4", fill = "Mean = 0, SD = 4")) +  
  ylab("Density") +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = 1), linetype = 3, geom = "area", alpha = 0.3, position = "identity", aes(color = "Mean = 0, SD = 1", fill = "Mean = 0, SD = 1")) + 
  stat_function(fun = dnorm, n = 1001, args = list(mean = 1, sd = 0.5), geom = "area", alpha = 0.3, position = "identity", aes(color = "Mean = 1, SD = 0.5", fill = "Mean = 1, SD = 0.5")) + 
  geom_hline(yintercept=0, colour="white", size=1) + 
  scale_color_manual(name = "", values = parametersnormaldistrplot, aesthetics = c("colour", "fill")) + 
  theme(legend.title = element_text(size = 8), 
        legend.text=element_text(size = 9, margin = margin(t = 3)), 
        legend.key.width=unit(0.35,"cm"), 
        legend.key.height=unit(0.3,"cm"), 
        legend.position=c(.4,.5), 
        legend.margin = margin(0, unit = "lines"), 
        axis.text=element_text(size=10),
        axis.title=element_text(size=11)) + 
  labs(subtitle = "") + xlab("")


theme_set(theme_apa(base_size = 9))
normaldistriplot

# all set 1, 1 

parametersinvgdistrplot <- c("Shape = 1, Scale = 1" = "#D95F02", "Shape = 3, Scale = 1" = "#1B9E77", "Shape = 3, Scale = 0.5" = "#E7298A")

unidistriplot <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dinvgamma, n = 1001, args = list(1, 1), linetype = 2, geom = "area", alpha = 0.3, position = "identity", aes(color = "Shape = 1, Scale = 1", fill = "Shape = 1, Scale = 1")) +  ylab("Density") +
  stat_function(fun = dinvgamma, n = 1001, args = list(3, 1), linetype = 3, geom = "area", alpha = 0.3, position = "identity", aes(color = "Shape = 3, Scale = 1", fill = "Shape = 3, Scale = 1")) + 
  stat_function(fun = dinvgamma, n = 1001, args = list(3, 0.5), geom = "area", alpha = 0.3, position = "identity", aes(color = "Shape = 3, Scale = 0.5", fill = "Shape = 3, Scale = 0.5")) + geom_hline(yintercept=0, colour="white", size=1) + scale_color_manual(name = "", values = parametersinvgdistrplot, aesthetics = c("colour", "fill")) + xlim(0, 5) +  
  theme(legend.title = element_text(size = 8), 
        legend.text=element_text(size = 9, margin = margin(t = 3)), 
        legend.key.width=unit(0.35,"cm"), 
        legend.key.height=unit(0.3,"cm"), 
        legend.position=c(.4,.5), 
        legend.margin = margin(0, unit = "lines"), 
        # legend.key = element_rect(color="white", size = 4), 
        legend.spacing.y = unit(.5, "lines"), 
        axis.text=element_text(size=10),
        axis.title=element_text(size=11)) + 
  labs(subtitle = " ") + ylab("")  + xlab("Parameter value")


theme_set(theme_apa(base_size = 9))
unidistriplot
addSmallLegend(unidistriplot)


# all set 1, 1 

parametersstudentdistrplot <- c("Location = 0, Scale = 10" = "#D95F02", "Location = 1, Scale = 3" = "#1B9E77", "Location = 0, Scale = 1" = "#E7298A")

studentdistriplot <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dst, n = 1001, args = list(nu = 3, mu = 0, sigma = 10), linetype = 2, geom = "area", alpha = 0.3, position = "identity", aes(color = "Location = 0, Scale = 10", fill = "Location = 0, Scale = 10")) +  ylab("Density") +
  stat_function(fun = dst, n = 1001, args = list(nu = 3, mu = 1, sigma = 3), linetype = 3, geom = "area", alpha = 0.3, position = "identity", aes(color = "Location = 1, Scale = 3", fill = "Location = 1, Scale = 3")) + 
  stat_function(fun = dst, n = 1001, args = list(nu = 3, mu = 0, sigma = 1), geom = "area", alpha = 0.3, position = "identity", aes(color = "Location = 0, Scale = 1", fill = "Location = 0, Scale = 1")) + geom_hline(yintercept=0, colour="white", size=1) + scale_color_manual(name = "", values = parametersstudentdistrplot, aesthetics = c("colour", "fill")) + xlim(-10, 10) + 
  theme(legend.title = element_text(size = 8), 
        legend.text=element_text(size = 9, margin = margin(t = 3)), 
        legend.key.width=unit(0.35,"cm"), 
        legend.key.height=unit(0.3,"cm"), 
        legend.position=c(.4,.5), 
        legend.margin = margin(0, unit = "lines"), 
        axis.text=element_text(size=10),
        axis.title=element_text(size=11)) + 
  labs(subtitle = "") + xlab("") + ylab("")

# We won't be using this function for the paper 
addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 8, spaceLegend = 0.0001) { # 5.8
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}


# Apply on original plot - if you want legends on top, not using it for paper 
# normaldistriplot <- addSmallLegend(normaldistriplot) + theme(legend.position="top")
# unidistriplot <- addSmallLegend(unidistriplot) + theme(legend.position="top")
# studentdistriplot <- addSmallLegend(studentdistriplot) + theme(legend.position="top")


theme_set(theme_apa(base_size = 9))
studentdistriplot

library(gridExtra)
grid.arrange(normaldistriplot, unidistriplot, studentdistriplot, nrow = 1, ncol = 3)

# Save figure as png 
distrtypefigure_v2 <- arrangeGrob(normaldistriplot, unidistriplot, studentdistriplot, nrow = 1, ncol = 3)
ggsave(file = "distrtypefigure_v2.png", distrtypefigure_v2, width = 11, height = 4)


# Different try 
library(cowplot)
normaldistriplot_legend <- get_legend(normaldistriplot) 
unidistriplot_legend <- get_legend(unidistriplot)
studentdistriplot_legend <- get_legend(studentdistriplot)

# Remove legend from plots 
normaldistriplot <- normaldistriplot + theme(legend.position = "none")
unidistriplot <- unidistriplot + theme(legend.position = "none") 
studentdistriplot <- studentdistriplot + theme(legend.position = "none")


  
distrtypefigure_v2 <- arrangeGrob(normaldistriplot_legend, unidistriplot_legend, studentdistriplot_legend, 
                                  normaldistriplot, unidistriplot, studentdistriplot, nrow = 2, ncol = 3, 
                                  heights = c(0.75, 4), top = "A: Normal distribution                                                B: Inverse gamma distribution                                       C: Student's t-distribution                     ")       
ggsave(file = "Fig4_distrtypefigure_v3.png", distrtypefigure_v2, width = 12, height = 5)
 
