# BF rstan bridge-sampling 
library(ggplot2)
library(brms)
library(rstan )

# Load results
# rstan
resultsbsrstan <- read.csv("/myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/Model comparison/Bayes Factors per package/bssresults_07012022.csv")

## Restructure 
resultsbsrstan$package <- rep("Rstan", nrow(resultsbsrstan))
resultsbsrstan <- data.frame(Fit4 = resultsbsrstan$Fit4, model = resultsbsrstan$model, package = resultsbsrstan$package)

# brms 
resultsbsbrms <- read.csv("/myPath/Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment/R objects/Model comparison/Bayes Factors per package/bssresults_brms_15032022_10rep.csv")

## Restructure 
resultsbsbrms$package <- rep("Brms", nrow(resultsbsbrms))
resultsbsbrms <- data.frame(Fit4 = resultsbsbrms$Fit1, model = resultsbsbrms$model, package = resultsbsbrms$package)

# Combine rstan and brms results 
resultsbf10 <- rbind(resultsbsrstan, resultsbsbrms)

resultsbf10$BF01 <- 1/resultsbf10$Fit4

level_order <- c('Null model', 'Side model', 'Digit model') 

# Evidence of full model against alternative models 
normalbfplot <- ggplot(resultsbf10[which(resultsbf10$model == "Null model" | 
                                           resultsbf10$model == "Side model" |  
                                           resultsbf10$model == "Digit model"),],
                       aes(x = factor(model, level_order), y = Fit4, color = 
                             package, shape = package, fill = package)) + 
  geom_jitter(alpha = 0.5, position = position_jitterdodge(dodge.width = 0.5)) + 
  # geom_violin(trim = FALSE) + 
  scale_y_log10() + 
  theme_classic() + 
  labs(subtitle = "Linear model \n", 
       x = "", y = "BF01") + 
  scale_color_manual(name = "Package", 
                     labels = c("Rstan", "Brms"),
                     values = c("darkgreen", "darkorange")) + 
  scale_shape_manual(name = "Package", 
                     labels = c("Rstan", "Brms"),
                     values = c(17, 19)) + 
  scale_fill_manual(name = "Package", 
                    labels = c("Rstan", "Brms"), 
                    values = c("darkgreen", "darkorange")) + 
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.3) 

# Evidence of alternative models against full model 
normalbfplot01 <- ggplot(resultsbf10[which(resultsbf10$model == "Null model" | 
                                           resultsbf10$model == "Side model" |  
                                           resultsbf10$model == "Digit model"),],
                       aes(x = factor(model, level_order), y = BF01, color = 
                             package, shape = package, fill = package)) + 
  geom_jitter(alpha = 0.5, position = position_jitterdodge(dodge.width = 0.5)) + 
  # geom_violin(trim = FALSE) + 
  scale_y_log10() + 
  theme_classic() + 
  labs(subtitle = "Linear model \n", 
       x = "", y = "BF01") + 
  scale_color_manual(name = "Package",   # ggplot works alphabetically, so first brms then rstan
                     labels = c("Brms", "Rstan"),
                     values = c("darkorange", "darkgreen")) + 
  scale_shape_manual(name = "Package", 
                     labels = c("Brms", "Rstan"),
                     values = c(19, 17)) + 
  scale_fill_manual(name = "Package", 
                    labels = c("Brms", "Rstan"), 
                    values = c("darkorange", "darkgreen")) + 
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.3) 


# Save figure as png 
ggsave(file = "Fig17_normalbfplot01.png", normalbfplot01) 
