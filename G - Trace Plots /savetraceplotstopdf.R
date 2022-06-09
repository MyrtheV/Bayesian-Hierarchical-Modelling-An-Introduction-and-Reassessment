# Rstan traceplots all parameters 
# Load packages 
library(rstan)
library(brms)

# Rstan 
# Normal model 
# Load normal model fit 
hier_modelc_adj_v2 <- readRDS("/Users/myrtheveenman/hier_modelc_adj09042021_v2.rds")

names(hier_modelc_adj_v2)  # all parameter names 
# Plot figure 
# For one traceplot 
pdf("traceplots_normalmodel.pdf", onefile = TRUE)
plot(hier_modelc_adj_v2, plotfun = "trace", pars = names(hier_modelc_adj_v2)[1])
dev.off()
 
# Save all traceplots in .pdf 
pdf("traceplots_normalmodel_rstan.pdf", onefile = TRUE)
for (i in 1:326){
  print(plot(hier_modelc_adj_v2, plotfun = "trace", pars = names(hier_modelc_adj_v2)[i]))  # rstan 
}
dev.off()


# Log-normal model 
# Load log-normal model fit 
hier_logmodelc_adj13012022 <- readRDS("/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/rstan/non-linear model/hier_logmodelc_adj13012022.rds")

# Save traceplots in .pdf 
pdf("traceplots_lognormalmodel_rstan.pdf", onefile = TRUE)
for (i in 1:326){
  print(plot(hier_logmodelc_adj13012022, plotfun = "trace", pars = names(hier_logmodelc_adj13012022)[i]))   # rstan 
}
dev.off()

# Brms 
# Normal model 
# Load normal model fit 
model_fit <- readRDS(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/brms/Truncated/BRMSmodelfit18_03012022.rds") 
variables(model_fit)

# Save traceplots in .pdf 
pdf("traceplots_normalmodel_brms.pdf", onefile = TRUE)
for(i in 1:326){
  print(mcmc_plot(model_fit, variable = variables(model_fit)[i], type = "trace") + scale_color_manual(values=c("#E66101", "#998EC3", "#542788", "#F1A340")) + scale_x_continuous(breaks=c(0,1000,2000, 3000, 4000, 5000), 
                                                                                                                                                    labels=c(1000, 2000, 3000, 4000, 5000, 6000))) 
} 
dev.off()

# Log-normal model 
# Load log-normal model fit 
brms_logmodel_fit <- readRDS(file = "/Users/myrtheveenman/Documents/GitHub/multilevel-tutorial-paper/R objects/brms/non-linear/brmmodel8log13012022.rds") 

# Save traceplots in .pdf 
pdf("traceplots_lognormalmodel_brms.pdf", onefile = TRUE)
for(i in 1:326){
  print(mcmc_plot(brms_logmodel_fit, variable = variables(brms_logmodel_fit)[i], type = "trace") + scale_color_manual(values=c("#E66101", "#998EC3", "#542788", "#F1A340")) + scale_x_continuous(breaks=c(0,1000,2000, 3000, 4000, 5000), 
                                                                                                                                                                           labels=c(1000, 2000, 3000, 4000, 5000, 6000))) 
} 
dev.off()
