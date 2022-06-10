# Bayes Factors per package  

The data sets "bssresults_07012022.csv" and "bssresults_brms_15032022_10rep.csv" contain the Bayes factors of the full normal model against the other models (null, digit, and side normal model) for the packages `rstan` and `brms` respectively. For each data set, the first column, called "Fit4" or "Fit 1", contains the Bayes factors. The second column "model" specifies to which model the full model is compared to. Each comparison is repeated ten times, resulting in ten Bayes factors per model. The Bayes factor of the full model against the full model is trivial and, therefore, not reported ("NA").  

The `.csv` files are also available in `.rds` files. 

