# Bayesian-Hierarchical-Modelling-An-Introduction-and-Reassessment

Note: This version of the abstract and repository are posted prior to formal peer review. In the manuscript, changes compared to the previous version are in blue. 

# Abstract 

With the recent development of easy-to-use tools for Bayesian analysis, psychologists have started to embrace Bayesian hierarchical modeling. Bayesian hierarchical models provide an intuitive account of inter- and intraindividual variability and are particularly suited for the evaluation of repeated-measures designs. Here, we provide guidance for model specificaton and interpretation in Bayesian hierarchical modeling and describe common pitfalls that can arise in the process of model fitting and evaluation. Our introduction gives particular emphasis to prior specification and prior sensitivity, as well as to the calculation of Bayes factors for model comparisons. We illustrate the use of state-of-the-art software programs Stan and brms. The result is an overview over best practices in Bayesian hierarchical modeling that, as we hope, will help psychologists in making the best use of Bayesian hierarchical modeling.

# Outline 

This repository contains all the documents that are used to create the paper "Bayesian Hierarchical Modeling: Introduction and Reassessment". Here, we describe the content of the repository. Please read this information carefully before using the documents. 

1. Manuscript    
2. Folders A to K  
3. I - Images 
4. J - R code 
5. K - R objects 

# 1. Manuscript    

The manuscript is written in a R Markdown file called "manuscript.Rmd". It includes code for the prior visualizations that can be run by knitting the document. The folders "manuscript_cache", "manuscript_files" and "manuscript.tex" are provided when knitting the document. The files "apa6.csl" and "r-references.bib" are needed to knit the `.rmd` file. They contain a template for the APA style and the reference list. To only read the manuscript, you can open the "manuscript.pdf" file. 

# 2. Folders A to K    

The repository contains 11 folders (A to K), in the manuscript referred to as Online Supplements: 

- A: Model Parameterization  
- B: Default Priors Rstan  
- C: Tutorial Normal Model Rstan  
- D: Tutorial Normal Model brms 
- E: Tutorial Log-Normal Model Rstan 
- F: Tutorial Log-Normal Model brms 
- G: Trace Plots 
- H: Posterior Distributions Tables 
- I: Images 
- J: R code 
- K: R objects 

Online Supplements A, B, C, D, E, F, and H are written in R Markdown, therefore their folder contains, next to the `.pdf` file, a separate `.rmd` file and documents necessary to knit the file. To only read the supplements, you can open the `.pdf` file in each folder. Folder G contains `.pdf` files with trace plots for all the parameters of the models in the manuscript and the corresponding R code to create the trace plots. We will explain the other folders in more detail below. 

# 3. I - Images 

This folder contains images that are used in the manuscript. They are inserted in "manuscript.Rmd" and shown in "manuscript.pdf". For the images that are created in R, the R code is provided in the "R objects" folder under "Figures". 

# 4. J - R code 

This folder consists of five other folders: 

1. Brms 
2. Figures 
3. Model Comparison 
4. Prior transformation 
5. Rstan 

We will describe each folder. 

## 4.1 Brms 

This folder contains two folders "Log-normal model" and "Normal model" and contain R code estimating the log-normal and normal model using `brms` as done for the manuscript. The folder "Normal model" also contains a folder "correlation" with the code for estimating a normal model with correlation between individual effects. Note that the code, estimating an Bayesian hierarchical model in `brms`, is explained in more detail tutorials D and F. 

## 4.2 Figures 

This folder contains the R code for some of the figures in the Image folder. The name of the R file contains the figure number to which the code applies. The `.rmd` files of folders C to F provide code to create some of the figures as well. 

## 4.3 Model Comparison 

The Model Comparison folder consists of three folders "brms", "rstan", and "Sensitivity analysis". The folders "brms" and "rstan" contain the R code to perform bridge sampling. In the code, the models and Bayes Factors are estimated ten times. The folder "Sensitivity analysis" contains the R code to perform the sensitivity analysis as described in the section "Sensitivity Analysis" in the manuscript.   

## 4.4 Prior transformation 

This folder contains two `.R` files. The file "priortransformFunction.R" contains the function described in the manuscript to transfer the priors on the variance to priors on the standard deviation. The file "Prior_Transformation 2.R" shows the application of the function for the manuscript. 

## 4.5 Rstan 

In this folder are two folders "Log-normal model" and "Normal model" that contain R code estimating the log-normal and normal model using `Rstan` as done for the manuscript. The folder "Normal model" also contains a folder "correlation" with the code for estimating a normal model with correlation between individual effects. Note that the code, estimating an Bayesian hierarchical model in `Rstan`, is explained in more detail tutorials C and E. 

# 5. K - R objects 

This folder consists of five other folders: 

1. BF model to create figure 1B 
2. brms
3. Model comparison 
4. rstan  

We will describe the folders one by one. 

## 5.1 BF model to create figure 1B 

As the name of this folder tells you, the folder contains the fit of a normal model fitted by the BayesFactor package and is used to create figure 1B of the manuscript. 

## 5.2 brms 

This folder consists of four folders: "BF", "correlation", "log-normal", "Posterior", and "Truncated". In the BF folder, you can find the model fit that has been used to obtain the Bayes factor by the Savage-Dickey density ratio (`.rds` files): "brmmodel8_new_bf_03012022.rds" for the normal model and "brmmodel8log26052022_bf.rds" for the log-normal model. The "Posterior"" folder contains the normal model fit without obtaining the Bayes factor (`.rds` file). Therefore, the models in the two folders only differ in the specification of one option. For the "BF"", the option `sample_prior` in the fit function is set to `TRUE`, while this is set to `FALSE` in fit located in the "Posterior"" folder. The "log-normal" folder contains the log-normal model fit ("brmmodel8log13012022.rds") and the log-normal fit used to obtain the Bayes factor ("BRMSmodelfit18_26052022_bf.rds", this is a duplicate of the log-normal model fit in the "BF" folder). The "Truncated" folder mainly contains duplicates of previously discussed model fits, but it also contains the model fit of the side model ("side_model_fit.rds" and "side_model_fit_12042022.rds"). The "correlation" folder contains the model fit of the normal model with correlation between individual estimates ("brmmodel9_new28062023"). The fit of the models can be loaded as an object in R, instead of fitting the model yourself. Therefore, it contains the result we worked with in this study. 


## 5.3 Model Comparison 

The Model Comparison folder contains two folders "Bayes Factors per package" and "Sensitivity analysis". The folder "Bayes Factors per package" contains the ten estimated Bayes factors estimated by bridge sampling per package ("bssresults_07012022.csv" for `rstan` and "bssresults_brms_15032022_10rep.csv" for `brms`). The folder "Sensitivity analysis" contains the Bayes factors per scenario used for the sensitivity analysis. If the folders contains one or multiple `.csv` files, there will be an explanatory file describing the data set. In case you want to use the data, please read this description carefully as well. 

## 5.4 rstan 

In this folder, you can find again two other folders: "normal model"" and "log-normal model". As the name tells you, these folders contain the `rstan` materials for the normal model and the log-normal model. They contain the `.stan` files for all the models (null model, side model, digit model and full model) and the results of the full model fit that is saved in the `.RDS` file (this is an R object). The fit of the full model can be loaded as an object in R, instead of fitting the model yourself. Therefore, it also contains the result we worked with in this study. The "normal model" folder also contains the `.stan` files used for the sensitivity analysis. "myModel.stan" is a duplicate of the full normal model saved under "rewritestanmodel3_trunc.stan". "rstan_model_fit.rds" is a duplicate of "hier_modelc_adj_07012022.rds". "myLogModel.stan" is a duplicate of the full log-normal model saved under "logstanmodel3.stan". "rstan_logmodel_fit.rds" is a duplicate of "hier_logmodelc_adj13012022.rds". The "normal model" folder also contains a folder "correlation" with the `.stan` file to estimate a normal model with correlation between individual effects called "myModel_cor.stan" and it's model fit saved in "hier_modelc_cor_29062023.rds".

Other documents in the repository that are not discussed here are not of much interest, but are necessary to run the `.Rmd` files. 

# Data set 

In this study, we use publicly available data by Rouder et al. (2005), retrieved from https://github.com/PerceptionCognitionLab/data0/blob/master/lexDec-dist5/. For more information about this data, we refer to them. Participants that participated in the digit classification task performed multiple trials in multiple blocks, resulting in observations per condition (digits) per person. 

Roughly, the columns represent: 
- sub: This column represents the participant number. It starts at 0. 
- block: The block which the trials belong. 
- trial: The number of the trial in the block. 
- stim: The digit that was presented. 0 represent number 2, 1 number 3, 2 number 4, 3 number 6, 4 number 7, and finally 5 equals 8. 
- resp: Participants had to indicate whether the digit was greater or smaller than five. 0 means that they indicated that the digit was smaller than 5, 1 that it was greater than 5. 
- rt: The response time in milliseconds. 
- error: If the response was correct or not. 0 means the response was correct, 1 incorrect. 
- sub 2: This column also represents the participant number, however, this one starts at 1 and has chronical numbers (i.e., it does not skip numbers).   

For more information: 
Rouder, J. N., Lu, J., Speckman, P., Sun, D., & Jiang, Y. (2005). A hierarchical model for estimating response time distributions. Psychonomic Bulletin & Review, 12(2), 195-223.

# Contact Information 
In case you have questions about the repository or our study, you can contact: 

Myrthe Veenman  
Leiden University  
myrthe.veenman@gmail.com 

# Citation 
Veenman, M., Stefan, A. M., Haaf, J. M. (2022; preprint). Bayesian Hierarchical Modelling: An Introduction and Reassessment. 

@misc{Veenman_2022,
 title={Bayesian Hierarchical Modelling: An Introduction and Reassessment},
 url={},
 DOI={},
 publisher={},
 author={Veenman, Myrthe and Stefan, Angelika M. and Haaf, Julia M },
 year={2022},
}

# License 

## Code 

The code in this repository, including the code samples in the manuscript and tutorials, is released under the MIT [MIT license](LICENSE-CODE). Read more at the [Open Source Initiative](https://opensource.org/licenses/MIT).

## Text & Figures 

The text content and figures in this repository are licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-shield]][cc-by]

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg 



