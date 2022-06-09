# Inverse-gamma prior on standard deviation:

alpha = 1.745
beta = 0.02

dinvg <- function(x, alpha, beta){
  (beta^alpha)/gamma(alpha)*x^(-alpha-1)*exp(-beta*x^(-1))
}

# Sanity check: This gives the same results as LaplacesDemon::dinvgamma()

dinvg(0.3, alpha, beta)
dinvgamma(0.3, alpha, beta)

# This prior implies for the variance (replace x by sqrt(x) in inverse gamma equation)
dvar <- function(x, alpha, beta){
  (beta^alpha)/gamma(alpha)*x^(-0.5*alpha-0.5)*exp(-beta*x^(-0.5))  
}

dvar1 <- function(x, alpha, beta){  # same as dvar 
  (beta^alpha)/gamma(alpha)*(1/sqrt(x))^(alpha+1)*exp(-beta/sqrt(x))
}

# The implied distribution of the variance is not an inverse gamma distribution (?)
# However, different inverse gamma distributions can be more or less similar

x <- seq(0, 1, by=0.001)
plot(x, dvar1(x, alpha, beta), type="l") # implied distribution on variance
points(x, dinvg(x, 3, 0.3), type="l", col="red") # red distribution: prior on variance used in Stan code
points(x, dinvg(x, 3, 0.0006), type="l", col="blue") # blue distribution: "new" prior on variance (see Myrthe's e-mail) that produces more similar results to brms

# We can also do this the other way around and start with the prior on variance
alpha = 3
beta = 0.3

dinvg <- function(x, alpha, beta){
  (beta^alpha)/gamma(alpha)*x^(-alpha-1)*exp(-beta*x^(-1))
} # this is the same as before, but now the inverse gamma prior is on the variance, so x is the variance here

# This inverse-gamma prior on variance implies the following prior on standard deviation

dsd <- function(x, alpha, beta){
  (beta^alpha)/gamma(alpha)*x^(-2*alpha-2)*exp(-beta*x^(-2))
}

# Again, the implied distribution on the standard deviation is not an inverse gamma,
# but inverse gamma distributions can be more or less similar to the implied prior

plot(x, dsd(x, alpha, beta), type="l") # implied distribution on sd
points(x, dinvg(x, 1.745, 0.02), type="l", col="red") # red distribution: prior on sd used in brms code

# Another sanity check: Most likely value under sd prior should be approximately sqrt(most likely value under variance prior), and vice versa
alpha = 3
beta = 0.3
plot(x, dinvg(x, alpha, beta), type="l") # prior on variance
points(x, dsd(x, alpha, beta), type="l", col="turquoise") # implied prior on standard deviation
abline(v=0.075) # this is just eye-balled from the plot, didn't want to calculate the mode
abline(v=sqrt(0.075), col="turquoise")

alpha = 3
beta = 1 # yes I know these are not the values you used, but with your values the mode is basically 0, so it's super hard to see
plot(x, dinvg(x, alpha, beta), type="l") # prior on sd
points(x, dvar(x, alpha, beta), type="l", col="turquoise") # implied prior on variance
abline(v=0.25)
abline(v=0.25^2, col="turquoise")

# If all this is true: How to find the corresponding prior using optimization
# Say, I start with a prior on variance (this is what Julia prefers, right?) - Yes 

priorvariance <- function(x) dinvg(x, 3, 0.5)
impliedpriorsd <- function(x) dsd(x, 3, 0.5)
impliedpriorsd_normalized <- function(x) impliedpriorsd(x)/integrate(impliedpriorsd, lower=0, upper=Inf)$value # The implied prior on standard deviation didn't integrate to 1, so I divided by the area to normalize it

# but seems to apply a prior on standard deviation now because you are changing the values(alpha and beta) for the prior on the variance 
# Oh no I see, so you have the prior on the variance, that you use in the transformed prior for the standard deviation 
# That result you want to match, but want to have a prior on the standard deviation, so which alpha and beta do you need to match the tranformed prior 

minfun <- function(par){
  x <- seq(0.0001, 1, by=0.0001)
  sum((dinvg(x, par[1], par[2])-impliedpriorsd_normalized(x))^2) # minimize sum of squares
}
opt <- optim(par=c(1, 0.5), fn=minfun, method=)  # initial values to optimize over using function minfun (so minimize function minfun)
opt1 <- optim(par=c(20, 0.5), fn=minfun, method=)  # get same result as above 


x <- seq(0, 1, by=0.01)
plot(x, impliedpriorsd_normalized(x), type="l")
points(x, dinvg(x, opt$par[1], opt$par[2]), type="l", col="red")


# Say, I start with a prior on standard deviation  

priorvariance <- function(x) dinvg(x, 1.4, 0.05)
impliedpriorvar <- function(x) dvar(x, 1.4, 0.05)
impliedpriorvar_normalized <- function(x) impliedpriorvar(x)/integrate(impliedpriorvar, lower=0, upper=Inf)$value # The implied prior on standard deviation didn't integrate to 1, so I divided by the area to normalize it

# but seems to apply a prior on standard deviation now because you are changing the values(alpha and beta) for the prior on the variance 
# Oh no I see, so you have the prior on the variance, that you use in the transformed prior for the standard deviation 
# That result you want to match, but want to have a prior on the standard deviation, so which alpha and beta do you need to match the tranformed prior 

minfun2 <- function(par){
  x <- seq(0.0001, 1, by=0.0001)
  sum((dinvg(x, par[1], par[2])-impliedpriorvar_normalized(x))^2) # minimize sum of squares
}
opt2 <- optim(par=c(1, 0.5), fn=minfun2, method=)  # initial values to optimize over using function minfun (so minimize function minfun)
opt3 <- optim(par=c(2, 0.5), fn=minfun2, method=)  # get same result as above 
# 1.236052e-01 2.929477e-05
# 0.2467180855 0.0001129743  prior on sd was (1.745; 0.02)

# 0.1135208765 0.0006024468  prior on sd was (1.4; 0.05)


x <- seq(0, 1, by=0.01)
plot(x, impliedpriorvar_normalized(x), type="l")
points(x, dinvg(x, opt$par[1], opt$par[2]), type="l", col="red")

#' Function to transform priors on variance to priors on standard deviation and vice-versa
#' @param shape Shape parameter (alpha) of original inverse-gamma distribution
#' @param scale Scale parameter (beta) of original inverse-gamma distribution
#' @param direction Either "VarToSD" or "SDtoVar" depending on what the original prior is
#' @importFrom LaplacesDemon dnivgamma
#' @importFrom stats

priortransform <- function(shape, scale, direction){
  
  stopifnot("direction must be 'VarToSD' or 'SDtoVar'" = direction %in% c("VarToSD", "SDtoVar"))
  
  dvar <- function(x, alpha, beta){
    (beta^alpha)/gamma(alpha)*x^(-0.5*alpha-0.5)*exp(-beta*x^(-0.5))
  }
  
  dsd <- function(x, alpha, beta){
    (beta^alpha)/gamma(alpha)*x^(-2*alpha-2)*exp(-beta*x^(-2))
  }
  
  if(direction=="VarToSD"){
    impliedprior <- function(x) dsd(x, alpha=shape, beta=scale)
    meantrans <- sqrt(scale/(shape-1))
  } else if (direction=="SDtoVar") {
    impliedprior <- function(x) dvar(x, alpha=shape, beta=scale)
    meantrans <- (scale/(shape-1))^2
  }
  
  impliedprior_normalized <- function(x) impliedprior(x)/integrate(impliedprior, lower=0, upper=Inf)$value
  
  minfun <- function(par){
    x <- seq(0.0001, meantrans*3, length.out = 10000)
    s <- sum((dinvgamma(x, par[1], par[2])-impliedprior_normalized(x))^2) # minimize sum of squares
    if(is.nan(s)) return(100000)
    return(s)
  }
  
  opt <- stats::optim(par=c(2, meantrans), fn=minfun, lower=c(1e-10, 1e-10), method="L-BFGS-B")
  
  return(list(shape_original = shape,
              scale_original = scale,
              shape_new = opt$par[1],
              scale_new = opt$par[2]))
  
}
priortransform(3, 0.3, "VarToSD")
priortransform(3, 0.5, "VarToSD")
priortransform(3, 0.7, "VarToSD")
priortransform(1.745, 0.02, "SDtoVar")
priortransform(1.4, 0.05, "SDtoVar")

# For lognormal 
# N(0, 0.005) IG(3, 0.01)
priortransform(3, 0.01, "VarToSD")  # 13.75208 0.7489703

###############################################
# student's t on standard deviation 
# https://distribution-explorer.github.io/continuous/halfstudent_t.html

v_df <- 3  # degrees of freedom 
mu <- 0 
scal <- 10 

# we transform x (also called t) 
studt <- function(x, v_df, mu, scal){
  (gamma((v_df+1)/2)/(gamma(v_df/2)*(sqrt(pi*v_df))*scal))*((1+(1/v_df)*((x - mu)/scal)^2)^(-((v_df+1)/2)))
}

# Sanity check: This gives the same results as dstudent_t from brms 
studt(0.3, v_df, mu, scal)
dstudent_t(0.3, v_df, mu, scal)

# This prior implies for the var (replace x by x^2 in studt distribution), so from sd to var 
vstudt <- function(x, v_df, mu, scal){
  (gamma((v_df+1)/2)/(gamma(v_df/2)*(sqrt(pi*v_df))*scal))*((1+(1/v_df)*(((x^2) - mu)/scal)^2)^(-((v_df+1)/2)))
}

# plot 
x <- seq(0, 1, by=0.001)
plot(x, studt(x, v_df, mu, scal), type="l") # implied distribution on sd 
points(x, vstudt(x, v_df, mu, scal), type="l", col="red") # red distribution: prior on variance 

# This prior implies for the sd (replace x by sqrt(x) in studt distribution), so from var to sd 
dstudt <- function(x, v_df, mu, scal){
  (gamma((v_df+1)/2)/(gamma(v_df/2)*(sqrt(pi*v_df))*scal))*((1+(1/v_df)*(((sqrt(x)) - mu)/scal)^2)^(-((v_df+1)/2)))
}
# gives NaNs 
points(x, dstudt(x, v_df, mu, scal), type="l", col="blue") 


# Function
#' Function to transform priors on variance to priors on standard deviation and vice-versa
#' @param shape Shape parameter (alpha) of original inverse-gamma distribution
#' @param scale Scale parameter (beta) of original inverse-gamma distribution
#' @param direction Either "VarToSD" or "SDtoVar" depending on what the original prior is
#' @importFrom LaplacesDemon dnivgamma
#' @importFrom stats

priortransform_st <- function(v_df, mu, scal, direction){
  
  stopifnot("direction must be 'VarToSD' or 'SDtoVar'" = direction %in% c("VarToSD", "SDtoVar"))
  
  vstudt <- function(x, v_df, mu, scal){
    (gamma((v_df+1)/2)/(gamma(v_df/2)*(sqrt(pi*v_df))*scal))*((1+(1/v_df)*(((x^2) - mu)/scal)^2)^(-((v_df+1)/2)))
  }
  
  dstudt <- function(x, v_df, mu, scal){
    (gamma((v_df+1)/2)/(gamma(v_df/2)*(sqrt(pi*v_df))*scal))*((1+(1/v_df)*(((sqrt(x)) - mu)/scal)^2)^(-((v_df+1)/2)))
  }
  
  if(direction=="VarToSD"){
    impliedprior <- function(x) dstudt(x, v_df = v_df, mu = mu, scal = scal)
    # meantrans <- sqrt(scale/(shape-1))
  } else if (direction=="SDtoVar") {
    impliedprior <- function(x) vstudt(x, v_df = v_df, mu = mu, scal = scal)
    # meantrans <- (scale/(shape-1))^2
  }
  
  impliedprior_normalized <- function(x) impliedprior(x)/integrate(impliedprior, lower=0, upper=Inf)$value
  
  minfun <- function(par){
    x <- seq(0.0001, length.out = 10000)
    s <- sum((studt(x, par[1], par[2], par[3])-impliedprior_normalized(x))^2) # minimize sum of squares
    if(is.nan(s)) return(100000)
    return(s)
  }
  
  opt <- stats::optim(par=c(1,0,0.05), fn=minfun, lower=c(1e-10, -1000,1e-10), method="L-BFGS-B")
  
  return(list(df_original = v_df,
              mu_original = mu, 
              scale_original = scal,
              df_new = opt$par[1],
              mu_new = opt$par[2],
              scale_new = opt$par[3]))
  
}

priortransform_st(3, 0, 10, "SDtoVar")

# only find scale 

priortransform_st2 <- function(v_df, mu, scal, direction){
  
  stopifnot("direction must be 'VarToSD' or 'SDtoVar'" = direction %in% c("VarToSD", "SDtoVar"))
  
  vstudt <- function(x, v_df, mu, scal){
    (gamma((v_df+1)/2)/(gamma(v_df/2)*(sqrt(pi*v_df))*scal))*((1+(1/v_df)*(((x^2) - mu)/scal)^2)^(-((v_df+1)/2)))
  }
  
  dstudt <- function(x, v_df, mu, scal){
    (gamma((v_df+1)/2)/(gamma(v_df/2)*(sqrt(pi*v_df))*scal))*((1+(1/v_df)*(((sqrt(x)) - mu)/scal)^2)^(-((v_df+1)/2)))
  }
  
  if(direction=="VarToSD"){
    impliedprior <- function(x) dstudt(x, v_df = v_df, mu = mu, scal = scal)
    # meantrans <- sqrt(scale/(shape-1))
  } else if (direction=="SDtoVar") {
    impliedprior <- function(x) vstudt(x, v_df = v_df, mu = mu, scal = scal)
    # meantrans <- (scale/(shape-1))^2
  }
  
  impliedprior_normalized <- function(x) impliedprior(x)/integrate(impliedprior, lower=0, upper=Inf)$value
  
  minfun <- function(par){
    x <- seq(0.0001, length.out = 10000)
    s <- sum((studt(x, v_df, mu, par[1])-impliedprior_normalized(x))^2) # minimize sum of squares
    if(is.nan(s)) return(100000)
    return(s)
  }
  
  opt <- stats::optim(par=0.05, fn=minfun, lower=1e-10)
  
  return(list(df_original = v_df,
              mu_original = mu, 
              scale_original = scal,
              scale_new = opt$par[1]))
  
}

priortransform_st2(3, 0, 10, "SDtoVar")  # 1.481403 

# Priors for log-normal 
# invergamma log prior: https://pints.readthedocs.io/en/stable/log_priors.html#pints.InverseGammaLogPrior



