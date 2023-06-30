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
