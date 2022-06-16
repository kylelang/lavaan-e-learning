### Title:    Maximum Likelihood Demonstration Functions
### Author:   Kyle M. Lang
### Created:  2022-06-16
### Modified: 2022-06-16

###--------------------------------------------------------------------------###

## Convert between covariance matrix and vectorized Cholesky factor:
vecChol <- function(x, p, revert = FALSE) {
    if(revert) {
        tmp                  <- matrix(0, p, p)
        tmp[!lower.tri(tmp)] <- x
        crossprod(tmp)
    }
    else
        chol(x)[!lower.tri(x)]
}

###--------------------------------------------------------------------------###

## Extract parameter matrices from vectorized storage:
getParams <- function(par, p) {
    ## Extract the mean vector:
    mu <- par[1:p]
    
    ## Populate sigma from its Cholesky factor:
    sigma <- vecChol(tail(par, -p), p = p, revert = TRUE)
    
    list(mu = mu, sigma = sigma)
}

###--------------------------------------------------------------------------###

## Complete data loglikelihood function:
ll <- function(par, data) {
    ## Extract the parameter matrices:
    par <- getParams(par, p = ncol(data))
    
    ## Compute the row-wise contributions to the LL:
    ll0 <- dmvnorm(data, mean = par$mu, sigma = par$sigma, log = TRUE)
    
    sum(ll0) # Return the overall LL value
}

###--------------------------------------------------------------------------###

## Within-pattern contributions to the LL:
ll0 <- function(i, mu, sigma, pats, ind, data) {
    ## Define the current response pattern:
    p1 <- pats[i, ]
    
    if(sum(p1) > 1) # More than one observed variable?
        dmvnorm(x     = data[ind == i, p1],
                mean  = mu[p1],
                sigma = sigma[p1, p1],
                log   = TRUE)
    else
        dnorm(x    = data[ind == i, p1],
              mean = mu[p1],
              sd   = sqrt(sigma[p1, p1]),
              log  = TRUE)
}

###--------------------------------------------------------------------------###

## FIML loglikelihood function:
llm <- function(par, data, pats, ind) {
    ## Extract the parameter matrices:
    par <- getParams(par, p = ncol(data))
    
    ## Compute the pattern-wise contributions to the LL:
    ll1 <- sapply(X     = 1:nrow(pats),
                  FUN   = ll0,
                  mu    = par$mu,
                  sigma = par$sigma,
                  pats  = pats,
                  ind   = ind,
                  data  = data)

    sum(unlist(ll1))
}

###--------------------------------------------------------------------------###
