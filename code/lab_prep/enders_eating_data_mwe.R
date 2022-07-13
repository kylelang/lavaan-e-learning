### Title:    MWE for Enders (2010) Eating Attitudes Data NPD Theta Matrix
### Author:   Kyle M. Lang
### Created:  2022-07-02
### Modified: 2022-07-13

load("enders_eating_data.RData")

library(lavaan)
library(semTools)
library(dplyr)

## Define the measurement model syntax:
                                        #cfaMod <- '
                                        #drive =~ eat1 + eat2 + eat10 + eat11 + eat12 + eat14 + eat24
                                        #obsess =~ eat3 + eat18 + eat21
                                        #'

## Naive fit:
                                        #fit0 <- cfa(cfaMod, data = eat, std.lv = TRUE, missing = "fiml")

fitMeasures(fit0)
summary(fit0)

### Everything looks good with the naive fit.

## Fit with auxiliaries:
                                        #fit1 <- cfa.auxiliary(cfaMod,
                                        #                      data   = eat,
                                        #                      aux    = c("bmi", "anx", "wsb"),
                                        #                      std.lv = TRUE)

### When I add auxiliaries, I get a NPD residual covariance matrix:

## Warning message:
## In lav_object_post_check(object) :
##   lavaan WARNING: the covariance matrix of the residuals of the observed
##                 variables (theta) is not positive definite;
##                 use lavInspect(fit, "theta") to investigate.

fitMeasures(fit1)
summary(fit1)

### The estimates look pretty much identical to what we see in Enders (2010)

## Check the residual covariance matrix
theta <- lavInspect(fit1, "theta")

theta        # Looks fine
eigen(theta) # One (large) negative eigenvalue

## Are the covariances legal?
                                        #resVar <- diag(theta)
                                        #resCov <- theta["bmi", ] %>% head(10)

(abs(resCov) <= sqrt(head(resVar, 10) * resVar["bmi"])) %>% all()

### All residual variances and covariances are legal.

### The model runs fine and gives the same estimates reported in Enders (2010).
### The negative eigenvalue is too large to be due simply to numerical fuzz, and
### it doesn't seem to be caused by any illegal estimates.

inspect(fit1, "sigma") %>% eigen() # The model-implied covariance matrix is fine

solve(theta) # We can invert theta
chol(theta)  # But not factorize

## What happens when we fit the same model to the complete cases?
                                        #auxMod <- lavExport(fit1, export = FALSE)
                                        #fit2   <- cfa(auxMod, data = eat, std.lv = TRUE)

summary(fit2)
fitMeasures(fit2)

inspect(fit2, "theta") %>% eigen()

### Same issue

### I also get the same NPD residual covariance matrix if I run the model in
### Mplus using the code provided in the supplement for Enders (2010). So, it
### looks like this is a fundamental issue with the model, not a lavaan problem.

### WTF is going on!?
