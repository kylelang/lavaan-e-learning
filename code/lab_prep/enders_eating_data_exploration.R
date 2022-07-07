### Title:    Exploration of Enders (2010) Eating Attitudes Data
### Author:   Kyle M. Lang
### Created:  2022-07-02
### Modified: 2022-07-04

rm(list = ls(all = TRUE))

library(dplyr)
library(lavaan)
library(semTools)
library(mice)

dataDir <- "../../data/"

## Original, incomplete data:
eat0 <- readRDS(paste0(dataDir, "eating_attitudes.rds"))

## Data completed via single imputation:
eat1 <- readRDS(paste0(dataDir, "eating_attitudes_completed.rds"))

## mids object containing 500 multiple imputations:
eatMids <- readRDS(paste0(dataDir, "eating_attitudes_mids.rds"))

## Define the measurement model syntax:
cfaMod <- '
drive =~ eat1 + eat2 + eat10 + eat11 + eat12 + eat14 + eat24
obsess =~ eat3 + eat18 + eat21
'

###-FIML---------------------------------------------------------------------###

## Naive FIML fit:
fit0 <- cfa(cfaMod, data = eat0, std.lv = TRUE, missing = "fiml")

fitMeasures(fit0)
summary(fit0)

### Everything looks good with the naive fit.

## Fit with auxiliaries:
fit1 <- cfa.auxiliary(cfaMod, data = eat0, aux = "bmi", std.lv = TRUE)

### When I add auxiliaries, I get a NPD residual covariance matrix.

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
resVar <- diag(theta)
resCov <- theta["bmi", ] %>% head(10)

(abs(resCov) <= sqrt(head(resVar, 10) * resVar["bmi"])) %>% all()

### All residual variances and covariances are legal.

### The model runs fine and gives the same estimates reported in Enders (2010).
### The negative eigenvalue is too large to be due simply to numerical fuzz, and
### it doesn seem to be caused by any illegal estimates.

inspect(fit1, "sigma") %>% eigen() # The model-implied covariance matrix is fine

solve(theta) # We can invert theta
chol(theta)  # But not factorize

## What happens when we fit the same model to the complete cases?
auxMod <- lavExport(fit1, export = FALSE)
fit2   <- cfa(auxMod, data = eat0, std.lv = TRUE)

summary(fit2)
fitMeasures(fit2)

inspect(fit2, "theta") %>% eigen()

### Same issue...

### WTF is going on!?

save.image(file = "enders_eating_data_mwe.RData")

###-Singly Imputed Data------------------------------------------------------###

fit0 <- cfa(cfaMod, data = eat1, std.lv = TRUE)

fitMeasures(fit0)
summary(fit0)

###-Multiply Imputed Data----------------------------------------------------###

eat2 <- list()
for(m in 1:20)
    eat2[[m]] <- complete(eatMids, m)

fit0 <- cfa.mi(cfaMod, data = eat2, std.lv = TRUE)

fitMeasures(fit0, test = "D3")
summary(fit0, fmi = TRUE)

###-Simple Mediation---------------------------------------------------------###

## Define the structural part of a simple mediation model:
semMod1 <- '
obsess ~ b * drive + cp * wsb
drive  ~ a * wsb

ab := a * b
'

semMod1 <- paste(cfaMod, semMod1, sep = '\n')

## Estimate the model with Naive FIML:
semFit1 <-
    sem(semMod1, data = eat0, std.lv = TRUE, missing = "fiml", fixed.x = FALSE)
summary(semFit1)

## Estimate the model on the singly imputed data:
semFit1 <- sem(semMod1, data = eat1, std.lv = TRUE)
summary(semFit1)

###-Serial Multiple Mediation------------------------------------------------###

## Define the structural part of a simple mediation model:
semMod2 <- '
obsess ~ b1 * drive + b2 * anx + cp * wsb
anx    ~ d * drive + a2 * wsb
drive  ~ a1 * wsb

ab1 := a1 * b1
ab2 := a2 * a2
adb := a1 * d * b2
ie  := ab1 + ab2 + adb
'

semMod2 <- paste(cfaMod, semMod2, sep = '\n')

## Estimate the model with Naive FIML:
semFit2 <-
    sem(semMod2, data = eat0, std.lv = TRUE, missing = "fiml", fixed.x = FALSE)
summary(semFit2)

## Estimate the model on the singly imputed data:
semFit2 <- sem(semMod2, data = eat1, std.lv = TRUE)
summary(semFit2)
