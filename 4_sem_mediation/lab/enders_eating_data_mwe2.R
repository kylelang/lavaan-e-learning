### Title:    MWE for Enders (2010) Eating Attitudes Data NPD Theta Matrix
### Author:   Kyle M. Lang
### Created:  2022-07-02
### Modified: 2022-07-02

rm(list = ls(all = TRUE))

load("enders_eating_data_npd_theta.RData")

## Define the measurement model syntax:
cfaMod <- '
drive =~ eat1 + eat2 + eat10 + eat11 + eat12 + eat14 + eat24
obsess =~ eat3 + eat18 + eat21
'

## Naive fit:
fit0 <- cfa(cfaMod, data = eat, std.lv = TRUE, missing = "fiml")

fitMeasures(fit0)
summary(fit0)

### Everything looks good with the naive fit.

## Fit with auxiliaries:
fit1 <- cfa.auxiliary(cfaMod,
                      data   = eat,
                      aux    = c("bmi", "anx", "wsb"),
                      std.lv = TRUE)

### When I add auxiliaries, I get a NPD residual covariance matrix.

fitMeasures(fit1)
summary(fit1)

### The estimates look pretty much identical to what we see in Enders (2010)

## Check the residual covariance matrix
theta <- lavInspect(fit1, "theta")

theta        # Looks fine
eigen(theta) # One (large) negative eigenvalue

## Are the covariances legal?
v   <- diag(theta)
tmp <- theta["bmi", ] %>% head(10)

(abs(tmp) <= sqrt(head(v, 10) * v["bmi"])) %>% all()

### All residual variances and covariances are legal.

### The model runs fine and gives the same estimates reported in Enders (2010).
### The negative eigenvalue is too large to be due simply to numerical fuzz, and
### it doesn seem to be caused by any illegal estimates.

inspect(fit1, "sigma") %>% eigen() # The model-implied covariance matrix is fine

solve(theta) # We can invert theta
chol(theta)  # But not factorize

### WTF is going on!?

save.image(file = "enders_eating_data_npd_theta.RData")
