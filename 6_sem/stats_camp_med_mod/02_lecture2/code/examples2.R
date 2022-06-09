### Title:    Mediation & Moderation: Lecture 2 Examples
### Author:   Kyle M. Lang
### Created:  2016-FEB-06
### Modified: 2016-JUN-09

library(mvtnorm)

## Simulate some data:
sigma <- matrix(0.3, 3, 3)
diag(sigma) <- 1.0
dat1 <- data.frame(rmvnorm(500, c(0, 0, 0), sigma))
colnames(dat1) <- c("x", "y", "z")

## Fit model 1:
fit1 <- lm(y ~ x, data = dat1)

## Fit model 2:
fit2 <- lm(y ~ x + z, data = dat1)

## Look at the results:
summary(fit1)
summary(fit2)

## Get the coefficients:
coef(fit1)
coef(fit2)

## Get the standard errors:
sqrt(diag(vcov(fit1)))
sqrt(diag(vcov(fit2)))

## Read in data:
dataDir <- "../data/"
fileName <- "adamsKlpsScaleScore.rds"

dat1 <- readRDS(paste0(dataDir, fileName))

## Check pre-conditions:
mod1 <- lm(policy ~ polAffil, data = dat1)
mod2 <- lm(policy ~ sysRac, data = dat1)
mod3 <- lm(sysRac ~ polAffil, data = dat1)

## Partial out the mediator's effect:
mod4 <- lm(policy ~ sysRac + polAffil, data = dat1)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

## Extract important parameter estimates:
a <- coef(mod3)["polAffil"]
b <- coef(mod4)["sysRac"]
c <- coef(mod1)["polAffil"]
cPrime <- coef(mod4)["polAffil"]

## Compute indirect effects:
ieDiff <- c - cPrime
ieProd <- a * b

## Calculate Sobel's Z:
seA <- sqrt(diag(vcov(mod3)))["polAffil"]
seB <- sqrt(diag(vcov(mod4)))["sysRac"]

sobelZ <- ieProd / sqrt(b^2 * seA^2 + a^2 * seB^2)
sobelZ

sobelP <- 2 * pnorm(sobelZ, lower = FALSE)
sobelP

### Test a different model:
mod1 <- lm(policy ~ revDisc, data = dat1)
mod2 <- lm(sysRac ~ revDisc, data = dat1)
mod3 <- lm(policy ~ sysRac, data = dat1)

mod4 <- lm(policy ~ revDisc + sysRac, data = dat1)

a <- coef(mod2)["revDisc"]
b <- coef(mod4)["sysRac"]
seA <- sqrt(diag(vcov(mod2)))["revDisc"]
seB <- sqrt(diag(vcov(mod4)))["sysRac"]

sobelSE <- sqrt(a^2 * seB^2 + b^2 * seA^2)

sobelZ <- (a * b) / sobelSE
sobelZ

sobelP <- 2 * pnorm(sobelZ, lower = TRUE)
sobelP

### Practice problem:
sigma <- matrix(0.5, 3, 3)
diag(sigma) <- 1.0

dat1 <- data.frame(rmvnorm(500, c(0, 0, 0), sigma))
colnames(dat1) <- c("x", "y", "m")

mod1 <- lm(y ~ x, data = dat1)
mod2 <- lm(y ~ m, data = dat1)
mod3 <- lm(m ~ x, data = dat1)

mod4 <- lm(y ~ m + x, data = dat1)

round(summary(mod1)$coef, 3)
round(summary(mod3)$coef, 3)
round(summary(mod4)$coef, 3)

