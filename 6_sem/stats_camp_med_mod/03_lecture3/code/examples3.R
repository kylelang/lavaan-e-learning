### Title:    Mediation & Moderation: Lecture 3 Examples
### Author:   Kyle M. Lang
### Created:  2016-FEB-06
### Modified: 2016-JUN-09

library(mvtnorm)
library(lavaan)

dataDir <- "../data/"
fileName <- "adamsKlpsScaleScore.rds"

dat1 <- readRDS(paste0(dataDir, fileName))

## Partial out the mediator's effect:
mod1 <- lm(policy ~ sysRac + polAffil, data = dat1)
mod2 <- lm(sysRac ~ polAffil, data = dat1)

summary(mod1)$coef
summary(mod2)$coef

## Extract important parameter estimates:
a <- coef(mod2)["polAffil"]
b <- coef(mod1)["sysRac"]

## Compute indirect effects:
ieProd <- a * b

## Calculate Sobel's Z:
seA <- sqrt(diag(vcov(mod2)))["polAffil"]
seB <- sqrt(diag(vcov(mod1)))["sysRac"]

sobelZ <- ieProd / sqrt(b^2 * seA^2 + a^2 * seB^2)
sobelZ

sobelP <- 2 * pnorm(sobelZ, lower = FALSE)
sobelP


### Path Modeling ###


## Specify basic path model:
mod1 <- "
policy ~ polAffil + sysRac
sysRac ~ polAffil
"

## Fit the model:
out1 <- sem(mod1, dat = dat1)
summary(out1)

## Add the indirect effect:
mod2 <- "
policy ~ polAffil + b*sysRac
sysRac ~ a*polAffil

ab := a*b # Define the indirect effect
"

## Fit the model:
out2 <- sem(mod2, data = dat1)
summary(out2)
parameterEstimates(out2)

### Bootstrapping Intro ###

## Generate the raw data:
set.seed(235711)

tmp <- sample(c(200 : 365),
              size = 40,
              replace = TRUE)
rawData <- cbind.data.frame(tmp, 365 - tmp)
colnames(rawData) <- c("goodDays", "badDays")

saveRDS(rawData, file = paste0(dataDir, "daysData.rds"))

## Compute the observed DRI:
obsStat <- median(rawData$goodDays / rawData$badDays)

## Resample the raw data:
set.seed(235711)
nSams <- 5000
bootStat <- rep(NA, nSams)
for(b in 1 : nSams) {
    bootSam <- rawData[sample(c(1 : nrow(rawData)),
                              replace = TRUE), ]
    bootDRI[b] <- median(bootSam$goodDays / bootSam$badDays)
}

## Plot the empirical sampling distribution:
hist(bootStat, breaks = 20, freq = FALSE)
lines(density(bootStat, adjust = 3), col = "red")

## Compute the CI:
sort(bootStat)[0.05 * nSams]
sort(bootStat)[0.95 * nSams]


### Bootstrapped Path Modeling ###

nSams <- 1000
abVec <- rep(NA, nSams)
for(i in 1 : nSams) {
    ## Resample the data:
    bootSam <- dat1[sample(c(1 : nrow(dat1)), replace = TRUE), ]
    ## Fit the path model:
    bootOut <- sem(mod2, data = bootSam)
    ## Store the estimated indirect effect:
    abVec[i] <- prod(coef(bootOut)[c("a", "b")])
}

## Plot the empirical sampling distribution of a*b:
hist(abVec,
     freq = FALSE,
     main = "Empirical Sampling Distribution of a*b",
     xlab = "a*b")
lines(density(abVec), col = "red")

## Calculate the percentile CI:
lb <- sort(abVec)[0.025 * nSams]
ub <- sort(abVec)[0.975 * nSams]
c(lb, ub)

## OR, much more parsimoniously:
bootOut2 <- sem(mod2, data = dat1, se = "boot", bootstrap = 1000)

parameterEstimates(bootOut2)


### Monte Carlo Simulation Method ###

mod3 <- "
policy ~ polAffil + b*sysRac
sysRac ~ a*polAffil
"

## Fit the model:
out4 <- sem(mod3, data = dat1)

## Extract the important estimates:
a <- coef(out4)["a"]
b <- coef(out4)["b"]
acov <- vcov(out4)[c("a", "b"), c("a", "b")]

## Draw the Monte Carlo samples:
samMat <- rmvnorm(nSams, c(a, b), acov)
abVec <- samMat[ , "a"] * samMat[ , "b"]

## Plot the empirical sampling distribution of a*b:
hist(abVec,
     freq = FALSE,
     main = "Parametric Bootsrap Distribution of a*b",
     xlab = "a*b")
lines(density(abVec), col = "red")

## Calculate the percentile CI:
lb <- sort(abVec)[0.025 * nSams]
ub <- sort(abVec)[0.975 * nSams]
c(lb, ub)
