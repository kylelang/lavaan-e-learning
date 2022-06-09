### Title:    Mediation & Moderation: Lecture 5 Examples
### Author:   Kyle M. Lang
### Created:  2016-FEB-06
### Modified: 2016-JUN-09

library(lavaan)
library(mvtnorm)

dataDir <- "../data/"

dat1 <- readRDS(paste0(dataDir, "adamsKlpsData.rds"))

mod1.1 <- "
merit =~ meritP1 + meritP2 + meritP3
policy =~ policyP1 + policyP2 + policyP3
"

out1.1 <- cfa(mod1.1, data = dat1, std.lv = TRUE)
fitMeasures(out1.1)
summary(out1.1)


mod1.2 <- "
merit =~ meritP1 + meritP2 + meritP3
policy =~ policyP1 + policyP2 + policyP3

policy ~ b*merit + polAffil
merit ~ a*polAffil

ab := a*b
"

out1.2 <- sem(mod1.2, data = dat1, std.lv = TRUE, se = "boot", boot = 500)
summary(out1.2)
parameterEstimates(out1.2, boot.ci.type = "bca.simple")


mod2 <- "
policy ~ b*sysRac + cp*polAffil
sysRac ~ a*polAffil

ab := a*b
"

## Estimate the model:
out2 <- sem(mod2, data = dat1)
summary(out2)

## Extract the necessary estimates:
ab <- prod(coef(out2)[c("a", "b")])
cPrime <- coef(out2)["cp"]

sdY <- sd(dat1$policy)
sdX <- sd(dat1$polAffil)

r2MY <- with(dat1, cor(policy, sysRac))
r2XY <- with(dat1, cor(policy, polAffil))
R2Y.MX <- inspect(out2, "r2")["policy"]

### Compute the various effect sizes:

## Paritally Standardized:
abPS <- ab / sdY
cPrimePS <- cPrime / sdY
cPS <- abPS + cPS

## Completely Standardized:
abCS <- (sdX * ab) / sdY
cPrimeCS <- (sdX * cPrime) / sdY
cCS <- abCS + cPrimeCS

## Proportions:
pm <- ab / (cPrime + ab)
rm <- ab / cPrime

## R2:
R2med <- r2MY - (R2Y.MX - r2XY)

## Kappa2:
tmpData <- dat1[ , c("polAffil", "sysRac", "policy")]
colnames(tmpData) <- c("x", "m", "y")

cov1 <- cov(tmpData)

sYM <- cov1["x", "m"]
sYX <- cov1["y", "x"]
sMX <- cov1["m", "x"]
s2X <- cov1["x", "x"]
s2M <- cov1["m", "m"]
s2Y <- cov1["y", "y"]

aMarg <- sqrt(s2M * s2Y - sYM^2) * sqrt(s2X * s2Y - sYX^2)
aInt <- c(
    (sYM * sYX - aMarg) / (s2X * s2Y),
    (sYM * sYX + aMarg) / (s2X * s2Y)
)

bMarg <- sqrt(s2X * s2Y - sYX^2) / sqrt(s2X * s2M - sMX^2)
bInt <- c(-1 * bMarg, bMarg)

aMax <- ifelse(coef(out2)["a"] < 0,
               aInt[1],
               aInt[2])

bMax <- ifelse(coef(out2)["b"] < 0,
               bInt[1],
               bInt[2])

abMax <- aMax * bMax

k2 <- ab / abMax


## Practice:
set.seed(235711)

sigma <- matrix(c(1.5, 0.3, 0.6,
                  0.3, 1.4, 0.45,
                  0.6, 0.45, 1.55),
                3, 3)

rownames(sigma) <- colnames(sigma) <- c("x", "m", "y")

low <- lower.tri(sigma, diag = TRUE)
up <- upper.tri(sigma)

sigma[up] <- ""

a <- sigma["x", "m"] / sigma["x", "x"]
betaYMX <- solve(sigma[c("x", "m"), c("x", "m")]) %*% sigma[c("x", "m"), "y"]
b <- betaYMX["m", ]

ab <- a*b

s2X <- sigma["x", "x"]
s2M <- sigma["m", "m"]
s2Y <- sigma["y", "y"]
sYX <- sigma["y", "x"]
sYM <- sigma["y", "m"]
sMX <- sigma["m", "x"]

aMax <- (sYM * sYX + sqrt(s2M * s2Y - sYM^2) * sqrt(s2X * s2Y - sYX^2)) / (s2X * s2Y)
bMax <- sqrt(s2X * s2Y - sYX^2) / sqrt(s2X * s2M - sMX^2)

abMax <- aMax * bMax

k2 <- ab / abMax
k2

testData <- data.frame(rmvnorm(10000000, c(0, 0, 0), sigma))
colnames(testData) <- c("x", "m", "y")

betaMX
out1 <- lm(m ~ x, data = testData)
out2 <- lm(y ~ m + x, data = testData)


coef(out1)["x"] * coef(out2)["m"]

diag(sigma) <- 1.0

dat1 <- rmvnorm(100, rep(0, 3), sigma)
colnames(dat1) <- c("x", "m", "y")

cov1 <- cov(dat1)

cov1
