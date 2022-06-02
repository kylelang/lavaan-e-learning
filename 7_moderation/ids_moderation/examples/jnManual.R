### Title:    Example of Manually Finding Johnson-Neyman Roots
### Author:   Kyle M. Lang
### Created:  2018-SEP-07
### Modified: 2018-SEP-07

rm(list = ls(all = TRUE))

dataDir <- "../data/"

source("../../../code/supportFunctions.R")

##### Example models

## Read data:
dDat <- readRDS("../data/diabetes.rds")

## Estimate model:
out <- lm(bp ~ bmi*ldl, data = dDat)

## Get (squared) critical value of t:
t2 <- qt(0.975, df = out$df.residual)^2

## Extract pertinent elements from the asymptotic covariance matrix:
aCov <- vcov(out)
v1   <- diag(aCov)["bmi"]
v3   <- diag(aCov)["bmi:ldl"]
cv   <- aCov["bmi", "bmi:ldl"]

## Extract pertinent slope coefficients:
b1 <- coef(out)["bmi"]
b3 <- coef(out)["bmi:ldl"]

## Compute coefficients of the quadratic equation:
a <- t2 * v3 - b3^2
b <- 2 * t2 * cv - 2 * b1 * b3
c <- t2 * v1 - b1^2

## Compute roots:
myRoots <- c(
(-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
(-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
)

## Do the calculations with the "rockchalk" package:
plotOut <- plotSlopes(model      = out,
                      plotx      = "bmi",
                      modx       = "ldl",
                      plotPoints = FALSE)

jnOut <- testSlopes(plotOut)

rcRoots <- jnOut$jn$roots

## Compare:
rcRoots
myRoots
