### Title:    Mediation & Moderation: Lecture 8 Examples
### Author:   Kyle M. Lang
### Created:  2016-MAR-17
### Modified: 2016-JUN-09

install.packages("rockchalk", repos = "http://rweb.quant.ku.edu/cran")

library(probemod)
library(rockchalk)

dataDir <- "../data/"
fileName <- "nlsyData.rds"
dat1 <- readRDS(paste0(dataDir, fileName))

out1 <- lm(depress1 ~ ratio1*perception1, data = dat1)
summary(out1)

## Compute critical values of Z:
zMean <- mean(dat1$perception1)
zSD <- sd(dat1$perception1)
dat1$zCen <- dat1$perception1 - zMean 
dat1$zHigh <- dat1$perception1 - (zMean + zSD)
dat1$zLow <- dat1$perception1 - (zMean - zSD)

## Test simple slopes:
out2.1 <- lm(depress1 ~ ratio1*zLow, data = dat1)
summary(out2.1)

out2.2 <- lm(depress1 ~ ratio1*zCen, data = dat1)
summary(out2.2)

out2.3 <- lm(depress1 ~ ratio1*zHigh, data = dat1)
summary(out2.3)

## Implement J-N Technique via "rockchalk"
plotOut <- plotSlopes(model = out1,
                      plotx = "ratio1",
                      modx = "perception1")

testOut <- testSlopes(plotOut)

testOut$jn$roots

plot(testOut)

## Centering demonstration:
n <- 10000
x <- rnorm(n, 10, 1)
z <- rnorm(n, 20, 2)
xz <- x*z

plot(density(x))

cor(x, xz)

xc <- x - mean(x)
zc <- z - mean(z)
xzc <- xc*zc

cor(xc, xzc)

plot(x = x, y = xz)
plot(x = xc, y = xzc)

y <- 5*x + 5*z + 2*xz + rnorm(n, 0, 0.5)

summary(lm(y ~ x*z))$coef
summary(lm(y ~ xc*z))$coef 
summary(lm(y ~ xc*zc))$coef

zLow <- z - (mean(z) - sd(z))
zCen <- z - mean(z)
zHigh <- z - (mean(z) + sd(z))

summary(lm(y ~ x*zLow))$coef
summary(lm(y ~ xc*zLow))$coef

summary(lm(y ~ x*zCen))$coef
summary(lm(y ~ xc*zCen))$coef

summary(lm(y ~ x*zHigh))$coef
summary(lm(y ~ xc*zHigh))$coef

out3.1 <- lm(y ~ x*z)
out3.2 <- lm(y ~ xc*zc)

sum(out3.1$fitted - out3.2$fitted)

summary(out3.1)$r.squared
summary(out3.2)$r.squared
