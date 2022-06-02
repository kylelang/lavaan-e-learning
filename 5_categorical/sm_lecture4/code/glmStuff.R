### Title:    GLM Stuff
### Author:   Kyle M. Lang
### Created:  2020-04-18
### Modified: 2020-04-18

dataDir <- "../data/"

library(ggplot2)
library(MLmetrics)
source("../../../code/supportFunctions.R")

### Exam Passing Example from Slides ###

beta <- c(-5.5, 1)
x   <- runif(100, 0, 12)
eta <- beta[1] + beta[2] * x
pi  <- exp(eta) / (1 + exp(eta))
y   <- rbinom(100, 1, pi)

dat1 <- data.frame(x, y)

p1 <- gg0(x = dat1$x, y = dat1$y) +
    xlab("Hours of Study") +
    ylab("Probability of Passing")
p1

tmp  <- with(dat1, seq(min(x), max(x), length.out = 500))
eta2 <- beta[1] + beta[2] * tmp
pi2  <- exp(eta2) / (1 + exp(eta2))
dat2 <- data.frame(x = tmp, y = pi2)

p1 + geom_line(aes(x = x, y = y), data = dat2, colour = "blue", size = 1)

fit <- glm(y ~ x, family = binomial())

summary(fit)
exp(coef(fit))

ip <- -coef(fit)[1] / coef(fit)[2]

### Classification Stuff ###

wvs <- readRDS(paste0(dataDir, "wvs_data.rds"))

## Recode missing data:
wvs[wvs < 0] <- NA

colnames(wvs)


## Identify "good" nominal variables:
catNums <- c(24:35, 60:66, 81:83, 147:151, 176:180, 187, 229:230, 234:236, 240,
             243:246, 250, 252, 255, 265)
catVars <- intersect(paste0("V", catNums), colnames(wvs))

## Cast nominal variables as factors:

levels[catVars]

table(wvs$V80, useNA = "ifany")
table(wvs$V148, useNA = "ifany")
table(wvs$V149, useNA = "ifany")
table(wvs$V66, useNA = "ifany")

### Classification Error Measures ###

cee <- function(y, pi) -mean(y * log(pi) + (1 - y) * log(1 - pi))
mce <- function(yTrue, yPred) {
    tmp <- table(yTrue, yPred)
    sum(tmp) - sum(diag(tmp))
}

n <- 100

set.seed(235711)
yTrue <- rbinom(n, 1, 0.5)

pi1   <- yTrue * 0.9 + (1 - yTrue) * 0.1
pred1 <- as.numeric(pi1 > 0.5)

pi2   <- yTrue * 0.55 + (1 - yTrue) * 0.45
pred2 <- as.numeric(pi2 > 0.5)

cee1 <- cee(y = yTrue, pi = pi1)
cee2 <- cee(y = yTrue, pi = pi2)

mce(yTrue, pred1)
mce(yTrue, pred2)

cee333 <- c(cee1, cee2)

cee100
cee1000
cee333

tmp

tmp["0", "1"] / sum(tmp)

dim(tmp)

t(tmp)

x <- seq(0, 1000, 0.1)
y <- log(x)

plot(x = x, y = y)

log(1)
log(10)
log(1000)
log(10000)
log(100000)
log(1000000)

exp(2)
log(8)
