### Title:    Mediation & Moderation: Lecture 4 Examples
### Author:   Kyle M. Lang
### Created:  2016-FEB-06
### Modified: 2016-FEB-21

library(lavaan)

nBoot <- 1000
bootType <- "bca.simple"

dataDir <- "../data/"
fileName <- "adamsKlpsScaleScores.rds"
dat1 <- readRDS(paste0(dataDir, fileName))

## Parallel Multiple Mediator Model:
mod1.1 <- "
policy ~ polAffil + b1*merit + b2*sysRac + b3*indRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil
indRac ~ a3*polAffil

merit ~~ sysRac + indRac
sysRac ~~ indRac

ab1 := a1*b1
ab2 := a2*b2
ab3 := a3*b3
totalIE := ab1 + ab2 + ab3
"

out1.1 <-
    sem(mod1.1, data = dat1, se = "boot", boot = nBoot)

summary(out1.1)
parameterEstimates(out1.1, boot.ci.type = bootType)[ , -c(1 : 3)]


## Test differences in specific indirect effects (Method 1):
mod1.2 <- "
policy ~ polAffil + b1*merit + b2*sysRac + b3*indRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil
indRac ~ a3*polAffil

merit ~~ sysRac + indRac
sysRac ~~ indRac

ab1 := a1*b1
ab2 := a2*b2
ab3 := a3*b3
totalIE := ab1 + ab2 + ab3

ab2 == ab3
"

out1.2 <- sem(mod1.2, data = dat1, se = "bootstrap", bootstrap = 1000)
summary(out1.2)

## Conduct a chi-squared difference test:
chiDiff <- fitMeasures(out1.2)["chisq"] - fitMeasures(out1)["chisq"]
dfDiff <- fitMeasures(out1.2)["df"] - fitMeasures(out1)["df"]
pchisq(chiDiff, dfDiff, lower = FALSE)


## Test differences in specific indirect effects (Method 2):
mod1.3 <- "
policy ~ polAffil + b1*merit + b2*sysRac + b3*indRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil
indRac ~ a3*polAffil

merit ~~ sysRac + indRac
sysRac ~~ indRac

ab1 := a1*b1
ab2 := a2*b2
ab3 := a3*b3
totalIE := ab1 + ab2 + ab3

test1 := ab2 - ab3
"

out1.3 <- sem(mod1.3, data = dat1, se = "bootstrap", bootstrap = 1000)

summary(out1.3)
parameterEstimates(out1.3, boot.ci.type = "bca.simple")


## Serial Multiple Mediator Model:
mod2.1 <- "
policy ~ polAffil + b1*merit + b2*sysRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil + d21*merit

ab1 := a1*b1
ab2 := a2*b2
fullIE := a1*d21*b2
totalIE := ab1 + ab2 + fullIE 
"

out2.1 <- sem(mod2.1, data = dat1, se = "bootstrap", bootstrap = 1000)

summary(out2.1)
parameterEstimates(out2.1, boot.ci.type = "bca.simple")
parameterEstimates(out2.1)


## Restricted Serial Multiple Mediator Model:
mod2.2 <- "
policy ~ polAffil + b2*sysRac
merit ~ a1*polAffil
sysRac ~ d21*merit

fullIE := a1*d21*b2
"

out2.2 <- sem(mod2.2, data = dat1, se = "bootstrap", bootstrap = 1000)

summary(out2.2)
parameterEstimates(out2.2, boot.ci.type = "bca.simple")
parameterEstimates(out2.2)


## Test Differences between Indirect Effects
## in Serial Multiple Mediator Model (Method 1):
mod2.3 <- "
policy ~ polAffil + b1*merit + b2*sysRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil + d21*merit

ab1 := a1*b1
ab2 := a2*b2
fullIE := a1*d21*b2
totalIE := ab1 + ab2 + fullIE 

fullIE == ab1
fullIE == ab2
"

out2.3 <- sem(mod2.3, data = dat1, se = "bootstrap", bootstrap = 1000)
summary(out2.3)

## Conduct a chi-squared difference test:
chiDiff <- fitMeasures(out2.3)["chisq"] - fitMeasures(out2.1)["chisq"]
dfDiff <- fitMeasures(out2.3)["df"] - fitMeasures(out2.1)["df"]
pchisq(chiDiff, dfDiff, lower = FALSE)



## Test Differences between Indirect Effects
## in Serial Multiple Mediator Model (Method 1):
mod2.4 <- "
policy ~ polAffil + b1*merit + b2*sysRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil + d21*merit

ab1 := a1*b1
ab2 := a2*b2
fullIE := a1*d21*b2
totalIE := ab1 + ab2 + fullIE 

fullIE == ab1
"

out2.4 <- sem(mod2.4, data = dat1, se = "bootstrap", bootstrap = 1000)
summary(out2.4)

## Conduct a chi-squared difference test:
chiDiff <- fitMeasures(out2.4)["chisq"] - fitMeasures(out2.1)["chisq"]
dfDiff <- fitMeasures(out2.4)["df"] - fitMeasures(out2.1)["df"]
pchisq(chiDiff, dfDiff, lower = FALSE)


## Test Differences between Indirect Effects
## in Serial Multiple Mediator Model (Method 1):
mod2.5 <- "
policy ~ polAffil + b1*merit + b2*sysRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil + d21*merit

ab1 := a1*b1
ab2 := a2*b2
fullIE := a1*d21*b2
totalIE := ab1 + ab2 + fullIE 

fullIE == ab2
"

out2.5 <- sem(mod2.5, data = dat1, se = "bootstrap", bootstrap = 1000)
summary(out2.5)

## Conduct a chi-squared difference test:
chiDiff <- fitMeasures(out2.5)["chisq"] - fitMeasures(out2.1)["chisq"]
dfDiff <- fitMeasures(out2.5)["df"] - fitMeasures(out2.1)["df"]
pchisq(chiDiff, dfDiff, lower = FALSE)


## Test Differences between Indirect Effects
## in Serial Multiple Mediator Model (Method 2):
mod2.6 <- "
policy ~ polAffil + b1*merit + b2*sysRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil + d21*merit

ab1 := a1*b1
ab2 := a2*b2
fullIE := a1*d21*b2
totalIE := ab1 + ab2 + fullIE 

test1 := fullIE - ab1
"

out2.6 <- sem(mod2.6, data = dat1, se = "bootstrap", bootstrap = 1000)

summary(out2.6)
parameterEstimates(out2.6, boot.ci.type = "bca.simple")


## Test Differences between Indirect Effects
## in Serial Multiple Mediator Model (Method 2):
mod2.7 <- "
policy ~ polAffil + b1*merit + b2*sysRac
merit ~ a1*polAffil
sysRac ~ a2*polAffil + d21*merit

ab1 := a1*b1
ab2 := a2*b2
fullIE := a1*d21*b2
totalIE := ab1 + ab2 + fullIE 

test1 := fullIE - ab2
"

out2.7 <- sem(mod2.6, data = dat1, se = "bootstrap", bootstrap = 1000)

summary(out2.7)
parameterEstimates(out2.7, boot.ci.type = "bca.simple")


## Serial Multiple Mediator Model with 3 Mediators:
mod3.1 <- "
policy ~ polAffil + b1*merit + b2*sysRac + b3*revDisc
merit ~ a1*polAffil
sysRac ~ a2*polAffil + d21*merit
revDisc ~ a3*polAffil + d32*sysRac + d31*merit

ab1 := a1*b1
ab2 := a2*b2
ab3 := a3*b3

partIE1 := a1*d21*b2
partIE2 := a2*d32*b3
partIE3 := a1*d31*b3

fullIE := a1*d21*d32*b3

totalIE := ab1 + ab2 + ab3 + partIE1 + partIE2 + partIE3 + fullIE 
"

out3.1 <- sem(mod3.1, data = dat1, se = "bootstrap", bootstrap = 5000)

summary(out3.1)
parameterEstimates(out3.1, boot.ci.type = "bca.simple")


## Hybrid Multiple Mediator Model:
mod4.1 <- "
policy ~ b1*sysRac + b3*revDisc + b2*merit + polAffil
sysRac ~ d211*merit + a1*polAffil
revDisc ~ d212*merit + a3*polAffil
merit ~ a2*polAffil

sysRac ~~ revDisc

ab1 := a1*b1
ab2 := a2*b2
ab3 := a3*b3

fullIE1 := a2*d211*b1
fullIE2 := a2*d212*b3

totalIE := ab1 + ab2 + ab3 + fullIE1 + fullIE2
"

out4.1 <- sem(mod4.1, data = dat1, se = "bootstrap", bootstrap = 1000)

summary(out4.1)
parameterEstimates(out4.1, boot.ci.type = "bca.simple")
