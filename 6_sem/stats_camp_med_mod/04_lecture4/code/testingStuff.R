### Title:    Mediation & Moderation: Testing Stuff
### Author:   Kyle M. Lang
### Created:  2016-FEB-06
### Modified: 2016-FEB-21

library(mvtnorm)
library(lavaan)

dataDir <- "../data/"
fileName <- "adamsKlpsScaleScores.rds"

dat1 <- readRDS(paste0(dataDir, fileName))


## Serial Multiple Mediator Model with 3 Mediators:
mod3.2 <- "
policy ~ polAffil + b1*merit + b2*revDisc + b3*sysRac
merit ~ a1*polAffil
revDisc ~ a2*polAffil + d21*merit
sysRac ~ a3*polAffil + d32*revDisc + d31*merit

ab1 := a1*b1
ab2 := a2*b2
ab3 := a3*b3

partIE1 := a1*d21*b2
partIE2 := a2*d32*b3
partIE3 := a1*d31*b3

fullIE := a1*d21*d32*b3

totalIE := ab1 + ab2 + ab3 + partIE1 + partIE2 + partIE3 + fullIE 
"

out3.2 <- sem(mod3.2, data = dat1, se = "bootstrap", bootstrap = 5000)

summary(out3.2)
parameterEstimates(out3.2, boot.ci.type = "bca.simple")



## Serial Multiple Mediator Model with 3 Mediators:
mod3.3 <- "
policy ~ polAffil + b3*sysRac
merit ~ a1*polAffil
revDisc ~ d21*merit
sysRac ~ d32*revDisc

fullIE := a1*d21*d32*b3 
"

out3.3 <- sem(mod3.3, data = dat1, se = "bootstrap", bootstrap = 5000)

summary(out3.3)
parameterEstimates(out3.3, boot.ci.type = "bca.simple")


## Serial Multiple Mediator Model with 3 Mediators:
mod3.4 <- "
policy ~ polAffil + b3*revDisc
merit ~ a1*polAffil
sysRac ~ d21*merit
revDisc ~ d32*sysRac

fullIE := a1*d21*d32*b3 
"

out3.4 <- sem(mod3.4, data = dat1, se = "bootstrap", bootstrap = 5000)

summary(out3.4)
summary(out3.3)

fitMeasures(out3.4)
fitMeasures(out3.3)

parameterEstimates(out3.4, boot.ci.type = "bca.simple")
