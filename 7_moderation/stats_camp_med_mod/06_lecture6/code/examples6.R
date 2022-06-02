### Title:    Mediation & Moderation: Lecture 6 Examples
### Author:   Kyle M. Lang
### Created:  2016-FEB-06
### Modified: 2016-JUN-09

library(lavaan)
dataDir <- "../data/"

## Read in the data:
dat1 <- readRDS("../data/unicorn2.rds")

mod1 <- "
att3 ~ att2 + b2*conf2 + cp2*horn2
att2 ~ att1 + b1*conf1 + cp1*horn1

conf3 ~ conf2 + a2*horn2
conf2 ~ conf1 + a1*horn1

horn3 ~ horn2
horn2 ~ horn1

horn3 ~~ conf3 + att3
conf3 ~~ att3

horn2 ~~ conf2 + att2
conf2 ~~ att2

ab1 := a1*b1
ab2 := a2*b2
"

out1 <- sem(mod1, data = dat1, se = "boot", boot = nBoot)
summary(out1)
parameterEstimates(out1, boot = "bca.simple")[ , -c(1 : 3)]

mod2 <- "
att3 ~ att2 + b2*conf2 + cp*horn1
att2 ~ att1 + b1*conf1

conf3 ~ conf2 + a2*horn2
conf2 ~ conf1 + a1*horn1

horn3 ~ horn2
horn2 ~ horn1

horn3 ~~ conf3 + att3
conf3 ~~ att3

horn2 ~~ conf2 + att2
conf2 ~~ att2

ab := a1*b2
"

out2 <- sem(mod2, data = dat1, se = "boot", boot = nBoot)
summary(out2)
parameterEstimates(out2, boot = "bca.simple")[ , -c(1 : 3)]


mod3 <- "
att3 ~ att2 + b2*conf2 + cp2*horn2
att2 ~ att1 + b1*conf1 + cp1*horn1

conf3 ~ conf2 + a2*horn2
conf2 ~ conf1 + a1*horn1

horn3 ~ horn2
horn2 ~ horn1

horn3 ~~ conf3 + att3
conf3 ~~ att3

horn2 ~~ conf2 + att2
conf2 ~~ att2

a1 == a2
b1 == b2
cp1 == cp2
"

out3 <- sem(mod3, data = dat1)
summary(out3)

chiDiff <- fitMeasures(out3)["chisq"] -
    fitMeasures(out1)["chisq"]

dfDiff <- fitMeasures(out3)["df"] -
    fitMeasures(out1)["df"]

pchisq(chiDiff, dfDiff, lower = FALSE)


mod4 <- "
att3 ~ att2 + b2*conf2 + cp*horn1
att2 ~ att1 + b1*conf1

conf3 ~ conf2 + a2*horn2
conf2 ~ conf1 + a1*horn1

att2 + att3 ~ income
conf2 + conf3 ~ income 
horn2 + horn3 ~ income

horn3 ~ horn2
horn2 ~ horn1

horn3 ~~ conf3 + att3
conf3 ~~ att3

horn2 ~~ conf2 + att2
conf2 ~~ att2

ab := a1*b2
"

out4 <- sem(mod4, data = dat1, se = "boot", boot = nBoot)
summary(out4)
parameterEstimates(out4, boot = "bca.simple")[ , -c(1 : 3)]
