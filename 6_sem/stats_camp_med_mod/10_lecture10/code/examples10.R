### Title:    Mediation & Moderation: Lecture 10 Examples
### Author:   Kyle M. Lang
### Created:  2016-MAR-17
### Modified: 2016-JUN-09

rm(list = ls(all = TRUE))

library(lavaan)
library(psych)

## Prep data:
dat1 <- readRDS("../data/lecture10Data.rds")

## Simple indirect effects model:
mod1 <- "
agree ~ b*open + cp*extra
open ~ a*extra

ab := a*b
"

out1 <- sem(mod1, data = dat1, se = "boot")
summary(out1)
tab1 <- parameterEstimates(out1, boot.ci.type = "bca.simple")
tab1[grep("ab", tab1$label), c("label", "est", "ci.lower", "ci.upper")]

## Construct the product term:
dat1$openXconsc <- dat1$open*dat1$consc

## Find 'interesting' quantiles of 'conc':
quantile(dat1$conc, c(0.25, 0.50, 0.75))

## Conditional process model with b path moderated:
mod2 <- "
agree ~ b1*open + cp*extra + b2*consc + b3*openXconsc
open ~ a*extra

abLo  := a*(b1 + b3*(-0.4045))
abMid := a*(b1 + b3*(-0.0045))
abHi  := a*(b1 + b3*0.3955)
"

out2 <- sem(mod2, data = dat1, se = "boot")
summary(out2)
tab2 <- parameterEstimates(out2, boot.ci.type = "bca.simple")
tab2[grep("ab", tab2$label), c("label", "est", "ci.lower", "ci.upper")]

## Construct another product term:
dat1$extraXneuro <- dat1$extra*dat1$neuro

## Find interesting quantiles of 'neuro':
quantile(dat1$neuro, c(0.25, 0.50, 0.75))

## Conditional process model with a and b paths moderated:
mod3 <- "
agree ~ b1*open + cp*extra + b2*consc + b3*openXconsc
open ~ a1*extra + a2*neuro + a3*extraXneuro

abLoLo  := (a1 + a3*(-0.962268)) * (b1 + b3*(-0.4045))
abLoMid := (a1 + a3*(-0.962268)) * (b1 + b3*(-0.0045))
abLoHi  := (a1 + a3*(-0.962268)) * (b1 + b3*0.3955)

abMidLo  := (a1 + a3*(-0.162268)) * (b1 + b3*(-0.4045))
abMidMid := (a1 + a3*(-0.162268)) * (b1 + b3*(-0.0045))
abMidHi  := (a1 + a3*(-0.162268)) * (b1 + b3*0.3955)

abHiLo  := (a1 + a3*0.837732) * (b1 + b3*(-0.4045))
abHiMid := (a1 + a3*0.837732) * (b1 + b3*(-0.0045))
abHiHi  := (a1 + a3*0.837732) * (b1 + b3*0.3955)
"

out3 <- sem(mod3, data = dat1, se = "boot", boot = nBoot)
summary(out3)

tab3 <- 
    parameterEstimates(out3, boot.ci.type = "bca.simple")
tab3[grep("ab", tab3$label), 
     c("label", "est", "ci.lower", "ci.upper")]

## Conditional process model with a, b, c paths moderated:
mod4 <- "
agree ~ b1*open + b2*consc + cp1*extra + cp2*neuro + 
        cp3*extraXneuro + b3*openXconsc
open ~ a1*extra + a2*neuro + a3*extraXneuro

cpLo  := cp1 + cp3*(-0.962268)
cpMid := cp1 + cp3*(-0.162268)
cpHi  := cp1 + cp3*0.837732

abLoLo  := (a1 + a3*(-0.962268)) * (b1 + b3*(-0.4045))
abLoMid := (a1 + a3*(-0.962268)) * (b1 + b3*(-0.0045))
abLoHi  := (a1 + a3*(-0.962268)) * (b1 + b3*0.3955)

abMidLo  := (a1 + a3*(-0.162268)) * (b1 + b3*(-0.4045))
abMidMid := (a1 + a3*(-0.162268)) * (b1 + b3*(-0.0045))
abMidHi  := (a1 + a3*(-0.162268)) * (b1 + b3*0.3955)

abHiLo  := (a1 + a3*0.837732) * (b1 + b3*(-0.4045))
abHiMid := (a1 + a3*0.837732) * (b1 + b3*(-0.0045))
abHiHi  := (a1 + a3*0.837732) * (b1 + b3*0.3955)
"

out4 <- sem(mod4, data = dat1, se = "boot", boot = nBoot)
summary(out4)

tab4 <-
    parameterEstimates(out4, boot.ci.type = "bca.simple")
tab4[grep("Lo|Mid|Hi", tab4$label),
     c("label", "est", "ci.lower", "ci.upper")]

## Conditional process model with b, c paths moderated:
mod5 <- "
agree ~ b1*open + b2*consc + cp1*extra + cp2*neuro + 
        cp3*extraXneuro + b3*openXconsc
open ~ a*extra

cpLo  := cp1 + cp3*(-0.962268)
cpMid := cp1 + cp3*(-0.162268)
cpHi  := cp1 + cp3*0.837732

abLo := a * (b1 + b3*(-0.4045))
abMid := a * (b1 + b3*(-0.0045))
abHi := a * (b1 + b3*0.3955)
"

out5 <- sem(mod5, data = dat1, se = "boot", boot = nBoot)
summary(out5)

tab5 <-
    parameterEstimates(out5, boot.ci.type = "bca.simple")
tab5[grep("Lo|Mid|Hi", tab5$label),
     c("label", "est", "ci.lower", "ci.upper")]
