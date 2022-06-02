### Title:    Med/Mod Lecture 10: Examples
### Author:   Kyle M. Lang
### Created:  2016-MAR-17
### Modified: 2016-APR-18

rm(list = ls(all = TRUE))

library(lavaan)
library(psych)

## Prep data:
data(bfi)

dat1 <- data.frame(
    agree = rowMeans(bfi[ , paste0("A", c(1 : 5))], na.rm = TRUE),
    consc = rowMeans(bfi[ , paste0("C", c(1 : 5))], na.rm = TRUE),
    extra = rowMeans(bfi[ , paste0("E", c(1 : 5))], na.rm = TRUE),
    neuro = rowMeans(bfi[ , paste0("N", c(1 : 5))], na.rm = TRUE),
    open  = rowMeans(bfi[ , paste0("O", c(1 : 5))], na.rm = TRUE)
    )

dat1 <- apply(dat1, 2, scale, scale = FALSE)
dat2 <- data.frame(dat1,
                   gender = as.factor(bfi$gender),
                   educ = as.factor(bfi$education)
                   )
levels(dat2$gender) <- c("male", "female")
levels(dat2$educ) <- c("someHS",
                       "finishHS",
                       "someCollege",
                       "finishCollege",
                       "gradSchool")

saveRDS(dat2, "../data/lecture10Data.rds")

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
dat2$openXconsc <- dat2$open*dat2$consc

## Find 'interesting' quantiles of 'conc':
quantile(dat2$conc, c(0.25, 0.50, 0.75))

## Conditional process model with b path moderated:
mod2 <- "
agree ~ b1*open + cp*extra + b2*consc + b3*openXconsc
open ~ a*extra

abLo  := a*(b1 + b3*(-0.4045))
abMid := a*(b1 + b3*(-0.0045))
abHi  := a*(b1 + b3*0.3955)
"

out2 <- sem(mod2, data = dat2, se = "boot")
summary(out2)
tab2 <- parameterEstimates(out2, boot.ci.type = "bca.simple")
tab2[grep("ab", tab2$label), c("label", "est", "ci.lower", "ci.upper")]

## Construct another product term:
dat2$extraXneuro <- dat2$extra*dat2$neuro

## Find interesting quantiles of 'neuro':
quantile(dat2$neuro, c(0.25, 0.50, 0.75))

## Conditional process model with a, b, & c paths moderated:
mod4 <- "
agree ~ b1*open + b2*consc + cp1*extra + cp2*neuro + cp3*extraXneuro + b3*openXconsc
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

out4 <- sem(mod4, data = dat2, se = "boot")
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
