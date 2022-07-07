### Title:    Exercises based on the Outlook on Life Survey data
### Author:   Kyle M. Lang
### Created:  2022-07-05
### Modified: 2022-07-05

rm(list = ls(all = TRUE))

library(dplyr)
library(magrittr)
library(lavaan)
library(semTools)
library(rockchalk)

dataDir <- "../../data/"
fn      <- "outlook.rds"

## Read in the data:
dat1 <- readRDS(paste0(dataDir, fn))

###-OLS Regression-----------------------------------------------------------###

fit0 <- lm(progress ~ success + disillusion + lib2Con, data = dat1)
fit1 <- lm(progress ~ success * disillusion + lib2Con, data = dat1)

summary(fit0)
summary(fit1)

psOut <- plotSlopes(fit1, plotx = "success", modx = "disillusion")
testSlopes(psOut)$hypotests

###-Path Analysis------------------------------------------------------------###

pathMod <- '
progress ~ 1 + success + disillusion + success:disillusion
'

fit <- sem(pathMod, data = dat1)

summary(fit)

ssOut <- probe2WayMC(fit,
                     nameX    = c("success", "disillusion", "success:disillusion"),
                     nameY    = "progress",
                     modVar   = "disillusion",
                     valProbe = quantile(dat1$disillusion, c(0.25, 0.5, 0.75))
                     )

ssOut

plotProbe(ssOut, xlim = range(dat1$success))

###-Latent Variables---------------------------------------------------------###

## Measurement model:
cfaMod <- '
success      =~ s1 + s2 + s3 + s4
disillusion  =~ d1 + d2 + d3
'

fit <- dat1 %>%
    select(-success, -disillusion) %>%
    cfa(cfaMod, data = ., std.lv = TRUE)

summary(fit)
fitMeasures(fit)

## Additive structural model:
semMod <- '
progress ~ 1 + success + disillusion + lib2Con
'

semMod <- paste(cfaMod, semMod, sep = '\n')

fit <- dat1 %>%
    select(-success, -disillusion) %>%
    sem(semMod, data = ., std.lv = TRUE)

summary(fit)
fitMeasures(fit)

## Moderation with double mean centering:
dat2 <- indProd(data      = dat1,
                var1      = paste0("s", 1:4),
                var2      = paste0("d", 1:3),
                match     = FALSE,
                meanC     = TRUE,
                doubleMC  = TRUE,
                residualC = FALSE)

semMod <- '
## Define the interaction factor:
interact =~ s1.d1 + s1.d2 + s1.d3
interact =~ s2.d1 + s2.d2 + s2.d3
interact =~ s3.d1 + s3.d2 + s3.d3
interact =~ s4.d1 + s4.d2 + s4.d3

## Correlated residuals for product terms involving the same item:
s1.d1 ~~ s1.d2 + s1.d3 + s2.d1 + s3.d1 + s4.d1
s2.d1 ~~ s2.d2 + s2.d3 + s3.d1 + s4.d1
s3.d1 ~~ s3.d2 + s3.d3 + s4.d1
s4.d1 ~~ s4.d2 + s4.d3

s1.d2 ~~ s1.d3 + s2.d2 + s3.d2 + s4.d2
s2.d2 ~~ s2.d3 + s3.d2 + s4.d2
s3.d2 ~~ s3.d3 + s4.d2
s4.d2 ~~ s4.d3

s1.d3 ~~ s2.d3 + s3.d3 + s4.d3
s2.d3 ~~ s3.d3 + s4.d3
s3.d3 ~~ s4.d3

## Define the structural relations:
progress ~ 1 + success + disillusion + interact + lib2Con
'

semMod <- paste(cfaMod, semMod, sep = '\n')

fit <- dat2 %>%
    select(-success, -disillusion) %>%
    sem(semMod, data = ., std.lv = TRUE)

summary(fit)
fitMeasures(fit)

## Moderation with residual centering
dat2 <- indProd(data      = dat1,
                var1      = paste0("s", 1:4),
                var2      = paste0("d", 1:3),
                match     = FALSE,
                meanC     = FALSE,
                doubleMC  = FALSE,
                residualC = TRUE)

semMod <- paste(semMod,
                'success     ~~ 0 * interact',
                'disillusion ~~ 0 * interact',
                sep = '\n')

fit <- dat2 %>%
    select(-disillusion, -success) %>%
    sem(semMod, data = ., std.lv = TRUE)

summary(fit)
fitMeasures(fit)
