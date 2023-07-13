### Title:    Categorical Indicator Models Using the Enders (2010) Eating Attitudes Data
### Author:   Kyle M. Lang
### Created:  2022-07-02
### Modified: 2023-07-11

rm(list = ls(all = TRUE))

install.packages(c("polycor"), repos = "https://cloud.r-project.org")

library(dplyr)
library(lavaan)
library(semTools)
library(mice)
library(magrittr)
library(mvtnorm)
library(polycor)

dataDir <- "../../data/"


###-Polychoric Correlations-------------------------------------------------###

X <- rmvnorm(100000, c(0, 0), matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)) %>%
  data.frame()

Z <- X %>% mutate(X1 = cut(X1, c(-Inf, -0.5, 0.5, Inf), labels = FALSE), 
                  X2 = cut(X2, c(-Inf, -1.0, 1.0, Inf), labels = FALSE)
)

X %$% cor(X1, X2)
Z %$% cor(X1, X2)
Z %$% polychor(X1, X2)

head(X)
head(Z)

hist(Z$X2)

cov(X)

## Original, incomplete data:
eat0 <- readRDS(paste0(dataDir, "eating_attitudes.rds"))

## Data completed via single imputation:
eat1 <- readRDS(paste0(dataDir, "eating_attitudes_completed.rds"))

## mids object containing 500 multiple imputations:
eatMids <- readRDS(paste0(dataDir, "eating_attitudes_mids.rds"))

## Outlook on life data:
outlook <- readRDS(paste0(dataDir, "outlook.rds")) %>% 
  select(-disillusion, -success)

## Define the measurement model syntax:
cfaMod <- '
drive =~ eat1 + eat2 + eat10 + eat11 + eat12 + eat14 + eat24
obsess =~ eat3 + eat18 + eat21
'

cfaMod <- '
disillusion =~ d1 + d2 + d3
success =~ s1 + s2 + s3 + s4
'

fit1.0 <- cfa(cfaMod, data = outlook, std.lv = TRUE)
fit1.1 <- cfa(cfaMod, data = outlook, std.lv = TRUE, ordered = TRUE, estimator = "WLSMV")

summary(fit1.0)
summary(fit1.1)

?lavOptions

## DWLS fit:
fit1.1 <- cfa(cfaMod, data = eat1, std.lv = TRUE, ordered = TRUE)

fitMeasures(fit1.1)
summary(fit1.1)

p <- {eat1 %$% table(eat1) %>% cumsum()} / nrow(eat1)
qnorm(p)

lavInspect(fit1.1, "est")$tau[1:5]

## WLS fit:
fit1.2 <- cfa(cfaMod,
              data = eat1,
              std.lv = TRUE,
              ordered = TRUE,
              estimator = "WLS")

fitMeasures(fit1.2)
summary(fit1.2)

## DLS fit:
fit1.3 <- cfa(cfaMod,
              data = eat1,
              std.lv = TRUE,
              ordered = TRUE,
              estimator = "DLS")

fitMeasures(fit1.3)
summary(fit1.3)

## ULS fit:
fit1.4 <- cfa(cfaMod,
              data = eat1,
              std.lv = TRUE,
              ordered = TRUE,
              estimator = "ULS")

fitMeasures(fit1.4)
summary(fit1.4)

coef(fit1.1)
coef(fit1.2)
coef(fit1.4)

###-Multiply Imputed Data----------------------------------------------------###

eat2 <- list()
for(m in 1:20)
    eat2[[m]] <- complete(eatMids, m)

fit0 <- cfa.mi(cfaMod, data = eat2, std.lv = TRUE)

fitMeasures(fit0, test = "D3")
summary(fit0, fmi = TRUE)

###-Simple Mediation---------------------------------------------------------###

## Define the structural part of a simple mediation model:
semMod1 <- '
obsess ~ b * drive + cp * wsb
drive  ~ a * wsb

ab := a * b
'

semMod1 <- paste(cfaMod, semMod1, sep = '\n')

## Estimate the model with Naive FIML:
semFit1 <-
    sem(semMod1, data = eat1, std.lv = TRUE, ordered = TRUE)
summary(semFit1)


###-Serial Multiple Mediation------------------------------------------------###

## Define the structural part of a simple mediation model:
semMod2 <- '
obsess ~ b1 * drive + b2 * anx + cp * wsb
anx    ~ d * drive + a2 * wsb
drive  ~ a1 * wsb

ab1 := a1 * b1
ab2 := a2 * a2
adb := a1 * d * b2
ie  := ab1 + ab2 + adb
'

semMod2 <- paste(cfaMod, semMod2, sep = '\n')

ordVars <- grep("eat\\d", colnames(eat1), value = TRUE)

## Estimate the model with Naive FIML:
semFit2 <-
    sem(semMod2, data = eat1, std.lv = TRUE, ordered = ordVars)
summary(semFit2)

