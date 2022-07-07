### Title:    Exploration of Holzinger & Swineford Data
### Author:   Kyle M. Lang
### Created:  2022-07-01
### Modified: 2022-07-03

rm(list = ls(all = TRUE))

library(lavaan)
library(semTools)
library(dplyr)
library(magrittr)
library(ggplot2)
library(psych)

dataDir <- "../../data/"

## Read in data:
hs    <- readRDS(paste0(dataDir, "holzinger_swineford.rds"))
items <- readRDS(paste0(dataDir, "hs_subscale_item_names.rds"))$new


###-Measurement Models-------------------------------------------------------###

## Define univariate measurement model syntax:
mods <- list()
for(lv in names(items))
    mods[[lv]] <- paste(lv,
                        paste0(items[[lv]], collapse = " + "),
                        sep = " =~ ")

## Fit the univariate CFA models:
fits <- lapply(mods, cfa, data = hs, std.lv = TRUE)

## Check fits:
lapply(fits, fitMeasures)
lapply(fits, summary)

## Fit the multidimensional CFA:
fullMod <- paste(mods[-3], collapse = "\n")
fullFit <- cfa(fullMod, data = hs, std.lv = TRUE)

fitMeasures(fullFit)
summary(fullFit)


###-Simple Mediation 1-------------------------------------------------------###

## Measurement model:
cfaMod1 <- with(mods, paste(spatial, math, sep = "\n"))
cfaFit1 <- cfa(cfaMod1, data = hs, std.lv = TRUE)

fitMeasures(cfaFit1)
summary(cfaFit1)

## Structural model:
medMod1 <- paste(cfaMod1,
                 "spatial ~ b * math + cp * age",
                 "math ~ a * age",
                 "ab := a * b",
                 "c := ab + cp",
                 sep = "\n")

sem(medMod1, data = hs, std.lv = TRUE) %>% summary()


###-Simple Mediation 1-------------------------------------------------------###

## Measurement model:
cfaMod2 <- with(mods, paste(spatial, verbal, sep = "\n"))
cfaFit2 <- cfa(cfaMod2, data = hs, std.lv = TRUE)

fitMeasures(cfaFit2)
summary(cfaFit2)

## Structural model:
medMod2 <- paste(cfaMod2,
                 "spatial ~ b * verbal + cp * age",
                 "verbal ~ a * age",
                 "ab := a * b",
                 "c := ab + cp",
                 sep = "\n")

sem(medMod2, data = hs, std.lv = TRUE) %>% summary()


###-Parallel Multiple Mediation----------------------------------------------###

## Measurement model:
cfaMod3 <- with(mods, paste(spatial, math, verbal, sep = "\n"))
cfaFit3 <- cfa(cfaMod3, data = hs, std.lv = TRUE)

fitMeasures(cfaFit3)
summary(cfaFit3)

## Structural model:
medMod3 <- paste(cfaMod3,
                 "spatial ~ b1 * math + b2 * verbal + c * age",
                 "math ~ a1 * age",
                 "verbal ~ a2 * age",
                 "ab1 := a1 * b1",
                 "ab2 := a2 * b2",
                 "ie := ab1 + ab2",
                 sep = "\n")

sem(medMod3, data = hs, std.lv = TRUE) %>% summary()

###-Data Processing----------------------------------------------------------###

## Create scale scores:
keys <- list(math   = paste0("math", 1:5),
             verbal = paste0("verbal", 1:5),
             memory = paste0("memory", 1:6)
             )
scores <- scoreVeryFast(keys = keys, items = hs)
hs2    <- data.frame(scores, sex = hs$sex)

###-Moderation---------------------------------------------------------------###

## OLS
fit <- lm(math ~ verbal * sex + memory * sex, data = hs2)
summary(fit)

## Path analysis
mod <- '
math ~ 1 + verbal + memory + male + verbal:male + memory:male
'

fit <- hs2 %>%
    mutate(male = as.numeric(sex == "Male")) %>%
    sem(mod, data = .)

summary(fit)

ssOut <- probe2WayMC(fit,
                     nameX   = c("verbal", "male", "verbal:male"),
                     nameY   = "math",
                     modVar  = "male",
                     valProb = 0:1)

ssOut

plotProbe(ssOut, xlim = range(hs2$verbal))

cfaMod <- with(mods, paste(math, verbal, memory, sep = '\n'))

fit <- cfa(cfaMod, data = hs, std.lv = TRUE)
summary(fit)
fitMeasures(fit)

### Measurement Invariance:

group <- "sex"

configFit <- cfa(cfaMod, data = hs, std.lv = TRUE, group = group)

fitMeasures(configFit)
summary(configFit)

weakMod   <- measEq.syntax(configFit, group.equal = "loadings") %>% as.character()
strongMod <- measEq.syntax(configFit, group.equal = c("loadings", "intercepts")) %>% as.character()

weakFit   <- cfa(weakMod, data = hs, std.lv = TRUE, group = group)
strongFit <- cfa(strongMod, data = hs, std.lv = TRUE, group = group)

summary(weakFit)
summary(strongFit)

compareFit(configFit, weakFit, strongFit) %>% summary()

### Unrestricted structural model:

semMod <- paste(cfaMod,
                'math ~ c(beta11, beta12) * verbal + c(beta21, beta22) * memory',
                'verbal ~~ c(psi1, psi2) * memory',
                sep = '\n')

semFit <- sem(semMod,
              data = hs,
              std.lv = TRUE,
              group = group,
              group.equal = "loadings")

summary(semFit)

cons <- '
beta11 == beta12
beta21 == beta22
psi1 == psi2
'

lavTestWald(semFit, cons)

semMod0 <- paste(cfaMod,
                 'math ~ c(beta11, beta12) * verbal + c(beta22, beta22) * memory',
                 'verbal ~~ c(psi1, psi2) * memory',
                 sep = '\n')

semFit0 <- sem(semMod0,
               data = hs,
               std.lv = TRUE,
               group = group,
               group.equal = "loadings")

summary(semFit0)

tmp <- lavTestWald(semFit, 'beta11 == beta12')

sqrt(tmp$stat)

lavTestWald(semFit0, 'beta11 == beta12')

anova(semFit, semFit0)
fitMeasures(semFit0)


## Generate factor scores:
tmp <- predict(semFit)

## Stack factor scores into a "tidy" dataset:
pData <- data.frame(do.call(rbind, tmp),
                    group = rep(names(tmp), sapply(tmp, nrow))
                    )

tmp <- inspect(semFit, "est")

mBeta <- tmp$Male$beta["math", c("verbal", "memory")]
fBeta <- tmp$Female$beta["math", c("verbal", "memory")]

## Create a simple slopes plot:
ssPlot <- ggplot(pData, aes(verbal, math, color = group)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(method = "lm") +
    geom_abline(slope = mBeta[1], intercept = 0, color = "blue") +
    geom_abline(slope = fBeta[1], intercept = 0, color = "red") +
    theme_classic()

ssPlot <- ggplot(pData, aes(memory, math, color = group)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(method = "lm") +
    geom_abline(slope = mBeta[2], intercept = 0, color = "blue") +
    geom_abline(slope = fBeta[2], intercept = 0, color = "red") +
    theme_classic()

ssPlot

semMod2 <- paste(semMod,
                 'test1 := beta11 - beta12',
                 'test2 := beta21 - beta22',
                 sep = '\n')

semFit2 <- sem(semMod2,
               data = hs,
               std.lv = TRUE,
               group = group,
               group.equal = "loadings")

summary(semFit2)
