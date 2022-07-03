### Title:    Lavaan Summer School Exercise Prep
### Author:   Kyle M. Lang
### Created:  2022-07-01
### Modified: 2022-07-02

rm(list = ls(all = TRUE))

library(lavaan, lib = "~/R/manual_installs/")
library(semTools)
library(dplyr)
library(magrittr)

dataDir <- "../data/"

## Read in data:
hs    <- readRDS(paste0(dataDir, "holzinger_swineford.rds"))
adams <- readRDS(paste0(dataDir, "sandbox/adams_klps_data-synthetic.rds"))

colnames(adams)

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


###-Measurement Invariance---------------------------------------------------###

group <- "grade"

configFit <- cfa(fullMod, data = hs, std.lv = TRUE, group = group)

fitMeasures(configFit)
summary(configFit)

weakMod   <- measEq.syntax(configFit, group.equal = "loadings") %>% as.character()
strongMod <- measEq.syntax(configFit, group.equal = c("loadings", "intercepts")) %>% as.character()

weakFit   <- cfa(weakMod, data = hs, std.lv = TRUE, group = group)
strongFit <- cfa(strongMod, data = hs, std.lv = TRUE, group = group)

summary(weakFit)
summary(strongFit)

compareFit(configFit, weakFit, strongFit) %>% summary()

as.character(weakMod) %>% cat()


###-Adams KLPS EFA-----------------------------------------------------------###

rhs <- paste(colnames(adams), collapse = ' + ')
       
mods <- list()

mods[[1]] <- paste('efa("efa") * f1',
                   rhs,
                   sep = ' =~ ')

for(i in 9:12)
    mods[[i]] <- paste(
        paste0('efa("efa") * f', 1:i, collapse = ' +\n'),
        rhs,
        sep = ' =~ '
    )

fits <- lapply(mods[9:12], cfa, data = adams)
warnings()

lapply(fits, summary)

tmp <- inspect(fits[[3]], "cov.lv") %>% eigen()

plot(tmp$values, type = "b")

items <- list(
    rac1  = colnames(adams)[8:10],
    rac2  = colnames(adams)[16:18],
    pol   = colnames(adams)[19:23],
    wpriv = grep("wpriv\\d", colnames(adams), value = TRUE)
)

mods <- lapply(items,
               function(x) paste("f",
                                 paste(x, collapse = " + "),
                                 sep = " =~ ")
               )

fits <- lapply(mods, cfa, data = adams,  std.lv = TRUE)

lapply(fits, fitMeasures)
lapply(fits, summary)