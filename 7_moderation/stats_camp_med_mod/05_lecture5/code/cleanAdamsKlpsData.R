### Title:    Clean Adams KLPS Data
### Author:   Kyle M. Lang
### Created:  2015-OCT-04
### Modified: 2016-FEB-21

rm(list = ls(all = TRUE))

dataDir <- "../data/"

library(psych)

## Read the data:
tmp <- readLines(paste0(dataDir, "adamsklps_sem_data.txt"), -1)
tmp2 <- strsplit(tmp, "\t")
dat1 <- do.call("rbind", lapply(tmp2[-1], function(x) {x[1 : 33]}))
dat1 <- apply(dat1, 2, as.numeric)
colnames(dat1) <- tmp2[[1]][1 : 33]

sysRacNames <- paste0("NORI", c(2, 3, 6, 7, 8, 9, 11))
sysRacNames <- sysRacNames[sysRacNames %in% colnames(dat1)]

indRacNames <- paste0("NORI", c(1, 4, 5, 10))
indRacNames <- indRacNames[indRacNames %in% colnames(dat1)]

policyNames <- paste0("POLICY", c(1, 3, 4, 5, 6))

meritNames <- paste0("WPRIV", c(1 : 10))
meritNames <- meritNames[meritNames %in% colnames(dat1)]

sysRacData <- dat1[ , sysRacNames]
indRacData <- dat1[ , indRacNames]
policyData <- dat1[ , policyNames]
meritData <- dat1[ , meritNames]

colnames(sysRacData) <- paste0("sysRac", c(1 : ncol(sysRacData)))
colnames(indRacData) <- paste0("indRac", c(1 : ncol(indRacData)))
colnames(policyData) <- paste0("policy", c(1 : ncol(policyData)))
colnames(meritData) <- paste0("merit", c(1 : ncol(meritData)))

mod1 <- "
merit =~ merit1 + merit2 + merit3 + merit4 + merit5 + merit6 + merit7 + merit8
"
out1 <- cfa(mod1, data = meritData, std.lv = TRUE)

tmp <- inspect(out1, "coef")$lambda
rownames(tmp)[order(tmp)]

## P1 P2 P3
## 1, 6, 2,
## 7, 3, 5,
## 4, 8

meritP1 <- rowMeans(meritData[ , c(1, 7, 4)])
meritP2 <- rowMeans(meritData[ , c(6, 3, 8)])
meritP3 <- rowMeans(meritData[ , c(2, 5)])

mod2 <- "
policy =~ policy1 + policy2 + policy3 + policy4 + policy5
"

out2 <- cfa(mod2, data = policyData, std.lv = TRUE)

tmp <- inspect(out2, "coef")$lambda
rownames(tmp)[order(tmp)]

## P1 P2 P3
## 5, 4, 1,
##    3, 2

policyP1 <- policyData[ , 5]
policyP2 <- rowMeans(policyData[ , c(4, 3)])
policyP3 <- rowMeans(policyData[ , c(1, 2)])

alpha(sysRacData)
alpha(indRacData)
alpha(policyData)
alpha(meritData)

sysRac <- rowMeans(sysRacData)
indRac <- rowMeans(indRacData)
policy <- rowMeans(policyData)
merit <- rowMeans(meritData)
polAffil <- dat1[ , "POLV"]
revDisc <- dat1[ , "POLICY2"]

dat2 <- data.frame(sysRacData,
                   indRacData,
                   policyData,
                   meritData,
                   policyP1,
                   policyP2,
                   policyP3,
                   meritP1,
                   meritP2,
                   meritP3,
                   sysRac,
                   indRac,
                   policy,
                   merit,
                   polAffil,
                   revDisc)

saveRDS(dat2, paste0(dataDir, "adamsKlpsData.rds"))
