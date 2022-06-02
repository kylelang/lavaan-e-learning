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

dat2 <- data.frame(sysRac, indRac, policy, merit, polAffil, revDisc)

saveRDS(dat2, paste0(dataDir, "adamsKlpsScaleScores.rds"))
