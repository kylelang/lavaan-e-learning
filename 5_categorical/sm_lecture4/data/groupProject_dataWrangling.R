### Title:    DSS Stats & Methods Group Project: Data Wrangling
### Author:   Kyle M. Lang
### Created:  2018-09-12
### Modified: 2020-02-10

rm(list = ls(all = TRUE))

library(mice)

dataDir  <- "../data/"
fileName <- "F00007762-WV6_Data_R_v20180912.rds"
saveDate <- format(Sys.time(), format = "%Y%m%d")

## Load the data:
dat1 <- readRDS(paste0(dataDir, fileName))

## Exclude indices and weights:
dat1 <- dat1[ , grep("^V\\d", colnames(dat1))]

## Subset by country:
filter <- dat1$V2 %in% c(156, 276, 356, 643, 840)
dat1   <- dat1[filter, ]

## Convert various missing data codes to 'NA':
dat1[dat1 < 0] <- NA

## Compute within-country PM:
pm <- lapply(X       = dat1,
             FUN     = function(x, country)
                 tapply(x, country, FUN = function(x) mean(is.na(x))),
             country = dat1$V2)
pm <- do.call(rbind, pm)

## Remove variables for which PM >= 50% within any country:
good <- names(which(apply(pm < 0.5, 1, all)))
dat1 <- dat1[ , good]

## Remove otherwise problematic varaibles:
dat1 <- dat1[ , -grep("V2[5-9]$|V3[0-5]$|V2A$|V3$|125|144|211|228|241|247|254|265|257|258|260|262",
                      colnames(dat1)
                      )
             ]

## Identify "good" nominal variables:
catNums <- c(24:35, 60:66, 81:83, 147:151, 176:180, 187, 229:230, 234:236, 240,
             243:246, 250, 252, 255, 265)
catVars <- intersect(paste0("V", catNums), colnames(dat1))

## Cast nominal variables as factors:
dat1[ , catVars] <- data.frame(lapply(dat1[ , catVars], factor))

## Remove "point-process" variables:
filter <- sapply(dat1,
                 FUN = function(x)
                     is.factor(x) | length(unique(na.omit(x))) > 2
                 )
dat1 <- dat1[ , filter]

## Check for constants:
any(sapply(dat1, function(x) length(unique(na.omit(x))) == 1))

## Exclude columns that are constant within country:
check <- sapply(X   = dat1[-1],
                FUN = function(x, y)
                    tapply(x, y, function(x) length(unique(na.omit(x))) == 1),
                y   = dat1$V2)

table(colSums(check)) # How many offenders?

drops <- names(which(colSums(check) > 0))
dat1  <- dat1[setdiff(colnames(dat1), drops)]

## Check the status of the missing data:
range(colMeans(is.na(dat1)))
range(md.pairs(dat1)$rr / nrow(dat1))

## Save the column names of the processed data:
saveRDS(colnames(dat1), file = paste0(dataDir, "wvs_column_names.rds"))


