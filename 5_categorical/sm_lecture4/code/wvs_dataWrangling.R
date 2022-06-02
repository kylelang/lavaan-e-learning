### Title:    World Valus Survey Data Wrangling
### Author:   Kyle M. Lang
### Created:  2018-09-12
### Modified: 2020-04-19

rm(list = ls(all = TRUE))

library(mice)
library(miceadds)

### Load Data ###

dataDir  <- "../data/"
fileName <- "F00007762-WV6_Data_R_v20180912.rds"
saveDate <- format(Sys.time(), format = "%Y%m%d")

## Load the data:
wvs <- readRDS(paste0(dataDir, fileName))

### Process Data ###

## Exclude indices and weights:
wvs <- wvs[ , grep("^V\\d", colnames(wvs))]

## Subset by country:
filter <- wvs$V2 %in% c(156, 276, 356, 643, 840)
wvs    <- wvs[filter, ]

## Convert various missing data codes to 'NA':
wvs[wvs < 0] <- NA

## Compute within-country PM:
pm <- lapply(X       = wvs,
             FUN     = function(x, country)
                 tapply(x, country, FUN = function(x) mean(is.na(x))),
             country = wvs$V2)
pm <- do.call(rbind, pm)

## Remove variables for which PM >= 50% within any country:
good <- names(which(apply(pm < 0.5, 1, all)))
wvs  <- wvs[ , good]

## Remove otherwise problematic varaibles:
wvs <- wvs[ , -grep("V2[5-9]$|V3[0-5]$|V2A$|V3$|125|144|211|228|241|247|254|265|257|258|260|262",
                    colnames(wvs)
                    )
           ]

## Identify "good" nominal variables:
catNums <- c(24:35, 60:66, 81:83, 147:151, 176:180, 187, 229:230, 234:236, 240,
             243:246, 250, 252, 255, 265)
catVars <- intersect(paste0("V", catNums), colnames(wvs))

## Cast nominal variables as factors:
wvs[ , catVars] <- data.frame(lapply(wvs[ , catVars], factor))

## Remove "point-process" variables:
filter <- sapply(wvs,
                 FUN = function(x)
                     is.factor(x) | length(unique(na.omit(x))) > 2
                 )
wvs <- wvs[ , filter]

## Check for constants:
any(sapply(wvs, function(x) length(unique(na.omit(x))) == 1))

## Exclude columns that are constant within country:
check <- sapply(X   = wvs[-1],
                FUN = function(x, y)
                    tapply(x, y, function(x) length(unique(na.omit(x))) == 1),
                y   = wvs$V2)

table(colSums(check)) # How many offenders?

drops <- names(which(colSums(check) > 0))
wvs   <- wvs[setdiff(colnames(wvs), drops)]

## Check the status of the missing data:
pm <- colMeans(is.na(wvs))
range(pm)
range(md.pairs(wvs)$rr / nrow(wvs))

### Impute Missing Data ###

## Construct method vector:
levs <- sapply(wvs, function(x) length(unique(na.omit(x))))
facs <- sapply(wvs, is.factor)

meth        <- rep("pmm", ncol(wvs))
names(meth) <- colnames(meth)

meth[facs]             <- "polyreg"
meth[facs & levs == 2] <- "logreg"
meth[pm == 0]          <- ""

## Check method vector:
data.frame(meth, levs, facs, pm)

## Construct predictor matrix:
preds <- quickpred(data = wvs, mincor = 0.3, include = "V2")
rowSums(preds)

## Impute missing data:
miceOut <- mice(data   = wvs,
                m      = 3,
                maxit  = 100,
                method = meth,
                pred   = preds,
                seed   = 235711)

## Check problems:
miceOut$loggedEvents

## Check convergence:
for(v in colnames(wvs)[pm > 0]) {
    print(plot(miceOut, v))
    readline("Hit key ")
}

## Save the mids object:
saveRDS(miceOut, file = paste0(dataDir, "wvs_mids.rds"))


