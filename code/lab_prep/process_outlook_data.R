### Title:    Process the Outlook on Life Survey data
### Author:   Kyle M. Lang
### Created:  2022-07-05
### Modified: 2022-07-05

rm(list = ls(all = TRUE))

library(dplyr)
library(magrittr)
library(psych)
library(mvtnorm)
library(semTools)
library(mice)
library(ggmice)

set.seed(235711)

### ACCESS NOTE ###
### The raw data file can be downloaded via the ICPSR here:
### https://doi.org/10.3886/ICPSR35348.v1

### The 'dataDir0' directory is not part of the git repo becuase I cannot
### redistribute the original data file.

dataDir0 <- "../../../data/ICPSR_35348/DS0001/" # Raw data
dataDir1 <- "../../data/"                       # Processed data
fn       <- "35348-0001-Data.rda"

###-Data Processing----------------------------------------------------------###

## Load the raw data:
load(paste0(dataDir0, fn))

## Simplify dataset name:
dat0 <- da35348.0001
rm(da35348.0001)

## Select interesting variables:
items <- list(
    disillusion = paste0("B", 1:3),
    success     = paste0("F4_", LETTERS[c(1:3, 5)]),
    progress    = "F6",
    merit       = "F3",
    lib2Con     = "C2",
    party       = "C1"
)
items <- lapply(items, function(x) paste0("W1_", x))
dat1  <- dat0[unlist(items)]

## Convert variables to numeric:
dat1 %<>% select(-W1_C1) %>%
    lapply(as.numeric) %>%
    data.frame()

## Any "Refused" responses still coded numerically?
sum(dat1 == -1, na.rm = TRUE)

## Do the NA counts match the number of "Refused" responses in the codebook?
dat1$W1_F3 %>% is.na() %>% sum() # YES

## Create a nice party ID factor:
dat1$party <- factor(dat0$W1_C1,
                     labels = c("republican",
                                "democrat",
                                "independent",
                                "other")
                     )

## Clean up column names and reverse code 'merit':
newNames <- c(paste0("d", 1:length(items$disillusion)),
              paste0("s", 1:length(items$success)),
              names(items)[3:5])
oldNames <- unlist(items[-6])

dat1 %<>% rename_with(~ newNames, all_of(oldNames)) %>%
    mutate(merit = 8 - merit)

###-Missing Data Treatment & Data Synthesis----------------------------------###

## What's the missing data situation?
dat1 %>% is.na() %>% colMeans()
plot_pattern(dat1)
fmi(data = dat1, group = "party")

### Very little missing data. We'll fill them via a single imputation while we
### syntehsize the data.

## Drop empty cases:
empty <- (dat1 %>% is.na() %>% rowMeans()) == 1
dat1 %<>% filter(!empty)

## Use MICE to both impute the missing data and synthesize the observed data:
miceOut <- mice(dat1,
                m = 1,
                method = "cart",
                maxit = 50,
                where = matrix(TRUE, nrow(dat1), ncol(dat1)),
                seed = 235711)

## Sanity check the imputations:
densityplot(miceOut)

## Generate the synthetic data via imputed values:
synDat <- complete(miceOut, 1)

###-Post Processing----------------------------------------------------------###

## Create scale scores:
keys <- with(items,
             list(disillusion = paste0("d", 1:length(disillusion)),
                  success     = paste0("s", 1:length(success))
                  )
             )
scores <- scoreVeryFast(keys = keys, items = synDat)

## Aggregate processed variables:
dat2 <- data.frame(synDat, scores)

## Save the processed data:
saveRDS(dat2, paste0(dataDir1, "outlook.rds"))

