### Title:    Prep Data for Missing Data Lecture
### Author:   Kyle M. Lang
### Created:  2022-06-14
### Modified: 2022-06-14

rm(list = ls(all = TRUE))

library(dplyr)
library(magrittr)
library(mice)

source("../../code/sim_missing/code/simMissingness.R")

## Load the "bfi" data from the psych package:
data(bfi, package = "psych")

## Recode some variables:
bfi %<>% mutate(male = as.numeric(gender == 1),
                sex = factor(gender, levels = 1:2, labels = c("male", "female")),
                educ = factor(education,
                              levels = 1:5,
                              labels = c("some_hs",
                                         "finished_hs",
                                         "some_college",
                                         "finished_college",
                                         "graduate_degree")
                              )
                ) %>%
    select(-gender, -education)

## Check:
head(bfi)
str(bfi)

###--------------------------------------------------------------------------###

## Define a predictor matrix:
preds <- quickpred(bfi, exclude = "male", mincor = 0.2)

## Impute the extant missing data quick-and-dirty:
bfi0 <- bfi <- mice(bfi, m = 1, maxit = 100, predictorMatrix = preds, seed = 235711) %>%
    complete(1)

###--------------------------------------------------------------------------###

## Define MAR predictors and incomplete variables:
preds   <- c(paste0("O", 1:5), "age", "male")
targets <- grep("A\\d|C\\d|E\\d|N\\d", colnames(bfi), value = TRUE)

for(v in targets) {
    ## Define weights for MAR predictors:
    beta  <- c(runif(6, -0.6, -0.4), runif(1, 0.5, 0.75))

    ## Define missingness vector:
    m <- simLogisticMissingness0(data    = bfi,
                                 pm      = 0.3,
                                 preds   = preds,
                                 beta    = beta,
                                 type    = sample(c("high", "low", "center", "tails"), 1),
                                 stdData = TRUE)$r
    
    ## Punch holes in the target variable:
    bfi[m, v] <- NA
}

## Check:
bfi %>% is.na() %>% colMeans()

## Save the result:
saveRDS(list(complete = bfi0, incomplete = bfi), "../data/bfi_datasets.rds") 
