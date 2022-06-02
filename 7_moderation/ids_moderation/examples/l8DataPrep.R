### Title:    Lecture 8 Data Processing
### Author:   Kyle M. Lang
### Created:  2017-AUG-24
### Modified: 2018-SEP-20

rm(list = ls(all = TRUE))

install.packages("rockchalk", repos = "http://cloud.r-project.org")

library(DAAG)
library(mice)

source("../../../code/supportFunctions.R")

plotDir <- "../latexStuff/figures/"
dataDir <- "../data/"

data(socsupport)
socSup <- socsupport

head(socSup)
sapply(socSup, class)

miceOut <- mice(socSup, m = 1, maxit = 10)
socSup  <- complete(miceOut, 1)

colnames(socSup) <- gsub("BDI", "bdi", colnames(socSup))
colnames(socSup) <- gsub("gender", "sex", colnames(socSup))
colnames(socSup) <- gsub("tangiblesat", "tanSat", colnames(socSup))

saveRDS(socSup, paste0(dataDir, "social_support.rds"))
