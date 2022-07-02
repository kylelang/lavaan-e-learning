### Title:    Lavaan Summer School: Process Enders (2010) Eating Attitudes Data
### Author:   Kyle M. Lang
### Created:  2022-07-01
### Modified: 2022-07-02

rm(list = ls(all = TRUE))

set.seed(235711)

library(dplyr)
library(magrittr)
library(ggmice)
library(lavaan, lib.loc = "~/R/manual_installs/")
library(semTools)
library(mvtnorm)

dataDir <- "../data/"

## Read in the data:
eat <- read.table(paste0(dataDir, "eatingattitudes.dat"))

## Assign columns names:
colnames(eat) <- c("id",
                   "eat1",
                   "eat2",
                   "eat10",
                   "eat11",
                   "eat12",
                   "eat14",
                   "eat24",
                   "eat3",
                   "eat18",
                   "eat21",
                   "bmi",
                   "wsb",
                   "anx")

## Code missing values:
eat[eat == -99] <- NA

sapply(eat, range, na.rm = TRUE)

## Save the data as RDS:
saveRDS(eat, paste0(dataDir, "eating_attitudes.rds"))

eat %>% select(-id) %>% cov(use = "pairwise") %>% eigen()

eat %>% is.na() %>% colMeans()

tmp <- plot_pattern(eat)


flag <- (eat %>% is.na() %>% rowSums()) > 0

sum(flag)

tmp <- eat[flag, ][sample(1:sum(flag), 400, TRUE), ]

eat <- rbind.data.frame(eat[!flag, ], tmp)

colMeans(is.na(eat))

## Check measurement structure:
cfaMod <- paste(
    paste('drive',
          paste(colnames(eat)[2:8], collapse = ' + '),
          sep = ' =~ '),
    paste('pre',
          paste(colnames(eat)[9:11], collapse = ' + '),
          sep = ' =~ '),
    sep = '\n'
)

cfaMod <- '
drive =~ eat1 + eat2 + eat10 + eat11 + eat12 + eat14 + eat24
obsess =~ eat3 + eat18 + eat21
'

sum(is.na(eat$bmi))

## Naive fit:
fit0 <- cfa(cfaMod, data = eat, std.lv = TRUE, missing = "fiml")

fitMeasures(fit0)
summary(fit0)

## Fit with auxiliaries:
fit1 <- cfa.auxiliary(cfaMod,
                      data = eat,
                      aux = c("bmi", "anx", "wsb"),
                      std.lv = TRUE,
                      test = "yuan.bentler")

?lavaan
?lavOptions

fitMeasures(fit1)
summary(fit1)

theta <- lavInspect(fit1, "theta")
tmp <- lavInspect(fit1, "est")

theta <- tmp$theta
lambda <- tmp$lambda
psi <- tmp$psi

sigma1 <- lambda %*% psi %*% t(lambda) + theta
sigma2 <- lavInspect(fit1, "sigma")

sigma1 - sigma2

?lavInspect

## Are the covariances legal?
v   <- diag(theta)
tmp <- theta["bmi", ] %>% head(10)

(abs(tmp) <= sqrt(head(v, 10) * v["bmi"])) %>% all()


## Try to synthesize the data:
dat1 <- rmvnorm(1000, stats$mean, stats$cov)

## Naive fit:
fit0 <- cfa(cfaMod, data = dat1, std.lv = TRUE)

fitMeasures(fit0)
summary(fit0)

## Fit with auxiliaries:
fit1 <- cfa.auxiliary(cfaMod, data = eat, aux = "bmi", std.lv = TRUE)
summary(fit1)

theta <- lavInspect(fit1, "theta")

eigen(theta)

## Load the full dataset from the MBESS package: 
data(HS, package = "MBESS")

## Do a bit of minor processing:
HS %<>% select(-matches("^t25_|^t26_|month|age$")) %>%
    mutate(grade = as.factor(grade)) %>%
    rename(age = age_years)

## Standardize the scale items:
tmp  <- HS %>% select(-id, -age) %>% mutate(across(where(is.numeric), scale))
dat1 <- HS %>% select(id, age) %>% data.frame(tmp)

## Differentiate the subscale indicators in the HS data:
items0 <- list(
    spatial = c("t1_visual_perception",
                "t2_cubes",
                "t3_paper_form_board",
                "t4_lozenges"),
    verbal  = c("t5_general_information",
                "t6_paragraph_comprehension",
                "t7_sentence",
                "t8_word_classification",
                "t9_word_meaning"),
    speed   = c("t10_addition",
                "t11_code",
                "t12_counting_groups_of_dots",
                "t13_straight_and_curved_capitals"),
    memory  = c("t14_word_recognition",
                "t15_number_recognition",
                "t16_figure_recognition",
                "t17_object_number",
                "t18_number_figure",
                "t19_figure_word"),
    math    = c("t20_deduction",
                "t21_numerical_puzzles",
                "t22_problem_reasoning",
                "t23_series_completion",
                "t24_woody_mccall")
)

## Give some more succinct column names:
items <- lapply(names(items0),
                function(x, y) paste0(x, 1:length(y[[x]])),
                y = items0)
names(items) <- names(items0)

for(i in 1:length(items))
    for(j in 1:length(items[[i]]))
        colnames(dat1) <- gsub(items0[[i]][j], items[[i]][j], colnames(dat1))

## Save the processed data:
saveRDS(dat1, paste0(dataDir, "holzinger_swineford.rds"))
