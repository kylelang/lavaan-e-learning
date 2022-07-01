### Title:    Lavaan Summer School Exercise Prep
### Author:   Kyle M. Lang
### Created:  2022-07-01
### Modified: 2022-07-01

rm(list = ls(all = TRUE))

library(lavaan, lib = "~/R/manual_installs/")
library(dplyr)
library(MBESS)
library(semTools)
                                        #install.packages("MBESS", repos = "http://cloud.r-project.org")

dataDir <- "../data/sandbox/"

data(HolzingerSwineford1939)

?HolzingerSwineford1939

data(HS, package = "MBESS")

HS %>% is.na() %>% colMeans()

table(HS$school)
table(HS$sex)
table(HS$grade)

dat1 <- HS %>% mutate(across(where(is.numeric), scale))


###-Measurement Model--------------------------------------------------------###

## Differentiate the subscale indicators in the HS data:
items <- list(
    spatial = c("t1_visual_perception", "t2_cubes", "t3_paper_form_board", "t4_lozenges"),
    verbal  = c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence", "t8_word_classification", "t9_word_meaning"),
    speed   = c("t10_addition", "t11_code", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals"),
    memory  = c("t14_word_recognition", "t15_number_recognition", "t16_figure_recognition", "t17_object_number", "t18_number_figure", "t19_figure_word"),
    math    = c("t20_deduction", "t21_numerical_puzzles", "t22_problem_reasoning", "t23_series_completion", "t24_woody_mccall")
)

## Define univariate measurement model syntax:
mods <- list()
for(lv in names(items))
    mods[[lv]] <- paste(lv,
                        paste0(items[[lv]], collapse = " + "),
                        sep = " =~ ")

## Fit the univariate CFA models:
fits <- lapply(mods, cfa, data = dat1, std.lv = TRUE)

## Check fits:
lapply(fits, fitMeasures)
lapply(fits, summary)

## Fit the multidimensional CFA:
fullMod <- paste(mods[-3], collapse = "\n")
fullFit <- cfa(fullMod, data = dat1, std.lv = TRUE)

fitMeasures(fullFit)
summary(fullFit)


###-Mediation----------------------------------------------------------------###

medMod1 <- paste(mods$spatial,
                 mods$math,
                 "spatial ~ b * math + c * age",
                 "math ~ a * age",
                 "ab := a * b",
                 sep = "\n")

sem(medMod1, data = dat1, std.lv = TRUE) %>% summary()

###--------------------------------------------------------------------------###

medMod2 <- paste(mods$spatial,
                 mods$math,
                 mods$verbal,
                 "spatial ~ b1 * math + b2 * verbal + c * age",
                 "math ~ a1 * age",
                 "verbal ~ a2 * age",
                 "ab1 := a1 * b1",
                 "ab2 := a2 * b2",
                 "ie := ab1 + ab2",
                 sep = "\n")

sem(medMod2, data = dat1, std.lv = TRUE) %>% summary()


###-Measurement Invariance---------------------------------------------------###

group <- "grade"

configFit <- cfa(fullMod, data = dat1, std.lv = TRUE, group = group)

fitMeasures(configFit)
summary(configFit)

weakMod   <- measEq.syntax(configFit, group.equal = "loadings") %>% as.character()
strongMod <- measEq.syntax(configFit, group.equal = c("loadings", "intercepts")) %>% as.character()

weakFit   <- cfa(weakMod, data = dat1, std.lv = TRUE, group = group)
strongFit <- cfa(strongMod, data = dat1, std.lv = TRUE, group = group)

summary(weakFit)
summary(strongFit)

compareFit(configFit, weakFit, strongFit) %>% summary()

as.character(weakMod) %>% cat()

