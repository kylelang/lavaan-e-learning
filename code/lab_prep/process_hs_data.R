### Title:    Lavaan Summer School: Process Holzinger & Swineford Data
### Author:   Kyle M. Lang
### Created:  2022-07-01
### Modified: 2022-07-03

rm(list = ls(all = TRUE))

library(dplyr)
library(magrittr)

dataDir <- "../../data/"

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

## Covert subscale items back to numeric vectors:
targets          <- unlist(items)
dat1[ , targets] <- sapply(dat1[targets], as.numeric)

## Save the processed data:
saveRDS(dat1, paste0(dataDir, "holzinger_swineford.rds"))

## Save the subscale item names:
saveRDS(list(new = items, old = items0),
        paste0(dataDir, "hs_subscale_item_names.rds")
        )
