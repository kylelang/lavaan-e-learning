#library(devtools)

install.packages(c("devtools",
                   "lavaan",
                   "semTools", 
                   "mvtnorm",
                   "mice", 
                   "miceadds", 
                   "ggplot2", 
                   "dplyr",
                   "magrittr",
                   "MBESS", 
                   "bookdown",
                   "ggmice",
                   "naniar"), 
                 dependencies = TRUE,
                 repos = "http://cloud.r-project.org")

#devtools::install_github("https://github.com/yrosseel/lavaan", 
#                         dependencies = TRUE, 
#                         lib = "~/R/manual_installs")
