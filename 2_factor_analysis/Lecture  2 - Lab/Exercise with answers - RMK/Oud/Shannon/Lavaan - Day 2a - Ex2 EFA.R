
# Regression #

#The lavaan package is available on CRAN. 
#Therefore, to install lavaan, simply start up R (studio), and type in the R console:
if (!require("lavaan")) install.packages("lavaan", dependencies = TRUE) # install this package first (once)
#You can check if the installation was succesful by typing
library(lavaan) 
#A startup message will be displayed showing the version number (always report this in your papers), 
#and a reminder that this is free software. 
#If you see this message, you are ready to start.



# Data
#
# Read in data
data_pop <- read.table("popular_factor.txt", header = T)
#
# Denote missing values
data_pop[sapply(data_pop, function(x) as.character(x) %in% c("99") )] <- NA 


# Correlations for the items of interest
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
res <- rcorr(as.matrix(data_pop)) # rcorr() accepts matrices only
round(res$r, 3) # Correlation matrix (rounded to 3 decimals)

# TO DO first item is called 'ï..c1' instead of c1, so:
colnames(data_pop)[1] <- "c1"



# EFA

# one-factor EFA
model.1EFA <- "
 efa('block1')*F1  =~ c1 + c2 + c3 + o1 + o2 + o3
"
fit_1EFA <- cfa(model.1EFA, data=data_pop,
                missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_1EFA)
fitMeasures(fit_1EFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr")) 
#lavInspect(fit_1EFA)

# TO DO not same factor loadings as in Mplus - fit etc is the same


# two-factor EFA
model.2EFA <- "
 efa('block1')*F1  =~ c1 + c2 + c3 + o1 + o2 + o3
 efa('block1')*F2  =~ c1 + c2 + c3 + o1 + o2 + o3
"
fit_2EFA <- cfa(model.2EFA, data=data_pop,
                missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_2EFA)
fitMeasures(fit_2EFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr"))
#One can conclude that the 2-factor structure fits well to the data:
#Chi-Square Test of Model Fit is not significant, 
#CFI/TLI is  > 0.95, and 
#RMSEA is < 0.05.
#
#lavInspect(fit_2EFA)

# TO DO not same factor loadings as in Mplus - fit etc is the same


# Compare the one-factor model to the two-factor model
# Option 1
anova(fit_1EFA, fit_2EFA)
#The Chi-square difference test is significant,
# implying that the null stating that two factors equal one and the same
# is rejected. 
#The AIC and BIC for the 2-factor model is lower than for the 1-factor model.
#Hence, we prefer the 2-factor model.
#
# Option 2
if (!require("performance")) install.packages("performance", dependencies = TRUE)
library(performance)
performance::compare_performance(fit_1EFA, fit_2EFA)
#
# There are more options; 
# e.g., see the following link where fit measures are compared:
# https://solomonkurz.netlify.app/post/2021-05-11-yes-you-can-fit-an-exploratory-factor-analysis-with-lavaan/



# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit_2EFA, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_2EFA)
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit_2EFA, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)




  


##############################################


# Helpful notation in factor analysis
# =~ indicator, used for latent variable to observed indicator in factor analysis measurement models
# ~~ covariance
# ~1 intercept or mean (e.g., q01 ~ 1 estimates the mean of variable q01)
# 1* fixes parameter or loading to one
# NA* frees parameter or loading (useful to override default marker method)
# a* labels the parameter ‘a’, used for model constraints



# lavaan notation
formula type                operator  mnemonic
regression                  ~         is regressed on
(residual) (co)variance     ~~        is correlated with
intercept                   ~ 1       intercept
latent variable definition  =~        is measured by    
# Note:
# ~ predict, used for regression of observed outcome to observed predictors
# =~ indicator, used for latent variable to observed indicator in 
#                              factor analysis measurement models


# General lavaan specification

# regressions
y1 + y2 ~ f1 + f2 + x1 + x2
f1 ~ f2 + f3
f2 ~ f3 + x1 + x2

# latent variable definitions 
f1 =~ y1 + y2 + y3 
f2 =~ y4 + y5 + y6 
f3 =~ y7 + y8 + y9 + y10

# variances and covariances 
y1 ~~ y1 
y ~~ y2 
f1 ~~ f2

# intercepts
y1 ~ 1 
f1 ~ 1



