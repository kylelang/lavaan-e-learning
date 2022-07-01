
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



# CFA

# one-factor CFA
model.1CFA <- "
 anti  =~ NA*c1 + c2 + c3 + o1 + o2 + o3
 anti ~~ 1*anti
"
fit_1CFA <- cfa(model.1CFA, data=data_pop,
                missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_1CFA)
fitMeasures(fit_1CFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr")) 
#lavInspect(fit_1CFA)


# two-factor CFA
model.2CFA <- "
 covert  =~ NA*c1 + c2 + c3
 overt   =~ NA*o1 + o2 + o3
 covert ~~ 1*covert
 overt ~~ 1*overt
"
fit_2CFA <- cfa(model.2CFA, data=data_pop,
                missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_2CFA)
fitMeasures(fit_2CFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr"))
#lavInspect(fit_2CFA)


# Compare the one-factor model to the two-factor model
anova(fit_1CFA, fit_2CFA)
#The Chi-square difference test is significant,
# implying that the null stating that two factors equal one and the same
# is rejected. 
#The AIC and BIC for the 2-factor model is lower than for the 1-factor model.
#Hence, we prefer the 2-factor model.
#
fitMeasures(fit_1CFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr"))
fitMeasures(fit_2CFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr"))
#Fit measures are better for the 2-factor CFA model.
#Additionally, one can conclude that the 2-factor structure fits the data well:
#Chi-Square Test of Model Fit is not significant, 
#CFI/TLI is  > 0.95, and 
#RMSEA is < 0.05.


#The correlation between the two factors
#Covariances:
#  Estimate  Std.Err  z-value  P(>|z|)
#covert ~~                                           
#  overt             0.431    0.055    7.806    0.000
#The correlation between OVERT and COVERT is 0.431 
#with a standard error of 0.055. 
#So, the proportion of shared variance is 0.4312^2 = 0.186 = 18,6%. 
#Note that in the output one can find this in the ‘standardized results’. 
#
#Whether the unstandardized ‘covert ~~ overt’ can be interpreted as 
#a correlation or a covariance, depends on how the latent variable was scaled. 
#If the factor variances are fixed to 1 
#(i.e., when scaling is done via the factor variances), 
#then the ‘unstandardized’ result reflects the factor correlation, 
#otherwise they reflect the covariance.



# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit_2CFA, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_2CFA)
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit_2CFA, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
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



