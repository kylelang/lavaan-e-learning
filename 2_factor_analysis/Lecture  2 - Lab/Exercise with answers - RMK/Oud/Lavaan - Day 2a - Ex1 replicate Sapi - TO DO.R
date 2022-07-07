
# EFA and CFA # # TO DO in all files, if Shannon did not correct it

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
data_sapi <- read.table("Sapi.txt", header = T)
#
# Denote missing values
# -999 is missing, so set this to NA:
data_sapi[sapply(data_sapi, function(x) as.character(x) %in% c("-999") )] <- NA 
#If you ‘forget’ to tell that -999 is used to denote missing data, 
#these values will be treated as being observed. 



# Correlations for the items of interest: Q77 Q84 Q170 Q196
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
res <- rcorr(as.matrix(data_sapi[, c(9:10, 12:13)])) # rcorr() accepts matrices only
round(res$r, 3) # Correlation matrix (rounded to 3 decimals)
#
#The correlation table gives the correlations between pairs of items, that is,
#the standardized covariances between a pair of items, 
#equivalent to running covariances on the Z-scores of each item. 
#Bear in mind that the correlation for Items X and Y is the same as for Y and X.
#The diagonal elements are always one 
#because an item is always perfectly correlated with itself. 
#Recall that the magnitude of a correlation is determined by 
#the absolute value of the correlation. 
#From this table, we see that the items have magnitudes ranging from 0.2 to 0.6.
#In psychology and the social sciences, the magnitude of correlations above 0.30 
#is considered a medium effect size. 
#Due to relatively high correlations among many of the items, 
#these data would be a good candidate for factor analysis. 
#
#The goal of factor analysis is to model the interrelationships between 
#many items with fewer unobserved or latent variables.



# Reflective (confirmatory) Factor Model
#
#With factor analysis you can determine the extent to which items designed 
#to measure a particular factor (in this case extraversion) actually do so. 
#It is the ‘true’ correlation between an indicator and a factor.
#So, every factor (in this case Extraversion) is a weighted sum of the items.
#
# In lavaan, the operator for the latent variable definition is: =~
# This, thus, denotes 'is measured by' (cf., BY in Mplus).
#
# Model statement: one factor, four items, default marker method
model.CFA <- '
 Extraversion =~ Q77 + Q84 + Q170 + Q196
'
#Recall that =~ represents the indicator equation where 
#the latent variable is on the left and 
#the indicators (or observed variables), separated by + signs, 
#                                 are to the right the symbol.
#Note that, for the latter, the names come from the data set.

# Fit model
fit_CFA <- cfa(model.CFA, data=data_sapi,
               missing='fiml', fixed.x=F) # Specify FIML 
#Runs a confirmatory factor analysis using the cfa function, 
#which is actually a wrapper for the lavaan function (we used on Day 1).

# Output
summary(fit_CFA) 
#summary(fit_CFA, fit.measures=TRUE)
#This requests textual output, listing for example 
# the estimator used, 
# the number of free parameters, 
# the test statistic, 
# estimated means, 
# loadings, and 
# variances.


# Scaling
# Technical output
lavInspect(fit_CFA)  # Comparable to TECH1 in Mplus
# From this one can see what type of scaling is used.
# Because of:
#$lambda
#Extrvr
#Q77       0
#Q84       1
#Q170      2
#Q196      3
#one can see that the first factor loading is fixed (to 1).
# Hence, the marker variable method is used.
#
#By default, lavaan chooses the marker method.
#To use the 'var std' method (i.e., reference group scaling),
#one should make use of the following:
#- To free a parameter, put NA* in front of the parameter to be freed:
#  The syntax NA*Q77 frees the loading of the first item 
#  because by default marker method fixes it to one.
#- To fix a parameter to 1, put 1* in front of the parameter to be fixed. 
#   The syntax Extraversion ~~ 1*Extraversion means to fix the variance 
#   of the factor Extraversion to one.
model.CFA_RefGr <- '
 Extraversion =~ NA*Q77 + Q84 + Q170 + Q196
 Extraversion ~~ 1*Extraversion
'
fit_CFA_RefGr <- cfa(model.CFA_RefGr, data=data_sapi,
               missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_CFA_RefGr)
#Alternatively you can use std.lv=TRUE and obtain the same results.
#
# Note that the chi-square fit results are exactly the same across the models,
# as will be explained below. 
#That is why both solutions can be asked for by using 'std.lv=TRUE'.
#
# For better interpretation of the factor loadings, 
# you would request the standardized solutions. 
# Therefore, one may want to request the summary of the marker method 
# but also specify that standardized=TRUE;
# Then you obtain both results:
summary(fit_CFA, standardized=TRUE)
# Now, there are two additional columns: Std.lv and Std.all 
#                                      (cf. STD and STDYX in Mplus).
# Std.all: standardizes the factor loadings by the standard deviation 
#          of both the predictor (the factor, X) and the outcome (the item, Y). 
# Std.lv:  standardizes the factor loadings by the predictor (the factor, X). 
#Comparing the two: The loadings and variance of the factors are different 
# but the residual variances are the same. 
#
#
# In case you want, in the Marker variable method, 
# another factor loading to be 1, say, the last one:
model.CFA_last <- '
 Extraversion =~ NA*Q77 + Q84 + Q170 + 1*Q196
'
#Notes:
# 1* fixes parameter or loading to one
# NA* frees parameter or loading (useful to override default marker method)
fit_CFA_last <- cfa(model.CFA_last, data=data_sapi,
                     missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_CFA_last)
#
#
# Table with results for the three scaling methods:
#                                               Factor loading          Factor
#Scaling Method	        Chi-square, df, p-value	Q77	  Q84	  Q170  Q196	variance
#Reference Group 	      57.075,     2,  <0.00	  0.835	0.591	0.473	0.619	1
#Marker Variable	      57.075,     2,  <0.00	  1	    0.708	0.567	0.742	.696
#Marker Variable Bonus	57.075,     2,  <0.00	  1.347	0.953	0.764	1	    .384
#
#The first thing you may notice is that all chi-square fit results are 
#exactly the same across the models. This is because the different ways of 
#scaling all result in equivalent statistical models. You don’t estimate 
#anything more or less with any of the three scaling methods.
#
#The second thing you may notice is that the values of the loadings have 
#changed. However, loadings that are relatively large (or small) in one model, 
#are also relatively large (or small) in the other models. For example, in each 
#model, the loading for Q77 is ~1.4 times as large as the loading of Q84.
#
#The take home message is that it does not matter how you scale, 
#the information that you get is the same. 	



## Fit measures
#summary(fit_CFA, fit.measures=TRUE) # many model fit measures
#fitMeasures(fit_CFA) # many model fit measures
# Four of them are:
# - Model chi-square is the chi-square statistic obtained from 
#   the maximum likelihood statistic. 
#   (in lavaan: 'the Test Statistic for the Model Test User Model')
# - CFI is the Comparative Fit Index.
#   Values can range between 0 and 1 
#   (values greater than 0.90, conservatively 0.95 indicate good fit)
# - TLI Tucker Lewis Index which also ranges between 0 and 1 
#   (if it’s greater than 1 it should be rounded to 1) 
#   with values greater than 0.90 indicating good fit. 
# If the CFI and TLI are less than one, the CFI is always greater than the TLI.
# - RMSEA is the root mean square error of approximation. 
#   In lavaan, you also obtain a p-value of close fit, that the RMSEA < 0.05. 
#   If you reject the model, it means your model is not a close fitting model.
#
# Notes: 
# - Failing to reject the model is good for the model at hand
#   because it implies failing to disprove that the model is bad.
#   Here, we reject the null hypothesis that the model fits the data. 
#   It is well documented in CFA and SEM literature that the chi-square is 
#   often overly sensitive in model testing especially for large samples. 
#   David Kenny states that for 
#   - models with 75 to 200 cases chi-square is a reasonable measure of fit, 
#   - but for 400 cases or more it is nearly almost always significant.
#   Our sample size is 1000.
#   Kline (2016) notes the N:q rule, which states that 
#   the sample size (N) should be determined by 
#   the number of parameters in your model (q), 
#   and the recommended ratio is 20:1. 
#   This means that if you have 12 parameters, you should have N=240. 
#   A sample size less than 100 is almost always untenable according to Kline.
# - The larger the chi-square value the larger the difference between 
#   the sample implied covariance matrix and the sample observed one, 
#   and the more likely you will reject your model. 
#
fitMeasures(fit_CFA, c("cfi", "tli", "rmsea","srmr")) # ask for specific model fit measures
#  cfi   tli rmsea srmr 
#0.934 0.803 0.166 0.040
#For CFI, value slightly below the recommended cut-off criterion of 0.97/0.95. 
#For TLI, value below the recommended cut-off criterion of 0.97/0.95.
#The RMSEA is above the cut-off of 0.05. 
#The SRMR points to a good model fit (SRMR < 0.08).
# Overall, the model does not seem to fit well.


# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit_CFA, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_CFA)
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit_CFA, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)



# Test equality of factor loadings
#
#Test whether factor loadings of Q84 and Q196 are the same.
#
# Model specification
model.CFA_equal <- '
 # model with labeled parameters
 Extraversion =~ Q77 + a*Q84 + Q170 + b*Q196
'
#
# Fit model
fit_CFA_equal <- cfa(model.CFA_equal, data=data_sapi,
                     missing='fiml', fixed.x=F) # Specify FIML 
#
# Wald test
lavTestWald(fit_CFA_equal, constraints = 'a == b')
# Wald test with df=1 is 0.3619674, p = 0.5474156. 
# Thus, we do not reject the null that the two loadings are equal.
# Hence, we conclude that there is no evidence for a difference between a and b.
#
#
# In case one wants the model results for which the loadings are the same:
#
# Model specification
model.CFA_equal <- '
 # model with labeled parameters
 Extraversion =~ Q77 + a*Q84 + Q170 + b*Q196
 #
 # constraints
 a == b
'
#Note: a* labels the parameter ‘a’, used for model constraints.
#
# Fit model
fit_CFA_equal <- cfa(model.CFA_equal, data=data_sapi,
                     missing='fiml', fixed.x=F) # Specify FIML 
#
# Output
summary(fit_CFA_equal, standardized=TRUE, fit.measures=TRUE)
fitMeasures(fit_CFA_equal, c("cfi", "tli", "rmsea","srmr"))
# The measures did improve, but do model still does not fit well.



# two-factor EFA versus CFA
#
# Correlations for the items of interest: Q44 Q63 Q76 Q77 Q84 Q98 Q170 Q196
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
res <- rcorr(as.matrix(data_sapi[, c(6:13)])) # rcorr() accepts matrices only
round(res$r, 3) # Correlation matrix (rounded to 3 decimals)
#
# two-factor EFA
model.2EFA <- "
 efa('block1')*Having fun  =~ Q77 + Q84 + Q170 + Q196 + Q44 + Q63 + Q76 + Q98
 efa('block1')*Being liked =~ Q77 + Q84 + Q170 + Q196 + Q44 + Q63 + Q76 + Q98
"
# Or:
#model.2EFA <- "
# efa('block1')*Having fun  + efa('block1')*Being liked =~ 
# Q77 + Q84 + Q170 + Q196 + Q44 + Q63 + Q76 + Q98
#"
#The efa('block1') modifier indicates that the factors comprise an exploratory 
# block (with the name block1) and thus that we do not exactly know the loading 
# structure of the variables / items on these factors.
#
fit_2EFA <- cfa(model.2EFA, data=data_sapi,
                missing='fiml', fixed.x=F) # Specify FIML 
# estimates the EFA model 
# (releasing the residual variances and implying the EFA constraints)
# using the oblique geomin rotation.
#
summary(fit_2EFA)
lavInspect(fit_2EFA)
#
# TO DO not same factor loadings as in Mplus + two warnings (not in Mplus)
# TO DO ms andere estimator (estimator = "WLSMV",) of andere rotatie (rotation = "oblimin",) of wel/niet ordered (ordered = TRUE)
# TO DO Dat lijkt het allemaal niet te zijn. En als geen FIML dan ook andere Chi2! Ws dan toch rotatie, nog andere?
# TO DO ms ligt  het aan: 
#by default, Mplus provides a geomin rotated solution, where geomin is an oblique type of rotation; so, the correlations between the factors are given in the output. 
# In techn output lavaan en in estimates zie ik geen correlaties!
#
# two-factor CFA
model.2CFA <- "
 Having fun  =~ Q77 + Q84 + Q170 + Q196
 Being liked =~ Q44 + Q63 + Q76 + Q98
"
fit_2CFA <- cfa(model.2CFA, data=data_sapi,
                missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_2CFA)
#lavInspect(fit_2CFA)



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
