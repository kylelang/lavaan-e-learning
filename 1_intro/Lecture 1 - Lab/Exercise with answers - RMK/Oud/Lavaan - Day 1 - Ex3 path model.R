
# Regression #

#The lavaan package is available on CRAN. 
#Therefore, to install lavaan, simply start up R (studio), and type in the R console:
if (!require("lavaan")) install.packages("lavaan", dependencies = TRUE) # install this package first (once)
#You can check if the installation was succesful by typing
library(lavaan) 
#A startup message will be displayed showing the version number (always report this in your papers), 
#and a reminder that this is free software. 
#If you see this message, you are ready to start.



# Number of parameters #
#
# The parameters you expect to be estimated: 
#•	2 x variances (harsh, just)
#•	1 x covariance (harsh with just)
#•	3 x regression paths 
#•	2 x residual variances (reject and maladj) 
#•	2 x means (just and harsh, if means are available in the data)
#•	2 x intercepts (reject and maladj, if means are available in the data)
# Thus, 8 in total without means or 12 with means.



# Data
#
# Note:
# If you have no full dataset, but you do have a sample covariance matrix, you can still fit your model. 
# If you wish to add a mean structure, you need to provide a mean vector too. 
# Importantly, if only sample statistics are provided, 
# you must specify the number of observations that were used to compute the sample moments ('sample.nobs = '). 
# The following example illustrates the use of a sample covariance matrix as input ('sample.cov = ').
#
# Read in data: a covariance matrix
lower <- scan("CorPun.txt")
CovMx <- getCov(lower, names = c("harsh", "just", "reject", "maladj"))


# Specify the path model
#
#Now, let us run the model 
#- predicting psychological maladjustment by perceived rejection. 
#- predicting rejection by perceived harshness and perceived justness. 

# Our path model is specified as follows:
model.path <- '
  
  # regressions
  maladj ~ reject 
  reject ~ harsh + just
  
  # residual variances
  reject ~~ reject 
  maladj ~~ maladj
  #
  # variances and covariances of predictors # *
  harsh + just ~~ harsh
  just ~~ just
  
  ## intercepts - if we would have mean value as input too
  ## or use meanstructure=TRUE in the lavaan function
  #maladj ~ 1
  #reject ~ 1 
'
# *: This code is not needed to estimate the parameters, but it is needed
#    such that lavaan will count these parameters correctly (see below).
#
# Fit the regression model using the lavaan function
fit_path <- lavaan(model = model.path, 
                       sample.cov = CovMx, # Now, using a covariance matrix as input
                       sample.nobs = 175) # In that case, the sample size is needed as well
#
# Output
summary(fit_path, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)
# - Note that the residual variances are denoted by stating a "." before the 
#   variable name. Here: .reject and .maladj
#   The variances are denoted without a ".". Here: harsh and just
# - Part of output: 
# The chi-square statistic and its degrees of freedom:
#Model Test User Model:
#Test statistic                                 1.546
#Degrees of freedom                                 2
#P-value (Chi-square)                           0.462
#
# Parameter estimates
#coef(fit_path)                 # unstandardized regression parameters
#parameterEstimates(fit_path)   # all unstandardized parameters - with extra information
#standardizedSolution(fit_path) # all standardized parameters - with extra information
#
# Fit measures
fitMeasures(fit_path)   # many model fit measures 
# Ask for specific model fit measures:
#fitMeasures(fit_path, c("cfi", "tli", "rmsea","srmr")) 
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
#inspect(fit_path, 'r2') # R-square for all dependent variables
#
# Technical output
lavInspect(fit_path) # Comparable to TECH1 in Mplus
# This returns a list of the model matrices that are used internally to represent the model.
# 
# Which parameter is listed in which matrix:
# The regression coefficients belong in the Beta matrix, 
# the variances and residual variances in the Psi matrix. # If you specified them!
#
# Note (related to * above): 
# When there is no specification of the variances of harsh and just and their covariance, 
# the technical output (here, Psi) indicates that these 3 parameters are not estimated. 
# Like in Mplus, this does not affect the degrees of freedom and thus the test result!
# In case you want to check, run:
model.path_check <- '
  
  # regressions
  maladj ~ reject 
  reject ~ harsh + just
  
  # residual variances
  reject ~~ reject 
  maladj ~~ maladj
  #
  # Check: Not specifying these variances and covariances
  # variances and covariances of predictors # *
  #harsh + just ~~ harsh
  #just ~~ just
  
  ## intercepts - if we would have mean value as input too
  ## or use meanstructure=TRUE in the lavaan function
  #maladj ~ 1
  #reject ~ 1 
'
fit_path_check <- lavaan(model = model.path_check, 
                   sample.cov = CovMx, # Now, using a covariance matrix as input
                   sample.nobs = 175) # In that case, the sample size is needed as well
summary(fit_path_check, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)
#
# Hence, always cross check that lavaan has understood what you wanted it to do 
# by looking at the model degrees of freedom, the technical output 
# (even the starting values) and model results. 
# When these pieces of information do not contradict each other and 
# lavaan gives you exactly the output you expect, 
# then you can move forward with interpreting the model effects. 



# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit_path, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_path) # make sure that in Rstudio your 'plot box' is wide enough
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit_path, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)
