
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
data_multivar <- read.table("CDSsummerschool.txt", header = T)
#
# Denote missing values
# -999 is missing, so set this to NA:
data_multivar[sapply(data_multivar, function(x) as.character(x) %in% c("-999") )] <- NA 



# Descriptive statistics of variables of interest: APst02, Problems, Selfesteem, LWst02, DS02
# colnames(data_multivar) # so, first 5 columns
if (!require("psych")) install.packages("psych", dependencies = TRUE)
library(psych) # To obtain some extended summary statistics
# Descriptives like mean, median, min, and max
describe(data_multivar[,1:5])
#
# Correlations for the five variables of interest, incl correlation significance test
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
res <- rcorr(as.matrix(data_multivar[, 1:5])) # rcorr() accepts matrices only
round(res$r, 3) # Correlation matrix (rounded to 3 decimals)
round(res$P, 3) # p-values (rounded to 3 decimals)
res$n # sample size



# Specify the regression model
#
#Now, let us run a multivariate regression model predicting 
#Applied problems (APst02), Behavioral problems (Problems), and Self-esteem (Selfesteem), 
#using the predictors digit span (DS02) and letters words (LWst02) . 
#
#To run a multivariate regression with lavaan, you first specify the model, then fit the model and finally acquire the summary. 
#To fit the model we use the lavaan() function, which needs a model= and a data= input.
#
# The model specification is comparable to the multiple regression model in exercise 1,
# but now there are multiple dependent variables.
# One does not have to specify the regression model for each dependent variable,
# but one can state the dependent variables separated by the + sign:
#   dependent1 + dependent2 + dependent3 ~ predictor1 + predictor2 # + etc
# The ~ is like the = in a regression equation. 
# Before the ~, one states the dependent variables, each separated by the + sign.
# After the ~, one states the predictors, each separated by the + sign.
# Additionally, one needs to specify that the error (of an dependent variable) has a variance:
#   dependent1 ~~ dependent1 
# And now one also has to specify the residual covariances:
# dependent1 ~~ dependent2
# To specify all the combinations more easily use:
#   dependent1 + dependent2 + Selfesteem ~~ dependent1 
#   dependent2 + dependent3 ~~ dependent2
#   dependent3 ~~ dependent3
# If needed (as is the case here), one can specify to include intercepts:
#   dependent1 + dependent2 + dependent3 ~ 1
#
# Our regression model is specified as follows:
model.multivar <- '
  
  # regressions
  APst02 + Problems + Selfesteem ~ DS02 + LWst02 
  
  # residual variances and covariances
  APst02 + Problems + Selfesteem ~~ APst02 
  Problems + Selfesteem ~~ Problems
  Selfesteem ~~ Selfesteem
  #
  # variances and covariances of predictors  # *
  DS02 + LWst02 ~~ DS02
  LWst02 ~~ LWst02 
  
  # intercepts / means
  APst02 + Problems + Selfesteem ~ 1  # intercept for each dependent
  DS02 + LWst02 ~ 1                   # means of predictors # *
'
# *: This code is not needed to estimate the parameters, but it is needed
#    such that lavaan will count these parameters correctly (see exercise 3).
# Note: Instead of specifying intercepts and means in the model, 
#       one can include 'meanstructure=TRUE' in the lavaan function.
#
# Fit the regression model using the lavaan function
#fit_multivar <- lavaan(model = model.multivar, data = data_multivar)
# Because of the missing data, we will use FIML (as explained later in the course):
fit_multivar <- lavaan(model = model.multivar, data = data_multivar,
                       missing='fiml', fixed.x=FALSE) # Specify FIML
#
# Output
summary(fit_multivar, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)
# - Note that the residual variances are denoted by stating a "." before the 
#   variable name. Here: .APst02, .Problems, and .Selfesteem
#   The variances are denoted without a ".". Here: DS02 and LWst02
# - Note that the intercepts are denoted by stating a "." before the 
#   variable name. Here: .APst02, .Problems, and .Selfesteem
#   The means are denoted without a ".". Here: DS02 and LWst02
#
# Parameter estimates
#coef(fit_multivar)                 # unstandardized regression parameters
#parameterEstimates(fit_multivar)   # all unstandardized parameters - with extra information
standardizedSolution(fit_multivar) # all standardized parameters - with extra information
#
## Fit measures
#fitMeasures(fit_multivar)   # many model fit measures
#inspect(fit_multivar, 'r2') # R-square for all dependent variables
#
## Technical output
#lavInspect(fit_multivar) # Comparable to TECH1 in Mplus
## More details bout this in exercise 3.
#
#
# Possibly conclusions:
# - digit span (DS02) and letters words (LWst02) significantly predict all three outcomes.
# - The two predictors positively relate to APst02 and Selfesteem and negatively to Problems.
# - The two predictors explain 36.5%, 5.2%, and 1.1% of the oucomes APst02, Problems, and Selfesteem.
#   So, quite a lot of the variance in APst02 is explained.
#   In all, we do miss relevant other predictors.
# - When you are the expert (and look at the scale of the data), 
#   you can judge whether the effect are relevant or not.
# - When looking at the absolute values of the standardized effects, 
#   LWst02 is a more important predictor than DS02 for APst02 and Problems;
#   while DS02 is a more (or equally) important predictor than LWst02 for Selfesteem.
#
# Note that the zero chi-square implies that the model is fully saturated: 
# meaning that the theoretical covariance matrix is a function of as many 
# parameters # as there are variances and covariances (on one side of the main 
# diagonal) in the covariance matrix. You have essentially re-interpreted the 
# covariance matrix, and this may be quite meaningful. 
# Then, what you have is essentially a regression model, 
# and just must evaluate your model in those terms: 
# Are the estimated relationships strong? 
# Are the direction of the parameter estimates consistent with theory? 
# Are any assumptions violated (note that this would be checked before analysis)? 
# Any basic regression text can guide you.


# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit_multivar, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_multivar) # make sure that in Rstudio your 'plot box' is wide enough
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit_multivar, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)
