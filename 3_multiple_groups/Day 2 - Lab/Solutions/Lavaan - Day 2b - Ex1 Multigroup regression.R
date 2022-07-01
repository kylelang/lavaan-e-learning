
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
data_regr <- read.table("popular_regr.txt", header = T)
#If data without header, then: 
#colnames(data_regr) <- c("respnr", "Dutch", "gender", "sw", "covert", "overt")
#
# Denote missing values
# -99 and -999 is missing, so set this to NA:
data_regr[sapply(data_regr, 
                 function(x) as.character(x) %in% c("-99", "-999") )] <- NA 
#If you ‘forget’ to tell that -99 and -999 are used to denote missing data, 
#these values will be treated as being observed. 

# Besides sw, overt, and covert we now also use the variable gender.
# Since we read in a text file, there is no information about measurement level.
# Thus, we want to denote that the variable 'gender' is a factor,
# although it is not necessary in case of two levels.
# It is, however, helpful in assigning names to the levels.
data_regr$gender <- factor(data_regr$gender, labels = c("male", "female"))
#summary(data_regr$gender)


# Specify the multigroup regression model
#
#Now, let us run a multigroup regression model predicting levels of socially 
# desirable answering patterns of adolescents (sw) using the predictors overt 
# (overt) and covert antisocial behaviour (covert), for both males and females.
#
#To run a multigroup regression with lavaan, we need to add: group = "gender"
# to the lavaan function. It does not affect the model specification:
model.regression <- '
  sw ~ overt + covert # regression
  sw ~~ sw            # residual variance
  sw ~ 1              # intercept
'
#
# Fit the multigroup regression model using the lavaan function
fit_MGregr <- lavaan(model = model.regression, data = data_regr,
                     group = "gender") # multigroup specification
#
# Output
summary(fit_MGregr, standardized=TRUE, fit.measures = TRUE, rsquare = TRUE, 
        ci = TRUE)
#
# Parameter estimates
#coef(fit_MGregr)                 # unstandardized regression parameters
#parameterEstimates(fit_MGregr)   # all unstandardized parameters - with extra information
standardizedSolution(fit_MGregr) # all standardized parameters - with extra information
#
## Fit measures
#fitMeasures(fit_MGregr)   # many model fit measures
#fitMeasures(fit_MGregr, c("cfi", "tli", "rmsea","srmr")) 
#                        # ask for specific model fit measures
#inspect(fit_MGregr, 'r2') # R-square (for sw)
#
## Technical output
#lavInspect(fit_MGregr) # Comparable to TECH1 in Mplus
#
#
# Possibly conclusions:
# - The standardized effect of covert on sw appears to be stronger for females 
#   (-0.478) than males (-0.438) and reverse for the effect of overt (-0.161 for 
#   males compared to -0.112 for females). 
# - However, the confidence intervals for sw on covert and overt show a large 
#   overlap for males and females. 
# - Additionally, the regression coefficients for the males are included in the 
#   confidence intervals for the females and vice versa. 
#Therefore, we are below going to test whether the differences between the 
# regression coefficients for males and females are statistically significant. 



# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit_MGregr, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_MGregr)
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit_MGregr, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)



# Test equality of regression coefficients
#
#Test whether the regression coefficients for males and females are the same.
#
# Now, we want to constrain 'a single parameter' to be equal across two groups.
# Therefore, we need to label this parameter for both groups, so using 2 labels.
# This we need to do for both regression coefficient parameters.
#
# Model specification
model.regression_equal <- '
  # model with labeled parameters
  sw ~ c(b1_m,b1_f)*overt + c(b2_m,b2_f)*covert # regression
  sw ~~ sw                                      # residual variance
  sw ~ 1                                        # intercept
'
#
# Fit model
fit_MGregr_equal <- lavaan(model = model.regression_equal, data = data_regr,
                     group = "gender") # multigroup specification
#
# Wald test
lavTestWald(fit_MGregr_equal, constraints = 'b1_m == b1_f; b2_m == b2_f')
#Wald test with df=1 is 1.035524, p = 0.5958526. 
#Thus, we do not reject the null that the two regression coefficients for both
# groups are equal.
#Hence, we conclude that there is no evidence for a difference between
# b1_m and b1_f and between b2_m and b2_f.
#
#
# In case one wants the model results for which the regression coefficients are 
# the same:
#
# Model specification
model.regression_equal <- '
  # model with labeled parameters
  sw ~ c(b1_m,b1_f)*overt + c(b2_m,b2_f)*covert # regression
  sw ~~ sw                                      # residual variance
  sw ~ 1                                        # intercept
  #
  # constraints
  b1_m == b1_f; b2_m == b2_f
'
#
# Fit model
fit_MGregr_equal <- lavaan(model = model.regression_equal, data = data_regr,
                           group = "gender") # multigroup specification 
#
# Output
summary(fit_MGregr_equal, standardized=TRUE, fit.measures=TRUE)
#fitMeasures(fit_MGregr_equal, c("cfi", "tli", "rmsea","srmr"))

  


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



