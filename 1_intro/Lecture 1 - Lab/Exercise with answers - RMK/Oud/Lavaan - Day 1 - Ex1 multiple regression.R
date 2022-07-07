
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
#If data without header, then: colnames(data_regr) <- c("respnr", "Dutch", "gender", "sw", "covert", "overt")
#
# Denote missing values
# -99 and -999 is missing, so set this to NA:
data_regr[sapply(data_regr, function(x) as.character(x) %in% c("-99", "-999") )] <- NA 
#If you ‘forget’ to tell that -99 and -999 are used to denote missing data, 
#these values will be treated as being observed. 



# Descriptive statistics of variables of interest: sw, covert, and overt
# colnames(data_regr) # so, columns 4 to 6
if (!require("psych")) install.packages("psych", dependencies = TRUE)
library(psych) # To obtain some extended summary statistics
# Descriptives like mean, median, min, and max
describe(data_regr[, 4:6])
#
# Correlations for the three variables of interest, incl correlation significance test
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
res <- rcorr(as.matrix(data_regr[, 4:6])) # rcorr() accepts matrices only
round(res$r, 3) # Correlation matrix (rounded to 3 decimals)
round(res$P, 3) # p-values (rounded to 3 decimals)
res$n # sample size
#
#
# Possibly conclusions:
# - covert and overt are related to sw, so makes sense (in combination with theory) 
#                                                      to use them as predictors.
#   Their relationship is negative, so an decrease in (c)overt relates to an increase in sw.
# - covert and overt are also related (positively) to each other, 
#   but not that much (in absolute sense) that we should worry about multicolinearity.



# Specify the regression model
#
#Now, let us run a multiple regression model predicting levels of socially desirable answering patterns of adolescents (sw) 
# using the predictors overt (overt) and covert antisocial behaviour (covert). 
#
#To run a multiple regression with lavaan, you first specify the model, then fit the model and finally acquire the summary. 
#To fit the model we use the lavaan() function, which needs a model= and a data= input.
#
# The model is specified as follows:
#   dependent ~ predictor1 + predictor2 # + etc
# The ~ is like the = in a regression equation. 
# Before the ~, one states the dependent variable.
# After the ~, one states the predictors, each separated by the + sign.
# Additionally, one needs to specify that the error (of the dependent variable) has a variance:
#   dependent ~~ dependent 
# If needed (as is the case here), one can specify to include an intercept:
#   dependent ~ 1
#
# Our regression model is specified as follows:
model.regression <- '
  sw ~ overt + covert # regression
  sw ~~ sw            # residual variance
  sw ~ 1              # intercept
'
#
# Fit the regression model using the lavaan function
fit_regr <- lavaan(model = model.regression, data = data_regr)
# Note: Instead of specifying intercepts and means in the model, 
#       one can include 'meanstructure=TRUE' in the lavaan function.
#
# Output
summary(fit_regr, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)
# Can add 'standardized=TRUE' to obtain standardized estimates as well.
# - Note that the residual variances are denoted by stating a "." before the 
#   variable name. Here: .sw
# - Note that the intercepts are denoted by stating a "." before the 
#   variable name. Here: .sw
#
# Parameter estimates
#coef(fit_regr)                 # unstandardized regression parameters
#parameterEstimates(fit_regr)   # all unstandardized parameters - with extra information
standardizedSolution(fit_regr) # all standardized parameters - with extra information
# Note that now you also obtain the confidence interval of the standardized estimates.
#
## Fit measures
#fitMeasures(fit_regr)   # many model fit measures
#fitMeasures(fit_regr, c("cfi", "tli", "rmsea","srmr")) 
#                        # ask for specific model fit measures
#inspect(fit_regr, 'r2') # R-square (for sw)
#
## Technical output
#lavInspect(fit_regr) # Comparable to TECH1 in Mplus
## More details bout this in exercise 3.
## Note: to come to the correct number of model parameters and thus df, use:
#model.regression <- '
#  sw ~ overt + covert # regression
#  sw ~~ sw            # residual variance
#  sw ~ 1              # intercept
#  #
#  # (co)variances predictors
#  overt + covert ~~ overt 
#  covert ~~ covert        
#'
#
#
# Possibly conclusions:
# - The regression equation looks as follows:
#   sw = 4.997 - 0.292 * overt - 0.507 * covert + error
# - The average sw (levels of socially desirable answering patterns of adolescents )
#   when covert and overt antisocial behaviour are 0 is 4.997;
#   When overt goes up with one point, then sw goes down with 0.292 points; 
#   When covert goes up with one point, then sw goes down with 0.507 points.
#   When you are the expert (and look at the scale of the data), 
#   you can judge whether these are relevant effects.
# - the p-values and confidence intervals indicate that 
#   both effects significantly differ from zero.
#   Thus, both variables are (statistically) meaningful predictors.
# - The two predictors explain 0.282*100=28.2 percent of sw.
# - When looking at the absolute values of the standardized effects, 
#   covert is a more important predictor than overt: |-0.448| > |-0.160|.



# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit_regr, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_regr)
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit_regr, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)


# Handling missing data #
#
# Note that in the above listwise deletion is used.
# From the output, one can see that the total sample size used for the analyses is n=1343.
# while the total number of observations os 1491.
# lavaan used listwise deletion and deleted 148 cases:
# - 145 cases because of missingness on the dependent variables and 
# -   3 cases because of missingness on the predictors.
#
# It is better to use, for instance, FIML, as will be discusses later in the course.
# Note that in this data set, the three missing predictor cases will then be used in the analyses. 
# This difference in sample size will most probably result in minor difference in the output.
#
# The code to use FIML is:
model.regression <- '
  sw ~ overt + covert # regression
  sw ~~ sw            # residual variance
  sw ~ 1              # intercept
  #
  # (co)variances predictors
  overt + covert ~~ overt 
  covert ~~ covert        
'
fit_regr <- lavaan(model = model.regression, data = data_regr,
                       missing='fiml', fixed.x=F) # Specify FIML
# Afterwards one can do the same as above. E.g.,
# Output
summary(fit_regr, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)


# The code to use FIML is:
model.regression <- '
  sw ~ overt + covert # regression
  sw ~~ sw            # residual variance
  sw ~ 1              # intercept
'
fit_regr <- lavaan(model = model.regression, data = data_regr,
                   missing='fiml', fixed.x=F) # Specify FIML
# Afterwards one can do the same as above. E.g.,
# Output
summary(fit_regr, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)


##############################################

# lavaan notation
formula type                operator  mnemonic
regression                  ~         is regressed on
(residual) (co)variance     ~~        is correlated with
intercept                   ~ 1       intercept
#latent variable definition  =~        is measured by   # Used in other days of the course 


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
