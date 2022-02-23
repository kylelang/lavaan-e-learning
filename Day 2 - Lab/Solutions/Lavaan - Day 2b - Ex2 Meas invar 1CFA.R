
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
data_PGD <- read.table("PGDdata2.txt", header = T)
#
# Denote missing values
data_PGD[sapply(data_PGD, function(x) as.character(x) %in% c("-999") )] <- NA 
#
data_PGD$Kin2 <- factor(data_PGD$Kin2, labels = c("partner", "else"))
#summary(data_PGD$Kin2)


# Correlations for the items of interest: b1pss1 b2pss2 b3pss3 b4pss4 b5pss5
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
res <- rcorr(as.matrix(data_PGD[,2:6])) # rcorr() accepts matrices only
round(res$r, 3) # Correlation matrix (rounded to 3 decimals)



# one-factor CFA
model.1CFA <- "
 F  =~ b1pss1 + b2pss2 + b3pss3 + b4pss4 + b5pss5
"
fit_1CFA <- cfa(model.1CFA, data=data_PGD,
                missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_1CFA, standardized = T)
fitMeasures(fit_1CFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr")) 
#lavInspect(fit_1CFA)
#
# Number of observations                           571
#Thus, there are 571 subjects.
#
#Using the rules of thumb, we can conclude that the fit of the model is 
# acceptable (RMSEA < .06, SRMR < .08, CFI/TLI > .95). 
#
#Item 2 has the weakest contribution to the factor. 
#Its standardized factor loading (Std.all) is 0.494.
#Hence, the item explains 100%*0.494^2 = approx 24.4% of the factor its variance.



# multigroup one-factor CFA
model.1CFA <- "
 F  =~ b1pss1 + b2pss2 + b3pss3 + b4pss4 + b5pss5
"
fit_MG1CFA <- cfa(model.1CFA, data=data_PGD,
                group = 'Kin2',
                missing='fiml', fixed.x=F) # Specify FIML 
summary(fit_MG1CFA, standardized = T)
fitMeasures(fit_MG1CFA, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr")) 
#lavInspect(fit_MG1CFA)
#
#Number of observations per group:                   
#  partner                                        190
#  else                                           379



#Measurement invariance testing
#
#Before one compares parameters across multiple groups, one first needs to 
# establish measurement invariance. 
#When data is continuous, testing for measurement invariance involves a fixed 
# sequence of model comparison tests. A typical sequence involves three models:
# Model 1: configural invariance. 
#          The same factor structure is imposed on all groups.
# Model 2: metric/weak invariance. 
#          The factor loadings are constrained to be equal across groups.
# Model 3: scalar/strong invariance. 
#          The factor loadings and intercepts are constrained to be equal across groups.
#
# configural invariance
fit_MG1CFA_ci <- cfa(model.1CFA, data=data_PGD,
                  group = 'Kin2',
                  missing='fiml', fixed.x=F)
# The same as fit_MG1CFA
#
# metric/weak invariance
fit_MG1CFA_wi <- cfa(model.1CFA, data=data_PGD,
                     group = 'Kin2',
                     missing='fiml', fixed.x=F,
                      group.equal = "loadings") 

# scalar/strong invariance
fit_MG1CFA_si <- cfa(model.1CFA, data=data_PGD,
                     group = 'Kin2',
                     missing='fiml', fixed.x=F,
                      group.equal = c("intercepts", "loadings"))
#
# model comparison tests
lavTestLRT(fit_MG1CFA_ci, fit_MG1CFA_wi, fit_MG1CFA_si)
#Because we provided three model fits, it will produce two tests: 
# 1. the first model versus the second model, 
# 2. the second model versus the third model. 
#Chi-Squared Difference Test
#
#Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
#fit_MG1CFA_ci 10 6488.1 6618.4 11.329                                
#fit_MG1CFA_wi 14 6488.2 6601.1 19.417     8.0876       4    0.08842 .
#fit_MG1CFA_si 18 6485.1 6580.6 24.297     4.8802       4    0.29982  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#
# Possible conclusions:
#- The configural model fits the data (chi-square = 11.329, p = .33 ):
fitMeasures(fit_MG1CFA_ci, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr"))
#- From there on, we evaluate whether the more constrained model does not fit 
#  the data worse than the less constrained model:
#  - Because the first p-value is non-significant, we conclude that 
#    weak invariance (equal factor loadings) is supported in this dataset.
#    Stated otherwise: 
#    The metric invariance model does not fit worse than the configural model.
#  - Because the second p-value is also non-significant, we conclude that 
#    strong invariance (equal intercepts and factor loadings) is also supported.
# - In SEM, one always prefer parsimony: the model with the most df. 
#   Thus, we prefer the scalar invariance model, 
#   where both factor loadings and intercepts are constrained (to be equal). 
# - Since the latent variable in the scalar model means the same thing across 
#   the two groups, we can compare values on the latent variable.

# Note: Let us say that the second p-value is significant.
# Then, strong invariance is not supported. 
# Then, it is unwise to directly compare parameters across the two groups.


# Plot
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit_MG1CFA_si)


  
#Add constraints such that also the residual variances are constrained to be the 
#same for both groups: 
fit_MG1CFA_si_res <- cfa(model.1CFA, data=data_PGD,
                     group = 'Kin2',
                     missing='fiml', fixed.x=F,
                     group.equal = c("intercepts", "loadings", "residuals"))
#group.equal = c("residual.covariances")
fitMeasures(fit_MG1CFA_si_res, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr"))
#lavInspect(fit_MG1CFA_si_res)
#
# Compare this model (fit_MG1CFA_si_res) to one without (fit_MG1CFA_si):
anova(fit_MG1CFA_si_res, fit_MG1CFA_si)
#Chi-Squared Difference Test
#
#Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
#fit_MG1CFA_si     18 6485.1 6580.6 24.297                                
#fit_MG1CFA_si_res 23 6485.3 6559.2 34.565     10.268       5    0.06799 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Delta chi-square (5) = 10.268, p= 0.068.
# Hence, the model is not significantly worse.
# That is, The residual variances are also equal. 
# Bear in mind that we always prefer a more parsimonious model (= more df,
# that is, less parameters to be estimated) when we compare two models. 



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



