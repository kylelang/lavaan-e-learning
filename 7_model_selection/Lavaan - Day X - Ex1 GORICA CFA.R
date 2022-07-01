
# Exercise 1: Confirmatory factor analysis
# Using the goric function in the package restriktor


# Load the restriktor and lavaan libraries. 
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
if (!require("lavaan")) install.packages("lavaan") # install this package first (once)
library(restriktor)
library(lavaan)
#
# Also load the gorica library because it contains the data 'sesamesim'
if (!require("gorica")) install.packages("gorica") # install this package first (once)
library(gorica)


# Specify the confirmatory factor model
# Note: The goric function cannot use the default labeling, so:
# Give your own labels to estimates by including them in the lavaan model:
model1 <- '
    A =~ A1*Ab + A2*Al + A3*Af + A4*An + A5*Ar + A6*Ac 
    B =~ B1*Bb + B2*Bl + B3*Bf + B4*Bn + B5*Br + B6*Bc 
'

# Hypotheses of interest
# Formulate the hypothesis of interest (here, consisting of 12 order restrictions)
# Notably, using our own labeling
H1.1 <- "
A1 > .6; A2 > .6; A3 > .6; A4 > .6; A5 > .6; A6 > .6; 
B1 > .6; B2 > .6; B3 > .6; B4 > .6; B5 > .6; B6 > .6
"
# Note: The restrictions are 'connected' by using ';'


# Fit the confirmatory factor model using the lavaan sem function
fit1 <- sem(model1, data = sesamesim, std.lv = TRUE)
#summary(fit1, standardize = T)
#fitmeasures(fit1)

# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


# Call goric ('type = "gorica"')
#
# Note: we need standardized estimates for a meaningful comparison:
# 'standardized = TRUE'.
#
# Calculate GORICA values and weights for H1.1 and its complement: 
# 'comparison = "complement"'.
#
set.seed(100) # seed value, since penalty (PT) is obtained via sampling. 
# At the bottom, one can find information regarding a sensitivity check.
#
results1 <- goric(fit1, H1.1, comparison = "complement", type = "gorica", 
                    standardized = TRUE) 
summary(results1) # Note: This also includes the comparison of hypotheses
# In this case, there are 12 order restrictions and thus the computation time
# is long. See below for possibilities to improve the computation time.
#
# Output:
#        model  loglik  penalty   gorica  gorica.weights
#1        H1.1  33.581    8.139  -50.884           0.988
#2  complement  32.879   11.886  -41.985           0.012
#
#The order-restricted hypothesis ‘H1.1’ has 85.561 times more support 
#                                                 than its complement.
#
# Possible conclusion:
# - The table shows that the hypothesis of interest (H1.1) has the largest fit 
#   and the smallest complexity and, thus, the smallest GORICA value and
#   highest GORICA weight. 
# - The GORICA weight for H1 (against it complement) is 0.988.
# - Hence, the support in the data in favor of H1 is overwhelming: 
#   H1.1 is 0.988/0.012 = approx. 86 times more supported than its complement.
# - Thus, there is overwhelming support for the hypothesis that each factor 
#   loading is larger than .6.



################################################################################

# Extra

# Note: The default way of calculating the PT in goric() is slow(er) when the number of parameters is large. 
# Here, there are 12 parameters, so may want to use: 'mix.weights = "boot"'.
# That is, we will use bootstrap in the calculation of the level probabilities (LPs) needed in PT. 
# The results of course do not change, but the computation time may.
# 
# Determine number of cores that can be used, to fasten the calculation of PT even more.
if (!require("parallel")) install.packages("parallel") # install this package first (once)
library(parallel)
nrCPUcores <- detectCores(all.tests = FALSE, logical = TRUE)
#
set.seed(100)
results1_b <- goric(fit1, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
                   mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)
summary(results1_b) # Note: This also includes the comparison of hypotheses
#
#        model  loglik  penalty   gorica  gorica.weights
#1        H1.1  33.581    8.148  -50.866           0.988
#2  complement  32.879   11.883  -41.993           0.012


################################################################################

# Sensitivity check

# Influence of seed on PT, but negligible:
#
#set.seed(100)
#goric(fit1, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
#      mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)$result[,3]
results1_b$result[,3]
set.seed(100100)
goric(fit1, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
       mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)$result[,3]
set.seed(123456)
goric(fit1, H1.1, comparison = "complement", type = "gorica", standardized = TRUE, 
       mix.weights = "boot", parallel = "snow", ncpus = nrCPUcores, mix.bootstrap = 99999)$result[,3]
#
#[1]  8.147561 11.882759
#[1]  8.134921 11.893199
#[1]  8.140621 11.886719


################################################################################

