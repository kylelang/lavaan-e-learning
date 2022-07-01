
# Exercise 2: Latent Regression
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


# Specify the latent regression model
# Note: The goric function cannot use the default labeling, so:
# Give your own labels to estimates by including them in the lavaan model:
model2 <- '
    A =~ Ab + Al + Af + An + Ar + Ac 
    B =~ Bb + Bl + Bf + Bn + Br + Bc 
    A ~ AB*B + AAge*age + APeabody*peabody
'

# Note on age and peabody (i.e., biological and mental age): 
#cor(sesamesim$peabody, sesamesim$age)
# [1] 0.2396424


# Hypotheses of interest
# Formulate the hypotheses of interest
# Notably, using our own labeling
H1.2 <- "AB > APeabody = AAge = 0"
H2.2 <- "AB > APeabody > AAge = 0" 
H3.2 <- "AB > APeabody > AAge > 0"


# Fit the confirmatory factor model using the lavaan sem function
fit2 <- sem(model2, data = sesamesim, std.lv = TRUE)


# Plot
if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
library(lavaanPlot)
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
#
# Alternative
if (!require("tidySEM")) install.packages("tidySEM") # install this package first (once)
library(tidySEM)
graph_sem(fit2)
#
# Another alternative
if (!require("semPlot")) install.packages("semPlot") # install this package first (once)
library(semPlot)
semPaths(fit2, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)


# Call goric ('type = "gorica"')
# Note: We need standardized estimates for a meaningful comparison.
# Because there is more than 1 hypothesis, we need to use the unconstrained as
# failsafe, which is the default option in goric().
set.seed(100)
results2 <- goric(fit2, H1.2, H2.2, H3.2, type = "gorica", standardized = TRUE) 
summary(results2) # Note: This also includes the comparison of hypotheses
#
#           model  loglik  penalty   gorica  gorica.weights
#1           H1.2   6.836    0.500  -12.672           0.379
#2           H2.2   6.836    0.687  -12.297           0.314
#3           H3.2   6.836    0.822  -12.028           0.274
#4  unconstrained   6.894    3.000   -7.789           0.033
#
#Note: In case of equal log-likelihood (loglik) values, the 
#      ratio weights are solely based on the difference in penalty values.
#
#
# Possible conclusion:
# - Since the hypotheses overlap and the most restricted one (H1.2) receives the
#   most support (and is not weak), H1.2 is the preferred hypothesis. 
# - Because the support for the other hypotheses also contain support for H1.2, 
#   one could also compare H1.2 to its complement, see below.  
#   This shows convincing support for H1.2. 
# - Thus, there is support for the hypothesis that a larger score on B 
#   corresponds to a larger score on A (i.e., a positive effect) and that age 
#   and peabody do not predict A. 
#   Based on the evaluation below, it is approximately 7 times more likely than 
#   its complement containing competing hypotheses.


# Note:
# Hypotheses are nested, so hypotheses share support.
# Therefore, we examine the best of these against its compliment:
set.seed(100)
results2_c <- goric(fit2, H1.2, comparison = "complement", type = "gorica", 
                    standardized = TRUE) 
summary(results2_c) # Note: This also includes the comparison of hypotheses
#
#        model  loglik  penalty   gorica  gorica.weights
#1        H1.2   6.836    0.500  -12.672           0.874
#2  complement   6.894    2.500   -8.789           0.126
#
#The order-restricted hypothesis ‘H1.2’ has  6.967 times more support 
#                                                 than its complement.


################################################################################

# Sensitivity check

# Influence of seed in PT (of H3), but negligible:
#
#set.seed(100)
#goric(fit2, H1.2, H2.2, H3.2, type = "gorica", standardized = TRUE)$result[,3]
results2$result[,3]
set.seed(100100)
goric(fit2, H1.2, H2.2, H3.2, type = "gorica", standardized = TRUE)$result[,3]
set.seed(123456)
goric(fit2, H1.2, H2.2, H3.2, type = "gorica", standardized = TRUE)$result[,3]
#
# 0.5000000 0.6873956 0.8218584 3.0000000
# 0.5000000 0.6873956 0.8218222 3.0000000
# 0.5000000 0.6873956 0.8218407 3.0000000

################################################################################
