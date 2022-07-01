
# Exercise 3: Multiple group regression
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


# Make sure that the variable 'sex' is a factor with the right labels.
sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))


# Specify the multiple group regression model
model3 <- '
    postnumb ~ prenumb 
'


# Hypotheses of interest
# Formulate the hypotheses of interest
# Notably, using our own labeling (which should be the same as those used below)
H1.3 <- "Pre_boy < Pre_girl"


# Fit the multiple group regression model using the lavaan sem function
fit3 <- sem(model3, data = sesamesim, std.lv = TRUE, group = "sex")
#
#if (!require("lavaanPlot")) install.packages("lavaanPlot") # install this package first (once)
#library(lavaanPlot)
#lavaanPlot(model = fit3, node_options = list(shape = "box", fontname = "Helvetica"), 
#           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)



# Call goric ('type = "gorica"')
# Note: we need standardized estimates for a meaningful comparison ('standardized = TRUE').
#
# Now, we will extract the estimates ourselves and use that.
#
# Obtain standardized(!) estimates from lavaan object (labelled or unlabelled):
est_3 <- lavaan::standardizedsolution(fit3)[c(1,6), 'est.std']
names(est_3) <- c("Pre_boy", "Pre_girl") # Note: This should be the same as the labeling used the H1.3.
vcov_3 <- lavInspect(fit3, "vcov.std.all")[c(1,4), c(1,4)] # Note: Use this in the 'VCOV = vcov_3' command.
#
# Calculate GORICA values and weights for H1.3 and its complement ('comparison = "complement"').
set.seed(100)
results3 <- goric(est_3, VCOV = vcov_3, H1.3, comparison = "complement", type = "gorica") 
summary(results3) # Note: This also includes the comparison of hypotheses
#
#        model  loglik  penalty  gorica  gorica.weights
#1        H1.3   4.420    1.500  -5.841           0.498
#2  complement   4.428    1.500  -5.856           0.502
#
#
# Possible conclusion:
# - This output table shows that the hypothesis of interest and its compliment 
#   are equally likely, since both have a weight of approximately .50. 
#   That is, they have about the same support.
# - Since the hypotheses do not overlap and are equally complex (i.e., have the 
#   same penalty value), this implies that their boundary is the preferred 
#   hypothesis, that is, H0: Pre_boy = Pre_girl.
# - Thus, there is support for the boundary of the hypothesis of interest and 
#   its complement, indicating that the relationship between postnumb and 
#   prenumb is equally high for girls and boys.


################################################################################

# Sensitivity check

# No influence of seed in PT:
#
#set.seed(100)
#goric(est_3, VCOV = VCOV_3, H1.3, comparison = "complement", 
#      type = "gorica")$result[,3]
results3$result[,3]
set.seed(100100)
goric(est_3, VCOV = VCOV_3, H1.3, comparison = "complement", 
      type = "gorica")$result[,3]
set.seed(123456)
goric(est_3, VCOV = VCOV_3, H1.3, comparison = "complement", 
      type = "gorica")$result[,3]
#
# 1.5 1.5
# 1.5 1.5
# 1.5 1.5


################################################################################

