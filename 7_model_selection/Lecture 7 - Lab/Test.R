# install this package first (once)
if (!require("restriktor")) install.packages("restriktor")
if (!require("lavaan")) install.packages("lavaan")

# Load
library(restriktor)
library(lavaan)

# Also install and load the gorica library because it contains the data 'sesamesim'
if (!require("gorica")) install.packages("gorica") # install this package first (once)
library(gorica)

# If you want to peak at the top of the data set
#head(sesamesim)


sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))


model1 <- '
    A =~ A1*Ab + A2*Al + A3*Af + A4*An + A5*Ar + A6*Ac 
    B =~ B1*Bb + B2*Bl + B3*Bf + B4*Bn + B5*Br + B6*Bc 
'

H1.1 <- "
A1 > .6; A2 > .6; A3 > .6; A4 > .6; A5 > .6; A6 > .6; 
B1 > .6; B2 > .6; B3 > .6; B4 > .6; B5 > .6; B6 > .6
"

fit1 <- sem(model1, data = sesamesim, std.lv = TRUE)
# summary(fit1, standardize = T)
# fitmeasures(fit1)


# install this package first (once)
if (!require("lavaanPlot")) install.packages("lavaanPlot") 
library(lavaanPlot)

# plot the model
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)


set.seed(100) # since penalty (PT) is obtained via sampling.
results1 <- goric(fit1, hypotheses = list(H1.1 = H1.1), 
                  comparison = "complement", type = "gorica", 
                  standardized = TRUE) 
# Note: This also includes the comparison of hypotheses
#summary(results1)
results1



# Test code Boukje

cfa.ses <-'
After =~ A1*Ab + A2*Al + A3*Af + A4*An + A5*Ar + A6*Ac
Before =~ B1*Bb + B2*Bl + B3*Bf + B4*Bn + B5*Br + B6*Bc
'

H1 <- '
A1 > .6; A2 > .6; A3 > .6; A4 > .6; A5 > .6; A6 > .6; 
B1 > .6; B2 > .6; B3 > .6; B4 > .6; B5 > .6; B6 > .6
'

fit.cfa.ses <- cfa(cfa.ses, data = sesamesim, std.lv = TRUE)

lavaanPlot(model = fit.cfa.ses, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)
parameterEstimates(fit.cfa.ses)
fitMeasures(fit.cfa.ses, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea","srmr"))

set.seed(100)
results.fit.cfa.ses <- goric(fit.cfa.ses, 
                             hypotheses = list(H1 = H1), 
                             comparison = "complement", 
                             type = "gorica", 
                             standardized = TRUE)
results.fit.cfa.ses
# Now cfa function! 


# Subtract estimates and covariance matrix
# Unstand:
#est <- coef(fit.cfa.ses)[1:12]
#VCOV <- vcov(fit.cfa.ses)[1:12, 1:12]
#
# In case you need standardized estimates and their vcov:
est <- lavaan::standardizedsolution(fit.cfa.ses)[1:12, 'est.std']
names(est) <- names(coef(fit.cfa.ses)[1:12])
VCOV <- lavInspect(fit.cfa.ses, "vcov.std.all")[1:12, 1:12]

# GORICA
set.seed(100) # since penalty (PT) is obtained via sampling.
results.fit.cfa.ses <- goric(est, VCOV = VCOV, 
                  hypotheses = list(H1 = H1), 
                  comparison = "complement") 
results.fit.cfa.ses

# compare
results1



############

# test more code for Boukje

## My code
#3.2 Exercise 2b
#Specify the latent regression model, using labels.
reg.ses <- '
After =~ A1*Ab + A2*Al + A3*Af + A4*An + A5*Ar + A6*Ac
Before =~ B1*Bb + B2*Bl + B3*Bf + B4*Bn + B5*Br + B6*Bc

After ~ beta1*Before + beta2*age + beta3*peabody
'

# 3.3 Exercise 2c
#Come up with one or more hypotheses and a failsafe hypothesis.
H_reg <- 'beta1 > (beta2, beta3)'
set.seed(100)
results.fit.reg.ses <- goric(fit.reg.ses, 
                             hypotheses = list(H_reg = H_reg), 
                             comparison = "complement", 
                             standardized = TRUE)
results.fit.reg.ses
coef(fit.reg.ses)

# 3.4 Exercise 2d Run the latent regression model.
fit.reg.ses <- sem(reg.ses, data = sesamesim, std.lv = TRUE)
summary(fit.reg.ses)
#parameters are labelled bet1, bet2, bet3???
#Lijkt zo, in volgende nl niet:
coef(fit.reg.ses)
#
H_reg <- 'bet1 > (bet2, bet3)' # werkt dus niet:
set.seed(100)
results.fit.reg.ses <- goric(fit.reg.ses, 
                             hypotheses = list(H_reg = H_reg), 
                             comparison = "complement", 
                             standardized = TRUE)
results.fit.reg.ses


reg.ses <- '
After =~ A1*Ab + A2*Al + A3*Af + A4*An + A5*Ar + A6*Ac
Before =~ B1*Bb + B2*Bl + B3*Bf + B4*Bn + B5*Br + B6*Bc

After ~ bet1*Before + bet2*age + bet3*peabody
'
fit.reg.ses <- sem(reg.ses, data = sesamesim, std.lv = TRUE)
summary(fit.reg.ses)
H_reg <- 'bet1 > (bet2, bet3)'
set.seed(100)
results.fit.reg.ses <- goric(fit.reg.ses, 
                             hypotheses = list(H_reg = H_reg), 
                             comparison = "complement", 
                             standardized = TRUE)
results.fit.reg.ses
coef(fit.reg.ses)





# Code from the lab solutions which gives me the same error.
model2 <- '
    A =~ Ab + Al + Af + An + Ar + Ac 
    B =~ Bb + Bl + Bf + Bn + Br + Bc 
    A ~ AB*B + AAge*age + APeabody*peabody
'
#
H1.2 <- "AB > APeabody = AAge = 0"
H2.2 <- "AB > APeabody > AAge = 0" 
H3.2 <- "AB > APeabody > AAge > 0"
#
fit2 <- sem(model2, data = sesamesim, std.lv = TRUE)
summary(fit2)
# now suddenly:
#Regressions:
#  Estimate  Std.Err  z-value  P(>|z|)
#A ~                                                 
#  B         (AB)    1.284    0.129    9.941    0.000
#age     (AAge)   -0.000    0.012   -0.010    0.992
#peabody (APbd)   -0.002    0.005   -0.330    0.741
#
#maar namen in volgende wel goed:
coef(fit2)
#
set.seed(100)
results2 <- goric(fit2, 
                  hypotheses = list(H1.2 = H1.2, H2.2 = H2.2, H3.2 = H3.2),
                  type = "gorica", standardized = TRUE)
results2

