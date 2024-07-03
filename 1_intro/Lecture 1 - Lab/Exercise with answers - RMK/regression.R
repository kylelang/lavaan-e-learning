

if (!require("lavaan")) install.packages("lavaan", dependencies = TRUE)
library(lavaan)

data_regr <- read.table("popular_regr.txt", header = T)
colnames(data_regr) <- c("respnr", "Dutch", "gender", "sw", "covert", "overt")
data_regr[sapply(data_regr, function(x) as.character(x) %in% c("-99", "-999") )] <- NA


if (!require("psych")) install.packages("psych", dependencies = TRUE)
library(psych) 

colnames(data_regr)
describe(data_regr[, 4:6])


if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

res <- rcorr(as.matrix(data_regr[, 4:6])) # rcorr() accepts matrices only
round(res$r, 3)
round(res$P, 3) 
res$n 




model.regression <- '
  sw ~ overt + covert # regression
  sw ~~ sw            # residual variance
  sw ~ 1              # intercept
'

fit_regr <- lavaan(model = model.regression, data = data_regr)
summary(fit_regr, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)

coef(fit_regr) 
parameterEstimates(fit_regr)
standardizedSolution(fit_regr)


if (!require("lavaanPlot")) install.packages("lavaanPlot") 
library(lavaanPlot)
lavaanPlot(model = fit_regr, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)

if (!require("tidySEM")) install.packages("tidySEM")
library(tidySEM)
graph_sem(fit_regr)


fitMeasures(fit_regr)   
fitMeasures(fit_regr, c("cfi", "tli", "rmsea","srmr")) 
inspect(fit_regr, 'r2') 

lavInspect(fit_regr)




model.regression <- '
  sw ~ overt + covert # regression
  sw ~~ sw            # residual variance
  sw ~ 1              # intercept
  #
  # (co)variances predictors # Needed for FIML
  overt + covert ~~ overt 
  covert ~~ covert 
  #
  # means predictors # Needed for FIML
  covert + overt ~ 1
'
fit_regr <- lavaan(model = model.regression, data = data_regr,
                   missing='fiml', fixed.x=F) # Specify FIML
summary(fit_regr, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)



###

data_multivar <- read.table("CDSsummerschool.txt", header = T)
data_multivar[sapply(data_multivar, function(x) as.character(x) %in% c("-999") )] <- NA

colnames(data_multivar)
if (!require("psych")) install.packages("psych", dependencies = TRUE)
library(psych) 
describe(data_multivar[,1:5])
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
res <- rcorr(as.matrix(data_multivar[, 1:5])) # rcorr() accepts matrices only
round(res$r, 3)
round(res$P, 3) 
res$n 



model.multivar <- '
  
  # regressions
  APst02 + Problems + Selfesteem ~ DS02 + LWst02 
  
  # residual variances and covariances
  APst02 + Problems + Selfesteem ~~ APst02 
  Problems + Selfesteem ~~ Problems
  Selfesteem ~~ Selfesteem
  
  # variances and covariances of predictors  # *
  DS02 + LWst02 ~~ DS02
  LWst02 ~~ LWst02 
  
  # intercepts / means
  APst02 + Problems + Selfesteem ~ 1  # intercept for each dependent
  DS02 + LWst02 ~ 1                   # means of predictors # *
'

if (!require("lavaan")) install.packages("lavaan", dependencies = TRUE)
library(lavaan)
#`fit_multivar <- lavaan(model = model.multivar, data = data_multivar)`
fit_multivar <- lavaan(model = model.multivar, data = data_multivar,
                       missing='fiml', fixed.x=FALSE) # Specify FIML
lavInspect(fit_multivar)

summary(fit_multivar, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)

coef(fit_multivar) 
parameterEstimates(fit_multivar)
standardizedSolution(fit_multivar)

fitMeasures(fit_multivar)   
inspect(fit_multivar, 'r2') 



if (!require("lavaanPlot")) install.packages("lavaanPlot") 
library(lavaanPlot)
lavaanPlot(model = fit_multivar, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)

if (!require("tidySEM")) install.packages("tidySEM")
library(tidySEM)
graph_sem(fit_multivar)


###

lower <- scan("CorPun.txt")
CovMx <- getCov(lower, names = c("harsh", "just", "reject", "maladj"))

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
  #
  # means of predictors # *
  #harsh + just ~ 1
'

if (!require("lavaan")) install.packages("lavaan", dependencies = TRUE)
library(lavaan)

fit_path <- lavaan(model = model.path, 
                   sample.cov = CovMx, # Now, using a covariance matrix as input
                   sample.nobs = 175)  # In that case, the sample size is needed as well
summary(fit_path, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)

lavInspect(fit_path)


model.path_check <- '
  
  # regressions
  maladj ~ reject 
  reject ~ harsh + just
  
  # residual variances
  reject ~~ reject 
  maladj ~~ maladj
  #
  # Check: Not specifying these (co)variances and means
  # variances and covariances of predictors # *
  #harsh + just ~~ harsh
  #just ~~ just
  
  ## intercepts - if we would have mean value as input too
  ## or use meanstructure=TRUE in the lavaan function
  #maladj ~ 1
  #reject ~ 1 
  #
  # means of predictors # *
  #harsh + just ~ 1
'
fit_path_check <- lavaan(model = model.path_check, 
                         sample.cov = CovMx, # Now, using a covariance matrix as input
                         sample.nobs = 175) # In that case, the sample size is needed as well
summary(fit_path_check, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)
lavInspect(fit_path_check)


if (!require("lavaanPlot")) install.packages("lavaanPlot") 
library(lavaanPlot)
lavaanPlot(model = fit_path, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = T, stand = T, covs = T)

if (!require("tidySEM")) install.packages("tidySEM")
library(tidySEM)
graph_sem(fit_path)

summary(fit_path, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)

coef(fit_path) 
parameterEstimates(fit_path)
standardizedSolution(fit_path)

fitMeasures(fit_path)   
fitMeasures(fit_path, c("chisq", "cfi", "tli", "rmsea","srmr")) 
inspect(fit_path, 'r2')

