library(lavaan)

data(bfi, package = "psych")

mod <- '
agree =~ A1 + A2 + A3 + A4 + A5
neuro =~ N1 + N2 + N3 + N4 + N5
'

configFit1 <- cfa(mod, data = bfi, group = "gender")
configFit2 <- cfa(mod, data = bfi, group = "gender", std.lv = TRUE)

summary(configFit1)
summary(configFit2)

weakFit1 <- cfa(mod, data = bfi, group = "gender", group.equal = "loadings")
weakFit2 <- cfa(mod, data = bfi, group = "gender", group.equal = "loadings", std.lv = TRUE)

summary(weakFit1)
summary(weakFit2)

###

lavTestLRT(configFit1, weakFit1) # or:
#anova(configFit1, weakFit1)
#
lavTestLRT(configFit2, weakFit2)
# zelfde idd


Number of model parameters                        62
Number of equality constraints                     8

Number of observations per group:               Used       Total
1                                              868         919
2                                             1750        1881

Model Test User Model:
  
  Test statistic                               754.262
Degrees of freedom                                76


Number of model parameters                        64
Number of equality constraints                    10

Number of observations per group:               Used       Total
1                                              868         919
2                                             1750        1881

Model Test User Model:
  
  Test statistic                               754.262
Degrees of freedom                                76