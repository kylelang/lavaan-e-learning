### Title:    Mediation & Moderation: Lecture 12 Examples
### Author:   Kyle M. Lang
### Created:  2016-MAY-01
### Modified: 2016-JUN-09

rm(list = ls(all = TRUE))

set.seed(235711)

library(lavaan)
library(semTools)

dat1 <- readRDS("../data/lecture12Data.rds")

mod1 <- "
fX =~ x1 + x2 + x3
fZ =~ z1 + z2 + z3
fY =~ y1 + y2 + y3
"

out1 <- cfa(mod1, data = dat1, std.lv = TRUE)
summary(out1)
fitMeasures(out1)

mod2 <- "
fX =~ x1 + x2 + x3
fZ =~ z1 + z2 + z3
fY =~ y1 + y2 + y3

fY ~ fX + fZ
"

out2 <- sem(mod2, data = dat1, std.lv = TRUE)
summary(out2)
fitMeasures(out2)

x1z1 <- with(dat1, x1*z1)
x1z2 <- with(dat1, x1*z2)
x1z3 <- with(dat1, x1*z3)

x2z1 <- with(dat1, x2*z1)
x2z2 <- with(dat1, x2*z2)
x2z3 <- with(dat1, x2*z3)

x3z1 <- with(dat1, x3*z1)
x3z2 <- with(dat1, x3*z2)
x3z3 <- with(dat1, x3*z3)

predDat <- as.matrix(dat1[ , -grep("y", colnames(dat1))])

dat2 <- dat1

dat2$x1z1R <- lm(x1z1 ~ predDat)$resid
dat2$x1z2R <- lm(x1z2 ~ predDat)$resid
dat2$x1z3R <- lm(x1z3 ~ predDat)$resid

dat2$x2z1R <- lm(x2z1 ~ predDat)$resid
dat2$x2z2R <- lm(x2z2 ~ predDat)$resid
dat2$x2z3R <- lm(x2z3 ~ predDat)$resid

dat2$x3z1R <- lm(x3z1 ~ predDat)$resid
dat2$x3z2R <- lm(x3z2 ~ predDat)$resid
dat2$x3z3R <- lm(x3z3 ~ predDat)$resid

mod3 <- "
fX =~ x1 + x2 + x3
fZ =~ z1 + z2 + z3
fY =~ y1 + y2 + y3
fXZ =~ x1z1R + x1z2R + x1z3R +
x2z1R + x2z2R + x2z3R +
x3z1R + x3z2R + x3z3R

fY ~ fX + fZ + fXZ

fX ~~ fZ
fX ~~ 0*fXZ
fZ ~~ 0*fXZ

x1z1R ~~ x1z2R + x1z3R + x2z1R + x3z1R
x1z2R ~~ x1z3R + x2z2R + x3z2R
x1z3R ~~ x2z3R + x3z3R

x2z1R ~~ x2z2R + x2z3R + x3z1R
x2z2R ~~ x2z3R + x3z2R
x2z3R ~~ x3z3R

x3z1R ~~ x3z2R + x3z3R
x3z2R ~~ x3z3R
"

out3 <- sem(mod3, data = dat2, std.lv = TRUE, meanstructure = FALSE)
summary(out3)
fitMeasures(out3)

probeOut3 <- probe2WayRC(fit = out3,
                         nameX = c("fX", "fZ", "fXZ"),
                         nameY = "fY",
                         modVar = "fZ",
                         valProbe = c(-1, 0, 1)
                         )

mod4 <- "
fX =~ x1 + x2 + x3
fZ =~ z1 + z2 + z3
fY =~ y1 + y2 + y3
fXZ =~ x1z1R + x2z2R + x3z3R

fY ~ fX + fZ + fXZ

fX ~~ fZ
fX ~~ 0*fXZ
fZ ~~ 0*fXZ
"

out4 <- sem(mod4, data = dat2, std.lv = TRUE, meanstructure = TRUE)
summary(out4)
fitMeasures(out4)

fitMeasures(out3)[c("aic", "bic")]
fitMeasures(out4)[c("aic", "bic")]

probeOut4 <- probe2WayRC(fit = out4,
                         nameX = c("fX", "fZ", "fXZ"),
                         nameY = "fY",
                         modVar = "fZ",
                         valProbe = c(-1, 0, 1)
                         )

probeOut4$SimpleSlope

dat3 <- data.frame(lapply(dat1, scale, scale = FALSE)) 

tmpDat <- data.frame(
    x1z1 = with(dat3, x1*z1),
    x1z2 = with(dat3, x1*z2),
    x1z3 = with(dat3, x1*z3),
    
    x2z1 = with(dat3, x2*z1),
    x2z2 = with(dat3, x2*z2),
    x2z3 = with(dat3, x2*z3),
    
    x3z1 = with(dat3, x3*z1),
    x3z2 = with(dat3, x3*z2),
    x3z3 = with(dat3, x3*z3)
)

dat3 <- data.frame(dat3,
                   lapply(tmpDat, scale, scale = FALSE)
                   )


mod5 <- "
fX =~ x1 + x2 + x3
fZ =~ z1 + z2 + z3
fY =~ y1 + y2 + y3
fXZ =~ x1z1 + x1z2 + x1z3 +
x2z1 + x2z2 + x2z3 +
x3z1 + x3z2 + x3z3

fY ~ fX + fZ + fXZ

fX ~~ fZ

x1z1 ~~ x1z2 + x1z3 + x2z1 + x3z1
x1z2 ~~ x1z3 + x2z2 + x3z2
x1z3 ~~ x2z3 + x3z3

x2z1 ~~ x2z2 + x2z3 + x3z1
x2z2 ~~ x2z3 + x3z2
x2z3 ~~ x3z3

x3z1 ~~ x3z2 + x3z3
x3z2 ~~ x3z3
"

out5 <- sem(mod5, data = dat3, std.lv = TRUE)
summary(out5)
fitMeasures(out5)

out5.2 <- sem(mod5, data = dat3, std.lv = TRUE, meanstructure = TRUE)

probeOut5 <- probe2WayMC(fit = out5.2,
                         nameX = c("fX", "fZ", "fXZ"),
                         nameY = "fY",
                         modVar = "fZ",
                         valProbe = c(-1, 0, 1)
                         )
probeOut5$SimpleSlope

mod6 <- "
fX =~ x1 + x2 + x3
fZ =~ z1 + z2 + z3
fY =~ y1 + y2 + y3
fXZ =~ x1z1 + x2z2 + x3z3

fY ~ fX + fZ + fXZ

fX ~~ fZ
"

out6 <- sem(mod6, data = dat3, std.lv = TRUE)
summary(out6)
fitMeasures(out6)

fitMeasures(out5)[c("aic", "bic")]
fitMeasures(out6)[c("aic", "bic")]

out6.2 <- sem(mod6, data = dat3, std.lv = TRUE, meanstructure = TRUE)

probeOut6 <- probe2WayRC(fit = out6,
                         nameX = c("fX", "fZ", "fXZ"),
                         nameY = "fY",
                         modVar = "fZ",
                         valProbe = c(-1, 0, 1)
                         )

probeOut6$SimpleSlope

dat2.2 <- indProd(data = dat1,
                  var1 = c("x1", "x2", "x3"),
                  var2 = c("z1", "z2", "z3"),
                  match = FALSE,
                  residualC = TRUE)

sum(dat2 - dat2.2)

dat3 <- data.frame(lapply(dat1, scale, scale = FALSE)) 


tmpDat2 <- data.frame(
    x1z1 = with(dat1, x1*z1),
    x1z2 = with(dat1, x1*z2),
    x1z3 = with(dat1, x1*z3),
    
    x2z1 = with(dat1, x2*z1),
    x2z2 = with(dat1, x2*z2),
    x2z3 = with(dat1, x2*z3),
    
    x3z1 = with(dat1, x3*z1),
    x3z2 = with(dat1, x3*z2),
    x3z3 = with(dat1, x3*z3)
)

dat3.2 <- indProd(data = dat1,
                  var1 = c("x1", "x2", "x3"),
                  var2 = c("z1", "z2", "z3"),
                  match = FALSE,
                  doubleMC = TRUE)

sum(dat3[ , -c(1 : 9)] - dat3.2[ , -c(1 : 9)])
