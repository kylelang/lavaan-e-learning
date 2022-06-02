### Title:    Med/Mod Lecture 10: Examples
### Author:   Kyle M. Lang
### Created:  2016-MAR-17
### Modified: 2016-JUN-09

library(lavaan)
dat1 <- readRDS("../data/lecture11Data.rds")

## Simple mediation model:
mod1 <- "
y ~ cp*x + b*m1
m1 ~ a*x

ab := a*b
"

out1 <- sem(mod1, data = dat1, se = "boot")
summary(out1)
parameterEstimates(out1, boot.ci.type = "bca.simple")


## Moderate the a path:
dat1$xz <- dat1$x * dat1$z

mod2 <- "
y ~ cp*x + b*m1
m1 ~ a1*x + a2*z + a3*xz

imm := a3*b
"

out2 <- sem(mod2, data = dat1, se = "boot")
summary(out2)
parameterEstimates(out2, boot.ci.type = "bca.simple")


## Moderate the b and c' path:
dat1$m2w <- dat1$m2 * dat1$w
dat1$xw <- dat1$w * dat1$w

mod3 <- "
y ~ cp*x + b1*m2 + b2*w + b3*m2w + xw
m2 ~ a*x 

imm := a*b3
"

out3 <- sem(mod3, data = dat1, se = "boot")
summary(out3)
parameterEstimates(out3, boot.ci.type = "bca.simple")


## Serial multiple mediator model with b2 moderated:
mod4 <- "
y ~ cp*x + b1*m1 + b2*m2 + b3*w + b4*m2w
m2 ~ a2*x + d*m1
m1 ~ a1*x

ab1 := a1*b1
imm2 := a2*b4
fullImm := a1*d*b4
"

out4 <- sem(mod4, data = dat1, se = "boot")
summary(out4)
parameterEstimates(out4, boot.ci.type = "bca.simple")

## Serial multiple mediator model with a1 and d moderated:
dat1$m1z <- dat1$m1 * dat1$z
quantile(dat1$z, c(0.05, 0.95))

mod5 <- "
y ~ cp*x + b1*m1 + b2*m2
m2 ~ a2*x + d1*m1 + d2*z + d3*m1z
m1 ~ a1*x + a2*z + a3*xz

mmIndex1 := a3*b1
ab2 := a2*b2
fullIE1 := (a1 + a3 * (-1.244962)) * (d1 + d3 * (-1.244962)) * b2
fullIE2 := (a1 + a3 * 1.369550) * (d1 + d3 * 1.369550) * b2
mmTest2 := fullIE1 - fullIE2
"

out5 <- sem(mod5, data = dat1, se = "boot")
summary(out5)
parameterEstimates(out5, boot.ci.type = "bca.simple")


mod6.1 <- "
m2 ~ cp*x + b1*m1 + b2*z + b3*m1z
m1 ~ a1*x + a2*z + a3*xz

fullIE1 := (a1 + a3 * (-1.244962)) * (b1 + b3 * (-1.244962)) * b2
fullIE2 := (a1 + a3 * 1.369550) * (b1 + b3 * 1.369550) * b2
mmTest := fullIE1 - fullIE2
"

out6.1 <- sem(mod6.1, data = dat1, se = "boot")
summary(out6.1)
parameterEstimates(out6.1, boot.ci.type = "bca.simple")


mod6.2 <- "
m2 ~ cp*x + b1*m1 + b2*z + b3*m1z
m1 ~ a1*x + a2*z + a3*xz

fullIE1 := (a1 + a3 * (-1.0)) * (b1 + b3 * (-1.0))
fullIE2 := (a1 + a3 * (-1.05)) * (b1 + b3 * (-1.05))
mmTest := fullIE1 - fullIE2
"

out6.2 <- sem(mod6.2, data = dat1, se = "boot")
summary(out6.2)
parameterEstimates(out6.2, boot.ci.type = "bca.simple")


mod7.1 <- "
m2 ~ cp*x + b*m1
m1 ~ a1*x + a2*z + a3*xz

mmIndex := a3*b
fullIE1 := (a1 + a3 * (-1.244962)) * b
fullIE2 := (a1 + a3 * 1.369550) * b
mmTest2 := fullIE1 - fullIE2
"

out7.1 <- sem(mod7.1, data = dat1, se = "boot")
summary(out7.1)
parameterEstimates(out7.1, boot.ci.type = "bca.simple")


mod7.2 <- "
m2 ~ cp*x + b*m1
m1 ~ a1*x + a2*z + a3*xz

fullIE1 := (a1 + a3 * (-1.51)) * b
fullIE2 := (a1 + a3 * (-1.5)) * b
mmTest2 := fullIE1 - fullIE2
"

out7.2 <- sem(mod7.2, data = dat1, se = "boot")
summary(out7.2)
parameterEstimates(out7.2, boot.ci.type = "bca.simple")
