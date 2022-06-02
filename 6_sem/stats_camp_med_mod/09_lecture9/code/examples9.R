### Title:    Mediation & Moderation: Lecture 9 Examples
### Author:   Kyle M. Lang
### Created:  2016-MAR-17
### Modified: 2016-JUN-09

dataDir <- "../data/"

library(rockchalk)
library(lavaan)
library(semTools)

## Read in data:
dat1 <- readRDS(paste0(dataDir, "bfiData1.rds"))
dat2 <- readRDS(paste0(dataDir, "bfiData2.rds"))

out1.1 <- lm(agree ~ conc + open + neuro, data = dat1)
summary(out1.1)

out1.2 <- lm(agree ~ open*conc + open*neuro, data = dat1)
summary(out1.2)

dat1$concLo  <- with(dat1, conc - quantile(conc, 0.25, na.rm = TRUE))
dat1$concMid <- with(dat1, conc - quantile(conc, 0.5, na.rm = TRUE))
dat1$concHi  <- with(dat1, conc - quantile(conc, 0.75, na.rm = TRUE))

out1.2.1 <- lm(agree ~ open*concLo + neuro, data = dat1)
summary(out1.2.1)

out1.2.2 <- lm(agree ~ open*concMid + neuro, data = dat1)
summary(out1.2.2)

out1.2.3 <- lm(agree ~ open*concHi + neuro, data = dat1)
summary(out1.2.3)

plotSlopes(model = out1.2,
           plotx = "open",
           modx = "conc",
           plotPoints = FALSE,
           modxVals =
               quantile(dat1$conc,
                        c(0.25, 0.5, 0.75),
                        na.rm = TRUE)
           )

out1.3 <- lm(agree ~ open*conc*neuro, data = dat1)
summary(out1.3)

dat1$neuroLo <-
    with(dat1,
         neuro - (mean(neuro, na.rm = TRUE) - sd(neuro, na.rm = TRUE))
         )
dat1$neuroMid <- with(dat1, neuro - mean(neuro, na.rm = TRUE))
dat1$neuroHi <-
    with(dat1,
         neuro - (mean(neuro, na.rm = TRUE) + sd(neuro, na.rm = TRUE))
         )

out1.4.1 <- lm(agree ~ open*conc*neuroLo, data = dat1)
summary(out1.4.1)

out1.4.2 <- lm(agree ~ open*conc*neuroMid, data = dat1)
summary(out1.4.2)

out1.4.3 <- lm(agree ~ open*conc*neuroHi, data = dat1)
summary(out1.4.3)

dat1$openXneuro <- with(dat1, neuro*open)
dat1$concXneuro <- with(dat1, neuro*conc)
dat1$openXconc <- with(dat1, open*conc)
dat1$openXconcXneuro <- with(dat1, open*conc*neuro)

out1.5 <- lm(agree ~ open + conc + neuro +
                 openXconc + openXneuro + concXneuro + openXconc*neuro,
             data = dat1)
summary(out1.5)

coef(out1.3) - coef(out1.5)

plotOut1.5 <- plotSlopes(model = out1.5,
                         plotx = "openXconc",
                         modx = "neuro",
                         plotPoints = FALSE)
testOut1.5 <- testSlopes(plotOut1.5)
plot(testOut1.5)

## Categorical moderator:
out2.1 <- lm(conc ~ neuro, data = dat1)
summary(out2.1)

out2.2 <- lm(conc ~ neuro*educ, data = dat1)
summary(out2.2)

plotSlopes(out2.2,
           plotx = "neuro",
           modx = "educ",
           plotPoints = FALSE)

ssSubHS <- coef(out2.2)[2]
ssHighSchool <- sum(coef(out2.2)[c(2, 5)])
ssCollege <- sum(coef(out2.2)[c(2, 6)])

ssSubHS
ssHighSchool
ssCollege

dat1$educ2 <- relevel(dat1$educ, ref = "highSchool")
dat1$educ3 <- relevel(dat1$educ, ref = "college")

out2.3 <- lm(conc ~ neuro*educ2, data = dat1)
summary(out2.3)

out2.4 <- lm(conc ~ neuro*educ3, data = dat1)
summary(out2.4)


## Multiple group moderation:
mod1 <- "
conc =~ C1 + C2 + C3 + C4 + C5
neuro =~ N1 + N2 + N3 + N4 + N5
"

fit1 <- measurementInvariance(mod1,
                              data = dat2,
                              group = "educ",
                              std.lv = TRUE)

mod2 <- "
conc =~ C1 + C2 + C3 + C4 + C5
neuro =~ N1 + N2 + N3 + N4 + N5

conc ~ neuro

conc ~~ c(1.0, NA, NA)*conc
neuro ~~ c(1.0, NA, NA)*neuro

conc ~ c(0.0, NA, NA)*1.0
neuro ~ c(0.0, NA, NA)*1.0
"

fit2 <- lavaan(mod2,
               data = dat2,
               std.lv = FALSE,
               auto.fix.first = FALSE,
               auto.var = TRUE,
               int.ov.free = TRUE,
               group = "educ",
               group.equal = c("loadings", "intercepts")
               )
summary(fit2)


mod3 <- "
conc =~ C1 + C2 + C3 + C4 + C5
neuro =~ N1 + N2 + N3 + N4 + N5

conc ~ c(b1, b1, b1)*neuro

conc ~~ c(1.0, NA, NA)*conc
neuro ~~ c(1.0, NA, NA)*neuro

conc ~ c(0.0, NA, NA)*1.0
neuro ~ c(0.0, NA, NA)*1.0
"

fit3 <- lavaan(mod3,
               data = dat2,
               std.lv = FALSE,
               auto.fix.first = FALSE,
               auto.var = TRUE,
               int.ov.free = TRUE,
               group = "educ",
               group.equal = c("loadings", "intercepts")
               )
summary(fit3)

diffVec <- fitMeasures(fit3)[c("chisq", "df")] -
    fitMeasures(fit2)[c("chisq", "df")]

pchisq(diffVec[1], diffVec[2], lower = FALSE)


mod4 <- "
conc =~ C1 + C2 + C3 + C4 + C5
neuro =~ N1 + N2 + N3 + N4 + N5

conc ~ c(b1, b1, b2)*neuro

conc ~~ c(1.0, NA, NA)*conc
neuro ~~ c(1.0, NA, NA)*neuro

conc ~ c(0.0, NA, NA)*1.0
neuro ~ c(0.0, NA, NA)*1.0
"

fit4 <- lavaan(mod4,
               data = dat2,
               std.lv = FALSE,
               auto.fix.first = FALSE,
               auto.var = TRUE,
               int.ov.free = TRUE,
               group = "educ",
               group.equal = c("loadings", "intercepts")
               )
summary(fit4)

diffVec <- fitMeasures(fit4)[c("chisq", "df")] -
    fitMeasures(fit2)[c("chisq", "df")]

pchisq(diffVec[1], diffVec[2], lower = FALSE)

tmp <- inspect(fit2, "est")

ints <- c(0,
          coef(fit2)[c("conc~1.g2",
                       "conc~1.g3")]
          )
slopes <- coef(fit2)[c("conc~neuro",
                       "conc~neuro.g2",
                       "conc~neuro.g3")]

fScores <- do.call(rbind, predict(fit2))
plot(y = fScores[ , "conc"],
     x = fScores[ , "neuro"],
     type = "n",
     main = "Simple Slopes",
     xlab = "Neuroticism",
     ylab = "Conscientiousness")

abline(a = ints[1], b = slopes[1])
abline(a = ints[2], b = slopes[2], col = "red")
abline(a = ints[3], b = slopes[3], col = "blue")

legend(x = "topright",
       inset = 0.01,
       legend =
           c("< High School",
             "High School",
             "College"),
       col =
           c("black",
             "red",
             "blue"),
       lty = 1)

       
?abline
