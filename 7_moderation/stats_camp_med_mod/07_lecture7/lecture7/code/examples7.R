### Title:    Med/Mod Lecture7: Examples
### Author:   Kyle M. Lang
### Created:  2016-MAR-17
### Modified: 2017-MAR-20

dataDir <- "../data/"
fileName <- "nlsyData.rds"
dat1 <- readRDS(paste0(dataDir, fileName))

out1 <- lm(depress1 ~ ratio1, data = dat1)
summary(out1)

out2 <- lm(depress1 ~ ratio1 + perception1, data = dat1)
summary(out2)

out3 <- lm(depress1 ~ ratio1*perception1, data = dat1)
summary(out3)

## Plot the conditional effects:
meanX <- mean(dat1$ratio1)
meanZ <- mean(dat1$perception1)
sdX <- sd(dat1$ratio1)
sdZ <- sd(dat1$perception1)

xVals <- c(meanX - sdX, meanX, meanX + sdX)

preds1 <- cbind(1,
                xVals,
                meanZ - sdZ,
                xVals * (meanZ - sdZ)
                )
preds2 <- cbind(1,
                xVals,
                meanZ,
                xVals * meanZ
                )
preds3 <- cbind(1,
                xVals,
                meanZ + sdZ,
                xVals * (meanZ + sdZ)
                )

yHats1 <- preds1 %*% matrix(coef(out3))
yHats2 <- preds2 %*% matrix(coef(out3))
yHats3 <- preds3 %*% matrix(coef(out3))

plot(yHats1,
     ylim = range(c(yHats1, yHats2, yHats3)),
     type = "l",
     lty = 3,
     xlab = "Weight:Height Ratio",
     ylab = "Predicted Depression",
     main = "Conditional Effects of\nWeight:Height Ratio on Depression Scores")
lines(yHats2)
lines(yHats3,
      lty = 5)

legend(x = "topleft",
       inset = 0.05,
       legend =
           c("Mean Perception -1 SD",
             "Mean Perception",
             "Mean Perception +1 SD"),
       lty = c(3, 1, 5)
       )

## Pick-a-point probing:
getSS <- function(z, lmOut) {
    tmp <- coef(lmOut)
    tmp[2] + tmp[4]*z
}

getSE <- function(z, lmOut) {
    tmp <- vcov(lmOut)
    varB1 <- tmp[2, 2]
    varB3 <- tmp[4, 4]
    covB13 <- tmp[4, 2]

    sqrt(varB1 + 2 * z * covB13 + z^2 * varB3)
}

ssVec <- sapply(c(meanZ - sdZ,
                  meanZ,
                  meanZ + sdZ),
                FUN = getSS,
                lmOut = out3)

seVec <- sapply(c(meanZ - sdZ,
                  meanZ,
                  meanZ + sdZ),
                FUN = getSE,
                lmOut = out3)

waldVec <- ssVec / seVec
names(waldVec) <- c("Mean - SD", "Mean", "Mean + SD")
waldVec

ciMat <- cbind(ssVec - 1.96 * seVec,
               ssVec + 1.96 * seVec)
rownames(ciMat) <- c("Mean - SD", "Mean", "Mean + SD")
colnames(ciMat) <- c("LB", "UB")
ciMat
