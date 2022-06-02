### Title:    Lecture 8 Examples
### Author:   Kyle M. Lang
### Created:  2017-AUG-24
### Modified: 2018-AUG-23

rm(list = ls(all = TRUE))

install.packages("rockchalk", repos = "http://cloud.r-project.org")

plotDir <- "../latexStuff/figures/"
dataDir <- "../data/"

source("../../../code/supportFunctions.R")

##### Example models
dDat <- readRDS("../data/diabetes.rds")

m1 <- mean(dDat$ldl)
s1 <- sd(dDat$ldl)

dDat$ldlLo  <- dDat$ldl - (m1 - s1)
dDat$ldlMid <- dDat$ldl - m1
dDat$ldlHi  <- dDat$ldl - (m1 + s1)

outLo  <- lm(bp ~ bmi*ldlLo, data = dDat)
outMid <- lm(bp ~ bmi*ldlMid, data = dDat)
outHi  <- lm(bp ~ bmi*ldlHi, data = dDat)

partSummary(outLo)

print(summary(outLo), signif.stars = FALSE)

b0Lo <- coef(outLo)[1]
b1Lo <- coef(outLo)["bmi"]

b0Mid <- coef(outMid)[1]
b1Mid <- coef(outMid)["bmi"]

b0Hi <- coef(outHi)[1]
b1Hi <- coef(outHi)["bmi"]

x    <- seq(min(dDat$bmi), max(dDat$bmi), 0.1)
dat1 <- data.frame(x    = x,
                   yLo  = b0Lo + b1Lo * x,
                   yMid = b0Mid + b1Mid * x,
                   yHi  = b0Hi + b1Hi * x)

p1 <- ggplot(data = dDat, aes(x = bmi, y = bp)) +
    theme_classic() +
    theme(text = element_text(family = "Courier", size = 16))
p2 <- p1 + geom_point(colour = "gray") +
    geom_line(mapping = aes(x = x, y = yLo, colour = "Mean LDL - 1 SD"),
              data    = dat1,
              size    = 1.5) +
    geom_line(mapping = aes(x = x, y = yMid, colour = "Mean LDL"),
              data    = dat1,
              size    = 1.5) +
    geom_line(mapping = aes(x = x, y = yHi, colour = "Mean LDL + 1 SD"),
              data    = dat1,
              size    = 1.5) +
    xlab("BMI") +
    ylab("BP")

p2 + scale_colour_manual(name = "", values = c("Mean LDL" = "black",
                                               "Mean LDL - 1 SD" = "red",
                                               "Mean LDL + 1 SD" = "blue")
                         ) +
    theme(legend.justification = c(1, 0), legend.position = c(0.975, 0.025))

p2

p2 + guides()


p2 + guides(guide_legend())

?guides
?guide_legend

?geom_abline

out12 <- lm(bp ~ bmi * ldl, data = dDat)

data(socsupport)

library(DAAG)
data(socsupport)
socSup <- socsupport

colnames(socSup) <- gsub("BDI", "bdi", colnames(socSup))
colnames(socSup) <- gsub("gender", "sex", colnames(socSup))
colnames(socSup) <- gsub("tangiblesat", "tanSat", colnames(socSup))

head(socSup)

out3 <- lm(bdi ~ tanSat, data = socSup)
partSummary(out3, -c(1, 2))

out4 <- lm(bdi ~ tanSat * sex, data = socSup)
partSummary(out4, -c(1, 2))



out0 <- lm(BDI ~ tangiblesat, data = socsupport)
summary(out0)

out1 <- lm(BDI ~ tangiblesat * gender, data = socsupport)
summary(out1)

socsupport$gender2 <- relevel(socsupport$gender, ref = "male")
levels(socsupport$gender2)

out2 <- lm(BDI ~ tangiblesat * gender2, data = socsupport)
summary(out2)

out3 <- lm(BDI ~ tangiblesat + gender, data = socsupport)
summary(out3)

round(coef(out3), 2)

coef(out1)
coef(out3)

##### Plots:

p1 <- ggplot(data = socsupport, mapping = aes(x = tangiblesat, y = BDI, colour = gender)) +
    theme_classic() +
    theme(text = element_text(family = "courier", size = 16))

?scale_color_discrete

cols <- rep("blue", nrow(socsupport))
cols[socsupport$gender == "female"] <- "red"

p1 + geom_jitter(aes(colour = gender)) + scale_color_manual(values = c("red", "blue"))


?scale_colour_manual

p2 <- p1 + geom_jitter() +
    scale_colour_manual(name = "gender", values = c("red", "blue"))

p2 + geom_abline(slope     = coef(out1)["tangiblesat"],
                 intercept = coef(out1)[1],
                 colour    = "red",
                 size      = 1.5) +
    geom_abline(slope     = coef(out2)["tangiblesat"],
                intercept = coef(out2)[1],
                colour    = "blue",
                size      = 1.5)

p2 + geom_abline(slope     = coef(out3)["tangiblesat"],
                 intercept = coef(out3)[1],
                 colour    = "red",
                 size      = 1.5) +
    geom_abline(slope     = coef(out3)["tangiblesat"],
                intercept = (coef(out3)[1] + coef(out3)["gendermale"]),
                colour    = "blue",
                size      = 1.5)


dat1 <- readRDS("../data/diabetes.rds") 

dat1$age30  <- dat1$age - 30
dat1$ldl100 <- dat1$ldl - 100
dat1$hdl60  <- dat1$hdl - 60
dat1$bmi25  <- dat1$bmi - 25

out0 <- lm(bp ~ ldl + bmi, data = dat1)
summary(out0)

tmp0 <- visreg2d(fit       = out0,
                 x         = "bmi",
                 y         = "ldl",
                 plot.type = "persp",
                 col       = c("blue", "red"),
                 xlab      = "BMI",
                 ylab      = "LDL",
                 zlab      = "BP")

out1 <- lm(bp ~ ldl * bmi, data = dat1)
summary(out1)

tmp1 <- visreg2d(fit       = out1,
                 x         = "bmi",
                 y         = "ldl",
                 plot.type = "persp",
                 col       = c("blue", "red"),
                 xlab      = "BMI",
                 ylab      = "LDL",
                 zlab      = "BP")

z1 <- tmp1$z
x1 <- rep(tmp1$x, each = length(tmp1$y))
y1 <- rep(tmp1$y, length(tmp1$x))

ldlNorm <- with(dat1, (ldl - min(ldl)) / diff(range(ldl)))
bmiNorm <- with(dat1, (bmi - min(bmi)) / diff(range(bmi)))

ramp    <- colorRampPalette(c("red", "blue"))
n       <- 100
colsLdl <- ramp(n)[ldlNorm * (n - 1) + 1]
colsBmi <- ramp(n)[bmiNorm * (n - 1) + 1]

visFlag <- resid(out1) > 0

z2 <- z3 <- dat1$bp
z2[!visFlag] <- NA
z3[visFlag]  <- NA

rz   <- range(z1, dat1$bp)
m    <- 0.05
zLim <- c(rz[1] - m * diff(rz), rz[2] + m * diff(rz))

pdf(paste0(plotDir, "3d_data_plot.pdf"), family = "Courier")

cloud(x            = bp ~ ldl * bmi,
      data         = dat1,
      screen       = c(x = 290, y = 30, z = 10),
      scales       = list(col = "black", arrows = FALSE),
      xlab         = "LDL",
      ylab         = "BMI",
      zlab         = "BP",
      par.settings = list(axis.line = list(col = 0)),
      pch          = 20,
      col.point    = colsBmi,
      cex          = 2,
      zlim         = zLim)

dev.off()

myPanel <- function(x1, y1, z1, x2, y2, z2, z3, ...) {
    panel.cloud(x2, y2, z3, ...)
    panel.wireframe(x1, y1, z1, ...)
    panel.cloud(x2, y2, z2, ...)
}

pdf(paste0(plotDir, "response_surface_plot.pdf"), family = "Courier")

wireframe(x            = z1 ~ y1 * x1,
          drape        = FALSE,
          screen       = c(x = 290, y = 30, z = 10),
          scales       = list(col = "black", arrows = FALSE),
          xlab         = "LDL",
          ylab         = "BMI",
          zlab         = "BP",
          par.settings = list(axis.line = list(col = 0)),
          col.regions  = colors(), # Exploit bug to make wireframe transparent
          panel        = myPanel,
          x2           = dat1$ldl,
          y2           = dat1$bmi,
          z2           = z2,
          z3           = z3,
          pch          = 20,
          col.point    = colsBmi,
          cex          = 2,
          zlim         = zLim)

dev.off()


z1 <- tmp0$z
x1 <- rep(tmp0$x, each = length(tmp0$y))
y1 <- rep(tmp0$y, length(tmp0$x))

rz   <- range(z1, dat1$bp)
m    <- 0.05
zLim <- c(rz[1] - m * diff(rz), rz[2] + m * diff(rz))

pdf(paste0(plotDir, "response_surface_plot0.pdf"), family = "Courier")

wireframe(x            = z1 ~ y1 * x1,
          drape        = FALSE,
          screen       = c(x = 290, y = 30, z = 10),
          scales       = list(col = "black", arrows = FALSE),
          xlab         = "LDL",
          ylab         = "BMI",
          zlab         = "BP",
          par.settings = list(axis.line = list(col = 0)),
          col.regions  = colors(), # Exploit bug to make wireframe transparent
          panel        = myPanel,
          x2           = dat1$ldl,
          y2           = dat1$bmi,
          z2           = z2,
          z3           = z3,
          pch          = 20,
          col.point    = colsBmi,
          cex          = 2,
          zlim         = zLim)

dev.off()



dat2 <- dat1[ , c("ldl", "age", "bmi")]
    
colMeans(dat1)
apply(dat1, 2, sd)
apply(dat1, 2, range)

mean(dat1$bp)
sd(dat1$bp)

out0 <- lm(age ~ sexF, data = dat1)
summary(out0)

out1 <- lm(bp ~ age, data = dat1)
out2 <- lm(bp ~ age + bmi, data = dat1)

anova(out1, out2)

out3 <- lm(bp ~ age + ldl + hdl, data = dat1)
out4 <- lm(bp ~ age + ldl + hdl + bmi, data = dat1)

tab1 <- rbind(coef(out3)[-c(1, 2)], coef(out4)[-c(1, 2, 5)])
tab1

rownames(tab1) <- c("Chol. Only", "With BMI")
colnames(tab1) <- c("LDL", "HDL")
xTab1 <- xtable(tab1, align = c("r|", "c|", "c|"), digits = 2)
print(xTab1)


s0 <- summary(out0)
s1 <- summary(out1)
s2 <- summary(out2)

f0 <- s0$fstat

pf(f0[1], f0[2], f0[3], lower = FALSE)

fStat <- s1$fstatistic["value"]
tStat <- s1$coefficients["age", "t value"]

colnames(dat1)

summary(dat1)

ls(s1)

tStat - sqrt(fStat)

summary(out2)$r.squared - summary(out1)$r.squared
anova(out1, out2)

ls(summary(out2))

out0 <- lm(bp ~ age, data = dat1)
summary(out0)

out1 <- lm(bp ~ age + ldl + hdl, data = dat1)
summary(out1)

out2 <- lm(bp ~ age + ldl + hdl + bmi, data = dat1)
summary(out2)

anova(out1, out2)

out3 <- lm(bp ~ age + bmi, data = dat1)
summary(out3)

anova(out3, out2)

with(dat1, cor(ldl, hdl))

out2 <- lm(bp ~ bmi + age, data = dat1)
summary(out2)

out3 <- lm(bp ~ age + bmi + ldl + hdl, data = dat1)
summary(out3)

with(dat1, mean(bmi[age == 30]))

with(dat1, tapply(bmi, age, mean))

summary(lm(bp ~ bmi*ldl, data = dat1))


cor(dat1[ , c("ldl", "age", "bmi")])

anova(out3, out4)




#### F plots

??fd

?FDist

x <- seq(0, 5, 0.01)
fDat <- data.frame(x = x,
                   y = df(x, 5, 500)
                   )

p3 <- ggplot(data = fDat, mapping = aes(x = x, y = y)) +
    theme_classic() +
    theme(text = element_text(size = 16, family = "Courier")) +
    labs(x = "F", y = "Density", title = "Sampling distribution of F(5, 500)") +
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

p3 + geom_line()

plot(x = x, y = y, type = "l")

#### PPCOR Examples

with(mtcars, cor.test(hp, mpg))
with(mtcars, pcor.test(hp, mpg, wt))
with(mtcars, spcor.test(mpg, hp, wt))

out1 <- lm(mpg ~ hp, data = mtcars)
out2 <- lm(mpg ~ hp + wt, data = mtcars)

s1 <- summary(out1)
s2 <- summary(out2)

s2$r.sq - s1$r.sq

with(mtcars, spcor.test(mpg, wt, hp))["estimate"]^2




data(iris)

mDat              <- iris[ , c("Petal.Length", "Species")]
colnames(mDat)    <- c("sat", "majF")
levels(mDat$majF) <- c("law", "econ", "ds")
mDat$majN         <- as.numeric(mDat$maj)

saveRDS(mDat, paste0(dataDir, "major_data.rds"))

summary(lm(sat ~ majN, data = mDat))

mDat <- readRDS(paste0(dataDir, "major_data.rds"))


gender <- factor(sample(c("male", "female"), 10, TRUE))
male   <- as.numeric(model.matrix(~gender)[ , -1])

tab1 <- data.frame(gender, male)

drink <- factor(sample(c("coffee", "tea", "juice"), 10, TRUE))
codes <- model.matrix(~drink)[ , -1]
colnames(codes) <- c("juice", "tea")

drink
codes

xTab3 <- xtable(data.frame(drink, codes), digits = 0)
print(xTab3, booktabs = TRUE)

library(psych)
data(bfi)

colnames(bfi)

bfi$gender

codes[drink == "coffee", ] <- -1
codes

data(mtcars)
data(Cars93)

?mtcars
?Cars93

mtOpt <- model.matrix(~Cars93$Man.trans.avail)[ , -1]
drive <- model.matrix(~Cars93$DriveTrain)[ , -1]

colnames(drive) <- c("front", "rear")

mtOpt.ec <- mtOpt
mtOpt.ec[mtOpt == 0] <- -1

drive.ec <- drive
drive.ec[rowSums(drive) == 0, ] <- -1
colnames(drive.ec) <- c("front.ec", "rear.ec")

dat1 <- data.frame(price = Cars93$Price,
                   mtOpt,
                   mtOpt.ec,
                   drive,
                   drive.ec)

saveRDS(dat1, paste0(dataDir, "cars_data.rds"))

head(dat1)

out1 <- lm(Price ~ Man.trans.avail, data = Cars93)
summary(out1)

out1.1 <- lm(price ~ mtOpt, data = dat1)
summary(out1.1)

out2 <- lm(Price ~ DriveTrain, data = Cars93)
summary(out2)

out2.2 <- lm(price ~ front + rear, data = dat1)
summary(out2.2)

out3 <- lm(price ~ mtOpt.ec, data = dat1)
summary(out3)

y <- dat1$price
x <- dat1$mtOpt.ec
x[x == 1] <- 0.5
x[x == -1] <- -0.5

out3.2 <- lm(y ~ x)
summary(out3.2)

coef(out3)[1] - mean(dat1$price)

mean(tapply(dat1$price, dat1$mtOpt, mean))

colnames(Cars93)

with(Cars93, plot(Horsepower, MPG.city))

p1 <- ggplot(data = Cars93, mapping = aes(x = Horsepower, y = MPG.city)) +
    theme_classic()
p2 <- p1 + geom_point() +
    theme(text = element_text(family = "Courier", size = 16))

p2 + geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, colour = "red") +
    xlim(c(0, 350)) +
    theme(axis.line.y = element_line(colour = "white")) +
    geom_vline(xintercept = 0)

p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE)

p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE)


Cars93$hpC <- with(Cars93, Horsepower - mean(Horsepower))

p1 <- ggplot(data = Cars93, mapping = aes(x = hpC, y = MPG.city)) +
    theme_classic()
p2 <- p1 + geom_point() +
    theme(text = element_text(family = "Courier", size = 16))

p22 <- p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, colour = "red") +
    geom_vline(xintercept = 0) +
    xlab("Horsepower") +
    ylab("MPG") +
    xlim(c(-100, 200))


p33 <- p22 + theme(axis.line.y = element_line(colour = "white"))

tmp <- lm(MPG.city ~ hpC + I(hpC^2), data = Cars93)

p33 + geom_abline(slope = coef(tmp)[2], intercept = coef(tmp)[1], colour = "purple") 


p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE)

p2 + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = FALSE)




## Load data:
socSup <- readRDS(paste0(dataDir, "social_support.rds"))

head(socSup)

## Focal effect:
out3 <- lm(bdi ~ tanSat, data = socSup)
partSummary(out3, -c(1, 2))

out4 <- lm(bdi ~ tanSat * sex, data = socSup)
partSummary(out4, -c(1, 2))

summary(out4)

library(MASS)
library(MLmetrics)

data(Cars93)

cv.lm(data = Cars93,
      models = c("MPG.city ~ Horsepower",
                 "MPG.city ~ Horsepower + I(Horsepower^2)"),
      K = 5)

out1 <- lm(MPG.city ~ Horsepower, data = Cars93)
summary(out1)

?set.seed

.Random.seed

set.seed(235711)

set.seed(NULL)

s0 <- .Random.seed

u0 <- runif(1000)

set.seed(235711)

u1 <- runif(1000)

.Random.seed <- s0

u01 <- runif(1000)

all.equal(u0, u01)

u0 - u01
s1 <- .Random.seed

?all.vars

models <- c("MPG.city ~ Horsepower",
            as.formula("MPG.city ~ Horsepower + I(Horsepower^2)"),
            "MPG.city ~ Horsepower + I(Horsepower^2) + I(Horsepower^3)")
            

out <- lm(mod, data = Cars93)

all.vars(formula(out), max.names = 1)

## Do K-Fold Cross-Validation with lm():
cv.lm <- function(data, models, names = NULL, K = 10, seed = NULL) {
    
    if(!is.null(seed)) set.seed(seed)
    
    ## Create a partition vector:
    part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]
    
    ## Find the DV:
    dv <- ifelse(is.character(models[[1]]),
                 trimws(strsplit(models[[1]], "~")[[1]][1]),
                 all.vars(models[[1]], max.names = 1)
                 )
    
    ## Apply over candidate models:
    cve <- sapply(X = models, FUN = function(model, data, dv, K, part) {
        ## Loop over K repititions:
        mse <- c()
        for(k in 1 : K) {
            ## Partition data:
            train <- data[part != k, ]
            valid <- data[part == k, ]
            
            ## Fit model, generate predictions, and save the MSE:
            fit    <- lm(model, data = train)
            pred   <- predict(fit, newdata = valid)
            mse[k] <- MSE(y_pred = pred, y_true = valid[ , dv])
        }
        ## Return the CVE:
        sum((table(part) / length(part)) * mse)
    },
    data = data,
    K    = K,
    dv   = dv,
    part = part)
    
    ## Name output:
    if(!is.null(names))          names(cve) <- names
    else if(is.null(names(cve))) names(cve) <- paste("Model", 1 : length(cve))
    
    cve
}


?sample

s0 <- .Random.seed
runif(100)

rm(s0)

rm(.Random.seed)

cv.lm(data = socSup, models = c(formula(out3), formula(out4)), K = 5, seed = 235711)
cv.lm(data = socSup, models = c("bdi ~ tanSat", "bdi ~ tanSat * sex"), K = 5, seed = 235711)

all.equal(s0, .Random.seed)


?reformulate

y <- "MPG.city"
x <- "Horsepower"
n <- 5
z <- NULL


f5 <- reformulate(response = "MPG.city",
                  termlabels = c("Horsepower",
                                 paste0("I(Horsepower^", 2 : 5, ")")
                                 )
                  )
)

drop.terms(terms(f5), 5, keep.response = TRUE)

polyFormula(y, x, n = c(2 : 4))





library(MASS)
library(ggplot2)
data(Cars93)

p5 <- ggplot(data = Cars93, mapping = aes(x = Horsepower, y = MPG.city)) +
    theme_classic()
p6 <- p5 + geom_point() +
    theme(text = element_text(family = "Courier", size = 16))

p7 <- p6 + geom_smooth(method  = "lm", 
                       formula = y ~ x + I(x^2), 
                       se      = FALSE, 
                       colour  = "red") +
    xlab("Horsepower") + 
    ylab("MPG") +
    xlim(c(0, 300)) +
    ylim(c(0, 50)) +
    theme(axis.line.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank()
          )          
p7

xInt <- 100

## Create some dummy data to use in generating a new y-axis:

range(p7$data$MPG.city)

yTicks <- seq(0, 50, 10)


xLen <- diff(tmp2$x.range) / 35

tmp <- data.frame(x1 = rep(xInt - xLen, length(yTicks)),
                  x2 = rep(xInt, length(yTicks)),
                  x3 = rep(xInt - xLen * 2, length(yTicks)),
                  y  = yTicks
                  )

## Shift the y-axis:
p8 <- p7 + geom_vline(xintercept = xInt) +
    geom_segment(data    = tmp,
                 mapping = aes(x = x1, y = y, xend = x2, yend = y)
                 ) +
    geom_text(data    = tmp,
              mapping = aes(x = x3, y = y, label = y),
              size    = 5,
              family  = "Courier")

p7
p8

tmp2 <- ggplot_build(p6)
tmp2 <- tmp2$layout$panel_params[[1]]
