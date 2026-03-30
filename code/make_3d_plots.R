### Title:    Generate 3D Response Surface Plots
### Author:   Kyle M. Lang
### Created:  2017-08-24
### Modified: 2026-03-30

rm(list = ls(all = TRUE))

plotDir <- "resources/figures"
dataDir <- "data"

library(visreg)
library(lattice)

## Load the data:
dat1 <- readRDS(here::here(dataDir, "diabetes.rds"))

## Center some variables:
dat1$age30  <- dat1$age - 30
dat1$ldl100 <- dat1$ldl - 100
dat1$hdl60  <- dat1$hdl - 60
dat1$bmi25  <- dat1$bmi - 25

## Estimate an additive model:
out0 <- lm(bp ~ ldl + bmi, data = dat1)
summary(out0)

## Estimate a moderated model:
out1 <- lm(bp ~ ldl * bmi, data = dat1)
summary(out1)

###------------------------------------------------------------------------------------------------------------------###

### NOTE: These quick-and-dirty visualizations are not used in the lecture slides ###

## Quick-and-dirty 3D visualization of the additive model:
tmp0 <- visreg2d(fit       = out0,
                 x         = "bmi",
                 y         = "ldl",
                 plot.type = "persp",
                 col       = c("blue", "red"),
                 xlab      = "BMI",
                 ylab      = "LDL",
                 zlab      = "BP")

## Quick-and-dirty visualization of the moderated model:
tmp1 <- visreg2d(fit       = out1,
                 x         = "bmi",
                 y         = "ldl",
                 plot.type = "persp",
                 col       = c("blue", "red"),
                 xlab      = "BMI",
                 ylab      = "LDL",
                 zlab      = "BP")

###------------------------------------------------------------------------------------------------------------------###

### NOTE: These visualizations are the ones used in the lecture slides ###

## Prep data for the lattice-based visualization:
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

## Use lattice::cloud() to draw a 3D point cloud:
pdf(here::here(plotDir, "3d_data_plot.pdf"), family = "Courier")

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

## Define a function to add layers to the lattice plot:
myPanel <- function(x1, y1, z1, x2, y2, z2, z3, ...) {
    panel.cloud(x2, y2, z3, ...)
    panel.wireframe(x1, y1, z1, ...)
    panel.cloud(x2, y2, z2, ...)
}

pdf(here::here(plotDir, "response_surface_plot.pdf"), family = "Courier")

## Use lattice::wireframe() to draw a point cloud with a wireframe plane visualizing the response surface from the
## moderated regression model:
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

pdf(here::here(plotDir, "response_surface_plot0.pdf"), family = "Courier")

## Use lattice::wireframe() to draw a point cloud with a wireframe plane visualizing the response surface from the
## additive regression model:
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
