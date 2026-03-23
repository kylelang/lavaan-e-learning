library(lavaan)
library(psych)
library(mvtnorm)

rm(list = ls(all = TRUE))

data(bfi, package = "psychTools")

keyVec <- unlist(bfi.keys) |>
  grepl("^-", x = _) |> 
  ifelse(-1, 1) |>
  c(rep(1, 3))

bfi2 <- reverse.code(key = keyVec, mini = 1, maxi = 5, items = bfi)

colnames(bfi2) <- colnames(bfi)

mod0 <- '
agree =~ A1 + A2 + A3 + A4 + A5
open =~ O1 + O2 + O3 + O4 + O5
extra =~ E1 + E2 + E3 + E4 + E5
neuro =~ N1 + N2 + N3 + N4 + N5
'

mod0 <- '
agree =~ A1 + A2 + A3 
open =~ O1 + O2 + O3 
extra =~ E1 + E2 + E3 
neuro =~ N1 + N2 + N3 
'

mod1 <- paste(mod0,
  'ao =~ agree + open',
  sep = '\n')

mod2 <- paste(mod0,
  'ao =~ l1 * agree + l1 * open',
  sep = '\n')

mod0 <- '
ao =~ A1 + A2 + A3 + A4 + A5 + O1 + O2 + O3 + O4 + O5
'

mod1 <- '
agree =~ A1 + A2 + A3 + A4 + A5
open =~ O1 + O2 + O3 + O4 + O5
'

mod2 <- paste(mod1,
  'ao =~ l1 * agree + l1 * open',
  sep = '\n')

mod0 <- '
open =~ O1 + O2 + O3 + O4 + O5
extra =~ E1 + E2 + E3 + E4 + E5
'

mod1 <- paste(mod0,
  'oe =~ l1 * open + l2 * extra',
  'l1 := -1 * l2',
  sep = '\n')

out0 <- cfa(mod0, data = bfi2, std.lv = TRUE)
out1 <- cfa(mod1, data = bfi2, std.lv = TRUE)
out2 <- cfa(mod2, data = bfi2, std.lv = TRUE)

summary(out0)
summary(out1)
summary(out2)

fitMeasures(out0) - fitMeasures(out1)
fitMeasures(out0) - fitMeasures(out2)

anova(out0, out1)
anova(out0, out2)
anova(out1, out2)

#----------------------------------------------------------------------------------------------------------------------#

set.seed(235711)

n <- 5000
nX <- 3
nY <- 4
lam <- 0.7
p0 <- 0.5

Psi <- matrix(p0, nX, nX)
diag(Psi) <- 1.0

eta <- rmvnorm(n, rep(0, nX), Psi)
lambda <- rep(c(rep(lam, nY), rep(0, nX * nY)), nX) |> 
  head(nX^2 * nY) |>
  matrix(ncol = nX)
theta <- diag(nX * nY) * (1 - lam^2)

Y <- eta %*% t(lambda) + rmvnorm(n, rep(0, nX * nY), theta)

colnames(Y) <- paste0(rep(letters[1:nX], each = nY), 1:nY)

aNames <- grep("^a", colnames(Y), value = TRUE)
bNames <- grep("^b", colnames(Y), value = TRUE)
cNames <- grep("^c", colnames(Y), value = TRUE)

abMod <- paste("fAB",
  paste(c(aNames, bNames), collapse = " + "),
  sep = " =~ ")

aMod <- paste("fA",
  paste(aNames, collapse = " + "),
  sep = " =~ ")
bMod <- paste("fB",
  paste(bNames, collapse = " + "),
  sep = " =~ ")
cMod <- paste("fC",
  paste(cNames, collapse = " + "),
  sep = " =~ ")

mod0 <- paste(abMod, cMod, sep = "\n")
mod1 <- paste(aMod, bMod, cMod, sep = "\n")

mod2 <- paste(mod1,
  "f0 =~ l1 * fA + l1 * fB",
  sep = "\n")

mod2 <- paste(mod1,
  "f0 =~ fA + fB",
  sep = "\n")

out0 <- cfa(mod0, data = Y, std.lv = TRUE)
out1 <- cfa(mod1, data = Y, std.lv = TRUE)
out2 <- cfa(mod2, data = Y, std.lv = TRUE)

summary(out0)
summary(out1)
summary(out2)

fitMeasures(out0)
fitMeasures(out1)
fitMeasures(out2)

anova(out0, out1)
anova(out0, out2)
anova(out1, out2)

mod01 <- paste(mod0, "fAB ~ fC", sep = "\n")
mod21 <- paste(mod2, "f0 ~ fC", sep = "\n")

out01 <- cfa(mod01, data = Y, std.lv = TRUE)
out21 <- cfa(mod21, data = Y, std.lv = TRUE)

summary(out01)
summary(out21)

fitMeasures(out01)
fitMeasures(out21)

anova(out0, out1)
