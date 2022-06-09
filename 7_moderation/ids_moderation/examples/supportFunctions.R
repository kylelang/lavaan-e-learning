### Title:    Support Functions for Examples
### Author:   Kyle M. Lang
### Created:  2017-AUG-24
### Modified: 2017-AUG-24


rangeNorm <- function(x, newMin = 0.0, newMax = 1.0) {

    r0 <- diff(range(x))
    r1 <- newMax - newMin

    (x * r1 - min(x) * r1) / r0 + newMin
}
