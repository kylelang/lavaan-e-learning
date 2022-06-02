### Title:    Support Functions for Examples
### Author:   Kyle M. Lang
### Created:  2017-AUG-24
### Modified: 2017-OCT-03

## Range normalize a vector:
rangeNorm <- function(x, newMin = 0.0, newMax = 1.0) {

    r0 <- diff(range(x))
    r1 <- newMax - newMin

    (x * r1 - min(x) * r1) / r0 + newMin
}

## Create an basic ggplot object using my preferred styling:
gg0 <- function(x, y, points = TRUE) {
    p0 <- ggplot(mapping = aes(x = x, y = y)) +
        theme_classic() +
        theme(text = element_text(family = "Courier", size = 16))
    
    if(points) p0 + geom_point()
    else       p0
}
