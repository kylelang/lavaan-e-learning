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
    geom_vline(xintercept = 0) +
    theme(axis.line.y = element_line(colour = "white")) +
    xlab("Horsepower") + 
    ylab("MPG") +
    xlim(c(0, 300)) +
    ylim(c(0, 50))
p7

Cars93$hpC <- with(Cars93, Horsepower - mean(Horsepower))

p8 <- ggplot(data = Cars93, mapping = aes(x = hpC, y = MPG.city)) +
    theme_classic()
p9 <- p8 + geom_point() +
    theme(text = element_text(family = "Courier", size = 16))

p10 <- p9 + 
    geom_smooth(method  = "lm", 
                formula = y ~ x + I(x^2), 
                se      = FALSE, 
                colour  = "red") +
    geom_vline(xintercept = 0) +
    theme(axis.line.y = element_line(colour = "white")) +
    xlab("Horsepower") +
    ylab("MPG") +
    xlim(c(-100, 200)) +
    ylim(c(0, 50))
p10

tmp1 <- lm(MPG.city ~ Horsepower + I(Horsepower^2), data = Cars93)
p7 + geom_abline(slope     = coef(tmp1)[2], 
                  intercept = coef(tmp1)[1], 
                  colour    = "purple") +
    geom_hline(yintercept = coef(tmp1)[1], 
               color      = "gray", 
               size       = 1.5,
               linetype   = "dashed")

tmp2 <- lm(MPG.city ~ hpC + I(hpC^2), data = Cars93)
p10 + geom_abline(slope     = coef(tmp2)[2], 
                  intercept = coef(tmp2)[1], 
                  colour    = "purple") +
    geom_hline(yintercept = coef(tmp2)[1], 
               color      = "gray", 
               size       = 1.5, 
               linetype   = "dashed")
