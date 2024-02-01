installed.packages("WaveletComp")
library("WaveletComp")
citation("WaveletComp")

x1 <- periodic.series(start.period = 1 * 24, length = 24 * 96)
x2 <- periodic.series(start.period = 2 * 24, length = 24 * 96)
x3 <- periodic.series(start.period = 4 * 24, length = 24 * 96)
x4 <- periodic.series(start.period = 8 * 24, length = 24 * 96)
x5 <- periodic.series(start.period = 16 * 24, length = 24 * 96)
x <- x1 + x2 + 3 * x3 + x4 + x5 + 0.5 * rnorm(24 * 96)
y <- x1 + x2 - 3 * x3 + x4 + 3 * x5 + 0.5 * rnorm(24 * 96)

my_data <- data.frame(x = x, y = y)
my_wc <- analyze.coherency(my_data, my.pair = c("x", "y"),
                           loess.span = 0,
                           dt = 1 / 24, dj = 1 / 100,
                           lowerPeriod = 1 / 2,
                           make.pval = TRUE, n.sim = 10)

wc.image(my_wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (days)")