installed.packages("WaveletComp")
library("WaveletComp")
citation("WaveletComp")

start.period <- 50
series.length <- 1000

x = periodic.series(start.period, series.length)
x = x + + rnorm(series.length)

my.data <- data.frame(x = x)

# Taken from page 10 in https://cran.r-project.org/web/packages/WaveletComp/WaveletComp.pdf
matplot(my.data, type = "l", lty = 1, xaxs = "i", col = 1:2,
        xlab = "years", ylab = "years",
        main = "[Enter Renewable Energy Source] Production in the [NYISO Zone]",
        sub = "sub")