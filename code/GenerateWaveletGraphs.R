library("WaveletComp")
citation("WaveletComp")

data1 <- read.csv("data/MRI-AGCM3.2/rsds/_zone_A.csv")
data2 <- read.csv("data/MRI-AGCM3.2/rsds/_zone_C.csv")

data1[, 1] <- as.Date(data1[, 1], format = "%Y-%m-%d")
component_ts1 <- ts(data1[, 2], start = c(1950, 1, 1),
                    end = c(2099, 12, 31), frequency = 365)

data2[, 1] <- as.Date(data2[, 1], format = "%Y-%m-%d")
component_ts2 <- ts(data2[, 2], start = c(1950, 1, 1),
                    end = c(2099, 12, 31), frequency = 365)
# Wavelet analysis
my_data <- data.frame(component_ts1, component_ts2)

my_wc <- analyze.coherency(my_data, my.pair = c(1, 2),
                           loess.span = 0,
                           dt = 1, dj = 1 / 100,
                           make.pval = TRUE, n.sim = 10)

wc.image(my_wc, n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05, ## default values
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "")