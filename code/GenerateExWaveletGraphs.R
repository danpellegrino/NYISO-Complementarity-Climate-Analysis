
library("WaveletComp")
library("reticulate")
citation("WaveletComp")

# Check if the user needs to install the required packages
if (!require("reticulate")) {
  install.packages("reticulate")
  library("reticulate")
}
if (!require("WaveletComp")) {
  install.packages("WaveletComp")
  library("WaveletComp")
}

directory <- "data/"

# Run a Python script to create the data files
py_run_file("code/CreateExampleCSV.py")

# Load the data
file1 <- paste(directory, "data_example1.csv", sep = "")
file2 <- paste(directory, "data_example2.csv", sep = "")

data1 <- read.csv(file1)
data2 <- read.csv(file2)

data1[, 1] <- as.Date(data1[, 1], format = "%Y-%m")
data2[, 1] <- as.Date(data2[, 1], format = "%Y-%m")

# Merge the data
my_data <- data.frame(x = data1[, 2], y = data2[, 2])

my_wc <- analyze.coherency(my_data)

png(paste("output/WaveletCoherence_Example.png", sep = ""),
    width = 800, height = 800, units = "px")

title <- paste("Example Wavelet Coherence", sep = "")
wc.image(my_wc,  n.levels = 250,
            legend.params = list(lab = title),
            timelab = "time (months)",
            periodlab = "period (months)")

dev.off()