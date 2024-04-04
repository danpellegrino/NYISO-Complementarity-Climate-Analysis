library("reticulate")

# Check if the user needs to install the required packages
if (!require("reticulate")) {
  install.packages("reticulate")
  library("reticulate")
}

directory <- "data/"

# Run a Python script to create the data files
py_run_file("code/CreateExampleCSV.py")

# Load the data
file1 <- paste(directory, "file1.csv", sep = "")
file2 <- paste(directory, "file2.csv", sep = "")

data1 <- read.csv(file1)
data2 <- read.csv(file2)

# grab the day and month from the data (it's formatted as YYYY-MM-DD)
data1$day <- as.numeric(format(as.Date(data1[, 1]), "%d"))
data1$month <- as.numeric(format(as.Date(data1[, 1]), "%m"))
data2$day <- as.numeric(format(as.Date(data2[, 1]), "%d"))
data2$month <- as.numeric(format(as.Date(data2[, 1]), "%m"))

data1 <- aggregate(data1[, 2:ncol(data1)],
                   by = list(data1$day, data1$month), FUN = mean)
data2 <- aggregate(data2[, 2:ncol(data2)],
                   by = list(data2$day, data2$month), FUN = mean)

# remove the day and month columns
data1 <- data1[, -c(1, 2)]
data2 <- data2[, -c(1, 2)]

# Normalize the data
data1[, 1] <- (data1[, 1] - mean(data1[, 1])) / sd(data1[, 1])
data2[, 1] <- (data2[, 1] - mean(data2[, 1])) / sd(data2[, 1])

# Smooth the data
data1[, 1] <- smooth.spline(data1[, 1])$y
data2[, 1] <- smooth.spline(data2[, 1])$y

png(paste("output/TimeSeriesComparisonExample.png", sep = ""),
    width = 800, height = 600)

# Plot the two graphs together
plot(data1[, 1], type = "l", col = "blue",
    xlab = "Day of the Year", ylab = "Normalized Value",
    ylim = c(min(data1[, 1], data2[, 1]), max(data1[, 1], data2[, 1])),
    main = paste("Comparison Example of Time Series Data"))

lines(data2[, 1], col = "red")

legend("topleft",
      legend = c("[Value 1]", "[Value 2]"),
      col = c("blue", "red"), lty = 1)

dev.off()