library("WaveletComp")
library("reticulate")
citation("WaveletComp")

# Check if the user needs to install the required packages
if (!require("WaveletComp")) {
  install.packages("WaveletComp")
  library("WaveletComp")
}
if (!require("reticulate")) {
  install.packages("reticulate")
  library("reticulate")
}

directory <- "data/MRI-AGCM3.2/"

# Ask the user if they need to convert the NetCDF files to CSV
convert <- ""
convert_prompt <- "Do you need to convert the NetCDF files to CSV? (Y/N): "
while (!(convert %in% c("Y", "N"))) {
  convert <- readline(prompt = convert_prompt)
  convert <- as.character(convert)
  convert <- toupper(convert)
  if (!(convert %in% c("Y", "N"))) {
    cat("\n", convert, "is not a valid option.\n\n")
  }
}

if (convert == "Y" || convert == "y") {
  # Convert the NetCDF files to CSV
  py_run_file("code/ExtractData.py")
}

# Do you want to compare all the zones for all the components?
confirm_all <- ""
all_prompt <- "Compare all the zones for all the components? (Y/N): "
while (!(confirm_all %in% c("Y", "N"))) {
  confirm_all <- readline(prompt = all_prompt)
  confirm_all <- as.character(confirm_all)
  confirm_all <- toupper(confirm_all)
  if (!(confirm_all %in% c("Y", "N"))) {
    cat("\n", confirm_all, "is not a valid option.\n\n")
  }
}

if (confirm_all != "Y") {
  zones <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
  components <- c("1", "2", "3", "4")

  # Ask the user which first NYISO Zone they want to compare
  nyszone1 <- ""
  nyszone_prompt1 <- paste("Enter the first NYISO Zone for analyzing:",
                           "\nA (West)",
                           "\nB (Genessee)",
                           "\nC (Central)",
                           "\nD (North)",
                           "\nE (Mohawk Valley)",
                           "\nF (Capital)",
                           "\nG (Hudson Valley)",
                           "\nH (Millwood)",
                           "\nI (Dunwoodie)",
                           "\nJ (NYC)",
                           "\nK (Long Island)",
                           "\n  Option: ")

  while (!(nyszone1 %in% c(zones))) {
    nyszone1 <- readline(prompt = nyszone_prompt1)
    nyszone1 <- as.character(nyszone1)
    nyszone1 <- toupper(nyszone1)
    if (!(nyszone1 %in% c(zones))) {
      cat("\n", nyszone1, "is not a valid NYISO Zone.\n\n")
    }
  }
  # Ask the component they want to analyze for the first NYISO Zone
  cat("\n")

  component1 <- ""
  component_prompt1 <- paste("What component for the first NYISO Zone?",
                             "\n 1. Wind Speed",
                             "\n 2. Precipitation",
                             "\n 3. Snowfall",
                             "\n 4. Solar Radiation",
                             "\n  Option: ")
  while (!(component1 %in% c(components))) {
    component1 <- readline(prompt = component_prompt1)
    component1 <- as.integer(component1)
    if (!(component1 %in% c(components))) {
      cat("\n", component1, "is not a valid option.\n\n")
    }
  }

  if (component1 == 1) {
    component_variable1 <- "sfcWind"
    component_name1 <- "Wind Speed"
  } else if (component1 == 2) {
    component_variable1 <- "pr"
    component_name1 <- "Precipitation"
  } else if (component1 == 3) {
    component_variable1 <- "prsn"
    component_name1 <- "Snowfall Flux"
  } else if (component1 == 4) {
    component_variable1 <- "rsds"
    component_name1 <- "Solar Radiation"
  }

  cat("\n")

  # Ask the user which second NYISO Zone they want to compare
  nyszone2 <- ""
  nyszone_prompt2 <- paste("Enter the second NYISO Zone for analyzing:",
                           "\nA (West)",
                           "\nB (Genessee)",
                           "\nC (Central)",
                           "\nD (North)",
                           "\nE (Mohawk Valley)",
                           "\nF (Capital)",
                           "\nG (Hudson Valley)",
                           "\nH (Millwood)",
                           "\nI (Dunwoodie)",
                           "\nJ (NYC)",
                           "\nK (Long Island)",
                           "\n  Option: ")

  while (!(nyszone2 %in% c(zones))) {
    nyszone2 <- readline(prompt = nyszone_prompt2)
    nyszone2 <- as.character(nyszone2)
    nyszone2 <- toupper(nyszone2)
    if (!(nyszone2 %in% c(zones))) {
      cat("\n", nyszone2, "is not a valid NYISO Zone.\n\n")
    }
  }
  # Ask the component they want to analyze for the second NYISO Zone
  cat("\n")

  component2 <- ""
  component_prompt2 <- paste("What component for the second NYISO Zone?",
                             "\n 1. Wind Speed",
                             "\n 2. Precipitation",
                             "\n 3. Snowfall",
                             "\n 4. Solar Radiation",
                             "\n  Option: ")
  while (!(component2 %in% c(components))) {
    component2 <- readline(prompt = component_prompt2)
    component2 <- as.integer(component2)
    if (!(component2 %in% c(components))) {
      cat("\n", component2, "is not a valid option.\n\n")
    }
  }

  if (component2 == 1) {
    component_variable2 <- "sfcWind"
    component_name2 <- "Wind Speed"
  } else if (component2 == 2) {
    component_variable2 <- "pr"
    component_name2 <- "Precipitation"
  } else if (component2 == 3) {
    component_variable2 <- "prsn"
    component_name2 <- "Snowfall Flux"
  } else if (component2 == 4) {
    component_variable2 <- "rsds"
    component_name2 <- "Solar Radiation"
  }

  cat("\n")

  # Load the data
  file1 <- paste(directory, component_variable1, "/",
                 "_zone_", nyszone1, ".csv", sep = "")
  file2 <- paste(directory, component_variable2, "/",
                 "_zone_", nyszone2, ".csv", sep = "")

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

  png(paste("output/",
            nyszone1, "_", component_variable1, "_vs_",
            nyszone2, "_", component_variable2, ".png", sep = ""),
      width = 800, height = 600)

  # Plot the two graphs together
  plot(data1[, 1], type = "l", col = "blue",
       xlab = "Day of the Year", ylab = "Normalized Value",
       ylim = c(min(data1[, 1], data2[, 1]), max(data1[, 1], data2[, 1])),
       main = paste("Zone", nyszone1, component_name1,
                    "against Zone", nyszone2, component_name2))

  lines(data2[, 1], col = "red")

  legend("topleft",
         legend = c(component_name1, component_name2),
         col = c("blue", "red"), lty = 1)

  dev.off()
} else {
  completed <- c()
  # Compare all the zones for all the components
  for (i in 1:length(zones)) {
    for (j in 1:length(components)) {
      for (k in 1:length(zones)) {
        for (l in 1:length(components)) {
          if (i != k) {
            if (k %in% completed) {
              next
            }
            if (j == 1) {
              component_variable1 <- "sfcWind"
              component_name1 <- "Wind Speed"
            } else if (j == 2) {
              component_variable1 <- "pr"
              component_name1 <- "Precipitation"
            } else if (j == 3) {
              component_variable1 <- "prsn"
              component_name1 <- "Snowfall Flux"
            } else if (j == 4) {
              component_variable1 <- "rsds"
              component_name1 <- "Solar Radiation"
            }

            if (l == 1) {
              component_variable2 <- "sfcWind"
              component_name2 <- "Wind Speed"
            } else if (l == 2) {
              component_variable2 <- "pr"
              component_name2 <- "Precipitation"
            } else if (l == 3) {
              component_variable2 <- "prsn"
              component_name2 <- "Snowfall Flux"
            } else if (l == 4) {
              component_variable2 <- "rsds"
              component_name2 <- "Solar Radiation"
            }
            print(paste("Comparing Zone", zones[i], component_name1,
                        "against Zone", zones[k], component_name2))
            # Load the data
            file1 <- paste(directory, component_variable1, "/",
                           "_zone_", zones[i], ".csv", sep = "")
            file2 <- paste(directory, component_variable2, "/",
                           "_zone_", zones[k], ".csv", sep = "")

            data1 <- read.csv(file1)
            data2 <- read.csv(file2)

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

            png(paste("output/",
                      zones[i], "_", component_variable1, "_vs_",
                      zones[k], "_", component_variable2, ".png", sep = ""),
                width = 800, height = 600)

            # Plot the two graphs together
            plot(data1[, 1], type = "l", col = "blue",
                 xlab = "Day of the Year", ylab = "Normalized Value",
                 ylim = c(min(data1[, 1], data2[, 1]),
                          max(data1[, 1], data2[, 1])),
                 main = paste("Zone", zones[i], component_name1,
                              "against Zone", zones[k], component_name2))

            lines(data2[, 1], col = "red")

            legend("topleft",
                   legend = c(component_name1, component_name2),
                   col = c("blue", "red"), lty = 1)

            dev.off()
          }
        }
      }
    }
    finished <- paste("Finished comparing Zone",
                      zones[i], "against all other zones.")
    completed <- c(completed, i)
    print(finished)
  }
}