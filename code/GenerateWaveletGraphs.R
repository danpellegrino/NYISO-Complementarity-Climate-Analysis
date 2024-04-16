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

zones <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
components <- c("1", "2", "3", "4")

if (confirm_all != "Y") {
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

  ####################
  # THE IMPORTANT PART
  ####################

  # Load the data
  file1 <- paste(directory, component_variable1, "/",
                 "_zone_", nyszone1, ".csv", sep = "")
  file2 <- paste(directory, component_variable2, "/",
                 "_zone_", nyszone2, ".csv", sep = "")

  data1 <- read.csv(file1)
  data2 <- read.csv(file2)

  data1[, 1] <- as.Date(data1[, 1], format = "%Y-%m-%d")
  data2[, 1] <- as.Date(data2[, 1], format = "%Y-%m-%d")

  # Convert the data to monthly averages
  data1 <- aggregate(data1[, 2], by = list(format(data1[, 1], "%Y-%m")), mean)
  data2 <- aggregate(data2[, 2], by = list(format(data2[, 1], "%Y-%m")), mean)

  # Fix the date column (it needs a day to be a valid date)
  data1[, 1] <- as.Date(paste(data1[, 1], "-01", sep = ""), format = "%Y-%m-%d")
  data2[, 1] <- as.Date(paste(data2[, 1], "-01", sep = ""), format = "%Y-%m-%d")

  ####################

  # (data - median)/(standard deviation) - this is the z-score
  # take all the januarys, feburarys, etc.. and calculate the mean and standard deviation
  # Calculate the mean and standard deviation for each month
  monthly_mean1 <- aggregate(data1[, 2], by = list(format(data1[, 1], "%m")), mean)
  monthly_sd1 <- aggregate(data1[, 2], by = list(format(data1[, 1], "%m")), sd)

  # Combine the mean and standard deviation into a single data frame
  monthly_stats1 <- data.frame(month = monthly_mean1[, 1], mean = monthly_mean1[, 2], sd = monthly_sd1[, 2])

  # Calculate the mean and standard deviation for each month
  monthly_mean2 <- aggregate(data2[, 2], by = list(format(data2[, 1], "%m")), mean)
  monthly_sd2 <- aggregate(data2[, 2], by = list(format(data2[, 1], "%m")), sd)

  # Combine the mean and standard deviation into a single data frame
  monthly_stats2 <- data.frame(month = monthly_mean2[, 1], mean = monthly_mean2[, 2], sd = monthly_sd2[, 2])

  print(monthly_stats2)

  # Normalize the data
  data1[, 2] <- ifelse(monthly_stats1[match(format(data1[, 1], "%m"), monthly_stats1[, 1]), 3] == 0,
                       0,
                       (data1[, 2] - monthly_stats1[match(format(data1[, 1], "%m"), monthly_stats1[, 1]), 2]) /
                         monthly_stats1[match(format(data1[, 1], "%m"), monthly_stats1[, 1]), 3])

  data2[, 2] <- ifelse(monthly_stats2[match(format(data2[, 1], "%m"), monthly_stats2[, 1]), 3] == 0,
                       0,
                       (data2[, 2] - monthly_stats2[match(format(data2[, 1], "%m"), monthly_stats2[, 1]), 2]) /
                         monthly_stats2[match(format(data2[, 1], "%m"), monthly_stats2[, 1]), 3])

  ####################

  # Merge the data
  my_data <- data.frame(x = data1[, 2], y = data2[, 2])

  my_wc <- analyze.coherency(my_data, upperPeriod = 72)

  png(paste("output/WaveletCoherence_", nyszone1, "_", component_variable1,
            "_vs_", nyszone2, "_", component_variable2, ".png", sep = ""),
      width = 800, height = 800, units = "px")

  title <- paste("Wavelet Coherence between",
                 nyszone1, component_name1,
                 "and", nyszone2, component_name2)

  # set up the time axis and the period axis
  index.ticks <- seq(12*11, nrow(my_data), by = 12*65)
  index.labels <- format(data1[, 1], "%Y")[index.ticks]

  # make this to 72 periods
  wc.image(my_wc,
           main = title,
           plot.legend = FALSE,
           timelab = "Date (year)",
           periodlab = "Period (months)",
           spec.time.axis = list(at = index.ticks, labels = index.labels))

  dev.off()

  ####################
} else {
  completed <- c()
  # Compare all the zones for all the components
  for (i in seq_along(zones)) {
    for (j in seq_along(components)) {
      for (k in seq_along(zones)) {
        for (l in seq_along(components)) {
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

            ####################
            # THE IMPORTANT PART
            ####################

            # Load the data
            file1 <- paste(directory, component_variable1, "/",
                           "_zone_", zones[i], ".csv", sep = "")
            file2 <- paste(directory, component_variable2, "/",
                           "_zone_", zones[k], ".csv", sep = "")

            data1 <- read.csv(file1)
            data2 <- read.csv(file2)

            data1[, 1] <- as.Date(data1[, 1], format = "%Y-%m-%d")
            data2[, 1] <- as.Date(data2[, 1], format = "%Y-%m-%d")
            
            # Convert the data to monthly averages
            data1 <- aggregate(data1[, 2], by = list(format(data1[, 1], "%Y-%m")), mean)
            data2 <- aggregate(data2[, 2], by = list(format(data2[, 1], "%Y-%m")), mean)

            # Fix the date column (it needs a day to be a valid date)
            data1[, 1] <- as.Date(paste(data1[, 1], "-01", sep = ""), format = "%Y-%m-%d")
            data2[, 1] <- as.Date(paste(data2[, 1], "-01", sep = ""), format = "%Y-%m-%d")

            ####################

            # (data - median)/(standard deviation) - this is the z-score
            # take all the januarys, feburarys, etc.. and calculate the mean and standard deviation
            # Calculate the mean and standard deviation for each month
            monthly_mean1 <- aggregate(data1[, 2], by = list(format(data1[, 1], "%m")), mean)
            monthly_sd1 <- aggregate(data1[, 2], by = list(format(data1[, 1], "%m")), sd)

            # Combine the mean and standard deviation into a single data frame
            monthly_stats1 <- data.frame(month = monthly_mean1[, 1], mean = monthly_mean1[, 2], sd = monthly_sd1[, 2])

            # Calculate the mean and standard deviation for each month
            monthly_mean2 <- aggregate(data2[, 2], by = list(format(data2[, 1], "%m")), mean)
            monthly_sd2 <- aggregate(data2[, 2], by = list(format(data2[, 1], "%m")), sd)

            # Combine the mean and standard deviation into a single data frame
            monthly_stats2 <- data.frame(month = monthly_mean2[, 1], mean = monthly_mean2[, 2], sd = monthly_sd2[, 2])

            # Normalize the data
            data1[, 2] <- ifelse(monthly_stats1[match(format(data1[, 1], "%m"), monthly_stats1[, 1]), 3] == 0,
                                 0,
                                 (data1[, 2] - monthly_stats1[match(format(data1[, 1], "%m"), monthly_stats1[, 1]), 2]) /
                                  monthly_stats1[match(format(data1[, 1], "%m"), monthly_stats1[, 1]), 3])

            data2[, 2] <- ifelse(monthly_stats2[match(format(data2[, 1], "%m"), monthly_stats2[, 1]), 3] == 0,
                                 0,
                                 (data2[, 2] - monthly_stats2[match(format(data2[, 1], "%m"), monthly_stats2[, 1]), 2]) /
                                  monthly_stats2[match(format(data2[, 1], "%m"), monthly_stats2[, 1]), 3])

            ####################

            my_data <- data.frame(x = data1[, 2], y = data2[, 2])

            my_wc <- analyze.coherency(my_data, upperPeriod = 72)

            png(paste("output/WaveletCoherence_", zones[i], "_", component_variable1,
                      "_vs_", zones[k], "_", component_variable2, ".png", sep = ""),
                width = 800, height = 800, units = "px")

            title <- paste("Wavelet Coherence between",
                           zones[i], component_name1,
                           "and", zones[k], component_name2)

            # set up the time axis and the period axis
            index.ticks <- seq(12*11, nrow(my_data), by = 12*65)
            index.labels <- format(data1[, 1], "%Y")[index.ticks]

            wc.image(my_wc,
                     main = title,
                     plot.legend = FALSE,
                     timelab = "Date (year)",
                     periodlab = "Period (months)",
                     spec.time.axis = list(at = index.ticks, labels = index.labels))

            dev.off()

            ####################
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