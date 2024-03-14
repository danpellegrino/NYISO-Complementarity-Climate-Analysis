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

zones <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
nyszone <- ""
nyszone_prompt <- paste("Enter a character for which NYISO Zone:",
                        "\nALL (All Zones)",
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

while (!(nyszone %in% c(zones, "ALL"))) {
  nyszone <- readline(prompt = nyszone_prompt)
  nyszone <- as.character(nyszone)
  nyszone <- toupper(nyszone)
  if (!(nyszone %in% c(zones, "ALL"))) {
    cat("\n", nyszone, "is not a valid NYISO Zone.\n\n")
  }
}

cat("\n")

# Select which information is being gathered
components <- c("1", "2", "3", "4")
component <- ""
component_prompt <- paste("Enter a number for which component you need:",
                          "\n 1. Wind Speed",
                          "\n 2. Precipitation",
                          "\n 3. Snowfall",
                          "\n 4. Solar Radiation",
                          "\n  Option: ")
while (!(component %in% c(components))) {
  component <- readline(prompt = component_prompt)
  component <- as.integer(component)
  if (!(component %in% c(components))) {
    cat("\n", component, "is not a valid option.\n\n")
  }
}

if (component == 1) {
  component_variable <- "sfcWind"
  component_name <- "Wind Speed"
  component_unit <- "m/s"
} else if (component == 2) {
  component_variable <- "pr"
  component_name <- "Precipitation"
  component_unit <- "mm/day"
} else if (component == 3) {
  component_variable <- "prsn"
  component_name <- "Snowfall Flux"
  component_unit <- "mm/day"
} else if (component == 4) {
  component_variable <- "rsds"
  component_name <- "Solar Radiation"
  component_unit <- "W/m²"
}

cat("\nNow entering Zone", nyszone, "with Component", component, "\n")

# Grab from the CSV file
if (nyszone == "ALL") {
  # Grab all zone files for the component
  files <- list.files(paste(directory,
                            component_variable,
                            "/",
                            sep = ""),
                      pattern = "\\.csv$",
                      full.names = TRUE)
} else {
  file <- paste(directory,
                component_variable,
                "/_zone_",
                nyszone,
                ".csv",
                sep = "")
  # Check if the file exists
  if (!file.exists(file)) {
    cat("The file", file, "does not exist.\n")
    stop()
  }
}


# send to an image file
if (nyszone == "ALL") {
  for (file in files) {
    nyszone <- gsub(".csv", "", gsub(".*_zone_", "", file))
    cat(
      paste("Processing", file, "for", component_name, "in Zone", nyszone, "\n")
    )
    data <- read.csv(file)
    if (component == 2 || component == 3) {
      data[, 2] <- data[, 2] * 86400
    }
    data[, 1] <- as.Date(data[, 1], format = "%Y-%m-%d")
    component_ts <- ts(data[, 2], start = c(1950, 1, 1),
                       end = c(2099, 12, 31), frequency = 365)
    png(paste("output/", component_name, "/zone_", nyszone, ".png", sep = ""),
        width = 800, height = 600)
    if (component == 2 || component == 3) {
      plot(component_ts,
           main = paste("NYISO Zone", nyszone, ":",
                        component_name, "(", component_unit, ")"),
           xlab = "Year",
           ylab = component_name)
    } else if (component == 1) {
      plot(component_ts,
           main = paste("NYISO Zone", nyszone, ":",
                        component_name, "(", component_unit, ")"),
           xlab = "Year",
           ylab = component_name)
    } else {
      plot(component_ts,
           main = paste("NYISO Zone", nyszone, ":",
                        component_name, "(", component_unit, ")"),
           xlab = "Year",
           ylab = component_name,
           ylim = c(min(component_ts) - 150, max(component_ts) + 150))
    }
    dev.off()
  }
} else {
  data <- read.csv(file)

  # Convert only pr and prsn to mm/day
  if (component == 2 || component == 3) {
    data[, 2] <- data[, 2] * 86400
  }

  data[, 1] <- as.Date(data[, 1], format = "%Y-%m-%d")
  component_ts <- ts(data[, 2], start = c(1950, 1, 1),
                     end = c(2099, 12, 31), frequency = 365)

  png(paste("output/", component_name, "/zone_", nyszone, ".png", sep = ""),
      width = 800, height = 600)
  if (component == 4) {
    plot(component_ts,
         main = paste("NYISO Zone", nyszone, ":",
                      component_name, "(", component_unit, ")"),
         xlab = "Year",
         ylab = component_name,
         ylim = c(min(component_ts) - 150, max(component_ts) + 150))
  } else {
    plot(component_ts,
         main = paste("NYISO Zone", nyszone, ":",
                      component_name, "(", component_unit, ")"),
         xlab = "Year",
         ylab = component_name)
  }

  dev.off()
}