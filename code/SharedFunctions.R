library("reticulate")
library("WaveletComp")

# Set the data directory
directory <- "data/MRI-AGCM3.2/"
# Using the following NYISO Zones, goes from A to K
zones <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
# Using the following components, goes from 1 to 4
components <- c("1", "2", "3", "4")

detect_uninstalled_packages <- function() {
  # Check if the user needs to install the required packages
  if (!require("reticulate")) {
    install.packages("reticulate")
    library("reticulate")
  }
}

netcdf_convert_prompt <- function() {
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

  if (convert == "Y") {
    # Convert the NetCDF files to CSV
    py_run_file("code/ExtractData.py")
  }

  return(convert)
}

all_zones_prompt <- function() {
  # Ask the user if they want to compare all the zones
  all_zones <- ""
  all_zones_prompt <- "Do you want to compare all the zones? (Y/N): "
  while (!(all_zones %in% c("Y", "N"))) {
    all_zones <- readline(prompt = all_zones_prompt)
    all_zones <- as.character(all_zones)
    all_zones <- toupper(all_zones)
    if (!(all_zones %in% c("Y", "N"))) {
      cat("\n", all_zones, "is not a valid option.\n\n")
    }
  }

  return(all_zones)
}

iso_zone_prompt <- function() {
  # Ask the user which NYISO Zone they want to analyze
  nyszone <- ""
  nyszone_prompt <- paste("Enter the NYISO Zone for analyzing:",
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

  while (!(nyszone %in% c(zones))) {
    nyszone <- readline(prompt = nyszone_prompt)
    nyszone <- as.character(nyszone)
    nyszone <- toupper(nyszone)
    if (!(nyszone %in% c(zones))) {
      cat("\n", nyszone, "is not a valid NYISO Zone.\n\n")
    }
  }
  cat("\n")

  return(nyszone)
}

component_prompt <- function() {
  # Ask the user which component they want to analyze
  component <- ""
  component_prompt <- paste("What component do you want to analyze?",
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
  cat("\n")

  return(component)
}

detect_component_variable <- function(component) {
  if (component == 1)
    component_variable <- "sfcWind"
  else if (component == 2)
    component_variable <- "pr"
  else if (component == 3)
    component_variable <- "prsn"
  else if (component == 4)
    component_variable <- "rsds"
  
  return(component_variable)
}

detect_component_name <- function(component) {
  if (component == 1)
    component_name <- "Wind Speed"
  else if (component == 2)
    component_name <- "Precipitation"
  else if (component == 3)
    component_name <- "Snowfall Flux"
  else if (component == 4)
    component_name <- "Solar Radiation"
  
  return(component_name)
}

detect_component <- function(component) {
  component_variable <- detect_component_variable(component)
  component_name <- detect_component_name(component)

  return(list(variable = component_variable, name = component_name))
}

load_file <- function(zone, component_variable) {
   # Load the data
   file <- paste(directory, component_variable, "/", "_zone_", zone, ".csv", sep = "")

   return(file)
}

create_data_frame <- function(place) {
  # Read from the data
  # Create a data frame with the first column as the date,
  # the second column as the first weather variable, and the third column as the second weather variable
  data <- data.frame(date = read.csv(place$file)[, 1], variable = read.csv(place$file)[, 2])

  data$date <- as.Date(data$date, format = "%Y-%m-%d")
  data$variable <- as.numeric(as.character(data$variable))

  return(data)
}

standardize_data <- function(data) {
  data <- scale(data, scale = TRUE)
  
  return(data)
}