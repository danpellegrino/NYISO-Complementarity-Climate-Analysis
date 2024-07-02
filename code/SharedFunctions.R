#' @title Shared Functions
#' @description Shared functions for the analysis of the weather data.
#' @author [Daniel Pellegrino](danieljpellegrino.com)

# Set the data directory
directory <- "data/MRI-AGCM3.2/"
# Using the following NYISO Zones, goes from A to K
zones <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
# Using the following components, goes from 1 to 4
components <- c("1", "2", "3", "4")

#' @title NetCDF Convert Prompt
#' @description Ask the user if they need to convert the NetCDF files to CSV (Y/N).
#' @return A character string with the user's response.
#' @examples
#' answer <- netcdf_convert_prompt()
#' print(answer)
#' @export
#' @importFrom base return
#' @importFrom base if
#' @importFrom base else
#' @importFrom base cat
#' @importFrom base readline
#' @importFrom base as.character
#' @importFrom base toupper
#' @importFrom base c
#' @importFrom reticulate py_run_file
#' @importFrom base while
#' @importFrom base paste
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

#' @title All Zones Prompt
#' @description Ask the user if they want to compare all the zones (Y/N).
#' @return A character string with the user's response.
#' @examples
#' answer <- all_zones_prompt()
#' print(answer)
#' @export
#' @importFrom base return
#' @importFrom base if
#' @importFrom base else
#' @importFrom base cat
#' @importFrom base readline
#' @importFrom base as.character
#' @importFrom base toupper
#' @importFrom base c
#' @importFrom base while
#' @importFrom base paste
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

#' @title NYISO Zone Prompt
#' @description Ask the user which NYISO Zone they want to analyze (A to K).
#' @return A character string with the user's response.
#' @examples
#' answer <- nyszone_prompt()
#' print(answer)
#' @export
#' @importFrom base return
#' @importFrom base if
#' @importFrom base else
#' @importFrom base cat
#' @importFrom base paste
#' @importFrom base readline
#' @importFrom base as.character
#' @importFrom base toupper
#' @importFrom base c
#' @importFrom base while
iso_zone_prompt <- function() {
  # Ask the user which NYISO Zone they want to analyze
  nyszone <- ""
  nyszone_prompt <- paste(
    "Enter the NYISO Zone for analyzing:",
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
    "\n  Option: "
  )

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

#' @title Component Prompt
#' @description Ask the user which component they want to analyze.
#' @return A character string with the user's response (1, 2, 3, or 4)
#' @examples
#' answer <- component_prompt()
#' print(answer)
#' @export
#' @importFrom base return
#' @importFrom base if
#' @importFrom base else
#' @importFrom base cat
#' @importFrom base paste
#' @importFrom base readline
#' @importFrom base as.integer
#' @importFrom base c
#' @importFrom base while
component_prompt <- function() {
  # Ask the user which component they want to analyze
  component <- ""
  component_prompt <- paste(
    "What component do you want to analyze?",
    "\n 1. Wind Speed",
    "\n 2. Precipitation",
    "\n 3. Snowfall",
    "\n 4. Solar Radiation",
    "\n  Option: "
  )
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

#' @title Detect Component Variable
#' @description Detect the component variable based on the component number.
#' @param component An integer with the component number.
#' @return A character string with the component variable.
#' @examples
#' component_variable <- detect_component_variable(1)
#' print(component_variable)
#' [1] "sfcWind"
#' @export
#' @importFrom base return
#' @importFrom base if
#' @importFrom base else
detect_component_variable <- function(component) {
  if (component == 1) {
    component_variable <- "sfcWind"
  } else if (component == 2) {
    component_variable <- "pr"
  } else if (component == 3) {
    component_variable <- "prsn"
  } else if (component == 4) {
    component_variable <- "rsds"
  }

  return(component_variable)
}

#' @title Detect Component Name
#' @description Detect the component name based on the component number.
#' @param component An integer with the component number.
#' @return A character string with the component name.
#' @examples
#' component_name <- detect_component_name(1)
#' print(component_name)
#' [1] "Wind Speed"
#' @export
#' @importFrom base return
#' @importFrom base if
#' @importFrom base else
detect_component_name <- function(component) {
  if (component == 1) {
    component_name <- "Wind Speed"
  } else if (component == 2) {
    component_name <- "Precipitation"
  } else if (component == 3) {
    component_name <- "Snowfall Flux"
  } else if (component == 4) {
    component_name <- "Solar Radiation"
  }

  return(component_name)
}

#' @title Detect Component
#' @description Detect the component variable and name based on the component number.
#' @param component An integer with the component number.
#' @return A list with the component variable and name.
#' @examples
#' component <- detect_component(1)
#' print(component$variable)
#' [1] "sfcWind"
#' print(component$name)
#' [1] "Wind Speed"
#' @export
#' @importFrom base list
#' @importFrom base return
detect_component <- function(component) {
  component_variable <- detect_component_variable(component)
  component_name <- detect_component_name(component)

  return(list(variable = component_variable, name = component_name))
}

#' @title Load File
#' @description Load the file based on the zone and component variable.
#' @param zone A character string with the NYISO Zone.
#' @param component_variable A character string with the component variable.
#' @return A character string with the file path.
#' @examples
#' file <- load_file("A", "sfcWind")
#' print(file)
#' @export
#' @importFrom base paste
#' @importFrom base return
load_file <- function(zone, component_variable) {
  # Load the data
  file <- paste(directory, component_variable, "/", "_zone_", zone, ".csv", sep = "")

  return(file)
}

#' @title Create Data Frame
#' @description Create a data frame with the date and the weather variable.
#' @param place A list with the file path.
#' @return A data frame with the date and the weather variable.
#' @examples
#' data <- create_data_frame(list(file = "data/MRI-AGCM3.2/sfcWind/_zone_A.csv"))
#' print(data)
#' @export
#' @importFrom utils read.csv
#' @importFrom base data.frame
#' @importFrom base as.Date
#' @importFrom base as.numeric
#' @importFrom base as.character
#' @importFrom base return
create_data_frame <- function(place) {
  # Read from the data
  # Create a data frame with the first column as the date,
  # the second column as the first weather variable, and the third column as the second weather variable
  data <- data.frame(date = read.csv(place$file)[, 1], variable = read.csv(place$file)[, 2])

  data$date <- as.Date(data$date, format = "%Y-%m-%d")
  data$variable <- as.numeric(as.character(data$variable))

  return(data)
}

#' @title Standardize Data
#' @description Standardize the data using the scale function.
#' @param data A numeric vector with the data.
#' @return A numeric vector with the standardized data.
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' standardized_data <- standardize_data(data)
#' print(standardized_data)
#' @export
#' @importFrom base scale
#' @importFrom base is.na
#' @importFrom base rep
#' @importFrom base length
#' @importFrom base return
standardize_data <- function(data) {
  data <- scale(data, scale = TRUE)

  # Incase the data is all NA
  if (is.na(data[1])) {
    data <- rep(0, length(data))
  }

  return(data)
}
