source("code/SharedFunctions.R")

# Check if the user needs to install the required packages
detect_uninstalled_packages()

# https://github.com/ropensci/nasapower
library("nasapower")
library("ggplot2")
library("reticulate")
library("cowplot")

seasons <- c("winter", "spring", "summer", "fall")
parameters <- c("WS2M", "PRECTOTCORR", "PRECSNOLAND", "ALLSKY_SFC_SW_DWN")



# Do you want to compare all the zones for all the components?
confirm_all <- all_zones_prompt()

set_lonlat <- function(zone) {
    lonlat <- c(0, 0)
    # Set the longitude and latitude for the NYISO Zones
    if (zone == "A") {
        lonlat <- c(-79.17747, 42.1875)
    } else if (zone == "B") {
        lonlat <- c(-77.67826, 43.1250)
    } else if (zone == "C") {
        lonlat <- c(-76.17904, 42.9375)
    } else if (zone == "D") {
        lonlat <- c(-74.86723, 45.0000)
    } else if (zone == "E") {
        # CHANGE THIS
        lonlat <- c(-75.42943, 42.375)
    } else if (zone == "F") {
        lonlat <- c(-73.55541, 42.375)
    } else if (zone == "G") {
        lonlat <- c(-73.93022, 41.8125)
    } else if (zone == "H") {
        lonlat <- c(-73.74282, 41.2500)
    } else if (zone == "I") {
        lonlat <- c(-73.74282, 41.0625)
    } else if (zone == "J") {
        lonlat <- c(-73.93022, 40.6875)
    } else if (zone == "K") {
        lonlat <- c(-73.18061, 41.0625)
    }
    
    return(lonlat)
}

detect_parameter_name <- function(parameter) {
  if (parameter == "WS2M")
    parameter_name <- "Wind Speed"
  else if (parameter == "PRECTOTCORR")
    parameter_name <- "Precipitation"
  else if (parameter == "PRECSNOLAND")
    parameter_name <- "Snow Precipitation"
  else if (parameter == "ALLSKY_SFC_SW_DWN")
    parameter_name <- "Direct Normal Irradiance (DNI)"
  
  return(parameter_name)
}

get_seasonal_hourly_avg <- function(data, parameter, season) {
    # Convert the season to uppercase
    season <- toupper(season)

    # Set the season
    if (season == "WINTER") {
        season <- c(12, 1, 2)
    } else if (season == "SPRING") {
        season <- c(3, 4, 5)
    } else if (season == "SUMMER") {
        season <- c(6, 7, 8)
    } else if (season == "FALL") {
        season <- c(9, 10, 11)
    }

    # Get the hourly average for each point of the day (0, 1, 2 ... 23) for each parameter
    parameter_avg <- aggregate(
                               subset(data, MO %in% season)[[parameter]],
                               by = list(format(
                                                as.POSIXct(
                                                           paste(
                                                                 subset(data, MO %in% season)$YEAR,
                                                                 subset(data, MO %in% season)$MO,
                                                                 subset(data, MO %in% season)$DY,
                                                                 subset(data, MO %in% season)$HR,
                                                                 sep = "-"
                                                                ),
                                                           format = "%Y-%m-%d-%H"
                                                          ),
                                                "%H"
                                               )
                                        ),
                               FUN = mean
                              )

    # Rename the columns
    colnames(parameter_avg) <- c("hour", paste(parameter, "avg", sep = "_"))

    # Assign both columns to numeric
    parameter_avg$hour <- as.numeric(parameter_avg$hour)
    parameter_avg[[paste(parameter, "avg", sep = "_")]] <- as.numeric(parameter_avg[[paste(parameter, "avg", sep = "_")]])

    # Standardize the data
    parameter_avg[[paste(parameter, "avg", sep = "_")]] <- standardize_data(parameter_avg[[paste(parameter, "avg", sep = "_")]])

    # Assign formating to the parameter column again
    parameter_avg[[paste(parameter, "avg", sep = "_")]] <- as.numeric(parameter_avg[[paste(parameter, "avg", sep = "_")]])

    return(parameter_avg)
}

pearson_map_seasonal <- function(parameter1, parameter2) {
  # If the results already exist, load them
  if (file.exists(paste("output/Pearson_", parameter1, "_vs_", parameter2, ".RData", sep = ""))) {
    # Load the data
    print(paste("Data already exists for Parameter", parameter1, "and Parameter", parameter2, ". Loading the data..."))
    load(paste("output/Pearson_", parameter1, "_vs_", parameter2, ".RData", sep = ""))

    # Verify if the data is loaded
    if (exists("nysiso1") && exists("nysiso2")) {
      print(paste("Data loaded successfully for Zone", nysiso1$zone, "and Zone", nysiso2$zone))
      
    } else {
      print(paste("Error loading the data for Zone", nysiso1$zone, "and Zone", nysiso2$zone))
    }
  } else {
    nysiso1$lonlat <- set_lonlat(nysiso1$zone)
    nysiso2$lonlat <- set_lonlat(nysiso2$zone)

    nysiso1$power <- data.frame(get_power(
      # The community code for the region of interest
      # ag - Agroclimatology Archive
      # sb - Sustainable Buildings Archive
      # re - Renewable Energy Archive

      # Parameters:
      # WS2M                  MERRA-2 Wind Speed at 2 Meters (m/s) ;
      # PRECTOTCORR           MERRA-2 Precipitation Corrected (mm/hour) ;
      # PRECSNOLAND           MERRA-2 Snow Precipitation Land (mm/hour) ;
      # ALLSKY_SFC_SW_DWN     CERES SYN1deg All Sky Surface Shortwave Downward Irradiance (MJ/hr)
      community = "ag",
      lonlat = nysiso1$lonlat,
      pars = parameters,
      dates = c("2001-01-01", "2022-12-31"),
      temporal_api = "hourly",
    ))

    nysiso2$power <- data.frame(get_power(
      # The community code for the region of interest
      # ag - Agroclimatology Archive
      # sb - Sustainable Buildings Archive
      # re - Renewable Energy Archive

      # Parameters:
      # WS2M                  MERRA-2 Wind Speed at 2 Meters (m/s) ;
      # PRECTOTCORR           MERRA-2 Precipitation Corrected (mm/hour) ;
      # PRECSNOLAND           MERRA-2 Snow Precipitation Land (mm/hour) ;
      # ALLSKY_SFC_SW_DWN     CERES SYN1deg All Sky Surface Shortwave Downward Irradiance (MJ/hr)
      community = "ag",
      lonlat = nysiso2$lonlat,
      pars = parameters,
      dates = c("2001-01-01", "2022-12-31"),
      temporal_api = "hourly",
    ))
}

if (confirm_all == "N") {

  parameter1 <- list(
    zone = component_prompt()
  )

  parameter2 <- list(
    zone = component_prompt()
  )

  pearson_map_seasonal(nysiso1, nysiso2)
  
} else {
    # Compare all the zones for all the components
    completed_zones <- c()  # List to keep track of completed zones

    for (zone1 in zones) {
        nysiso1 <- list(zone = zone1)
        for (zone2 in zones) {
            nysiso2 <- list(zone = zone2)

            # Skip if the two zones are the same or if the zone has already been completed
            if (nysiso1$zone == nysiso2$zone || nysiso2$zone %in% completed_zones) {
                next
            }

            pearson_map_seasonal(nysiso1, nysiso2)
        }

        completed_zones <- c(completed_zones, zone1) # Mark the zone as completed
    }
}