source("code/SharedFunctions.R")

# Check if the user needs to install the required packages
detect_uninstalled_packages()

# https://github.com/ropensci/nasapower
library("nasapower")
library("ggplot2")
library("ggcorrplot")

seasons <- c("winter", "spring", "summer", "fall")
parameters <- c("WS2M", "PRECTOTCORR", "PRECSNOLAND", "ALLSKY_SFC_SW_DWN")
correlations <- c("1", "2")

correlation_prompt <- function() {
  # Ask the user which correlation they want to use
  correlation <- ""
  correlation_prompt <- paste("What correlation do you want to use?",
                            "\n 1. Pearson",
                            "\n 2. Kendall",
                            "\n  Option: ")
  while (!(correlation %in% c(correlations))) {
    correlation <- readline(prompt = correlation_prompt)
    correlation <- as.integer(correlation)
    if (!(correlation %in% c(correlations))) {
      cat("\n", correlation, "is not a valid option.\n\n")
    }
  }
  cat("\n")

  return(correlation)
}

detect_component_variable <- function(component) {
  if (component == 1)
    component_variable <- "WS2M"
  else if (component == 2)
    component_variable <- "PRECTOTCORR"
  else if (component == 3)
    component_variable <- "PRECSNOLAND"
  else if (component == 4)
    component_variable <- "ALLSKY_SFC_SW_DWN"
  
  return(component_variable)
}

detect_correlation <- function(correlation) {
  if (correlation == 1)
    correlation_variable <- "pearson"
  else if (correlation == 2)
    correlation_variable <- "kendall"
  
  return(correlation_variable)
}

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

download_zone_data <- function(zone) {
  data <- data.frame(get_power(
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
      lonlat = set_lonlat(zone),
      pars = parameters,
      dates = c("2001-01-01", "2022-12-31"),
      temporal_api = "hourly",
    ))

  return(data)
}

correlation_heatmap_seasonal <- function(correlation, corrparameter1, parameter2) {
  for (zone in zones) {
    # Check to see if the data has already been downloaded
    if(file.exists(paste("data/hourly/", zone, ".RData", sep = ""))) {
      # Load the data
      print(paste("Loading data for Zone", zone))
      load(paste("data/hourly/", zone, ".RData", sep = ""))
    } else {
      # Make sure the data directory exists
      dir.create("data/hourly", showWarnings = FALSE)

      # Download the data
      print(paste("Downloading data for Zone", zone))
      data <- download_zone_data(zone)
      save(data, file = paste("data/hourly/", zone, ".RData", sep = ""))
    }
  }
  
  total_zones <- length(zones)

  # Make a data frame to store the Correlation Coefficients
  corr_df <- data.frame(matrix(ncol = total_zones, nrow = total_zones))

  rownames(corr_df) <- zones
  colnames(corr_df) <- zones

  # Calculate the Correlation Coefficient
  for (season in seasons) {
    season <- toupper(season)
    print(paste("Calculating", paste(toupper(substring(detect_correlation(correlation), 1, 1)), substring(tolower(detect_correlation(correlation)), 2), sep = ""), "Correlation Coefficient for the", paste(toupper(substring(season, 1, 1)), substring(tolower(season), 2), sep = ""), "Season"))
    # Set the season
    if (season == "WINTER") {
        months <- c(12, 1, 2)
    } else if (season == "SPRING") {
        months <- c(3, 4, 5)
    } else if (season == "SUMMER") {
        months <- c(6, 7, 8)
    } else if (season == "FALL") {
        months <- c(9, 10, 11)
    }
    for (zone1 in zones) {
      for (zone2 in zones) {
        
        # Skip if the two zones are the same
        if (zone1 == zone2) {
          corr_df[zone1, zone2] <- 1
          next
        }

        # Load the first variable's data
        load(paste("data/hourly/", zone1, ".RData", sep = ""))
        data1 <- data.frame(YEAR = data$YEAR, MO = data$MO, DY = data$DY, HR = data$HR, variable = data[[detect_component_variable(parameter1)]])
        # Take only the data for the current season
        data1 <- subset(data1, MO %in% months)
        data1$variable <- as.numeric(data1$variable)

        # Load the second variable's data
        load(paste("data/hourly/", zone2, ".RData", sep = ""))
        data2 <- data.frame(YEAR = data$YEAR, MO = data$MO, DY = data$DY, HR = data$HR, variable = data[[detect_component_variable(parameter2)]])
        # Take only the data for the current season
        data2 <- data2[data2$MO %in% months, ]
        data2$variable <- as.numeric(data2$variable)

        # Perform the Correlation Coefficient
        corr_df[zone1, zone2] <- round(cor(data1$variable, data2$variable, use="complete.obs", method = detect_correlation(correlation)), 2)
      }
    }
    # Create a heatmap for the Correlation Coefficients
    ggcorrplot(corr_df,
               title = paste("Correleation Coefficient Heatmap of", detect_parameter_name(detect_component_variable(parameter1)), "and", detect_parameter_name(detect_component_variable(parameter2))),
               legend.title = paste(paste(toupper(substring(detect_correlation(correlation), 1, 1)), substring(tolower(detect_correlation(correlation)), 2), sep = ""), "\nCorr."), lab=TRUE,
               lab_col="black",
               lab_size = 5, ggtheme = theme_gray,
               colors = c("red", "white", "blue"), show.diag=F) +
                theme(legend.position = "bottom") +
                theme(legend.key.height = unit(0.25, "in")) +
                theme(legend.key.width = unit(2, "in")) +
                theme(legend.title = element_text(size = 16, face = "italic", hjust = 0.5, margin = margin(t = 0, r = 10, b = 15, l = 0, unit = "pt"))) +
                theme(legend.text = element_text(size = 12)) +
                theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                theme(axis.title.x = element_text(size = 16, face = "italic", margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt"))) +
                theme(axis.title.y = element_text(size = 16, face = "italic", margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"), angle = 90)) +
                xlab(paste(detect_parameter_name(detect_component_variable(parameter1)))) +
                ylab(paste(detect_parameter_name(detect_component_variable(parameter2)))) +
                # Put some text on the outside of the plot to indicate the season
                labs(tag = paste("Season:", paste(toupper(substring(season, 1, 1)), substring(tolower(season), 2), sep = ""))) +
                theme(plot.tag.position = "bottomright", plot.tag = element_text(size = 16, face = "bold", hjust = 1, vjust = 0.5))
                
                

    # Save the heatmap
    ggsave(paste("output/", detect_correlation(correlation), "_heatmap_", detect_component_variable(parameter1), "_vs_", detect_component_variable(parameter2), "_", season, ".png", sep = ""), width = 18, height = 10, units = "in", dpi = 100)
  }
}

if (confirm_all == "N") {

  correlation_choice <- correlation_prompt()

  parameter1 <- list(
    zone = component_prompt()
  )

  parameter2 <- list(
    zone = component_prompt()
  )

  correlation_heatmap_seasonal(correlation_choice, parameter1, parameter2)
  
} else {

  correlation_choice <- correlation_prompt()

  completed_parameters <- c()
  i <- 1
  for (parameter1 in 1:length(parameters)) {
    j <- 1
    for (parameter2 in 1:length(parameters)) {
      if (j %in% completed_parameters) {
        j <- j + 1
        next
      }
      print(paste("Calculating", paste(toupper(substring(detect_correlation(correlation_choice), 1, 1)), substring(tolower(detect_correlation(correlation_choice)), 2), sep = ""), "Correlation Coefficient for", detect_parameter_name(detect_component_variable(i)), "and", detect_parameter_name(detect_component_variable(j))))
      correlation_heatmap_seasonal(correlation_choice, i, j)
      j <- j + 1
    }
    completed_parameters <- c(completed_parameters, i)
    i <- i + 1
  }
}