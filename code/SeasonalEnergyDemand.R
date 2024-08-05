library("dplyr")
library("lubridate")
library("nasapower")
library("ggplot2")

source("code/SharedFunctions.R")

parameters <- c("WS2M", "ALLSKY_SFC_SW_DWN")

components <- c("1", "2", "3")

new_component_prompt <- function() {
  # Ask the user which component they want to analyze
  component <- ""
  component_prompt <- paste(
    "What component do you want to analyze?",
    "\n 1. Wind Speed",
    "\n 2. Discharge",
    "\n 3. Solar Radiation",
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

new_detect_component_name <- function(component) {
  if (component == 1) {
    component_name <- "Wind Speed"
  } else if (component == 2) {
    component_name <- "Discharge"
  } else if (component == 3) {
    component_name <- "Solar Radiation"
  }

  return(component_name)
}

detect_component_variable <- function(component) {
  if (component == 1) {
    component_variable <- "WS2M"
  } else if (component == 2) {
    component_variable <- "DISCHARGE"
  } else if (component == 3) {
    component_variable <- "ALLSKY_SFC_SW_DWN"
  }

  return(component_variable)
}

all_seasons_prompt <- function() {
  # Ask the user if they want to graph all seasons
  all_seasons <- ""
  all_seasons_prompt <- "Do you want to graph all seasons? (Y/N): "
  while (!(all_seasons %in% c("Y", "N"))) {
    all_seasons <- readline(prompt = all_seasons_prompt)
    all_seasons <- as.character(all_seasons)
    all_seasons <- toupper(all_seasons)
    if (!(all_seasons %in% c("Y", "N"))) {
      cat("\n", all_seasons, "is not a valid option.\n\n")
    }
  }

  return(all_seasons)
}

season_prompt <- function() {
  # Ask the user which season they want to graph
  season <- ""
  season_prompt <- paste(
    "What season do you want to graph?",
    "\n 1. Winter",
    "\n 2. Spring",
    "\n 3. Summer",
    "\n 4. Fall",
    "\n  Option: "
  )
  while (!(season %in% c("1", "2", "3", "4"))) {
    season <- readline(prompt = season_prompt)
    season <- as.integer(season)
    if (!(season %in% c("1", "2", "3", "4"))) {
      cat("\n", season, "is not a valid option.\n\n")
    }
  }

  return(season)
}

detect_season <- function(season) {
  if (season == 1) {
    season_variable <- "Winter"
  } else if (season == 2) {
    season_variable <- "Spring"
  } else if (season == 3) {
    season_variable <- "Summer"
  } else if (season == 4) {
    season_variable <- "Fall"
  }

  return(season_variable)
}

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

retrieve_demand_data <- function() {
  # Load the energy demand data from the CSV file
  energy_demand <- read.csv("data/hourly/demand/nys-energydemand.csv", stringsAsFactors = FALSE)

  # Convert the date (YYYY-MM-DDTHH) to a POSIXct object
  energy_demand$period <- as.POSIXct(energy_demand$period, format = "%Y-%m-%dT%H")

  # Get the energy demand
  energy_demand <- energy_demand[, c("period", "value")]

  # Sort the data by the date
  energy_demand <- energy_demand[order(energy_demand$period), ]

  # Organize the data by it's hourly seasonal averages and making it one year long
  energy_demand <- mutate(energy_demand, month = month(period), day = day(period), hour = hour(period))

  return(energy_demand)
}

retrieve_discharge_data <- function(file) {
  # This discharge data is from USGS
  # It's quaretly data, so we need to convert it to hourly data, although some data is missing
  # Load the discharge data from the CSV file
  discharge_data <- read.csv(file, stringsAsFactors = FALSE)

  # The datetime column is in the following format (MM/DD/YYYY HH:MM)
  # Convert the datetime column to a POSIXct object
  discharge_data$datetime <- as.POSIXct(discharge_data$datetime, format = "%m/%d/%Y %H:%M")

  # Make a separate column for the month, day, and hour
  discharge_data <- mutate(discharge_data, month = month(datetime), day = day(datetime), hour = hour(datetime))

  # Convert quarter-hourly data to hourly data per season
  winter <- discharge_data %>%
    filter(month %in% c(12, 1, 2)) %>%
    group_by(hour) %>%
    summarize(n = n(), discharge = mean(discharge))

  spring <- discharge_data %>%
    filter(month %in% c(3, 4, 5)) %>%
    group_by(hour) %>%
    summarize(n = n(), discharge = mean(discharge))

  summer <- discharge_data %>%
    filter(month %in% c(6, 7, 8)) %>%
    group_by(hour) %>%
    summarize(n = n(), discharge = mean(discharge))

  fall <- discharge_data %>%
    filter(month %in% c(9, 10, 11)) %>%
    group_by(hour) %>%
    summarize(n = n(), discharge = mean(discharge))

  # Create a table with each row as a season
  discharge_data <- data.frame(
    hour = 0:23,
    Winter = winter$discharge,
    Spring = spring$discharge,
    Summer = summer$discharge,
    Fall = fall$discharge
  )

  return(discharge_data)
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

organize_seasonal_data <- function(energy_demand) {
  # Get the hourly seasonal averages (24 hours) for the energy demand (Winter, Spring, Summer, Fall)
  winter <- energy_demand %>%
    filter(month %in% c(12, 1, 2)) %>%
    group_by(hour) %>%
    summarize(value = mean(value))

  spring <- energy_demand %>%
    filter(month %in% c(3, 4, 5)) %>%
    group_by(hour) %>%
    summarize(value = mean(value))

  summer <- energy_demand %>%
    filter(month %in% c(6, 7, 8)) %>%
    group_by(hour) %>%
    summarize(value = mean(value))

  fall <- energy_demand %>%
    filter(month %in% c(9, 10, 11)) %>%
    group_by(hour) %>%
    summarize(value = mean(value))


  # Create a table with each row as a season
  seasonal_data <- data.frame(
    hour = 0:23,
    Winter = winter$value,
    Spring = spring$value,
    Summer = summer$value,
    Fall = fall$value
  )
}

seasonal_energy_demand <- function(season, parameter) {
  file <- "data/hourly/NYS_Seasonal_Energy_Demand.RData"

  # If the RData file exists, load it
  if (file.exists(file)) {
    load(file)
    print(paste("Loading data..."))
  } else {
    # Make sure the data directory exists
    dir.create("data/hourly", showWarnings = FALSE)

    # Get the energy demand data
    energy_demand <- retrieve_demand_data()

    # Organize the data by it's hourly seasonal averages and making it one year long
    demand_data <- organize_seasonal_data(energy_demand)

    # Save the data for future use
    save(demand_data, file = file)
  }

  # Special handling for discharge data
  if (parameter$component == 2) {
    if (file.exists(paste("data/hourly/Discharge_", parameter$zone, ".RData", sep = ""))) {
      print(paste("Loading Discharge Data"))
      load(paste("data/hourly/Discharge_", parameter$zone, ".RData", sep = ""))
    } else {
      # Make sure the data directory exists
      dir.create("data/hourly", showWarnings = FALSE)

      # Get the discharge data
      component_data <- retrieve_discharge_data(paste("data/hourly/discharge/", parameter$zone, ".csv", sep = ""))

      # Save the data for future use
      save(component_data, file = paste("data/hourly/Discharge_", parameter$zone, ".RData", sep = ""))
    }
  } else {
    # Load the data
    if (file.exists(paste("data/hourly/Parameters_", parameter$zone, ".RData", sep = ""))) {
      # Load the data
      print(paste("Loading data for Zone", parameter$zone))
      load(paste("data/hourly/Parameters_", parameter$zone, ".RData", sep = ""))
    } else {
      # Make sure the data directory exists
      dir.create("data/hourly", showWarnings = FALSE)

      # Download the data
      print(paste("Downloading data for Zone", parameter$zone))
      data <- download_zone_data(parameter$zone)
      save(data, file = paste("data/hourly/Parameters_", parameter$zone, ".RData", sep = ""))
    }

    # Rename the columns to be more descriptive
    data <- rename(data, month = MO)
    data <- rename(data, day = DY)
    data <- rename(data, hour = HR)
    data <- rename(data, value = detect_component_variable(parameter$component))

    component_data <- organize_seasonal_data(data)
  }

  # Seperate the data by the selected season
  demand_data <- data.frame(
    hour = 0:23,
    value = demand_data[[detect_season(season)]]
  )

  component_data <- data.frame(
    hour = 0:23,
    value = component_data[[detect_season(season)]]
  )

  # Normalize the data
  demand_data$value <- standardize_data(demand_data$value)
  component_data$value <- standardize_data(component_data$value)

  # Smooth the data
  demand_data$value <- smooth.spline(demand_data$value, spar = 0.5)$y
  component_data$value <- smooth.spline(component_data$value, spar = 0.5)$y

  # Plot the energy demand across the seasons ontop of each other
  ggplot() +
    geom_line(data = demand_data, aes(x = hour, y = value), color = "blue") +
    geom_line(data = component_data, aes(x = hour, y = value), color = "red") +
    scale_x_time(breaks = c(0, 6, 12, 18, 23), labels = c("Midnight", "6 AM", "Noon", "6 PM", "Midnight")) +
    labs(
      title = paste("Energy Demand and ", new_detect_component_name(parameter$component), " in Zone ", parameter$zone, " Across the ", detect_season(season), " Season", sep = ""),
      x = "Hour",
      y = "Normalized Value"
    ) +
    theme(legend.position = "top")

  ggsave(paste("output/SeasonalEnergyDemand_", parameter$zone, "_", detect_component_variable(parameter$component), "_", detect_season(season), ".png", sep = ""), width = 12, height = 5, units = "in", dpi = 300)
}

if (all_seasons_prompt() == "N") {
  season <- season_prompt()

  parameter <- list(
    zone = iso_zone_prompt(),
    component = new_component_prompt()
  )

  seasonal_energy_demand(season, parameter)
} else {
  print("All seasons")

  for (zone in c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")) {
    for (component in c(1, 2, 3)) {
      parameter <- list(
        zone = zone,
        component = component
      )
      for (season in 1:4) {
        seasonal_energy_demand(season, parameter)
      }
    }
  }
}
