library("dplyr")
library("lubridate")
library("ggplot2")

source("code/SharedFunctions.R")

new_component_prompt <- function() {
  # Ask the user which component they want to analyze
  component <- ""
  component_prompt <- paste(
    "What component do you want to analyze?",
    "\n 1. Wind Speed",
    "\n 2. Discharge",
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

new_detect_component_name <- function(component) {
  if (component == 1) {
    component_name <- "Wind Speed"
  } else if (component == 2) {
    component_name <- "Discharge"
  } else if (component == 3) {
    component_name <- "Snowfall Flux"
  } else if (component == 4) {
    component_name <- "Solar Radiation"
  }

  return(component_name)
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
  file <- "data/hourly/seasonal_energy_demand.RData"

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

  if (parameter$component == 2) {
    file <- paste("data/hourly/discharge_", parameter$zone, ".RData", sep = "")

    zone_file <- paste("data/hourly/discharge/", parameter$zone, ".csv", sep = "")
    component_data <- retrieve_discharge_data(zone_file)

    component_data <- data.frame(
      hour = 0:23,
      value = component_data[[detect_season(season)]]
    )
  }

  file <- paste("data/hourly/discharge_", parameter$zone, ".RData", sep = "")

  # Seperate the data by the selected season
  demand_data <- data.frame(
    hour = 0:23,
    value = demand_data[[detect_season(season)]]
  )

  # Normalize the data
  demand_data$value <- standardize_data(demand_data$value)
  component_data$value <- standardize_data(component_data$value)

  print(head(component_data))

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

  ggsave(paste("output/SeasonalEnergyDemand_", detect_season(season), ".png", sep = ""), width = 12, height = 5, units = "in", dpi = 300)
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

  for (season in 1:4) {
    seasonal_energy_demand(season)
  }
}
