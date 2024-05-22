library("WaveletComp")
library("reticulate")
citation("WaveletComp")

source("code/SharedFunctions.R")

# Check if the user needs to install the required packages
detect_uninstalled_packages()

# Do you need to convert the NetCDF files to CSV?
netcdf_convert_prompt()

# Do you want to compare all the zones for all the components?
confirm_all <- all_zones_prompt()

average_season_data <- function(data, date, season) {
  # Extract the day and month from the date
  data$day <- as.numeric(format(as.Date(data[, date]), "%d"))
  data$month <- as.numeric(format(as.Date(data[, date]), "%m"))

  if (season == "Winter") {
    # Winter is from December to February
    data <- data[data$month %in% c(12, 1, 2), ]
  } else if (season == "Spring") {
    # Spring is from March to May
    data <- data[data$month %in% c(3, 4, 5), ]
  } else if (season == "Summer") {
    # Summer is from June to August
    data <- data[data$month %in% c(6, 7, 8), ]
  } else if (season == "Autumn") {
    # Autumn is from September to November
    data <- data[data$month %in% c(9, 10, 11), ]
  } 

  # Aggregate the data by day and month
  data <- aggregate(data[, 2:ncol(data)],
                    by = list(data$day, data$month), FUN = mean)

  # Remove the extra day and month columns
  data <- data[, -c(1, 2)]
  
  print(data)

  return(data)
}

smooth_data <- function(data) {
  # Smooth the data using a moving average
  data <- smooth.spline(data, spar = 0.75)$y

  return(data)
}

compare_seasonal_series <- function(nys1, nys2) {
  nys1$file <- load_file(nys1$zone, nys1$component$variable)
  nys2$file <- load_file(nys2$zone, nys2$component$variable)

  # Iterate through different season values
  seasons <- c("Winter", "Spring", "Summer", "Autumn")
  for (season in seasons) {
    nys1_data <- NULL
    nys2_data <- NULL

    nys1_data <- create_data_frame(nys1)
    nys2_data <- create_data_frame(nys2)

    print(paste("Computing Wavelet Transform for Zone", nys1$zone, nys1$component$name, "and Zone", nys2$zone, nys2$component$name, "for the", season, "Season"))
  
    # Average out the data from x amount of years to one year
    nys1_data <- average_season_data(nys1_data, "date", season)
    nys2_data <- average_season_data(nys2_data, "date", season)

    # Standardize the data
    nys1_data$variable <- standardize_data(nys1_data$variable)
    nys2_data$variable <- standardize_data(nys2_data$variable)
    
    # Smooth the data
    nys1_data$variable <- smooth_data(nys1_data$variable)
    nys2_data$variable <- smooth_data(nys2_data$variable)

    svg(paste("output/TimeSeries_",
                nys1$zone, "_", nys1$component$variable, "_vs_",
                nys2$zone, "_", nys2$component$variable, "_", season, ".svg", sep = ""),
        width = 8, height = 4.4, pointsize = 6)

    # Plot the two graphs together
    plot(nys1_data$variable, type = "l", col = "blue",
        xlab = "Day of the Year", ylab = "Normalized Value",
        ylim = c(min(nys1_data$variable, nys2_data$variable), max(nys1_data$variable, nys2_data$variable)),
        main = paste("Zone", nys1$zone, nys1$component$name,
                        "against Zone", nys2$zone, nys2$component$name, "for the", season, "season"),
        xaxt = "n")  # Remove x-axis labels

    # Create a new date column for labeling the x-axis using the month and day
    nys1_data$date <- as.Date(paste("2020-", nys1_data$month, "-", nys1_data$day, sep = ""))

    # Add custom x-axis labels
    axis(1, at = seq(1, length(nys1_data$variable), by = 30), labels = format(as.Date(nys1_data$date[seq(1, length(nys1_data$variable), by = 30)]), "%m/%d"))

    lines(nys2_data$variable, col = "red")  

    legend("topleft",
            legend = c(nys1$component$name, nys2$component$name),
            col = c("blue", "red"), lty = c(1, 1))

    dev.off()
  }
}

if (confirm_all == "N") {
  ##############################
  nys1 <- list(zone = iso_zone_prompt(), component = detect_component(component_prompt()))
  nys2 <- list(zone = iso_zone_prompt(), component = detect_component(component_prompt()))

  compare_seasonal_series(nys1, nys2)
} else {
  # Compare all the zones for all the components
  completed_zones <- c()  # List to keep track of completed zones

  for (zone1 in zones) {
    for (component1 in components) {
      nys1 <- list(zone = zone1, component = detect_component(component1))

      for (zone2 in zones) {
        for (component2 in components) {
          nys2 <- list(zone = zone2, component = detect_component(component2))

          # Skip if the two zones are the same or if the zone has already been completed
          if (nys1$zone == nys2$zone || nys2$zone %in% completed_zones) {
            next
          }

          compare_seasonal_series(nys1, nys2)
        }
      }

      completed_zones <- c(completed_zones, zone1)  # Mark the zone as completed
    }
  }
}