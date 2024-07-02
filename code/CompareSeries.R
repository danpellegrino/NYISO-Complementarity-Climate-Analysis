library("WaveletComp")
library("reticulate")
citation("WaveletComp")

source("code/SharedFunctions.R")

# Do you need to convert the NetCDF files to CSV?
netcdf_convert_prompt()

# Do you want to compare all the zones for all the components?
confirm_all <- all_zones_prompt()

average_data <- function(data, date) {
  # Extract the day and month from the date
  data$day <- as.numeric(format(as.Date(data[, date]), "%d"))
  data$month <- as.numeric(format(as.Date(data[, date]), "%m"))

  # Aggregate the data by day and month
  data <- aggregate(data[, 2:ncol(data)],
    by = list(data$day, data$month), FUN = mean
  )

  # Remove the extra day and month columns
  data <- data[, -c(1, 2)]

  return(data)
}

smooth_data <- function(data) {
  # Smooth the data using a moving average
  data <- smooth.spline(data, spar = 0.75)$y

  return(data)
}

compare_series <- function(nys1, nys2) {
  ##############################
  nys1$file <- load_file(nys1$zone, nys1$component$variable)
  nys2$file <- load_file(nys2$zone, nys2$component$variable)

  nys1_data <- create_data_frame(nys1)
  nys2_data <- create_data_frame(nys2)
  ##############################

  print(paste("Computing Wavelet Transform for Zone", nys1$zone, nys1$component$name, "and Zone", nys2$zone, nys2$component$name))

  # Average out the data from x amount of years to one year
  nys1_data <- average_data(nys1_data, "date")
  nys2_data <- average_data(nys2_data, "date")

  # Standardize the data
  nys1_data$variable <- standardize_data(nys1_data$variable)
  nys2_data$variable <- standardize_data(nys2_data$variable)

  # Combine the two data frames into one
  combined_data <- data.frame(variable = ((nys1_data$variable + nys2_data$variable) + abs(max(nys1_data$variable)) + abs(max(nys2_data$variable))), day = nys1_data$day, month = nys1_data$month)

  # Smooth the data
  nys1_data$variable <- smooth_data(nys1_data$variable)
  nys2_data$variable <- smooth_data(nys2_data$variable)
  combined_data$variable <- smooth_data(combined_data$variable)

  svg(
    paste("output/TimeSeries_",
      nys1$zone, "_", nys1$component$variable, "_vs_",
      nys2$zone, "_", nys2$component$variable, ".svg",
      sep = ""
    ),
    width = 8, height = 4.4, pointsize = 6
  )

  # Plot the two graphs together
  plot(nys1_data$variable,
    type = "l", col = "blue",
    xlab = "Day of the Year", ylab = "Normalized Value",
    ylim = c(min(nys1_data$variable, nys2_data$variable, combined_data$variable), max(nys1_data$variable, nys2_data$variable, combined_data$variable)),
    main = paste(
      "Zone", nys1$zone, nys1$component$name,
      "against Zone", nys2$zone, nys2$component$name
    )
  )

  lines(nys2_data$variable, col = "red")

  lines(combined_data$variable, col = "purple", lty = 2)

  legend("topleft",
    legend = c(nys1$component$name, nys2$component$name, "Combined Wavelet"),
    col = c("blue", "red", "purple"), lty = c(1, 1, 2)
  )

  dev.off()
}

if (confirm_all == "N") {
  ##############################
  nys1 <- list(zone = iso_zone_prompt(), component = detect_component(component_prompt()))
  nys2 <- list(zone = iso_zone_prompt(), component = detect_component(component_prompt()))

  compare_series(nys1, nys2)
} else {
  # Compare all the zones for all the components
  completed_zones <- c() # List to keep track of completed zones

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

          compare_series(nys1, nys2)
        }
      }

      completed_zones <- c(completed_zones, zone1) # Mark the zone as completed
    }
  }
}
