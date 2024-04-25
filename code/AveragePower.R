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

monthly_prompt <- function() {
  # Ask the user if they need to convert the data to monthly
  monthly <- ""
  prompt <- "Do you need to convert the data to monthly? (Y/N): "
  while (!(monthly %in% c("Y", "N"))) {
    monthly <- readline(prompt = prompt)
    monthly <- as.character(monthly)
    monthly <- toupper(monthly)
    if (!(monthly %in% c("Y", "N"))) {
      cat("\n", monthly, "is not a valid option.\n\n")
    }
  }

  return(monthly)
}

# Convert the data to monthly?
monthly <- monthly_prompt()

convert_to_monthly <- function(data, date, x) {
  data <- aggregate(data[, x], by = list(format(data[, date], "%Y-%m")), FUN = mean)
  data_renamed <- data.frame(date = data$Group.1, variable = data$x)
  data_renamed$date <- as.Date(paste(data_renamed$date, "-01", sep = ""), format = "%Y-%m-%d")
  return(data_renamed)
}

plot_power <- function(my.w, title) {
  # Plot the power
  maximum.level = 1.001 * max(my.w$Power.avg, my.w$Power.avg)
  wt.avg(my.w, main = title, maximum.level = maximum.level)
}

average_power <- function(nysiso) {
  ##############################
  nysiso$file <- load_file(nysiso$zone, nysiso$component$variable)

  nysiso_data <- create_data_frame(nysiso)
  ##############################

  if (monthly == "Y") {
    # Convert the data to monthly
    nysiso_data <- convert_to_monthly(nysiso_data, "date", "variable")

    # Set the file name
    file <- paste("output/AveragePower_", nysiso$zone, "_", nysiso$component$variable, "_monthly.png", sep = "")
  } else {
    # Set the file name
    file <- paste("output/AveragePower_", nysiso$zone, "_", nysiso$component$variable, ".png", sep = "")
  }
  title <- paste("Average Power of Zone",
                 nysiso$zone, nysiso$component$name)

  #Perform the wavelet analysis
  my.w <- analyze.wavelet(nysiso_data, "variable")

  png(file, width = 3.47, height = 1.84, units = "in", res = 1000, pointsize = 3)

  # Produce the plot
  plot_power(my.w, title)

  dev.off()  
}

if (confirm_all == "N") {
    nysiso <- list(zone = iso_zone_prompt(), component = detect_component(component_prompt()))

    average_power(nysiso)
} else {
    for (zone in zones) {
        for (component in components) {
            nysiso <- list(zone = zone, component = detect_component(component))
            average_power(nysiso)
        }
    }
}