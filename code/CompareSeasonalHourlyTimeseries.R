source("code/SharedFunctions.R")

# https://github.com/ropensci/nasapower
library("nasapower")
library("ggplot2")
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
    if (parameter == "WS2M") {
        parameter_name <- "Wind Speed"
    } else if (parameter == "PRECTOTCORR") {
        parameter_name <- "Precipitation"
    } else if (parameter == "PRECSNOLAND") {
        parameter_name <- "Snow Precipitation"
    } else if (parameter == "ALLSKY_SFC_SW_DWN") {
        parameter_name <- "Direct Normal Irradiance (DNI)"
    }

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
        )),
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

compare_seasonal_hourly_timeseries <- function(nysiso1, nysiso2) {
    # If the results already exist, load them
    if (file.exists(paste("output/TimeSeries_", nysiso1$zone, "_vs_", nysiso2$zone, ".RData", sep = ""))) {
        # Load the data
        print(paste("Data already exists for Zone", nysiso1$zone, "and Zone", nysiso2$zone, ". Loading the data..."))
        load(paste("output/TimeSeries_", nysiso1$zone, "_vs_", nysiso2$zone, ".RData", sep = ""))

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

        # Compute the seasonal hourly average for each parameter
        for (season in seasons) {
            for (parameter in parameters) {
                # Print the message
                print(paste("Computing Seasonal Hourly Average for Zone", nysiso1$zone, detect_parameter_name(parameter), "in", season))
                nysiso1$results[[paste0(season, "_", parameter, "_avg")]] <- get_seasonal_hourly_avg(nysiso1$power, parameter, season)
                nysiso1$results[[paste0(season, "_", parameter, "_avg")]] <- smooth.spline(nysiso1$results[[paste0(season, "_", parameter, "_avg")]], spar = 0.75)$y

                print(paste("Computing Seasonal Hourly Average for Zone", nysiso2$zone, detect_parameter_name(parameter), "in", season))
                nysiso2$results[[paste0(season, "_", parameter, "_avg")]] <- get_seasonal_hourly_avg(nysiso2$power, parameter, season)
                nysiso2$results[[paste0(season, "_", parameter, "_avg")]] <- smooth.spline(nysiso2$results[[paste0(season, "_", parameter, "_avg")]], spar = 0.75)$y
            }
        }

        # Store the results in a file
        save(nysiso1, nysiso2, file = paste("output/TimeSeries_", nysiso1$zone, "_vs_", nysiso2$zone, ".RData", sep = ""))
    }

    total_seasons <- length(seasons)
    total_parameters <- length(parameters)
    for (parameter1 in parameters) {
        i <- 1
        plots <- vector("list", length = (total_seasons * total_parameters))
        for (parameter2 in parameters) {
            for (season in seasons) {
                # get an array of the hours of the day
                df <- data.frame(hour = 0:23, iso1 = nysiso1$results[[paste0(season, "_", parameter1, "_avg")]], iso2 = nysiso2$results[[paste0(season, "_", parameter2, "_avg")]])

                # Print the message
                print(paste("Plotting Time Series in", season, "for Zone", nysiso1$zone, detect_parameter_name(parameter1), "and Zone", nysiso2$zone, detect_parameter_name(parameter2)))

                # Create the plot
                plot <- ggplot(data = df, aes(x = hour))
                plot <- plot + geom_line(aes(y = iso1, color = paste("Zone", nysiso1$zone)))
                plot <- plot + geom_line(aes(y = iso2, color = paste("Zone", nysiso2$zone)))
                plot <- plot + labs(
                    x = "Hour of the Day",
                    y = "Normalized Value",
                    title = paste("Zone", nysiso1$zone, detect_parameter_name(parameter1), "against Zone", nysiso2$zone, detect_parameter_name(parameter2), "in", paste(toupper(substring(season, 1, 1)), substring(tolower(season), 2), sep = ""))
                )
                plot <- plot + scale_color_manual("",
                    breaks = c(paste("Zone", nysiso1$zone), paste("Zone", nysiso2$zone)),
                    values = c("red", "blue")
                )
                plot <- plot + scale_x_time(breaks = c(0, 6, 12, 18, 23), labels = c("Midnight", "6 AM", "Noon", "6 PM", "Midnight"))
                plot <- plot + scale_y_continuous(limits = c(min(df$iso1, df$iso2), max(df$iso1, df$iso2)))
                plot <- plot + theme(
                    plot.title = element_text(hjust = 0.5, size = 12),
                    axis.text.x = element_text(size = 8),
                    axis.text.y = element_text(size = 8),
                    axis.title = element_text(size = 10),
                    legend.title = element_text(size = 10),
                    legend.text = element_text(size = 10),
                    legend.position = "top"
                )

                # Save the plot
                plots[[i]] <- plot

                i <- i + 1
            }
        }
        # Output the plots
        plot_grid(plotlist = plots, ncol = total_seasons, nrow = total_parameters)
        ggsave(
            paste("output/TimeSeries_",
                nysiso1$zone, "_", parameter1, "_vs_",
                nysiso2$zone, "_seasonal", ".png",
                sep = ""
            ),
            width = 32, height = 16, dpi = 200
        )
    }
}

if (confirm_all == "N") {
    nysiso1 <- list(
        zone = iso_zone_prompt()
    )

    nysiso2 <- list(
        zone = iso_zone_prompt()
    )

    compare_seasonal_hourly_timeseries(nysiso1, nysiso2)
} else {
    # Compare all the zones for all the components
    completed_zones <- c() # List to keep track of completed zones

    for (zone1 in zones) {
        nysiso1 <- list(zone = zone1)
        for (zone2 in zones) {
            nysiso2 <- list(zone = zone2)

            # Skip if the two zones are the same or if the zone has already been completed
            if (nysiso1$zone == nysiso2$zone || nysiso2$zone %in% completed_zones) {
                next
            }

            compare_seasonal_hourly_timeseries(nysiso1, nysiso2)
        }

        completed_zones <- c(completed_zones, zone1) # Mark the zone as completed
    }
}
