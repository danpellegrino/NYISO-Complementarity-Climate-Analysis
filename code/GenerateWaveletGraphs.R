library("WaveletComp")
library("reticulate")
citation("WaveletComp")

source("code/SharedFunctions.R")

# Do you need to convert the NetCDF files to CSV?
netcdf_convert_prompt()

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

# Do you want to compare all the zones for all the components?
confirm_all <- all_zones_prompt()

# Convert the data to monthly?
monthly <- monthly_prompt()

convert_to_monthly <- function(data, date, x) {
    data <- aggregate(data[, x], by = list(format(data[, date], "%Y-%m")), FUN = mean)
    data_renamed <- data.frame(date = data$Group.1, variable = data$x)
    data_renamed$date <- as.Date(paste(data_renamed$date, "-01", sep = ""), format = "%Y-%m-%d")
    return(data_renamed)
}

daily_stdev <- function(data, date, x) {
    mean <- aggregate(data[, x], by = list(format(data[, date], "%j")), mean)
    stdev <- aggregate(data[, x], by = list(format(data[, date], "%j")), sd)

    stats <- data.frame(day = mean$Group.1, mean = mean$x, stdev = stdev$x)

    data[, x] <- ifelse(stats[match(format(data[, date], "%j"), stats$day), "stdev"] == 0,
        0,
        (data[, x] - stats[match(format(data[, date], "%j"), stats$day), "mean"]) /
            stats[match(format(data[, date], "%j"), stats$day), "stdev"]
    )

    return(data)
}

monthly_stdev <- function(data, date, x) {
    mean <- aggregate(data[, x], by = list(format(data[, date], "%m")), mean)
    stdev <- aggregate(data[, x], by = list(format(data[, date], "%m")), sd)

    stats <- data.frame(month = mean$Group.1, mean = mean$x, stdev = stdev$x)

    data[, x] <- ifelse(stats[match(format(data[, date], "%m"), stats$month), "stdev"] == 0,
        0,
        (data[, x] - stats[match(format(data[, date], "%m"), stats$month), "mean"]) /
            stats[match(format(data[, date], "%m"), stats$month), "stdev"]
    )

    return(data)
}

analyze_coherency <- function(data1, variable1, data2, variable2, monthly) {
    my_data <- data.frame(x = data1[, variable1], y = data2[, variable2])

    years <- 6
    if (monthly == "Y") {
        upper_bound <- 12 * years
    } else {
        upper_bound <- 365 * years
    }
    my_wc <- analyze.coherency(my_data, upperPeriod = upper_bound)

    return(my_wc)
}

plot_wave <- function(my_wc, title, index.ticks, index.labels, monthly) {
    if (monthly == "Y") {
        period_lab <- "Period (months)"
    } else {
        period_lab <- "Period (days)"
    }
    wc.image(my_wc,
        main = title,
        timelab = "Date (year)",
        periodlab = period_lab,
        spec.time.axis = list(at = index.ticks, labels = index.labels)
    )
}

wavelet_analysis <- function(nys1, nys2) {
    ##############################
    nys1$file <- load_file(nys1$zone, nys1$component$variable)
    nys2$file <- load_file(nys2$zone, nys2$component$variable)

    nys1_data <- create_data_frame(nys1)
    nys2_data <- create_data_frame(nys2)
    ##############################
    print(paste("Computing Wavelet Analysis for Zone", nys1$zone, nys1$component$name, "and Zone", nys2$zone, nys2$component$name))

    if (monthly == "Y") {
        # Convert the data to monthly
        nys1_data <- convert_to_monthly(nys1_data, "date", "variable")
        nys2_data <- convert_to_monthly(nys2_data, "date", "variable")

        # standard deviation
        # nys1_data <- monthly_stdev(nys1_data, "date", "variable")
        # nys2_data <- monthly_stdev(nys2_data, "date", "variable")

        index.ticks <- seq(12 * 11, nrow(nys1_data), by = 12 * 65)
        index.labels <- format(nys1_data$date, "%Y")[index.ticks]

        file <- paste("output/WaveletCoherence_", nys1$zone, "_", nys1$component$variable,
            "_vs_", nys2$zone, "_", nys2$component$variable, "_monthly.svg",
            sep = ""
        )
    } else {
        # standard deviation
        # nys1_data <- daily_stdev(nys1_data, "date", "variable")
        # nys2_data <- daily_stdev(nys2_data, "date", "variable")

        index.ticks <- seq(365 * 11, nrow(nys1_data), by = 365 * 65)
        index.labels <- format(nys1_data$date, "%Y")[index.ticks]

        file <- paste("output/WaveletCoherence_", nys1$zone, "_", nys1$component$variable,
            "_vs_", nys2$zone, "_", nys2$component$variable, ".svg",
            sep = ""
        )
    }

    title <- paste(
        "Wavelet Analysis of Zone",
        nys1$zone, nys1$component$name,
        "and Zone", nys2$zone, nys2$component$name
    )

    # Perform the wavelet analysis
    my_wc <- analyze_coherency(nys1_data, "variable", nys2_data, "variable", monthly)

    svg(file, width = 8, height = 4.4, pointsize = 6)

    # Produce the plot
    plot_wave(my_wc, title, index.ticks, index.labels, monthly)

    dev.off()
}

if (confirm_all == "N") {
    nys1 <- list(zone = iso_zone_prompt(), component = detect_component(component_prompt()))
    nys2 <- list(zone = iso_zone_prompt(), component = detect_component(component_prompt()))

    wavelet_analysis(nys1, nys2)
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

                    wavelet_analysis(nys1, nys2)
                }
            }

            completed_zones <- c(completed_zones, zone1) # Mark the zone as completed
        }
    }
}
