library("ncdf4")
library("WaveletComp")
citation("WaveletComp")

# Load the data
# Historical

nyszone = ""
while (!(nyszone %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"))) {
    nyszone = readline(prompt = "Enter a character for which NYISO Zone:\nA (West)\nB (Genessee)\nC (Central)\nD (North)\nE (Mohawk Valley)\nF (Capital)\nG (Hudson Valley)\nH (Millwood)\nI (Dunwoodie)\nJ (NYC)\nK (Long Island)\n  Option: ")
    nyszone = as.character(nyszone)
    if (!(nyszone %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"))) {
        cat("\n", nyszone, "is not a valid NYISO Zone.\n\n")
    }
}

cat("\n")

# Select which information is being gathered
component = ""
while (!(component %in% c("1", "2", "3", "4"))) {
    component = readline(prompt = "Enter any number:\n 1. Wind Speed\n 2. Precipitation\n 3. Snowfall\n 4. Solar Radiation\n  Option: ");
    component = as.integer(component);
    if (!(component %in% c(1, 2, 3, 4))) {
        cat("\n", component, "is not a valid option.\n\n")
    }
}

cat("You entered: Zone", nyszone, "and", component, "\n")

if(component == 1) {
    cat("Selecting from Wind Speed Data Files...\n")
    ncin1950 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin1960 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin1970 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin1980 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin1990 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2000 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2010 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2015 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2025 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2035 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2045 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2050 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2060 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2070 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2080 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    ncin2090 <- nc_open("data/MRI-AGCM3.2/sfcWind/")
    component <- "sfcWind"
} else if(component == 2) {
    cat("Selecting from Precipitation Data Files...\\n")
    # TEMP - Add precipitation data files
    q()
} else if(component == 3) {
    cat("Selecting from Snowfall Data Files...\n")
    # TEMP - Add snowfall data files
    q()
} else if(component == 4) {
    cat("Selecting from Solar Radiation Data Files...\n")
    # TEMP - Add solar radiation data files
    q()
}

# Grab the dimensions
lon <- ncvar_get(ncin1850, "lon")
lat <- ncvar_get(ncin1850, "lat")

if(nyszone == "A") {
    # NYISO Zone A
    lon_range <- which(lon == 43.125)
    lat_range <- which(lat == -78.5)
} else if(nyszone == "B") {
    # NYISO Zone B
    lon_range <- which(lon == 43.125)
    lat_range <- which(lat == -77.5)
} else if(nyszone == "C") {
    # NYISO Zone C
    lon_range <- which(lon == 43.125)
    lat_range <- which(lat == -76.5)
} else if(nyszone == "D") {
    # NYISO Zone D
    lon_range <- which(lon == 44.375)
    lat_range <- which(lat == -73.5)
} else if(nyszone == "E") {
    # NYISO Zone E
    lon_range <- which(lon == 43.125)
    lat_range <- which(lat == -75.5)
} else if(nyszone == "F") {
    # NYISO Zone F
    lon_range <- which(lon == 43.125)
    lat_range <- which(lat == -73.5)
} else if(nyszone == "G") {
    # NYISO Zone G
    lon_range <- which(lon == 41.875)
    lat_range <- which(lat == -73.5)
} else if(nyszone == "H") {
    # NYISO Zone H
    lon_range <- which(lon >= 41.0 & lon <= 41.875)
    lat_range <- which(lat >= -73.952179 & lat <= -73.598945)
    # TEMP - Add Zone H longitude and latitude
    lon_subset <- lon[lon_range]
    lat_subset <- lat[lat_range]
    print("Zone H")
    print (lon_subset)
    print (lat_subset)
} else if(nyszone == "I") {
    # NYISO Zone I
    lon_range <- which(lon >= 40.940217 & lon <= 41.142036)
    lat_range <- which(lat >= -73.85737 & lat <= -73.782007)
    # TEMP - Add Zone I longitude and latitude
    lon_subset <- lon[lon_range]
    lat_subset <- lat[lat_range]
    print("Zone I")
    print (lon_subset)
    print (lat_subset)
} else if(nyszone == "J") {
    # NYISO Zone J
    lon_range <- which(lon >= 40.589204 & lon <= 41)
    lat_range <- which(lat >= -74.214411 & lat <= -73.817446)
    # TEMP - Add Zone J longitude and latitude
    lon_subset <- lon[lon_range]
    lat_subset <- lat[lat_range]
    print("Zone J")
    print (lon_subset)
    print (lat_subset)
} else if(nyszone == "K") {
    # NYISO Zone K
    lon_range <- which(lon >= 40.610683 & lon <= 41.048885)
    lat_range <- which(lat >= -73.759891 & lat <= -72.208735)
    # TEMP - Add Zone K longitude and latitude
    lon_subset <- lon[lon_range]
    lat_subset <- lat[lat_range]
    print("Zone K")
    print (lon_subset)
    print (lat_subset)
}

# Subset the longitude and latitude
lon_subset <- lon[lon_range]
lat_subset <- lat[lat_range]

# Grab the data
# Add the different time periods together
tmp.array <- ncvar_get(ncin1850, component)[lon_range, lat_range, ]
tmp.array <- c(tmp.array, ncvar_get(ncin1870, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1890, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1900, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1910, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1930, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1950, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1970, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1990, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2010, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2015, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2035, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2055, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2075, component)[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2095, component)[lon_range, lat_range, ])

# Close the file
nc_close(ncin1850)
nc_close(ncin1870)
nc_close(ncin1890)
nc_close(ncin1900)
nc_close(ncin1910)
nc_close(ncin1930)
nc_close(ncin1950)
nc_close(ncin1970)
nc_close(ncin1990)
nc_close(ncin2010)
nc_close(ncin2015)
nc_close(ncin2035)
nc_close(ncin2055)
nc_close(ncin2075)
nc_close(ncin2095)

# Convert the array to a time series
tmp.ts <- ts(tmp.array, start = c(1850, 1), frequency = 365)

# Aggregate the data to yearly frequency
tmp.ts.year <- aggregate(tmp.ts, nfrequency = 1, FUN = mean)

# Create a sequence of years for the x-axis labels
years <- seq(from = 1850, length.out = length(tmp.ts.year))

# Filter the data up to the year 2100
tmp.ts.year <- tmp.ts.year[years <= 2100]
years <- years[years <= 2100]

# Plot the data with the year labels
matplot(years, tmp.ts.year, type = "l", xlab = "Year", ylab = "Wind Speed (m/s)", main = "Wind Speed Mean in NYISO Zone A (1850-2100)")
