library("ncdf4")
library("WaveletComp")
citation("WaveletComp")

# Load the data
# Historical
ncin1850 <- nc_open("Historical/dataset/Near-Surface Wind Speed/sfcWind_Amon_GISS-E2-H-CC_historical_r1i1p1_185001-190012.nc")
ncin1901 <- nc_open("Historical/dataset/Near-Surface Wind Speed/sfcWind_Amon_GISS-E2-H-CC_historical_r1i1p1_190101-195012.nc")
ncin1951 <- nc_open("Historical/dataset/Near-Surface Wind Speed/sfcWind_Amon_GISS-E2-H-CC_historical_r1i1p1_195101-201012.nc")
# RCP 4.5
ncin2006 <- nc_open("RCP 4.5/dataset/Near-Surface Wind Speed/sfcWind_Amon_GISS-E2-H-CC_rcp45_r1i1p1_200601-205012.nc")
ncin2051 <- nc_open("RCP 4.5/dataset/Near-Surface Wind Speed/sfcWind_Amon_GISS-E2-H-CC_rcp45_r1i1p1_205101-210012.nc")

# Grab the dimensions
lon <- ncvar_get(ncin2006, "lon")
lat <- ncvar_get(ncin2006, "lat")

# NYISO Zone C
lon_range <- which(lon == 43.75)
lat_range <- which(lat == -77)

# Subset the longitude and latitude
lon_subset <- lon[lon_range]
lat_subset <- lat[lat_range]

# Grab the data
# Add the different time periods together
tmp.array <- ncvar_get(ncin1850, "sfcWind")[lon_range, lat_range, ]
tmp.array <- c(tmp.array, ncvar_get(ncin1901, "sfcWind")[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin1951, "sfcWind")[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2006, "sfcWind")[lon_range, lat_range, ])
tmp.array <- c(tmp.array, ncvar_get(ncin2051, "sfcWind")[lon_range, lat_range, ])

# Close the file
nc_close(ncin1850)
nc_close(ncin1901)
nc_close(ncin1951)
nc_close(ncin2006)
nc_close(ncin2051)

# Convert the array to a time series
tmp.ts <- ts(tmp.array, start = c(1850, 1), frequency = 12)

# Aggregate the data to yearly frequency
tmp.ts.year <- aggregate(tmp.ts, nfrequency = 1, FUN = mean)

# Create a sequence of years for the x-axis labels
years <- seq(from = 1850, length.out = length(tmp.ts.year))

# Plot the data with the year labels
matplot(years, tmp.ts.year, type = "l", xlab = "Year", ylab = "Wind Speed (m/s)", main = "Wind Speed Mean in NYISO Zone C (1850-2100)")