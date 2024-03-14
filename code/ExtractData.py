import glob
from netCDF4 import Dataset
import pandas as pd
import numpy as np
import os

def rename_files(directory):
    for filename in os.listdir(directory):
        if filename.endswith('.nc'):
            year = filename.split('_')[-1].split('0101-')[0]
            new_filename = os.path.join(directory, year + '.nc')
            os.rename(os.path.join(directory, filename), new_filename)

# Ask the user which type of data they want to extract
choices = ['1', '2', '3', '4']
user_choice = None
while user_choice not in ['1', '2', '3', '4', 'ALL']:
    print("Which type of data would you like to extract?")
    print("ALL. All Data Types")
    print("1. Wind Speed")
    print("2. Precipitation")
    print("3. Snowfall")
    print("4. Solar Radiation") 
    # Get the user's choice
    user_choice = input("Enter the number of the data type: ")

# If the user chooses to extract all data types
if user_choice == 'ALL':
    # Create a list of all the data types
    user_choice = choices
else:
    # Convert the user's choice to a list
    user_choice = [user_choice]

# Ask the user which NYISO Zone they want to extract data for
zones = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K']
user_zone = None
while user_zone not in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'ALL']:
    print("Which NYISO Zone would you like to extract data for?")
    print("ALL. All Zones")
    print("A. Zone A")
    print("B. Zone B")
    print("C. Zone C")
    print("D. Zone D")
    print("E. Zone E")
    print("F. Zone F")
    print("G. Zone G")
    print("H. Zone H")
    print("I. Zone I")
    print("J. Zone J")
    print("K. Zone K")
    # Get the user's choice
    user_zone = input("Enter the letter of the zone: ")
    # Convert the user's choice to uppercase
    user_zone = user_zone.upper()

# If the user chooses to extract data for all zones
if user_zone == 'ALL':
    # Create a list of all the zones
    user_zone = zones
else:
    # Convert the user's choice to a list
    user_zone = [user_zone]

for choice in user_choice:
    if choice == '1':
        choice = 'sfcWind'
        component = 'Wind Speed'
    elif choice == '2':
        choice = 'pr'
        component = 'Precipitation'
    elif choice == '3':
        choice = 'prsn'
        component = 'Snowfall'
    elif choice == '4':
        choice = 'rsds'
        component = 'Solar Radiation'
    else:
        print("Invalid choice")
        # Exit the program
        exit()

    if not os.path.exists('data/MRI-AGCM3.2/' + choice):
        print("The path " + component + " does not exist")
        # Exit the program
        exit()
    if not os.path.exists('data/MRI-AGCM3.2/' + choice + '/2006.nc'):
        print("Renaming files for " + component)
        rename_files('data/MRI-AGCM3.2/' + choice)
        print("Renaming files for " + component + " complete!")

    print("Extracting data for " + component)
    

    all_years = []

    # Get a list of all the netCDF files in the data directory
    for file in glob.glob("data/MRI-AGCM3.2/" + choice + "/*.nc"):
        data = Dataset(file, "r")
        #print("\n\nVariables in the file:" + file + "\n")

        # get name of the file
        fileName = file.split("/")[-1]
        year = fileName.split(".")[0]
        all_years.append(year)

    # Sorting the all_years python list
    all_years.sort()

     # Creating an empty Pandas Dataframe covering the whole range of data
    year_start = min(all_years)
    year_end = max(all_years)
    date_range = pd.date_range(start = str(year_start) + '-01-01',
                                end = str(year_end) + '-12-31',
                                freq = 'D')

    df = pd.DataFrame(0.0, columns = [component], index = date_range)

    for zone in user_zone:
        # Defining the lat, lon of the location of interest
        lat_zone = None
        if zone == 'A':
            # Zone A - West
            lat_zone = 42.1875
            lon_zone = -79.17747
        elif zone == 'B':
            # Zone B - Genessee
            lat_zone = 43.1250
            lon_zone = -77.67826
        elif zone == 'C':
            # Zone C - Central
            lat_zone = 42.9375
            lon_zone = -76.17904
        elif zone == 'D':
            # Zone D - North
            lat_zone = 45.0000
            lon_zone = -74.86723
        elif zone == 'E':
            # Zone E - Mohawk Valley
            lat_zone = 42.375
            lon_zone = -75.42943
        elif zone == 'F':
            # Zone F - Capital
            lat_zone = 42.375
            lon_zone = -73.55541
        elif zone == 'G':
            # Zone G - Hudson Valley
            lat_zone = 41.8125
            lon_zone = -73.93022
        elif zone == 'H':
            # Zone H - Millwood
            lat_zone = 41.2500
            lon_zone = -73.74282
        elif zone == 'I':
            # Zone I - Dunwoodie
            lat_zone = 41.0625
            lon_zone = -73.74282
        elif zone == 'J':
            # Zone J - New York City
            lat_zone = 40.6875
            lon_zone = -73.93022
        elif zone == 'K':
            # Zone K - Long Island
            lat_zone = 41.0625
            lon_zone = -73.18061
        else:
            print("Invalid NYISO Zone")
            # Exit the program
            exit()

        # Converting the longitude to positive if it is negative
        if lon_zone < 0:
            lon_zone = 360 + lon_zone

        for yr in all_years:
            # Reading-in the data
            data = Dataset("data/MRI-AGCM3.2/" + choice + "/" + yr + ".nc", "r")

            # Storing the lat and lon data of the netCDF file into variables
            lat = data.variables['lat'][:]
            lon = data.variables['lon'][:]

            # Squared difference between the specified lat, lon and the lat, lon of the netCDF file
            sq_diff_lat = (lat - lat_zone)**2
            sq_diff_lon = (lon - lon_zone)**2

            # Identify the index of the min value for lat and lon
            min_index_lat = sq_diff_lat.argmin()
            min_index_lon = sq_diff_lon.argmin()

            print("Zone: " + zone + " " + component + " Year: " + yr + " Lat: " + str(lat[min_index_lat]) + " Lon: " + str(lon[min_index_lon]) + " " + str(data.variables[choice][0, min_index_lat, min_index_lon]) + " " + data.variables[choice].units)

            # Accessing the average data for the specified lat, lon
            component_data = data.variables[choice]

            # Creating the date range for each year during each iteration
            start = str(yr) + '-01-01'
            end = str(yr) + '-12-31'
            d_range = pd.date_range(start = start,
                                    end = end,
                                    freq = 'D')
            
            for t_index in np.arange(0, len(d_range)):
                df.loc[d_range[t_index]][component] = component_data[t_index, min_index_lat, min_index_lon]

        df.to_csv('data/MRI-AGCM3.2/' + choice + '/_zone_' + zone + '.csv')
        print("Data extraction for " + component + " from Zone " + zone + " complete!")
print("Data extraction complete!")