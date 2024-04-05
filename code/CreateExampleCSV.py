import csv
from datetime import datetime, timedelta
import math

start_date = datetime(1950, 1, 1)
end_date = datetime(2099, 12, 31)

# Generate dates
dates = []
current_date = start_date
while current_date <= end_date:
    dates.append(current_date.strftime('%Y-%m'))
    if current_date.month in [1, 3, 5, 7, 8, 10, 12]:
        current_date += timedelta(days=31)
    elif current_date.month == 2:
        if current_date.year % 4 == 0 and (current_date.year % 100 != 0 or current_date.year % 400 == 0):
            current_date += timedelta(days=29)
        else:
            current_date += timedelta(days=28)
    else:
        current_date += timedelta(days=30)

# Generate complimentary values
values1 = [math.sin((i * math.pi)/2) for i in range(1, len(dates))]
values2 = [math.cos((i * math.pi)/2) for i in range(1, len(dates))]

# Write to CSV files
with open('data/data_example1.csv', 'w', newline='') as file1, open('data/data_example2.csv', 'w', newline='') as file2:
    writer1 = csv.writer(file1)
    writer2 = csv.writer(file2)

    writer1.writerow(['Date', 'Value'])
    writer2.writerow(['Date', 'Value'])

    for date, value1, value2 in zip(dates, values1, values2):
        writer1.writerow([date, value1])
        writer2.writerow([date, value2])

print("CSV files created successfully!")