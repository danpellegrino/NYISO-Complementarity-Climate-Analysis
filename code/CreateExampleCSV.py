import csv
from datetime import datetime, timedelta

start_date = datetime(1950, 1, 1)
end_date = datetime(2099, 12, 31)

# Generate dates
dates = []
current_date = start_date
while current_date <= end_date:
    dates.append(current_date.strftime('%Y-%m-%d'))
    current_date += timedelta(days=1)

# Generate complimentary values
values1 = list(range(1, len(dates) + 1))
values2 = list(range(len(dates), 0, -1))

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