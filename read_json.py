import json
import csv

file = open('class_schedule.json')
classes = json.load(file)
file.close()

sections = classes['sections']

# now we will open a file for writing
data_file = open('catalog.csv', 'w')
 
# create the csv writer object
csv_writer = csv.writer(data_file)
 
# Counter variable used for writing 
# headers to the CSV file
count = 0
 
for sec in sections:
    if count == 0:
 
        # Writing headers of CSV file
        header = sec.keys()
        csv_writer.writerow(header)
        count += 1
 
    # Writing data of CSV file
    csv_writer.writerow(sec.values())
 
data_file.close()

