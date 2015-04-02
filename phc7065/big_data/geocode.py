# PACKAGE: https://github.com/DenisCarriere/geocoder
import geocoder
import os
import pandas as pd
import re
import matplotlib.image as mpimg
import numpy as np
import platform
import glob
import math

# Read in and combine hierarchical addresses fields
# Separated by commas
# e.g.: 530 NW 2nd Street, Gainesvill, FL, USA
# Name your pandas dataframe "dat" and name the 
# combined address field "address"
os.chdir('/home/joebrew/Documents/uf/phc7065/big_data')
dat = pd.read_csv('addresses.csv')
dat.head()

# Sort and clean
address = sorted(set(dat['address']))

# Make dataframe for each geocoder
template = pd.DataFrame(data = 
    {'address' : address,
    'ok' : '',
    'lat' : '',
    'lng' : '', 
    'confidence' : '',
    'quality' : '',
    'fields_used' : ''
    })

# Make a pd.df for each of the geocoders
arc = template.copy()
bing = template.copy()
geonames = template.copy()
google = template.copy()
here = template.copy()
mapquest = template.copy()
opencage = template.copy()
osm = template.copy()
tomtom = template.copy()
yahoo = template.copy()


# Define function for geocoding
def multi_geo(df = None, fun = geocoder.google):
    # Make sure df has been assigned
    try:
        df
    except NameError():
        df = template.copy()

    # Loop through each row to geocode
    for i in df.index:
        # isolate address
        add = df['address'][i]

        # stop if no address
        if pd.isnull(add):
            continue

        #split into its pieces
        add_split = add.split(',')

        # try to geocode
        gc = fun(add)

        #if it didn't work, chop off first part and try again
        #until it works
        while not gc.ok:
            if len(add_split) == 1:
                break
            else:
                add_split = add_split[1:]
                new_add = ','.join(add_split)
                gc = fun(new_add)
        # If not broken, populate the df
        if gc.ok:
            df['lat'][i] = gc.lat
            df['lng'][i] = gc.lng
            df['confidence'][i] = gc.confidence
            df['quality'][i] = gc.quality
            df['fields_used'][i] = len(add_split)
        print 'geocoding row ' + str(i) + ' of ' + str(len(df.index))
    return(df)


# Geocode #####
# Assign to write_dir wherever you want to export the csvs
write_dir = '/home/joebrew/Documents/uf/phc7065/big_data/addresses_processed'
os.chdir(write_dir)

# ArcGIS
arc = multi_geo(df = arc, fun = geocoder.arcgis)
arc.to_csv('arc.csv')

# Bing
bing = multi_geo(df = bing, fun = geocoder.bing)
bing.to_csv('bing.csv')

# Geonames
geonames = multi_geo(df = geonames, fun = geocoder.geonames)
geonames.to_csv('geonames.csv')

# Google
google = multi_geo(df = google, fun = geocoder.google)
google.to_csv('google.csv')

# HERE
here = multi_geo(df = here, fun = geocoder.here)
here.to_csv('here.csv')

# Mapquest
mapquest = multi_geo(df = mapquest, fun = geocoder.mapquest)
mapquest.to_csv('mapquest.csv')

# Opencage
opencage = multi_geo(df = opencage, fun = geocoder.opencage)
opencage.to_csv('opencage.csv')

# OSM
osm = multi_geo(df = osm, fun = geocoder.osm)
osm.to_csv('osm.csv')

# Tom-tom
tomtom = multi_geo(df = tomtom, fun = geocoder.tomtom)
tomtom.to_csv('tomtom.csv')

# yahoo
yahoo = multi_geo(df = yahoo, fun = geocoder.yahoo)
yahoo.to_csv('yahoo.csv')



# # Define function to calculate euclidean distance
# pi = 3.14159265358939
# def get_distance(lon1, lat1, lon2, lat2):
#     rad = pi / 180
#     a1 = lat1 * rad
#     a2 = lon1 * rad
#     b1 = lat2 * rad
#     b2 = lon2 * rad
#     dlon = b2 - a2
#     dlat = b1 - a1
#     a = (math.sin(dlat/2.0))**2.0 + math.cos(a1) * math.cos(b1) * (math.sin(dlon/2.0))**2.0
#     c = 2 * math.atan2(math.sqrt(a), math.sqrt(1.0 - a))
#     R = 6378.145
#     d = R * c
#     return d



# ##### Read in geocoded data
# # Go to where the files were written
# read_dir = write_dir
# os.chdir(read_dir)

# # Which files exist ?
# files = glob.glob('*.csv')

# # Loop through all those files, appending to df
# df = pd.DataFrame()
# for i in files:
#     frame = pd.read_csv(i)
#     frame['source'] = i.replace('.csv', '')
#     df = df.append(frame)

# # You've now got a dataframe of all the addresses
# # once for each geocoder (ie n_addresses * n_geocoders)

# # Eliminate the NaNs
# df = df[pd.notnull(df['fields_used'])]


# # Clean up df to group by address
# grouped = df.groupby('address')

# # Define functions for getting the indicators we want
# def get_centroids(df):
#     return pd.Series(data = {
#         "mean_confidence" : np.mean(df['confidence']),
#         "lng_centroid": np.mean(df['lng']),
#         "lat_centroid": np.mean(df['lat']),
#         "mean_fields_used": np.mean(df['fields_used'])
#         })

# # Get centroid for each address
# temp = pd.DataFrame(grouped.apply(get_centroids))
# temp['address'] = temp.index

# # Merge centroids back into df
# df = pd.merge(left = df, right = temp, on = 'address', how = 'left')

# # Get distance from centroid
# df['distance_from_centroid'] = ''
# for i in range(len(df)):
#     x = get_distance(lon1 = df['lng'][i], lat1 = df['lat'][i],lon2 = df['lng_centroid'][i], lat2 = df['lat_centroid'][i])
#     df['distance_from_centroid'][i] = x

# # Get mean distance from centroid and number of geocoders
# temp['mean_distance_from_centroid'] = ''
# temp['n_geocoders'] = ''
# for i in range(len(temp)):
#     good_address = temp['address'][i]
#     temp['mean_distance_from_centroid'][i] = np.mean(df[df['address'] == good_address]['distance_from_centroid'])
#     temp['n_geocoders'][i] = len(df[df['address'] == good_address])

# # final dataframe of addresses
# # (if an address didn't make it in, it's because it
# # was explicitly junked in previous steps)
# final = temp

# # Create an unweighted quality index
# # (this can be tweaked in the future)
# final['quality_index'] = (final['mean_confidence'] * final['mean_fields_used'] * final['n_geocoders']) / final['mean_distance_from_centroid']

# # Write csv
# os.chdir(write_dir)
# final.to_csv('processed/multi_geocoded_locations.csv')

# # Final is a dataframe with all the addresses,
# # the centroid lat/lng, mean geocoder provided confidence
# # mean number of fields used, number of successful geocoders
# # and mean distance from centroid

# import matplotlib.pyplot as plt
# plt.figure()
# final['quality_index'].diff().hist(color = 'blue', alpha=0.3, bins=30)
# plt.show()

# print final['mean_distance_from_centroid'].quantile([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0])

