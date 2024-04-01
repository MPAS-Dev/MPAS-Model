#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script to plot time series for a certain variable at a 
particular point (lon,lat,vertlevel) and time period.

Guilherme Torres Mendonça (guilherme.torresmendonca@ime.usp.br)
March 2024
"""
import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
import cfgrib
from haversine import haversine
from utils import add_mpas_mesh_variables, bash_array_to_list
import argparse

def closest_value_haversine(ds,lon,lat):
    df = ds.to_dataframe()
    df['dist_norm'] = df.apply(lambda row: 
                               haversine((row['latitude'],row['longitude']), 
                                         (lat,lon)), axis=1)
    mask = df['dist_norm'] == df['dist_norm'].min()
    nCells_value = df.loc[mask, :].index.get_level_values('nCells')[0]
    df_masked = df.loc[mask,:]
    return nCells_value

def closest_value_euclidean(ds,lon,lat):
    ds['dist_norm'] = np.sqrt((ds['longitude'] - lon)**2 + (ds['latitude'] - lat)**2)
    mask = ds['dist_norm'] == ds['dist_norm'].min()
    nCells_index = mask.argmax(dim='nCells')
    nCells_value = ds['nCells'].isel(nCells=nCells_index.values.item())
    return nCells_value

def closest_value_era5(ds,lon,lat):
    ds['dist_norm'] = np.sqrt((ds['longitude'] - lon)**2 + (ds['latitude'] - lat)**2)
    lon_min = ds['longitude'].where(ds['dist_norm'] == ds['dist_norm'].min()).values
    # Select only that value where lon is not nan
    lon_min = lon_min[~np.isnan(lon_min)]
    lat_min = ds['latitude'].where(ds['dist_norm'] == ds['dist_norm'].min()).values
    # Select only that value where lon is not nan
    lat_min = lat_min[~np.isnan(lat_min)]

    return lon_min,lat_min

def compute_wind_speed(data_dir,lat,lon,vertlevel):

    # Read dataset with concatenated data
    #ds = xr.open_dataset(f"{data_dir}/cat.nc")
    ds = xr.open_dataset(data_dir)
    ds = add_mpas_mesh_variables(ds)

    #index_closest  = closest_value_haversine(ds=ds.sel(Time=0),lon=lon,lat=lat)
    index_closest  = closest_value_euclidean(ds=ds.sel(Time=0),lon=lon,lat=lat)
    
    # Information on (lon,lat) point
    closest_lon = ds['longitude'].sel(Time=0,nCells=index_closest)
    closest_lat = ds['latitude'].sel(Time=0,nCells=index_closest)
    print ("nCells value:",index_closest)
    print ("ref (lon,lat):", (lon,lat))
    print ("closest (lon,lat):", (float(closest_lon),float(closest_lat)))
    print("zgrid:")
    print (ds['zgrid'].sel(Time=0,nCells=index_closest).values)

    # Obtain wind time series
    wind_speed = np.zeros((len(vertlevel),len(ds.Time)))
    for t in range(len(ds.Time)):
        for l in range(len(vertlevel)):
            wind_speed[l,t] = np.sqrt((ds['uReconstructZonal'].sel(nCells=index_closest,
                                                        nVertLevels=vertlevel[l],
                                                        Time=ds.Time[t]).values)**2
                            +(ds['uReconstructMeridional'].sel(nCells=index_closest,
                                                        nVertLevels=vertlevel[l],
                                                        Time=ds.Time[t]).values)**2)
    return ds.Time, wind_speed

def compute_wind_speed_era5(data_dir_u,data_dir_v,lat,lon,vertlevel):
    # see https://stackoverflow.com/questions/67963199/xarray-from-grib-file-to-dataset
    # Import data
    grib_data_u = cfgrib.open_datasets(data_dir_u)[0]
    grib_data_v = cfgrib.open_datasets(data_dir_v)[0]
    # Correct longitude values
    grib_data_u['longitude'] = grib_data_u['longitude'].where(grib_data_u['longitude'] <= 180.0, grib_data_u['longitude'] - 360.0)
    grib_data_v['longitude'] = grib_data_v['longitude'].where(grib_data_v['longitude'] <= 180.0, grib_data_v['longitude'] - 360.0)
    # Find (lon_min,lat_min) that are closest to (lon,lat) given in input
    lon_min_u,lat_min_u = closest_value_era5(ds=grib_data_u,lon=lon,lat=lat)
    lon_min_v,lat_min_v = closest_value_era5(ds=grib_data_v,lon=lon,lat=lat)
    # Check whether the same (lon_min,lat_min) was found for both u and v
    if lon_min_u == lon_min_v:
        if lat_min_u == lat_min_v:
            print ("same (lon,lat) found for u and v in era5 data")
            lat_min = lat_min_u
            lon_min = lon_min_u
        else:
            raise Exception("lat_min_u differs from lat_min_v")
    else:
        raise Exception("lon_min_u differs from lon_min_v")
    print ("lat_min", lat_min)
    print ("lon_min", lon_min)
    # Obtain wind time series
    wind_speed = np.zeros((len(vertlevel),len(grib_data_u.time)))
    for t in range(len(grib_data_u.time)):
        for l in range(len(vertlevel)):
            wind_speed[l,t] = np.sqrt((grib_data_u['u'].sel(longitude=lon_min,
                                                            latitude=lat_min,
                                                        isobaricInhPa=vertlevel[l],
                                                        time=grib_data_u.time[t]).values)**2
                            +(grib_data_v['v'].sel(longitude=lon_min,
                                                            latitude=lat_min,
                                                        isobaricInhPa=vertlevel[l],
                                                        time=grib_data_u.time[t]).values)**2)
    print (wind_speed)
    return grib_data_u.time,wind_speed

# Create ArgumentParser object
parser = argparse.ArgumentParser(description=
                                 'plotting wind speed for several experiments')
# Input arguments
parser.add_argument('--data_dir_array', type=str, help='data directories')
parser.add_argument('--fig_label_array', type=str, help='labels for figures')
parser.add_argument('--lat', type=str, help='latitude')
parser.add_argument('--lon', type=str, help='longitude')
parser.add_argument('--vertlevel', type=str, help='vertical level')

# Parse the command line arguments
args = parser.parse_args()

# Transform bash arrays into lists
data_dir_list = bash_array_to_list(args.data_dir_array)
fig_label_list = bash_array_to_list(args.fig_label_array)

# Remove empty items
data_dir_list = list(filter(None, data_dir_list))
fig_label_list = list(filter(None, fig_label_list))

# Get lat,lon,vertlevel
lat = float(args.lat)
lon = float(args.lon)
vertlevel = [int(args.vertlevel)]

# Set colormap
cmap = plt.get_cmap('Blues')

plt.figure("wind_speed_without_spinup")
plt.figure(figsize=(10, 6))
j=0
for data_dir_tmp in data_dir_list:
    print ("plotting:", fig_label_list[j])
    t,wind_speed = compute_wind_speed(data_dir=f"{data_dir_tmp}/output/cat.nc",
                                            lat=lat,
                                            lon=lon,
                                            vertlevel=vertlevel)
    if len(data_dir_list) > 1:
        color = cmap(j / (len(data_dir_list) - 1))
    else:
        color = "k" #cmap(1)
    plt.plot(t[0:25],wind_speed[0,5:30],linestyle='-',
             color=color,label=fig_label_list[j])
    j=j+1
plt.xlabel("Time [h]",fontsize=12)
plt.ylabel("Wind Speed [m/s]",fontsize=12)
plt.tick_params(axis='both', which='major', labelsize=12)
plt.xticks(np.arange(min(t), max(t)+1,1),
           rotation=45, ha='center')
plt.grid(True)
#plt.legend(loc='lower left', bbox_to_anchor=(0., 1.02), borderaxespad=0)
#plt.legend(loc='best')
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.xlim([0,24])
plt.ylim([0,12])
plt.title("wind speed at theta(1) = 390m; 2017-04-04")
#plt.show()
plt.tight_layout()
plt.savefig("windspeed.png")