#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script to plot time series for a list of variables at a 
particular point (lon,lat,vertlevel) and time period.

Usage:
python3 mpas_time_series_at_point.py --dir /path/to/data
--stream history --t0 '2021-01-01 00:00:00' --tf '2021-01-02 18:00:00' 
--dt 10800 --var swdnb,swupb

Guilherme Torres Mendonça (guilherme.torresmendonca@ime.usp.br)
March 2024
"""
import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
import cfgrib
from haversine import haversine
import argparse
import pandas as pd
from utils import (
    add_mpas_mesh_variables, 
    create_datetime_range,
    concat_mpas_output,
    cs_string_to_list,
    find_nCells_from_latlon
)
import os

# Create ArgumentParser object
parser = argparse.ArgumentParser(description=
                                 'plotting time series for a certain variable at a' 
                                +'particular point and time period.')
# Input arguments
parser.add_argument('--dir', default=os.getcwd(), type=str, 
                    help='data directory')
parser.add_argument('--stream', default='history', type=str, 
                    help='stream')
parser.add_argument('--var', default='swdnb, swupb', type=str, 
                    help='list of variables to plot (var1,var2,...)')
parser.add_argument('--lat', default='-2.124375557901979', type=str, 
                    help='latitude')
parser.add_argument('--lon', default='-59.003420487096946', type=str, 
                    help='longitude')
parser.add_argument('--vertlevel', default=0, type=str, 
                    help='vertical level')
parser.add_argument('--t0', type=str, 
                    help='initial datetime (YYYY-mm-dd HH:MM:SS)')
parser.add_argument('--tf', type=str, 
                    help='final datetime (YYYY-mm-dd HH:MM:SS)')
parser.add_argument('--dt', type=str, 
                    help='datetime step (seconds)')
parser.add_argument('--closest_value', type=str, default='euclidean',
                    help='method to find grid cell from (lat,lon)')

# Parse the command line arguments
args = parser.parse_args()

# Create datetime range and transform it into a list of datetime strings
datetime_list = create_datetime_range(
    t0=args.t0,
    tf=args.tf,
    dt=args.dt
)

# Select variable and concatenate files between t0 and tf
vars_list = cs_string_to_list(args.var)
cat_file = concat_mpas_output(
    stream=args.stream,
    datetime_list=datetime_list,
    data_dir=args.dir,
    vars_list=vars_list,
    )

# Get lat, lon, vertlevel
lat = float(args.lat)
lon = float(args.lon)
vertlevel = int(args.vertlevel)

# Find grid cell
nCells, ds = find_nCells_from_latlon(cat_file,lon=lon,lat=lat)

# Plot
## Get again list of variables for plotting
vars_list = cs_string_to_list(args.var)
## Get again list of datetimes, but abbreviated
datetime_list = create_datetime_range(
    t0=args.t0,
    tf=args.tf,
    dt=args.dt,
    short_strings='y'
)

plt.figure()
for var in vars_list:
    # Try choosing nVertLevels
    #try:
    #    y_series = ds[var].sel(nCells=nCells,nVertLevels=vertlevel).values
    #except: 
    y_series = ds[var].sel(nCells=nCells).values
    plt.plot(datetime_list,y_series,linestyle='-',label=var)
plt.tick_params(axis='both', which='major', labelsize=12)
plt.xticks(np.arange(min(ds.Time), max(ds.Time)+1,1),
           rotation=45, ha='right')
plt.grid(True)
plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.title(f"{args.stream} from {args.t0} to {args.tf}")
plt.tight_layout()
plt.savefig("time_series.png")
plt.show()