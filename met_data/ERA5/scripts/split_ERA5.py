#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct  7 15:45:55 2022

Split GRIB or NetCDF file: creates new files, each one containing a single
    variable, for a single timestep of the model.

@author: Danilo Couto de Souza
"""

import xarray as xr
from cfgrib.xarray_to_grib import to_grib
import pandas as pd 
import os
import sys

file = sys.argv[1]

print('opening file: '+file+'...')

ds = xr.open_dataset(file)

print('ok')

name = os.path.basename(file).split('.')[0]
coords = ds.coords

for var in ds.variables:
    print('exporting variable: '+var+'...')
    if var not in coords:
        for ts in ds.time:
            
            tmp = ds[var].sel(time=ts)
            
            t = pd.to_datetime(str(ts.values)) 
            d = t.strftime('%Y-%m-%d-%H%M')
            
            file_string = 'ERA5_'+var+'_'+d+'.grib'
            ds2 = tmp.to_dataset(name=var)
        
            print('saving file: '+file_string+'...')
            to_grib(ds2, file_string)
            print(file_string+' saved')