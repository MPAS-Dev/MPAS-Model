#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct  7 15:45:55 2022

Split GRIB or NetCDF file: creates new files, each one for a single timestep 
    of the model.

@author: Danilo Couto de Souza
"""

import xarray as xr
from cfgrib.xarray_to_grib import to_grib
import pandas as pd 
import os
import sys
import warnings

# supress the "GRIB write support is experimental" warning
warnings.simplefilter(action='ignore', category=FutureWarning)

# provide grib file as argument
file = sys.argv[1]
print('opening file: '+file+'...')
# open grib file
ds = xr.open_dataset(file, engine='cfgrib')
print('ok')

# get prefix
name = os.path.basename(file).split('.')[0]
coords = ds.coords

# for var in ds.variables:
#     print('exporting variable: '+var+'...')
#     if var not in coords:
for ts in ds.time:
            
    # tmp = ds[var].sel(time=ts)
    tmp = ds.sel(time=ts)
    
    t = pd.to_datetime(str(ts.values)) 
    d = t.strftime('%Y-%m-%d-%H%M')
    
    # file_string = 'ERA5_'+var+'_'+d+'.grib'
    file_string = 'ERA5_'+d+'.grib'
    # ds2 = tmp.to_dataset(name=var)
    # ds2 = tmp.to_dataset(name=d)

    print('saving file: '+file_string+'...')
    to_grib(ds, file_string)
    print(file_string+' saved~')