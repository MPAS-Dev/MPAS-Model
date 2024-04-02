#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import xarray as xr
import numpy as np
import math
import pandas as pd

#TODO: utils.py should contain generally useful functions for postprocessing.
#      Therefore, it should live under MPAS-BR/post_proc/py/.

derived_variables = {
    'latCell': ['latitude'],
    'lonCell': ['longitude'],
    'latVertex': ['latitudeVertex'],
    'lonVertex': ['longitudeVertex'],
    'areaCell': ['area', 'resolution'],
}

def add_mpas_mesh_variables(ds, full=True, **kwargs):
    for v in ds.data_vars:
        if v not in derived_variables:
            continue

        newvs = derived_variables[v]

        for newv in newvs:
            if newv in ds:
                #print(newv + ' already here')
                continue

            if 'lat' in v or 'lon' in v:
                ds[newv] = xr.apply_ufunc(np.rad2deg, ds[v])
                ds[newv] = ds[newv].where(ds[newv] <= 180.0, ds[newv] - 360.0)
                ds[newv].attrs['units'] = 'degrees'

            elif newv == 'area':
                radius_circle = ds.attrs.get('sphere_radius', 1.0)
                if radius_circle == 1:
                    # need to correct to earth radius
                    correction_rad_earth = 6371220.0
                else:
                    correction_rad_earth = 1

                ds[newv] = (ds[v] / 10 ** 6) * correction_rad_earth**2
                ds[newv].attrs['units'] = 'km^2 (assuming areaCell in m^2)'
                ds[newv].attrs['long_name'] = 'Area of the cell in km^2'

            elif newv == 'resolution':
                radius_circle = ds.attrs.get('sphere_radius', 1.0)
                if radius_circle == 1.0:
                    #print('need to correct to earth radius!!')
                    correction_rad_earth = 6371220.0
                else:
                    correction_rad_earth = 1

                # km^2 (assuming areaCell in m^2)
                area = (ds[v] / 10 ** 6) * correction_rad_earth**2

                ds[newv] = 2 * (xr.apply_ufunc(np.sqrt, area / math.pi))
                ds[newv].attrs['units'] = 'km'
                ds[newv].attrs['long_name'] = 'Resolution of the cell (approx)'

    return ds

def bash_array_to_list(bash_array):
    return bash_array.split("|")[:-1]

def cs_string_to_list(cs_string):
    return cs_string.split(",")

def create_datetime_range(t0,tf,dt,short_strings='n'):
    '''
    Creates a list of datetime strings between t0 and tf, with 
    time step dt.

    INPUT: t0 (str) - initial datetime in format 'YYYY.mm.dd HH:MM:SS'
           tf (str) - final datetime in format 'YYYY.mm.dd HH:MM:SS'
           dt (str) - time step in seconds

    OUTPUT: datetime_str_list (list) - list of datetime strings
    '''
    t0 = pd.Timestamp(t0)
    tf = pd.Timestamp(tf)
    datetime_range = pd.date_range(start=t0,end=tf,freq=f"{dt}S")
    if short_strings == 'y':
        datetime_str_list = [dt.strftime('%d/%m %Hh') for dt in datetime_range]
    else:
        datetime_str_list = [dt.strftime('%Y-%m-%d_%H.%M.%S') for dt in datetime_range]
    return datetime_str_list

def concat_mpas_output(stream,datetime_list,data_dir,vars_list):
    '''
    Concatenates in time, for variables vars_list,  
    MPAS output files named as follows:

    stream.YYYY.mm.dd_HH.MM.SS

    INPUT: stream (str) - name of the stream
           datetime_list (list) - list of datetime strings in format YYYY.mm.dd_HH.MM.SS
           data_dir (str) - directory where output files are stored
           vars_list (list) - list of variable names (strings)
    
    OUTPUT: cat_file (Xarray dataset) - concatenated dataset
    '''
    # Set variable list to keep in file
    vars_list.extend(['latCell','lonCell','latVertex','lonVertex','areaCell'])
    # Read first file
    filename1 = stream + '.' + datetime_list[0] + '.nc'
    cat_file = xr.open_dataset(f'{data_dir}/{filename1}', engine='netcdf4')[vars_list]
    # Read and concatenate that file to remaining files
    for datetime in datetime_list[1:]:
        filename = stream + '.' + datetime + '.nc'
        ds_temp = xr.open_dataset(f'{data_dir}/{filename}', engine='netcdf4')[vars_list]
        cat_file = xr.concat([cat_file, ds_temp], dim='Time')
    return cat_file

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

def find_nCells_from_latlon(ds,lon,lat,method='euclidean',verbose='y'):
    ds = add_mpas_mesh_variables(ds)
    if method == 'euclidean':
        index_closest  = closest_value_euclidean(ds=ds.sel(Time=0),lon=lon,lat=lat)
    elif method == 'haversine':
        index_closest  = closest_value_haversine(ds=ds.sel(Time=0),lon=lon,lat=lat)
    if verbose == 'y':
        # Print information on (lon,lat) point
        closest_lon = ds['longitude'].sel(Time=0,nCells=index_closest)
        closest_lat = ds['latitude'].sel(Time=0,nCells=index_closest)
        print ("input (lon,lat):", (lon,lat))
        print ("closest (lon,lat):", (float(closest_lon),float(closest_lat)))
        print ("corresponding nCells value:",index_closest.values)
    return index_closest, ds
