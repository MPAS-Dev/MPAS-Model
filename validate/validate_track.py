#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  8 09:52:10 2023

@author: daniloceano
"""

import sys
import os
import glob
import argparse
import f90nml
import datetime

import pandas as pd
import numpy as np
import numpy as np
import xarray as xr
import cmocean.cm as cmo

import metpy.calc as mpcalc
from metpy.units import units
from metpy.calc import vorticity

from wrf import interplevel
from scipy.signal import savgol_filter    

import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
from matplotlib import rcParams

import cartopy.crs as ccrs
import cartopy

colors = {'ERA':'tab:blue', 'fritsch':'tab:orange','tiedtke':'tab:red',
          'ntiedtke':'tab:purple', 'freitas':'tab:brown','off':'tab:green'}

lines = {'ERA':'solid', 'wsm6':'dashed','thompson':'dashdot',
         'kessler':(0, (3, 1, 1, 1)),'off':(0, (3, 1, 1, 1, 1, 1))}

markers = {'ERA':'o', 'wsm6':'x', 'thompson':'P','kessler':'D','off':'s'}


def get_times_nml(namelist,model_data):
    ## Identify time range of simulation using namelist ##
    # Get simulation start and end dates as strings
    start_date_str = namelist['nhyd_model']['config_start_time']
    run_duration_str = namelist['nhyd_model']['config_run_duration']
    # Convert strings to datetime object
    start_date = datetime.datetime.strptime(start_date_str, '%Y-%m-%d_%H:%M:%S')
    
    run_duration = datetime.datetime.strptime(run_duration_str,'%d_%H:%M:%S')
    # Get simulation finish date as object and string
    finish_date  = start_date + datetime.timedelta(days=run_duration.day,
                                                   hours=run_duration.hour)
    finish_date_str = finish_date.strftime('%Y-%m-%d_%H:%M:%S')
    ## Create a range of dates ##
    times = pd.date_range(start_date,finish_date,periods=len(model_data.Time)+1)[1:]
    return times

def get_track(track_variable, TimeIndexer):
        
    min_var, times = [], []
    lats, lons = [], []
    for t in track_variable[TimeIndexer]:
        datestr = pd.to_datetime(t.values)
        times.append(str(datestr))
        ivar = track_variable.sel({TimeIndexer:t})
        
        varmin = ivar.min()
        min_var.append(float(varmin))
        
        loc = ivar.argmin(dim=['latitude','longitude'])
        lats.append(float(ivar['latitude'][loc['latitude']]))
        lons.append(float(ivar['longitude'][loc['longitude']]))
    
    track = pd.DataFrame([lons, lats, min_var]).transpose()
    track.columns = ['lon','lat','min_zeta']
    track.index = times
    
    return track

## Workspace ##
work_dir = os.getenv('MPAS_DIR')
if work_dir is None:
    print('Error: MPAS_DIR environment variable not defined! It should direct\
to the MPAS-BR path')
    sys.exit(-1)
INMET_dir = work_dir+'/met_data/INMET/'

## Parser options ##
parser = argparse.ArgumentParser()

parser.add_argument('-bdir','--bench_directory', type=str, required=True,
                        help='''path to benchmark directory''')

parser.add_argument('-o','--output', type=str, default=None,
                        help='''output name to append file''')
parser.add_argument('-e','--ERA5', type=str, default=None,
                        help='''wether to validade with ERA5 data''')
args = parser.parse_args()

## Start the code ##
benchs = glob.glob(args.bench_directory+'/run*')
# benchs = glob.glob('/home/daniloceano/Documents/MPAS/MPAS-BR/benchmarks/Catarina_physics-test/Catarina_250-8km.microp_scheme.convection_scheme/run*')
# Dummy for getting model times
model_output = benchs[0]+'/latlon.nc'
namelist_path = benchs[0]+"/namelist.atmosphere"
# open data and namelist
model_data = xr.open_dataset(model_output)
namelist = f90nml.read(glob.glob(namelist_path)[0])
times = get_times_nml(namelist,model_data)
# start_date = times[0]

plt.close('all')
fig = plt.figure(figsize=(15, 12))
datacrs = ccrs.PlateCarree()
ax = fig.add_subplot(1, 1, 1, projection=datacrs)
ax.set_extent([-55, -30, -20, -35], crs=datacrs) 
ax.coastlines(zorder = 1)
ax.add_feature(cartopy.feature.LAND)
ax.add_feature(cartopy.feature.OCEAN,facecolor=("lightblue"))
gl = ax.gridlines(draw_labels=True,zorder=2,linestyle='dashed',alpha=0.8,
             color='#383838')
gl.xlabel_style = {'size': 14, 'color': '#383838'}
gl.ylabel_style = {'size': 14, 'color': '#383838'}
gl.bottom_labels = None
gl.right_labels = None

if args.ERA5:
    benchs.append(args.ERA5)

for bench in benchs:   
    
    if bench != benchs[-1]:
        
        expname = bench.split('/')[-1].split('run.')[-1]
        microp = expname.split('.')[0].split('_')[-1]
        cumulus = expname.split('.')[-1].split('_')[-1] 
        experiment = microp+'_'+cumulus
        print(experiment)
        
        model_data = xr.open_dataset(bench+'/latlon.nc')
        TimeIndexer = 'Time'
        LatIndexer, LonIndexer = 'latitude', 'longitude'
        
        model_data = model_data.assign_coords({"Time":times}).sel(
            latitude=slice(-20,-35),longitude=slice(-55,-30))
        
        pressure = (model_data['pressure'] * units(model_data['pressure'].units)
                    ).metpy.convert_units('hPa')
        z = model_data.zgrid.expand_dims({'Time':times})
        zlevs = np.arange(0,3100,100) * units.m
        pres_height = interplevel(pressure, z[:,:-1], zlevs)
        slp = pres_height.isel(level=1)
            
    else:
        expname,microp,cumulus = 'ERA','ERA','ERA'
        print('ERA5')
        model_data = xr.open_dataset(bench, engine='cfgrib')
        TimeIndexer = 'time'
        model_data = model_data.sel(time=slice(times[0],times[1]),
            latitude=slice(-20,-35),longitude=slice(-55,-30))
        slp = model_data.msl
    
    track = get_track(slp, TimeIndexer)
    
    lons, lats, min_slp = track['lon'], track['lat'], track['min_zeta']
    
    ax.plot(lons,lats,'-',c='k')
    
    ls = lines[microp]
    marker = markers[microp]
    color = colors[cumulus]
    
    pl = ax.plot(lons,lats,zorder=100,markeredgecolor=color,marker=marker,
                markerfacecolor='None',linewidth=2, linestyle=ls,
                c=color)
    ax.scatter(lons.iloc[0],lats.iloc[0], s=100,
                edgecolor='gray',facecolor='gray')
    ax.scatter(lons.iloc[-1],lats.iloc[-1], s=100,
                edgecolor='k',facecolor='gray')
    
    plt.legend(pl)

    
plt.savefig('test_track.png', dpi=500)
