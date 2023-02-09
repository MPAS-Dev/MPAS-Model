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

import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
from matplotlib import rcParams

import cartopy.crs as ccrs
import cartopy

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

def plot_track(lons, lats, min_zeta, fname):
    ax.set_extent([-80, 40, 0, -70], crs=datacrs) 
    ax.coastlines(zorder = 1)
    ax.add_feature(cartopy.feature.LAND)
    ax.add_feature(cartopy.feature.OCEAN,facecolor=("lightblue"))
    gl = ax.gridlines(draw_labels=True,zorder=2,linestyle='dashed',alpha=0.8,
                 color='#383838')
    gl.xlabel_style = {'size': 14, 'color': '#383838'}
    gl.ylabel_style = {'size': 14, 'color': '#383838'}
    gl.bottom_labels = None
    gl.right_labels = None
    ax.plot(lons,lats,'-',c='k')
    scatter = ax.scatter(lons,lats,zorder=100,cmap=cmo.deep_r,c=min_zeta)
    plt.colorbar(scatter, pad=0.07, orientation='vertical',fraction=0.026,
                      label=' 850 hPa vorticity (ζ)')

def plot_prec(acc_prec):
    ax.set_extent([-80, 40, 0, -70], crs=datacrs) 
    ax.coastlines(zorder = 1)
    gl = ax.gridlines(draw_labels=True,zorder=2,linestyle='dashed',alpha=0.8,
                 color='#383838')
    gl.xlabel_style = {'size': 14, 'color': '#383838'}
    gl.ylabel_style = {'size': 14, 'color': '#383838'}
    gl.bottom_labels = None
    gl.right_labels = None
    lon, lat = acc_prec.longitude, acc_prec.latitude
    cf = ax.contourf(lon, lat, acc_prec, cmap=cmo.rain, vmin=0)
    plt.colorbar(cf, pad=0.07, orientation='vertical',fraction=0.026)

# ## Workspace ##
# work_dir = os.getenv('MPAS_DIR')
# if work_dir is None:
#     print('Error: MPAS_DIR environment variable not defined! It should direct\
# to the MPAS-BR path')
#     sys.exit(-1)
# INMET_dir = work_dir+'/met_data/INMET/'

# ## Parser options ##
# parser = argparse.ArgumentParser()

# parser.add_argument('-bdir','--bench_directory', type=str, required=True,
#                         help='''path to benchmark directory''')

# parser.add_argument('-o','--output', type=str, default=None,
#                         help='''output name to append file''')
# parser.add_argument('-e','--ERA5', type=str, default=None,
#                         help='''wether to validade with ERA5 data''')
# args = parser.parse_args()

## Start the code ##
#benchs = glob.glob(args.bench_directory+'/run*')
benchs = glob.glob('/home/daniloceano/Documents/MPAS/MPAS-BR/benchmarks/Catarina_physics-test/Catarina_250-8km.microp_scheme.convection_scheme/run*')
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
gs = gridspec.GridSpec(6, 3)
datacrs = ccrs.PlateCarree()
# for bench in range(18):
for col in range(3):
    for row in range(6):
        
        model_data = xr.open_dataset(model_output)
        model_data = model_data.assign_coords({"Time":times})
        ax = fig.add_subplot(gs[row, col], projection=datacrs,frameon=True)
        
        if ('rainnc' in model_data.variables
            ) and ('rainc' in model_data.variables):
            prec = model_data['rainnc']+model_data['rainc']
        # Get only micrphysics precipitation
        elif ('rainnc' in model_data.variables
            ) and ('rainc' not in model_data.variables):
            prec = model_data['rainnc']
        # Get convective precipitation
        elif ('rainnc' not in model_data.variables
            ) and ('rainc' in model_data.variables):
            prec = model_data['rainc'] 
        
        acc_prec = prec.cumsum()[-1]
        plot_prec(acc_prec)
    
plt.tight_layout()
plt.savefig('test.png', dpi=500)