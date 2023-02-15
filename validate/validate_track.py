#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  8 09:52:10 2023

@author: daniloceano
"""

import glob
import argparse
import f90nml
import datetime

import pandas as pd
import numpy as np
import xarray as xr

from metpy.units import units

from wrf import interplevel

import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

from matplotlib.lines import Line2D
from matplotlib import pyplot

import cartopy.crs as ccrs
import cartopy

import dask
import dask.distributed

colors = {'ERA':'k', 'fritsch':'tab:orange','tiedtke':'tab:red',
          'ntiedtke':'tab:purple', 'freitas':'tab:brown','off':'tab:green'}

lines = {'ERA':'solid', 'wsm6':'dashed','thompson':'dashdot',
         'kessler':(0, (3, 1, 1, 1)),'off':(0, (3, 1, 1, 1, 1, 1))}

markers = {'ERA':'o', 'wsm6':'x', 'thompson':'P','kessler':'D','off':'s'}

colors = {'ERA':'k', 'convection':'tab:orange','mesoscale':'tab:red',
          'ntiedtke':'tab:purple', 'freitas':'tab:brown','off':'tab:green'}

lines = {'ERA':'solid', 'convection':'dashed','mesoscale':'dashdot',
         'kessler':(0, (3, 1, 1, 1)),'off':(0, (3, 1, 1, 1, 1, 1))}

markers = {'ERA':'o', 'convection':'x', 'mesoscale':'P','kessler':'D','off':'s'}

def get_exp_name(bench):
    expname = bench.split('/')[-1].split('run.')[-1]
    microp = expname.split('.')[0].split('_')[-1]
    cumulus = expname.split('.')[-1].split('_')[-1] 
    return microp+'_'+cumulus

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
    ## Create a range of dates ##
    times = pd.date_range(start_date,finish_date,periods=len(model_data.Time)+1)[1:]
    return times

@dask.delayed
def open_dataset_with_dask(bench, times):
    return xr.open_dataset(bench+'/latlon.nc').sortby(
            'latitude', ascending=False).sel(
                latitude=slice(-20,-35),longitude=slice(-55,-30)
                ).assign_coords({"Time":times})

def pressure_to_slp(pressure,z, zlevs):
    pres_height = interplevel(pressure, z[:,:-1], zlevs)
    slp = pres_height.isel(level=1)
    return slp


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
    track.columns = ['lon','lat','min']
    track.index = times
    return track

def initialize_map(ax, row, col, datacrs):
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
    if row != 0:
        gl.top_labels = None
    if col != 0:
        gl.left_labels = None

def make_legend(colors,markers,lines):
    labels, handles = zip(*[(k, mpatches.Rectangle((0, 0), 1, 1, facecolor=v)) for k,v in colors.items()])
    legend1 = pyplot.legend(handles, labels, loc=4,
                            framealpha=1, bbox_to_anchor=(1.105, 0.27))
    custom_lines = []
    lebels = []
    for line, marker in zip(lines,markers):
        custom_lines.append(Line2D([0], [0], color='k', lw=1,
                                linestyle=lines[line], marker=markers[marker]))
        lebels.append(line)
    legend2 = pyplot.legend(custom_lines, lebels, loc=4, framealpha=1,
                            bbox_to_anchor=(1.11, 0.1))
    return legend1, legend2    

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

# Dummy for getting model times
model_output = benchs[0]+'/latlon.nc'
namelist_path = benchs[0]+"/namelist.atmosphere"

# open data and namelist
model_data = xr.open_dataset(model_output).sortby(
        'latitude', ascending=False).sel(
            latitude=slice(-20,-35),longitude=slice(-55,-30))
namelist = f90nml.read(glob.glob(namelist_path)[0])
times = get_times_nml(namelist,model_data)
first_day = datetime.datetime.strftime(times[0], '%Y-%m-%d %HZ')
last_day = datetime.datetime.strftime(times[-1], '%Y-%m-%d %HZ')                      
print('Analysis is from',first_day,'to',last_day)  

# For interpolating pressure from height to isobaric
z = model_data.zgrid.expand_dims({'Time':times})
zmax = float(z.max())
dz = 100
zlevs = np.arange(0, zmax, dz) * units.m

print('\nOpening all data and putting it into a dictionary...')
data = {}
data['ERA'] = {}
data['ERA']['data'] = xr.open_dataset(args.ERA5, engine='cfgrib',
                filter_by_keys={'typeOfLevel': 'surface'}
                ).sel(time=slice(times[0],times[-1]),
                latitude=slice(-20,-35),longitude=slice(-55,-30)).msl
track = get_track(data['ERA']['data'], 'time')                                          
                      
for bench in benchs:
    
    experiment = get_exp_name(bench)
    print('\n',experiment)
    
    print('computing slp...')
    model_data = open_dataset_with_dask(bench,times).compute()
    pressure = (model_data['pressure'] * units(model_data['pressure'].units)
               ).metpy.convert_units('hPa')
    slp = pressure_to_slp(pressure,z, zlevs)
    print('tracking the system...')
    track = get_track(slp, 'Time')
    
    data[experiment] = {}
    data[experiment]['slp'] = slp
    data[experiment]['track'] = track

# =============================================================================
# Plot all tracks in one image
# =============================================================================

plt.close('all')
fig = plt.figure(figsize=(15, 12))
datacrs = ccrs.PlateCarree()
ax = fig.add_subplot(1, 1, 1, projection=datacrs)
initialize_map(ax, 0, 0, datacrs)

for exp in data:   
    
    slp = data[exp]['slp']
    track = data[exp]['track']
    lons, lats, min_slp = track['lon'], track['lat'], track['min']
    
    microp, cumulus = exp.split('_')[0], exp.split('_')[1]
    ls = lines[microp]
    marker = markers[microp]
    color = colors[cumulus]
    
    ax.plot(lons,lats,zorder=100,markeredgecolor=color,marker=marker,
                markerfacecolor='None',linewidth=1.5, linestyle=ls,
                c=color, label=exp)
    ax.scatter(lons.iloc[0],lats.iloc[0], s=150, marker=marker, color='gray')
    ax.scatter(lons.iloc[-1],lats.iloc[-1], s=150, marker=marker,
                facecolor=color, zorder=100)
    
legend1, legend2 = make_legend(colors,markers,lines)
ax.add_artist(legend1)
ax.add_artist(legend2)

if args.output is not None:
    fname = args.output
else:
    fname = (args.bench_directory).split('/')[-2].split('.nc')[0]
fname += '_track'
fig.savefig(fname+'.png', dpi=500)
print(fname+'.png created!')

# =============================================================================
# Plot multiple subplots with tracks
# =============================================================================

benchs = glob.glob(args.bench_directory+'/run*')
plt.close('all')
fig = plt.figure(figsize=(10, 13))
gs = gridspec.GridSpec(6, 3)
i = 0
for row in range(6):
    for col in range(3):

        bench = benchs[i]
        ax = fig.add_subplot(gs[row, col], projection=datacrs,frameon=True)
        initialize_map(ax, row, col, datacrs)
        
        expname = bench.split('/')[-1].split('run.')[-1]
        microp = expname.split('.')[0].split('_')[-1]
        cumulus = expname.split('.')[-1].split('_')[-1] 
        experiment = microp+'_'+cumulus
        print(experiment)
        
        ax.text(-50,-22,experiment,bbox=dict(facecolor='w', alpha=0.5))
        
        model_data = data[experiment]['data']
        track = data[experiment]['track']
        
        lons, lats, min_slp = track['lon'], track['lat'], track['min']
        
        ls = lines[microp]
        marker = markers[microp]
        color = colors[cumulus]
            
        ax.plot(lons,lats,zorder=100,markeredgecolor=color,marker=marker,
                    markerfacecolor='None',linewidth=0.5, linestyle=ls,
                    c=color, label=expname)
        
        if args.ERA5:
            track_era = data['ERA5']['track']
            lons_era, lats_era = track_era.lons, track_era.lats
            ax.plot(lons_era,lats_era,zorder=1,markeredgecolor='k',
                    marker='o',markerfacecolor='None',
                    linewidth=0.75, linestyle='solid',
                        c='gray', label=expname)
        i+=1
        
fname2 = fname+'_multipanel'
plt.savefig(fname2+'.png', dpi=500)
print(fname2+'.png created!')

# =============================================================================
# Plot minimum slp
# =============================================================================
fig = plt.figure(figsize=(10, 13))
ax = fig.add_subplot(1, 1, 1)

for exp in data:   
    
    slp = data[exp]['slp']
    track = data[exp]['track']
    time, min_slp = track.index, track['min']
    
    microp, cumulus = exp.split('_')[0], exp.split('_')[1]
    ls = lines[microp]
    marker = markers[microp]
    color = colors[cumulus]
    
    ax.plot(time,min_slp,markeredgecolor=color,marker=marker,
                markerfacecolor='None',linewidth=1.5, linestyle=ls,
                c=color, label=exp)
    
legend1, legend2 = make_legend(colors,markers,lines)
ax.add_artist(legend1)
ax.add_artist(legend2)

fname3 = fname+'_min-slp'
plt.savefig(fname3+'.png', dpi=500)
print(fname3+'.png created!')