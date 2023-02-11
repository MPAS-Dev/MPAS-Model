#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  8 09:52:10 2023

@author: daniloceano
"""

import os
import sys
import glob
import argparse
import f90nml
import datetime

import numpy as np
import pandas as pd
import xarray as xr
import cmocean.cm as cmo

import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt

import cartopy.crs as ccrs

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

## Parser options ##
parser = argparse.ArgumentParser()

parser.add_argument('-bdir','--bench_directory', type=str, required=True,
                        help='''path to benchmark directory''')
parser.add_argument('-i','--imerg', type=str, default=None, required=True,
                        help='''path to IMERG data''')
parser.add_argument('-o','--output', type=str, default=None,
                        help='''output name to append file''')

args = parser.parse_args()

## Start the code ##
benchs = glob.glob(args.bench_directory+'/run*')
# Dummy for getting model times
model_output = benchs[0]+'/latlon.nc'
namelist_path = benchs[0]+"/namelist.atmosphere"
# open data and namelist
model_data = xr.open_dataset(model_output)
namelist = f90nml.read(glob.glob(namelist_path)[0])
times = get_times_nml(namelist,model_data)

plt.close('all')
fig1 = plt.figure(figsize=(8, 16))
fig2 = plt.figure(figsize=(8, 16))
gs1 = gridspec.GridSpec(6, 3)
gs2 = gridspec.GridSpec(6, 3)
datacrs = ccrs.PlateCarree()

imerg = xr.open_dataset(args.imerg).sel(lat=slice(model_data.latitude[-1],
                 model_data.latitude[0]),lon=slice(model_data.longitude[0],
                                                  model_data.longitude[-1]))
imerg_accprec = imerg.precipitationCal.cumsum(dim='time')[-1]
print(imerg_accprec)
i = 0
for col in range(3):
    for row in range(6):
        
        bench = benchs[i]
        expname = bench.split('/')[-1].split('run.')[-1]
        microp = expname.split('.')[0].split('_')[-1]
        cumulus = expname.split('.')[-1].split('_')[-1] 
        experiment = microp+'_'+cumulus
        print('\n',experiment)
        
        model_data = xr.open_dataset(bench+'/latlon.nc')
        model_data = model_data.assign_coords({"Time":times})
    
        if ('rainnc' in model_data.variables
            ) and ('rainc' in model_data.variables):
            acc_prec = model_data['rainnc']+model_data['rainc']
        # Get only micrphysics precipitation
        elif ('rainnc' in model_data.variables
            ) and ('rainc' not in model_data.variables):
            acc_prec = model_data['rainnc']
        # Get convective precipitation
        elif ('rainnc' not in model_data.variables
            ) and ('rainc' in model_data.variables):
            acc_prec = model_data['rainc'] 
        elif ('rainnc' not in model_data.variables
            ) and ('rainc' not in model_data.variables):
            acc_prec = model_data.uReconstructMeridional[0]*0
  
        acc_prec = acc_prec[-1]
        lon, lat = acc_prec.longitude, acc_prec.latitude
        
        ax1 = fig1.add_subplot(gs1[row, col], projection=datacrs,frameon=True)
        ax2 = fig2.add_subplot(gs1[row, col], projection=datacrs,frameon=True)
        
        for ax in [ax1,ax2]:
            ax.set_extent([-55, -30, -20, -35], crs=datacrs) 
            gl = ax.gridlines(draw_labels=True,zorder=2,linestyle='dashed',
                              alpha=0.8, color='#383838')
            gl.xlabel_style = {'size': 12, 'color': '#383838'}
            gl.ylabel_style = {'size': 12, 'color': '#383838'}
            gl.right_labels = None
            gl.top_labels = None
            if row != 5:
                gl.bottom_labels = None
            if col != 0:
                gl.left_labels = None
        
            ax.text(-50,-19,experiment)
            
            if ax == ax1:
                print('Plotting accumulate prec..')
                cf1 = ax.contourf(lon, lat, acc_prec, cmap=cmo.rain, vmin=0)
            else:
                print('Plotting bias..')
                acc_prec_interp = acc_prec.interp(latitude=imerg_accprec.lat,
                                                  longitude=imerg_accprec.lon,
                                                  method='cubic') 
                cf2 = ax.contourf(imerg_accprec.lon, imerg_accprec.lat,
                                 acc_prec_interp-imerg_accprec,
                                 cmap=cmo.balance_r, vmin=0)
            ax.coastlines(zorder = 1)
        
        i+=1
    
for fig, ax, cf in zip([fig1, fig2], [ax1, ax2], [cf1, cf2]):
    cb_axes = fig.add_axes([0.9, 0.18, 0.04, 0.6])
    fig.colorbar(cf, cax=cb_axes, orientation="vertical")
    fig.subplots_adjust(wspace=0.1,hspace=0)
    
if args.output is not None:
    fname = args.output
else:
    fname = (args.bench_directory).split('/')[-2].split('.nc')[0]
fname1 = fname+'_acc_prec'
fname2 = fname+'_acc_prec_bias'
fig1.savefig(fname1+'.png', dpi=500)
fig2.savefig(fname2+'.png', dpi=500)
