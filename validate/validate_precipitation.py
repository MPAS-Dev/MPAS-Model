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

import numpy as np
import pandas as pd
import xarray as xr
import cmocean.cm as cmo

import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

from matplotlib.lines import Line2D
from matplotlib import pyplot

import cartopy.crs as ccrs

import scipy.stats as stats

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

def get_exp_name(bench):
    expname = bench.split('/')[-1].split('run.')[-1]
    microp = expname.split('.')[0].split('_')[-1]
    cumulus = expname.split('.')[-1].split('_')[-1] 
    return microp+'_'+cumulus

def get_model_accprec(model_data):
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
    return acc_prec[-1]

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


# =============================================================================
# Plot acc prec maps and bias
# =============================================================================
plt.close('all')
fig1 = plt.figure(figsize=(10, 16))
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
        experiment = get_exp_name(bench)
        print('\n',experiment)
        
        model_data = xr.open_dataset(bench+'/latlon.nc')
        model_data = model_data.assign_coords({"Time":times})
    
        acc_prec = get_model_accprec(model_data)
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
                fig1.colorbar(cf1, ax=ax1, fraction=0.03, pad=0.1,
                              orientation='vertical')
            else:
                print('Plotting bias..')
                acc_prec_interp = acc_prec.interp(latitude=imerg_accprec.lat,
                                                  longitude=imerg_accprec.lon,
                                                  method='cubic') 
                bias = acc_prec_interp-imerg_accprec
                cf2 = ax.contourf(imerg_accprec.lon, imerg_accprec.lat,bias,
                                 cmap=cmo.balance_r,
                                 levels=np.linspace(-700,400,21))
                print('bias limits:',float(bias.min()), float(bias.max()))
            ax.coastlines(zorder = 1)
        
        i+=1
    
cb_axes = fig2.add_axes([0.85, 0.18, 0.04, 0.6])
fig2.colorbar(cf2, cax=cb_axes, orientation="vertical")    

fig1.subplots_adjust(wspace=0.4,hspace=-0.15)
fig2.subplots_adjust(wspace=0.1,hspace=-0.3, right=0.8)
    
if args.output is not None:
    fname = args.output
else:
    fname = (args.bench_directory).split('/')[-2].split('.nc')[0]
fname1 = fname+'_acc_prec'
fname2 = fname+'_acc_prec_bias'
fig1.savefig(fname1+'.png', dpi=500)
fig2.savefig(fname2+'.png', dpi=500)
print(fname1,'and',fname1,'saved')

# =============================================================================
# Plot IMERG ac prec
# =============================================================================
print('\nPlotting CHIRPS data..')
plt.close('all')
fig = plt.figure(figsize=(10, 10))
datacrs = ccrs.PlateCarree()
ax = fig.add_subplot(111, projection=datacrs,frameon=True)
ax.set_extent([-55, -30, -20, -35], crs=datacrs) 
gl = ax.gridlines(draw_labels=True,zorder=2,linestyle='dashed',
                  alpha=0.8, color='#383838')
gl.xlabel_style = {'size': 12, 'color': '#383838'}
gl.ylabel_style = {'size': 12, 'color': '#383838'}
gl.right_labels = None
gl.top_labels = None
cf = ax.contourf(imerg_accprec.lon, imerg_accprec.lat,
                 imerg_accprec.T, cmap=cmo.rain, vmin=0,
                 levels=np.linspace(0,imerg_accprec.T.max(),21))
fig.colorbar(cf, ax=ax, fraction=0.03, pad=0.1)
ax.coastlines(zorder = 1)

imergname = args.imerg.split('/')[-1].split('.nc')[0]
fig.savefig(imergname+'.png', dpi=500)
print(imergname,'saved')

# =============================================================================
# PDFs 
# =============================================================================

colors = {'ERA':'k', 'fritsch':'tab:orange','tiedtke':'tab:red',
          'ntiedtke':'tab:purple', 'freitas':'tab:brown','off':'tab:green'}

lines = {'ERA':'solid', 'wsm6':'dashed','thompson':'dashdot',
         'kessler':(0, (3, 1, 1, 1)),'off':(0, (3, 1, 1, 1, 1, 1))}

markers = {'ERA':'o', 'wsm6':'x', 'thompson':'P','kessler':'D','off':'s'}

print('\nPlotting PDFs..')
plt.close('all')
fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(111,frameon=True)

for bench in benchs:
    experiment = get_exp_name(bench)
    print('\n',experiment)
    
    model_data = xr.open_dataset(bench+'/latlon.nc')
    model_data = model_data.assign_coords({"Time":times})

    acc_prec = get_model_accprec(model_data)
    acc_prec_interp = acc_prec.interp(latitude=imerg_accprec.lat,
                                      longitude=imerg_accprec.lon,
                                      method='cubic').values.flatten()
    
    if experiment != 'off_off':
    
        params = stats.gamma.fit(acc_prec_interp)
        x = np.linspace(stats.gamma.ppf(0.01, *params), stats.gamma.ppf(0.99, *params), 25)
        pdf = stats.gamma.pdf(x, *params)
        
        ls = lines[experiment.split('_')[0]]
        marker = markers[experiment.split('_')[0]]
        color = colors[experiment.split('_')[1]]
        ax.plot(x, pdf, lw=2, alpha=0.6, linestyle=ls, c=color,
                label=experiment)

labels, handles = zip(
    *[(k, mpatches.Rectangle((0, 0), 1, 1, facecolor=v)
       ) for k,v in colors.items()])
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
ax.add_artist(legend1)
ax.add_artist(legend2)

fig.savefig(fname+'_PDF.png', dpi=500)    
print(fname+'_PDF','saved')
