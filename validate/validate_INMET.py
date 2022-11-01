#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 31 17:30:12 2022

Created by:
    Danilo Couto de Souza
    Universidade de São Paulo (USP)
    Instituto de Astornomia, Ciências Atmosféricas e Geociências
    São Paulo - Brazil
    
Contact:
    danilo.oceano@gmail.com

    Scripts for validaitng MPAS-A output with ground station data from the
INMET (Brazilian Meteorological Service).

@author: daniloceano
"""

import os
import sys
import argparse
import xarray as xr
import glob
import f90nml
import datetime
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

## Workspace ##
work_dir = os.getenv('MPAS_DIR')
# work_dir = '~/Documents/MPAS/MPAS-BR/'
INMET_dir = work_dir+'/met_data/INMET/'

## Parser options ##
parser = argparse.ArgumentParser()
parser.add_argument('-m','--model', type=str,
                        help='''path to model data in nc format, regridded to \
latlon format (convert_mpas utility)''')

parser.add_argument('-s','--station', type=str,
                        help='''station name to compare model data to''')
parser.add_argument('-v','--variable', type=str, default='temperature',
                        help='''variable to open. Options:\
                            temperature, wind_speed, wind_direction, \
                                precipitation, pressure''')

## Dummy arguments for debugging ##
args = parser.parse_args(['--model','/home/daniloceano/Downloads/latlon.nc',
               '--station','Florianopolis', '--variable', 'temperature'])


def plot_timeseries(model_data, inmet_data, **kwargs):
    # Guarantee no plots are open
    plt.close('all')
    plt.figure(figsize=(8,8))
    ax = plt.gca()
    plt.plot(times,model_data,label='MPAS-A',c='#BF3D3B',linewidth=3,
             marker='s')
    plt.plot(times,inmet_data,label='INMET',c='#3B95BF',linewidth=3,
             marker='o')
    plt.grid(c='gray',linewidth=0.25,linestyle='dashdot')
    plt.tick_params(axis='x', labelrotation=20)
    ax.xaxis.set_tick_params(labelsize=16)
    ax.yaxis.set_tick_params(labelsize=16)
    plt.legend(prop={'size': 18})
    plt.xlim(times[0],times[-1])
    # Set x labels as dates
    ax = plt.gca()
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%m-%d'))
    figname = 'timeseries_'+station.replace(' ','-')+\
        '_'+args.variable+'.png'
    plt.savefig(figname,dpi=700)
    print(figname+' created!')

## Open model data with xarray ##
if not os.path.isfile(args.model):
    print("MPAS-A file was not found")
    sys.exit(-1)
else:
    print('Opening MPAS-A file: '+args.model)
    model_data = xr.open_dataset(args.model)
    
## Open namelist.atmosphere file ##
namelist_path = "/".join((args.model).split('/')[:-1])+"/namelist.atmosphere"
if not os.path.isfile(namelist_path):
    print("namelist.atmosphere file was not found")
    sys.exit(-1)
else:
    print('namelist.atmosphere file found')
    namelist = f90nml.read(glob.glob(namelist_path)[0])
    

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
times = pd.date_range(start_date,finish_date,periods=len(model_data.Time))
# Get time interval
dt = times[1] - times[0]

## Get station data ##
station = (args.station).upper()
# Get data for corresponding year
station_file = glob.glob(INMET_dir+'/'+str(start_date.year)+'/*'+station+'*')[0]
# Open file with Pandas
station_data = pd.read_csv(station_file,header=8,sep=';',encoding='latin-1',
                           parse_dates=[[0,1]],decimal=',')
station_data.index = station_data['DATA (YYYY-MM-DD)_HORA (UTC)']
# Get station lati and lon
with open(station_file, 'r',encoding='latin-1') as file:
    for line in file:
        if 'LATITUDE' in line:
            lat_station = float((line[10:-1].replace(',','.')))
        elif 'LONGITUDE' in line:
            lon_station = float((line[11:-1].replace(',','.')))
            pass
    

default_vars = ['temperature', 'wind_speed', 'wind_direction',
                         'precipitation', 'pressure']

if args.variable not in default_vars:
    print('Invalid option for variable! Choose one from: temperature, wind_speed\
, wind_direction, precipitation, pressure')
    sys.exit(-1)
else:
    print('variable choosen for validation: '+args.variable)    

if args.variable == 'temperature':
    model_var_data = model_data['t2m']-273.15
    model_var_data_station = model_var_data.sel(latitude=lat_station,
                method='nearest').sel(longitude=lon_station,method='nearest'
                                      ).values
    inmet_data = station_data[
        (station_data['DATA (YYYY-MM-DD)_HORA (UTC)'] >= times[0]) &
        (station_data['DATA (YYYY-MM-DD)_HORA (UTC)'] <= times[-1])
        ].resample(dt).mean()['TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)'
                             ]                         
                                      
elif args.variable == 'precipitation': 

    model_var_data = model_data['rainc']
    model_var_data_station = model_var_data.sel(latitude=lat_station,
                method='nearest').sel(longitude=lon_station,method='nearest'
                                      ).values                            
    
    inmet_data = station_data[
        (station_data['DATA (YYYY-MM-DD)_HORA (UTC)'] >= times[0]) &
        (station_data['DATA (YYYY-MM-DD)_HORA (UTC)'] <= times[-1])
        ].resample(dt).sum()['PRECIPITAÇÃO TOTAL, HORÁRIO (mm)'].cumsum()

else:
    print('variable error')
    
    
plot_timeseries(model_var_data_station, inmet_data)