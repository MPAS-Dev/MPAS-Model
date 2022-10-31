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


# Workspace
work_dir = os.getenv('MPAS_DIR')
INMET_dir = work_dir+'/met_data/INMET/'

## Parser options ##
parser = argparse.ArgumentParser()
parser.add_argument('-m','--model', type=str,
                        help='''path to model data in nc format, regridded to \
latlon format (convert_mpas utility)''')

parser.add_argument('-s','--station', type=str,
                        help='''station name to compare model data to''')
parser.add_argument(-'v','--variable', type=str, default='temperature',
                        help='''variable to open. Options:\
                            temperature, wind_speed, wind_direction, \
                                precipitation, pressure''')

args = parser.parse_args(['--model','/home/daniloceano/Downloads/latlon.nc',
               '--station','Florianopolis', '--variable', 'pressure'])


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

## Get station data ##
station = (args.station).upper()
# Get data for corresponding year
station_file = glob.glob(INMET_dir+'/'+str(start_date.year)+'/*'+station+'*')[0]
# Open file with Pandas
station_data = pd.read_csv(station_file,header=8,sep=';',encoding='latin-1')

inmet_vars = ['temperature', 'wind_speed', 'wind_direction',
                         'precipitation', 'pressure']

if args.variable not in inmet_vars:
    print('Invalid option for variable! Choose one from: temperature, wind_speed\
, wind_direction, precipitation, pressure')
    sys.exit(-1)

var_data = model_data[args.variable]

