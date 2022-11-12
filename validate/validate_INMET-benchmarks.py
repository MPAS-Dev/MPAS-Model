#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  3 16:48:43 2022

@author: daniloceano
"""

import sys
import argparse
import xarray as xr
import glob
import f90nml
import datetime
import pandas as pd
import metpy.calc as mpcalc
import seaborn as sns
from metpy.units import units
import matplotlib.pyplot as plt



sys.tracebacklimit = 0

def df_model_data(model_data,times,variable,experiment):
    # Dictionaries containing naming convections for each variable
    model_variables = {'temperature':'t2m', 'pressure':'surface_pressure'}
    
    # Get model data for the gridpoint closest to station   
    model_station = model_data.sel(latitude=lat_station,
                method='nearest').sel(longitude=lon_station,method='nearest')
    
    # If windpeed, calculate from components
    if variable == 'windspeed':
        model_var = mpcalc.wind_speed(model_station['u10'],model_station['v10'])
    # Sum grid-scale and convective precipitation
    elif variable == 'precipitation':
        model_var = model_station['rainnc']+model_station['rainc']
    # Convert pressure to MSLP
    elif variable == 'pressure':
        model_var = mpcalc.altimeter_to_sea_level_pressure(
            (model_station['surface_pressure'])*units('hPa'),
            model_station['zgrid'][0]*units('m'),
            model_station['t2m']*units('K'))
    else:
        model_var = model_station[model_variables[variable]]
        

    mpas_df = pd.DataFrame(model_var,columns=['value'],
                             index=times)
    
    # Convert temperature to celcius
    if variable == 'temperature':
        mpas_df = mpas_df-273.15
    # Convert pressure to hPa
    elif variable == 'pressure':
        mpas_df = mpas_df/100
        
    mpas_df['source'], mpas_df['variable'] = experiment, variable 
    # Add date as column and revert indexes to a range of numbers
    mpas_df['date'] = mpas_df.index
    mpas_df.index = range(len(mpas_df))
    
    return mpas_df

def df_inmet_data(inmet_data,times,variable,experiment): 
    inmet_variables = {'temperature':
                       'TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)',
                       'precipitation':'PRECIPITAÇÃO TOTAL, HORÁRIO (mm)',
                       'pressure':
                       'PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)',
                       'windspeed':'VENTO, VELOCIDADE HORARIA (m/s)'}
    # Get time interval
    dt = times[1] - times[0]
    # Get INMET data for the same dates as the model experiment
    inmet_var = station_data[
        (station_data['DATA (YYYY-MM-DD)_HORA (UTC)'] >= times[0]) &
        (station_data['DATA (YYYY-MM-DD)_HORA (UTC)'] <= times[-1])
        ].resample(dt)
    
    # If using precipitation, get accumulated values between model time steps
    if variable == 'precipitation':
        inmet_var = inmet_var.sum()[inmet_variables[variable]].cumsum()
    # Else, get mean values between model time steps
    else:
        inmet_var = inmet_var.mean()[inmet_variables[variable]]
        
    inmet_df = pd.DataFrame(inmet_var.rename('value'))
    inmet_df['source'],inmet_df['variable'] = 'INMET', variable
    # Add date as column and revert indexes to a range of numbers
    inmet_df['date'] = inmet_df.index
    inmet_df.index = range(len(inmet_df))
    
    return inmet_df

def get_times_nml(namelist):
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
    return times

## Workspace ##
path = '/Users/danilocoutodsouza/Documents/USP/MPAS/MPAS-BR/benchmarks/Catarina-physics/physics_suite/'
INMET_dir = '/Users/danilocoutodsouza/Documents/USP/MPAS/MPAS-BR/met_data/INMET/'
benchs = glob.glob(path+'/run*')
station = 'Florianopolis'.upper()
variable = 'temperature'

met_list = []
for variable in ['temperature','precipitation','windspeed','pressure']:
    j = 0
    for bench in benchs:
        model_output = bench+'/latlon.nc'
        namelist_path = bench+"/namelist.atmosphere"
        experiment = bench.split('/')[-1].split('run.')[-1]    
    
        model_data = xr.open_dataset(model_output)
        namelist = f90nml.read(glob.glob(namelist_path)[0])
        times = get_times_nml(namelist)
        start_date = times[0]
        
        # Only open INMET file once
        if j == 0:
            # Get data for corresponding year
            try:
                station_file = glob.glob(INMET_dir+'/'+str(start_date.year)+'/*'+station+'*')[0]
            except:
                raise SystemExit('Error: not found data for '+station.upper()+' station \
            for year '+str(start_date.year))
            # Open file with Pandas
            station_data = pd.read_csv(station_file,header=8,sep=';',encoding='latin-1',
                                       parse_dates=[[0,1]],decimal=',')
            station_data.index = station_data['DATA (YYYY-MM-DD)_HORA (UTC)']
            df_inmet = df_inmet_data(station_data,times,variable,experiment)
            # Get station lat and lon
            with open(station_file, 'r',encoding='latin-1') as file:
                for line in file:
                    if 'LATITUDE' in line:
                        lat_station = float((line[10:-1].replace(',','.')))
                    elif 'LONGITUDE' in line:
                        lon_station = float((line[11:-1].replace(',','.')))
                        pass
            # For the first iteration, concatenate INMET and Exp data
            exp_df = df_model_data(model_data,times,variable,experiment)
            var_data = pd.concat([df_inmet,exp_df])
        # For other iterations, concatenate with previous existing df
        else:
            exp_df = df_model_data(model_data,times,variable,experiment)
            var_data = pd.concat([var_data,exp_df])
            met_list.append(var_data)
        
        j+=1
met_data = pd.concat(met_list)
met_data.index = range(len(met_data))
palette=['#B9465F','#619147','#3F96BA','#E6C030']
sns.set_theme(style="whitegrid", palette="deep",font_scale=1.5,
              rc={"lines.linewidth":2,"grid.linewidth": 0.25})
sns.relplot(data=met_data, aspect=8, height=1.5,
            x='date',y='value', hue='source')
plt.savefig('test.png')

