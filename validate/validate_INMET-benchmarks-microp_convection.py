#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  3 16:48:43 2022

@author: daniloceano
"""

import sys
import os
import glob
import argparse
import xarray as xr
import f90nml
import datetime
import pandas as pd
import numpy as np
import metpy.calc as mpcalc
import seaborn as sns
from metpy.units import units
import matplotlib.pyplot as plt
from matplotlib import rcParams
import numpy as np
import skill_metrics as sm


def df_model_data(model_data,times,**kwargs):
    # Dictionaries containing naming convections for each variable
    model_variables = {'temperature':'t2m', 'pressure':'surface_pressure'}
    variable = kwargs['variable']
    # Get model data for the gridpoint closest to station   
    model_station = model_data.sel(latitude=kwargs['lat_station'],
                method='nearest').sel(longitude=kwargs['lon_station'],
                                      method='nearest')
    # If windpeed, calculate from components
    if variable == 'windspeed':
        model_var = mpcalc.wind_speed(model_station['u10'],model_station['v10'])
    # If testing microphysics and/or convection schemes, the data might not
    # have both grid-scale and convective precipitation variables, so the code
    # has to figure out what's in there
    elif variable == 'precipitation':
        # Sum grid-scale and convective precipitation
        if ('rainnc' in model_station.variables
            ) and ('rainc' in model_station.variables):
            model_var = model_station['rainnc']+model_station['rainc']
        # Get only micrphysics precipitation
        elif ('rainnc' in model_station.variables
            ) and ('rainc' not in model_station.variables):
            model_var = model_station['rainnc']
        # Get convective precipitation
        elif ('rainnc' not in model_station.variables
            ) and ('rainc' in model_station.variables):
            model_var = model_station['rainc']
        # If there is no precipitation variable:
        # (using nan gives error in statistical analysis)
        else:
            model_var = model_station['u10']*0
    # Convert pressure to MSLP
    elif variable == 'pressure':
        model_var = mpcalc.altimeter_to_sea_level_pressure(
            (model_station['surface_pressure'])*units('hPa'),
            model_station['zgrid'][0]*units('m'),
            model_station['t2m']*units('K'))
    else:
        model_var = model_station[model_variables[variable]]
    # create df
    mpas_df = pd.DataFrame(model_var,columns=['value'],
                             index=times)
    # Convert temperature to celcius
    if variable == 'temperature':
        mpas_df = mpas_df-273.15
    # Convert pressure to hPa
    elif variable == 'pressure':
        mpas_df = mpas_df/100
    # Add info to df
    mpas_df['source'] = 'MPAS'
    mpas_df['variable'] = kwargs['variable'] 
    mpas_df['microp'] = kwargs['microp']
    mpas_df['cumulus'] = kwargs['cumulus']
    mpas_df['experiment'] = kwargs['experiment']
    # Add date as column and revert indexes to a range of numbers
    mpas_df['date'] = mpas_df.index
    mpas_df.index = range(len(mpas_df))
    
    return mpas_df

def df_inmet_data(inmet_data,times,**kwargs): 
    inmet_variables = {'temperature':
                       'TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)',
                       'precipitation':'PRECIPITAÇÃO TOTAL, HORÁRIO (mm)',
                       'pressure':
                       'PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)',
                       'windspeed':'VENTO, VELOCIDADE HORARIA (m/s)'}
    # Get time interval
    dt = times[1] - times[0]
    # Get INMET data for the same dates as the model experiment
    inmet_var = inmet_data[
        (inmet_data['DATA (YYYY-MM-DD)_HORA (UTC)'] >= times[0]) &
        (inmet_data['DATA (YYYY-MM-DD)_HORA (UTC)'] <= times[-1])
        ].resample(dt)
    variable = kwargs['variable']
    # If using precipitation, get accumulated values between model time steps
    if variable == 'precipitation':
        inmet_var = inmet_var.sum()[inmet_variables[variable]].cumsum()
    # Else, get mean values between model time steps
    else:
        inmet_var = inmet_var.mean()[inmet_variables[variable]]
    # Add info to df
    inmet_df = pd.DataFrame(inmet_var.rename('value'))
    inmet_df['source'],inmet_df['experiment'] = 'INMET', 'INMET'
    inmet_df['microp'],inmet_df['cumulus'] = 'INMET','INMET'
    inmet_df['variable'] = variable
    # Add date as column and revert indexes to a range of numbers
    inmet_df['date'] = inmet_df.index
    inmet_df.index = range(len(inmet_df))
    
    return inmet_df

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

def get_inmet_data(start_date,INMET_dir,times,**kwargs):
    # Get data for corresponding year
    station = kwargs['station']
    station_file = glob.glob(
            INMET_dir+'/'+str(start_date.year)+'/*'+station+'*')[0]
    # Open file with Pandas
    station_data = pd.read_csv(station_file,header=8,sep=';',encoding='latin-1',
                               parse_dates=[[0,1]],decimal=',')
    station_data.index = station_data['DATA (YYYY-MM-DD)_HORA (UTC)']
    df_inmet = df_inmet_data(station_data,times,**kwargs)
    return df_inmet

def get_stats(data):
    ccoef, crmsd, sdev = [], [], []
    experiments = list(data['experiment'].unique())
    data.index = data['date']
    # Resample for 1H for standardization
    reference = data[
        data['experiment'] == 'INMET'].resample('1H').mean()['value']
    # Slice data for using only times present in model data
    reference = reference[(reference.index >= data.index.min()) &
                          (reference.index <= data.index.max())]
    for experiment in experiments:
        predicted = data[
                data['experiment'] == experiment].resample('1H').mean()['value']
        # Just to make sure
        predicted = predicted[(predicted.index >= data.index.min()) &
                              (predicted.index <= data.index.max())]
        # Compute and store stats
        stats = sm.taylor_statistics(predicted,reference)
        ccoef.append(stats['ccoef'][1])
        crmsd.append(stats['crmsd'][1])
        sdev.append(stats['sdev'][1])
    ccoef, crmsd, sdev = np.array(ccoef),np.array(crmsd),np.array(sdev)
    return sdev,crmsd,ccoef,experiments

def plot_taylor(sdevs,crmsds,ccoefs,experiments,col):
    '''
    Produce the Taylor diagram
    Label the points and change the axis options for SDEV, CRMSD, and CCOEF.
    Increase the upper limit for the SDEV axis and rotate the CRMSD contour 
    labels (counter-clockwise from x-axis). Exchange color and line style
    choices for SDEV, CRMSD, and CCOEFF variables to show effect. Increase
    the line width of all lines.
    For an exhaustive list of options to customize your diagram, 
    please call the function at a Python command line:
    >> taylor_diagram
    '''
    # Set the figure properties (optional)
    rcParams.update({'font.size': 14}) # font size of axes text
    STDmax = round(np.amax(sdevs))
    RMSmax = round(np.amax(crmsds))
    tickRMS = np.linspace(0,RMSmax,5)
    axismax = STDmax*1.2
    if col == 3:
        leg = 'on'
        label = experiments
    else:
        leg = 'off'
        label= None
    sm.taylor_diagram(sdevs,crmsds,ccoefs,
                      markerLabelColor = 'b', 
                      markerLabel = experiments,
                      markerColor = 'r', markerLegend = leg, markerSize = 15, 
                      tickRMS = tickRMS, titleRMS = 'off', widthRMS = 2.0,
                      colRMS = '#728B92', styleRMS = '--',  
                      widthSTD = 2, styleSTD = '--', colSTD = '#8A8A8A',
                      colCOR = 'k', styleCOR = '-',
                      widthCOR = 1.0, titleCOR = 'on',
                      colObs = 'k', markerObs = '^',
                      titleOBS = 'Obs.', styleObs =':',
                      axismax = axismax, alpha = 1)

def plot_qq(data,ax):
    for experiment in data.experiment.unique():
        if experiment == 'INMET':
            predicted = data[
                data['experiment'] == experiment].resample('1H').mean()['value']
        else:
            predicted = data[
                data['experiment'] == experiment].resample('1H').mean()['value']
        
        # Slice data for using teh same dates
        predicted = predicted[(predicted.index >= data.index.min()) &
                              (predicted.index <= data.index.max())]
        reference = data[
            data['experiment'] == 'INMET'].resample('1H').mean()['value']
        reference = reference[(reference.index >= data.index.min()) &
                              (reference.index <= data.index.max())]
        
        g = sns.regplot(x=reference, y=predicted, data=data, label=experiment,
                        ax=ax)
    if col == 3:
        ax.legend(loc='upper center', fontsize=14, bbox_to_anchor=(2.3, 1),ncol=3)
    g.set_ylabel('EXPERIMENT',fontsize=18)
    g.set_xlabel('INMET',fontsize=18)
    ax.xaxis.set_tick_params(labelsize=16)
    ax.yaxis.set_tick_params(labelsize=16)

## Workspace ##
work_dir = os.getenv('MPAS_DIR')
if work_dir is None:
    print('Error: MPAS_DIR environment variable not defined! It should direct\
to the MPAS-BR path')
    sys.exit(-1)
# work_dir = '~/Documents/MPAS/MPAS-BR/'
INMET_dir = work_dir+'/met_data/INMET/'

## Parser options ##
parser = argparse.ArgumentParser()

parser.add_argument('-s','--station', type=str, required=True,
                        help='''station name to compare model data to''')

parser.add_argument('-bdir','--bench_directory', type=str, required=True,
                        help='''path to benchmark directory''')

parser.add_argument('-o','--output', type=str, default=None,
                        help='''output name to append file''')
args = parser.parse_args()
benchs = glob.glob(args.bench_directory+'/run*')
station = (args.station).upper()
# Dummy for getting model times
model_output = benchs[0]+'/latlon.nc'
namelist_path = benchs[0]+"/namelist.atmosphere"
# open data and namelist
model_data = xr.open_dataset(model_output)
namelist = f90nml.read(glob.glob(namelist_path)[0])
times = get_times_nml(namelist,model_data)
start_date = times[0]
# Getting station lat and lon
try:
    station_file = glob.glob(
        INMET_dir+'/'+str(start_date.year)+'/*'+station+'*')[0]
except:
    raise SystemExit('Error: not found data for '+station.upper()+' station \
for year '+str(start_date.year))
# Get station lat and lon
with open(station_file, 'r',encoding='latin-1') as file:
    for line in file:
        if 'LATITUDE' in line:
            lat_station = float((line[10:-1].replace(',','.')))
        elif 'LONGITUDE' in line:
            lon_station = float((line[11:-1].replace(',','.')))
            pass

met_list = []
variables = ['temperature','precipitation','windspeed','pressure']
plt.close('all')
fig, axes = plt.subplots(3, 4, figsize=(30, 15))
plt.subplots_adjust(wspace=0.1, hspace=0.1)
# fig.tight_layout()
i,v = 1,0
for col in range(4):
    # One variable for each columns
    variable = variables[v]
    print('-------------------------------------')
    print('plotting variable: '+variable+'\n')
    for row in range(3):
        j = 0
        for bench in benchs:
            # Open data
            model_output = bench+'/latlon.nc'
            model_data = xr.open_dataset(model_output)
            expname = bench.split('/')[-1].split('run.')[-1]
            print('experiment: '+expname)
            microp = expname.split('.')[0].split('_')[-1]
            cumulus = expname.split('.')[-1].split('_')[-1] 
            experiment = microp+'_'+cumulus  
            # Define kwargs for using in fuctions
            kwargs = {'variable':variable,'station':station,
                      'experiment':experiment,
                      'microp':microp,'cumulus':cumulus,
                      'lat_station':lat_station,
                      'lon_station':lon_station}
            if j != 0:
                kwargs['lat_station']  = lat_station
                kwargs['lon_station'] = lon_station
            # Only open INMET file once and for the first iteration,
            # concatenate INMET and Exp data. For other iterations, 
            # concatenate with previous existing df
            if j == 0:
                inmet_data = get_inmet_data(start_date,INMET_dir,
                                            times,**kwargs)
                df_inmet = inmet_data
                
                exp_df = df_model_data(model_data,times,**kwargs)
                var_data = pd.concat([df_inmet,exp_df])
                j+=1
            else:
                exp_df = df_model_data(model_data,times,**kwargs)
                var_data = pd.concat([var_data,exp_df])
        # Plot timeseries in the first column
        if row == 0:
            data = var_data[var_data['variable'] == variable]
            data.index = range(len(data))
            g = sns.lineplot(x="date", y="value", size="source",
                             style='microp', hue='cumulus',
                             markers=True,
                         ax=axes[row,col],data=data)
            if col == 3:
                axes[row,col].legend(loc='upper center',fontsize=14,
                                 bbox_to_anchor=(2, 1),ncol=2)
            axes[row,col].set(xlabel=None)
            g.set_ylabel(variable,fontsize=18)
            axes[row,col].xaxis.set_tick_params(labelsize=16,rotation=35)
            axes[row,col].yaxis.set_tick_params(labelsize=16)
        # Plot taylo diagrams in the second columns
        if row == 1:
            ax = axes[row,col] = fig.add_subplot(3,4, i)
            ax.set_axis_off()
            stats = get_stats(data)
            sdev,crmsd,ccoef,expnames = stats[0],stats[1],stats[2],stats[3]
            plot_taylor(sdev,crmsd,ccoef,expnames,col)
        # Plot q-q plots in the third column
        if row == 2:
            ax = axes[row,col]
            plot_qq(data,ax)
        # Update column indexer
        i+=1
    # Update variable indexer
    v+=1
        
if args.output is not None:
    fname = args.output
else:
    fname = (args.bench_directory).split('/')[-2].split('.nc')[0]
plt.savefig(fname+'_timeseries_'+station+'.png')
print(fname+'_timeseries_'+station+'.png created!')
