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
    # Wind components components
    if variable == 'u component':
        model_var = model_station['u10']
    elif variable == 'v component':
        model_var = model_station['v10']
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
    elif variable == 'dew point':
        model_var = mpcalc.dewpoint_from_specific_humidity(
            model_station['surface_pressure'] * units.Pa,
            model_station['t2m'] * units.K,
            model_station['q2'] * units('kg kg^{-1}'))
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
                       'windspeed':'VENTO, VELOCIDADE HORARIA (m/s)',
                       'windir': 'VENTO, DIREÇÃO HORARIA (gr) (° (gr))',
                       'dew point':'TEMPERATURA DO PONTO DE ORVALHO (°C)'}
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
    # Compute wind components:
    elif variable == 'u component':
        ws = inmet_var.mean()[inmet_variables['windspeed']].values
        wd = inmet_var.mean()[inmet_variables['windir']].values
        inmet_var = pd.Series(
            mpcalc.wind_components((ws * units('m/s')),(wd * units.deg))[0],
            index=inmet_var.mean().index)
    elif variable == 'v component':
        ws = inmet_var.mean()[inmet_variables['windspeed']].values
        wd = inmet_var.mean()[inmet_variables['windir']].values
        inmet_var = pd.Series(
             mpcalc.wind_components((ws * units('m/s')),(wd * units.deg))[1],
             index=inmet_var.mean().index)
    elif variable == 'v component':
        inmet_var = mpcalc.wind_components(
            inmet_var[inmet_variables['windspeed']],
            inmet_var[inmet_variables['windir']])[1]
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

def df_era_data(times,**kwargs):
    era_data = xr.open_dataset(args.ERA5,engine='cfgrib',
            backend_kwargs={'filter_by_keys': {'typeOfLevel': 'surface'}})
    era_station = era_data.sel(latitude=kwargs['lat_station'],method='nearest'
                    ).sel(longitude=kwargs['lon_station'],method='nearest')
    if kwargs['variable'] == 'temperature':
        era_var = era_station.t2m-273.15
    elif kwargs['variable'] == 'precipitation':
        era_var = era_station.t2m*0
    elif kwargs['variable'] == 'dew point':
        era_var = era_station.d2m-273.15
    elif kwargs['variable'] == 'pressure':
        era_var = era_station.msl/100
    elif kwargs['variable'] == 'u component':
        era_var = era_station.u10
    elif kwargs['variable'] == 'v component':
        era_var = era_station.v10
    df_era = pd.DataFrame(era_var.values,columns=['value'])
    df_era.index = era_var.time
    df_era = df_era[(df_era.index >= times.min()) &
                          (df_era.index <= times.max())]
    df_era['source'],df_era['experiment'] = 'ERA','ERA'
    df_era['microp'],df_era['cumulus'] = 'ERA','ERA'
    df_era['variable'] = kwargs['variable']
    df_era['date'] = df_era.index
    df_era.index = range(len(df_era))
    return df_era

def get_stats(var_data):
    ccoef, crmsd, sdev = [], [], []
    experiments = list(var_data['experiment'].unique())
    var_data.index = var_data['date']
    # Resample for 1H for standardization
    reference = var_data[
        var_data['experiment'] == 'INMET'].resample('1H').mean()['value']
    # Slice data for using only times present in model data
    reference = reference[(reference.index >= var_data.index.min()) &
                          (reference.index <= var_data.index.max())]
    for experiment in experiments:
        predicted = var_data[
                var_data['experiment'] == experiment].resample('1H').mean()['value']
        # Just to make sure
        predicted = predicted[(predicted.index >= var_data.index.min()) &
                              (predicted.index <= var_data.index.max())]
        # Compute and store stats
        stats = sm.taylor_statistics(predicted,reference)
        ccoef.append(stats['ccoef'][1])
        crmsd.append(stats['crmsd'][1])
        sdev.append(stats['sdev'][1])
    ccoef, crmsd, sdev = np.array(ccoef),np.array(crmsd),np.array(sdev)
    return sdev,crmsd,ccoef,experiments

def plot_taylor(sdevs,crmsds,ccoefs,experiments,variable):
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
    tickRMS = np.linspace(0,round(RMSmax*1.2,1),6)
    axismax = round(STDmax*1.2,1)
    sm.taylor_diagram(sdevs,crmsds,ccoefs,
                      markerLabelColor = 'b', 
                      markerLabel = experiments,
                      markerColor = 'r', markerLegend = 'on', markerSize = 15, 
                      tickRMS = tickRMS, titleRMS = 'off', widthRMS = 2.0,
                      colRMS = '#728B92', styleRMS = '--',  
                      widthSTD = 2, styleSTD = '--', colSTD = '#8A8A8A',
                      titleSTD = 'on',
                      colCOR = 'k', styleCOR = '-',
                      widthCOR = 1.0, titleCOR = 'off',
                      colObs = 'k', markerObs = '^',
                      titleOBS = variable, styleObs =':',
                      axismax = axismax, alpha = 1)
    
def plot_time_series(data,ax, i):
    legend = 'full' if i == 3 else False
    g = sns.lineplot(x="date", y="value",
                     size="source", style='microp', hue='cumulus',
                     markers=True, ax=ax,data=data, legend=legend)
    ax.set(xlabel=None)
    g.set_ylabel(data.variable.unique()[0],fontsize=18)
    ax.xaxis.set_tick_params(labelsize=16,rotation=35)
    ax.yaxis.set_tick_params(labelsize=16)
    (ax.legend(loc='center',fontsize=20,
                         bbox_to_anchor=(1.5, 1.2))) if i == 3 else None

def plot_qq(var_data,ax,i,variable):
    # Take hourly means from INMET
    reference = var_data[var_data['experiment'] == 'INMET']
    reference.index = reference['date']
    reference = reference.resample('1H').mean()['value']
    # Slice data for using the dates contained in MPAS data
    MPAS_series = var_data[var_data['source']=='MPAS']
    MPAS_series.index = MPAS_series['date']
    reference = reference[(reference.index >= MPAS_series.index.min()) &
                          (reference.index <= MPAS_series.index.max())]
    for experiment in var_data.experiment.unique():
        predicted = var_data[var_data['experiment'] == experiment]
        predicted.index = predicted['date']
        # Take hourly means for all data
        predicted = predicted.resample('1H').mean()['value']
        predicted = predicted[(predicted.index >= MPAS_series.index.min()) &
                              (predicted.index <= MPAS_series.index.max())]
        # plot
        g = sns.regplot(x=reference, y=predicted, data=var_data,
                        label=experiment, ax=ax)
        ax.set_title(variable)
    (ax.legend(loc='center',fontsize=20, ncol=2,
                         bbox_to_anchor=(1.8, 0.8))) if i == 3 else None
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
INMET_dir = work_dir+'/met_data/INMET/'

## Parser options ##
parser = argparse.ArgumentParser()

parser.add_argument('-s','--station', type=str, required=True,
                        help='''station name to compare model data to''')

parser.add_argument('-bdir','--bench_directory', type=str, required=True,
                        help='''path to benchmark directory''')

parser.add_argument('-o','--output', type=str, default=None,
                        help='''output name to append file''')
parser.add_argument('-e','--ERA5', type=str, default=None,
                        help='''wether to validade with ERA5 data''')
args = parser.parse_args()

## Start the code ##
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
        elif 'ALTITUDE' in line:
            z_station = float((line[11:-1].replace(',','.')))
            pass
        
## Opens all data and create a single df ##
variables = ['temperature','precipitation','dew point','pressure',
             'u component', 'v component']
for variable in variables:
    print('-------------------------------------')
    print('organising data for variable: '+variable+'\n')
    # Flag for opening INMET data
    for bench in benchs:
        # Open data and get attributes
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
                  'lon_station':lon_station,
                  'z_station':z_station}
        # For first bench, open INMET data
        if bench == benchs[0]:
            df_inmet = get_inmet_data(start_date,INMET_dir,
                                        times,**kwargs)
        # For first bench and variable, create df from INMET data
        if variable == variables[0] and bench == benchs[0]:
            data = df_inmet
        # If other variable but still for first bench, concat inmet data to df
        elif variable != variables[0] and bench == benchs[0]:
            data = pd.concat([data,df_inmet])      
        # Then, just concat the df with bench data
        exp_df = df_model_data(model_data,times,**kwargs)
        data = pd.concat([data,exp_df])      
        # if requested, also add ERA5 data to df
        if args.ERA5:
            df_era = df_era_data(times,**kwargs)
            data = pd.concat([data,df_era])      
data.index = range(len(data))
            
## Start plotting ##
# Model location to export
model_station = model_data.sel(latitude=kwargs['lat_station'],
            method='nearest').sel(longitude=kwargs['lon_station'],
                                  method='nearest')
lat = round(float(model_station.latitude),2)
lon = round(float(model_station.longitude),2)
z = round(float(model_station.zgrid[0]),2)
# Directory for saving figures
outdir = './Figures/'; os.makedirs(outdir, exist_ok=True)
# Figure names
if args.output is not None:
    fname = args.output
else:
    fname = (args.bench_directory).split('/')[-2].split('.nc')[0]
    
## Plot time series ##
print('plotting time series..')
fig = plt.figure(figsize=(12,15))
for i in range(6):
    variable = variables[i]
    var_data = data[data['variable'] == variable]
    ax = fig.add_subplot(3,2, i+1)
    plot_time_series(var_data, ax, i)
plt.suptitle('Station: '+station+"\nStation lat, lon, z: "+
             str(round(lat_station,2))+", "+str(round(lon_station,2))+", "+
             str(round(z_station,2))+"\nModel lat, lon, z: "+ str(lat)+
             ", "+str(lon)+", "+str(z),fontsize=22)
plt.tight_layout(h_pad=-25, w_pad=-1)
plt.savefig(outdir+fname+'_timeseries_'+station+'.png', dpi=300)
print(fname+'_timeseries_'+station+'.png created!')

## Plot Taylor Diagrams ##
print('plotting taylor diagrams..')
fig = plt.figure(figsize=(20,15))
for i in range(6):
    variable = variables[i]
    var_data = data[data['variable'] == variable]
    stats = get_stats(var_data)
    sdev,crmsd,ccoef,expnames = stats[0],stats[1],stats[2],stats[3]
    ax = fig.add_subplot(3,2, i+1)
    plot_taylor(sdev,crmsd,ccoef,expnames,variable)
plt.suptitle('Station: '+station+"\nStation lat, lon, z: "+
             str(round(lat_station,2))+", "+str(round(lon_station,2))+", "+
             str(round(z_station,2))+"\nModel lat, lon, z: "+ str(lat)+
             ", "+str(lon)+", "+str(z),fontsize=22)
plt.tight_layout(w_pad=0.1)
plt.savefig(outdir+fname+'_taylor-diagram_'+station+'.png', dpi=300)
print(fname+'_taylor-diagram_'+station+'.png created!')

## Plot q-q plots ##
print('plotting q-q plots..')
fig = plt.figure(figsize=(20,10))
for i in range(6):
    variable = variables[i]
    var_data = data[data['variable'] == variable]
    ax = fig.add_subplot(3,2, i+1)
    plot_qq(var_data,ax,i,variable)
plt.suptitle('Station: '+station+"\nStation lat, lon, z: "+
             str(round(lat_station,2))+", "+str(round(lon_station,2))+", "+
             str(round(z_station,2))+"\nModel lat, lon, z: "+ str(lat)+
             ", "+str(lon)+", "+str(z),fontsize=22, y=0.98)
plt.subplots_adjust(hspace=0.6,wspace=0.35,right=0.7, top=0.83)
plt.savefig(outdir+fname+'_qq-plots_'+station+'.png',
            bbox_inches='tight', dpi=300)
print(fname+'_qq-plots_'+station+'.png created!')
