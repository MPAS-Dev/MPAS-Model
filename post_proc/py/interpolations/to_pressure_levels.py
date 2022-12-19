#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 19 08:59:48 2022

@author: danilocoutodsouza
"""

import datetime
import f90nml
import glob
import pandas as pd
import xarray as xr

from metpy.plots import add_timestamp
from metpy.units import units
from metpy.calc import temperature_from_potential_temperature

from wrf import getvar, interplevel, interp1d

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

# Open file and namelist so we can get the time dimension
# file = '/Users/danilocoutodsouza/Documents/USP/MPAS/MPAS-BR/benchmarks/Catarina-physics/physics_suite/run.convection/latlon.nc'
path = '/home/daniloceano/Documents/MPAS/MPAS-BR/benchmarks/Catarina_physics-test/Catarina_250-8km.microp_scheme.convection_scheme/run.microp_scheme_mp_kessler.convection_scheme_cu_grell_freitas/'
data = xr.open_dataset(path+'/latlon.nc')
namelist_path = path+"/namelist.atmosphere"
namelist = f90nml.read(glob.glob(namelist_path)[0])
time = get_times_nml(namelist,data)
# Get variables
lat = xr.DataArray(data.variables['latitude'])
lon = xr.DataArray(data.variables['longitude'])
theta = xr.DataArray(data.variables['theta']) * units.K
pressure = xr.DataArray(data.variables['pressure']) * units.Pa
hgt = xr.DataArray(data.variables['zgrid']) * units.m
temperature = temperature_from_potential_temperature(pressure, theta)

# Levels to interpolate to
plevs = [   1.,    2.,    3.,    5.,    7.,   10.,   20.,   30.,   50.,   70.,
        100.,  125.,  150.,  175.,  200.,  225.,  250.,  300.,  350.,  400.,
        450.,  500.,  550.,  600.,  650.,  700.,  750.,  775.,  800.,  825.,
        850.,  875.,  900.,  925.,  950.,  975., 1000.] * units.hPa

# Interpolate variables to pressure levels
t_isobaric = interplevel(temperature, pressure, plevs)
