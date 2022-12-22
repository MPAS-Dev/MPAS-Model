#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 19 08:59:48 2022

Created by:
    Danilo Couto de Souza
    Universidade de São Paulo (USP)
    Instituto de Astornomia, Ciências Atmosféricas e Geociências
    São Paulo - Brazil
    
Contact:
    danilo.oceano@gmail.com
    
    Script for interpolating MPAS-A model output from hybrid vertical levels to
pressure levels. The input must be the structured "latlon.nc" file created by 
the "convert_mpas" program. As MPAS-A data created by the "convert_mpas" does 
not (at least at the moment) present a time dimension with actual dates, there
is the option for using the namelist.atmosphere to assign dates to the variables
time dimension. This can be done using the -n flag.

"""

import argparse
import datetime
import f90nml
import glob
import pandas as pd
import xarray as xr

from metpy.units import units

from wrf import interplevel

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

def main():
    data = xr.open_dataset(infile)
    print('Data have '+str(len(data.nVertLevels))+
          ' levels in the nVertLevels dimension')
    #If requested, open namelist so we can get the time dimension
    if args.namelist:
        print('opening namelist.atmosphere...')
        namelist_path = infile.split("latlon.nc")[0]+"namelist.atmosphere"
        namelist = f90nml.read(glob.glob(namelist_path)[0])
        time = get_times_nml(namelist,data)
        print('ok, times:',time)
    pressure = (data['pressure'] * units(data['pressure'].units)
                ).metpy.convert_units('hPa')
    # Levels to interpolate to
    plevs = [   1.,    2.,    3.,    5.,    7.,   10.,   20.,   30.,   50.,   70.,
            100.,  125.,  150.,  175.,  200.,  225.,  250.,  300.,  350.,  400.,
            450.,  500.,  550.,  600.,  650.,  700.,  750.,  775.,  800.,  825.,
            850.,  875.,  900.,  925.,  950.,  975., 1000.] * units.hPa
    print("data will be interpolated to:",plevs)
    # Get variables
    outfile = xr.Dataset()
    for var in data.variables:
        if 'long_name' in data[var].attrs:
            print('writting variable:',var,'-',data[var].long_name)
        else:
            print('writting variable:',var)
        # Interpolate variables from hybrid vertical levels to pressure levels
        # when variable has same shape as pressure
        if ('nVertLevels' in data[var].dims) and (
                data[var].shape == pressure.shape):
            var_data = interplevel(data[var], pressure, plevs)
        # Data that has less dimensions than pressure but has vertical levels
        # are mostly grid parameters that are not useful.
        elif ('nVertLevels' in data[var].dims) and (
                len(data[var].shape) < len(pressure.shape)):
            print("ommiting variable:",var,'-',data[var].long_name)
            pass
        else:
            var_data = data[var]
        # Assign time values to corrdinate if namelist flag is used
        if args.namelist and "Time" in data[var].dims:
            var_data = data[var].assign_coords(Time=time)
            
        outfile = outfile.assign({var:var_data})
    
    if args.output and ".nc" not in args.output:
        fname = args.output+".nc"
    elif args.output and ".nc" in args.output:
        fname = args.output
    else:
        fname = infile.split("/")[-1].split(".nc")[0]+"_isobaric.nc"
    outfile.to_netcdf(fname, mode="w")

if __name__ == "__main__":
    ## Parser options ##
    parser = argparse.ArgumentParser()
    parser.add_argument('-i','--infile', type=str, required=True,
                            help='''Model output to interpolate''')
    parser.add_argument('-o','--output', type=str, default=None,
                            help='''output name to append file''')
    parser.add_argument('-n','--namelist', type=str, default="",
                            help='''Use this flag if namelist.atmosphere is on\
the same path as the infile so the program is able to create a time dimension.''')
    args = parser.parse_args()
    infile = args.infile
    print('Opening:',infile)
    main()
