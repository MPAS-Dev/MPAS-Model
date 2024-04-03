#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Module for interactively plotting time series from MPAS raw output files
at particular points. Usage example from Python shell:

########################################################################
import matplotlib.pyplot as plt; from interactive_ts import ts, plot_ts

dir='/path/to/data'; 
stream='history'; t0='2021-01-01 00:00:00'; tf='2021-01-02 18:00:00'; 
dt='10800'; var='swdnb'; lat='-52'; lon='-2'; vertlevel='0'

t1,y1,label1=ts(dir=dir,stream=stream,t0=t0,tf=tf,dt=dt,var=var,lat=lat,
lon=lon,vertlevel=vertlevel)

var='swupb'

t2,y2,label2=ts(dir=dir,stream=stream,t0=t0,tf=tf,dt=dt,var=var,lat=lat,
lon=lon,vertlevel=vertlevel)

y3=y1+y2
label3='total'

plot_ts(t1,y1,label1,ofile='y1.png')
plot_ts(t2,y2,label2,ofile='y1_y2.png')
plot_ts(t2,y3,label3,ofile='y1_y2_y3.png')
########################################################################

Guilherme Torres Mendonça (guilherme.torresmendonca@ime.usp.br)
March 2024
"""
import numpy as np
import matplotlib.pyplot as plt
import argparse
from utils import ( 
    create_datetime_range,
    concat_mpas_output,
    cs_string_to_list,
    find_nCells_from_latlon
)
import os

def ts_vars_list(t0,tf,dt,lat,lon,vertlevel,var,
                 dir,stream='history',closest_value='euclidean',
                 verbose='y'):
    '''
    Plots time series for a list of variables at a 
    particular point (lon,lat,vertlevel) and time period.

    Usage (optional input arguments may be given, see below):
    python3 mpas_time_series_at_point.py --dir /path/to/data
    --stream history --lat -2.12 --lon -59.00 --vertlevel 0 --t0 '2021-01-01 00:00:00' --tf '2021-01-02 18:00:00' 
    --dt 10800 --var swdnb,swupb
    '''

    # Create datetime range and transform it into a list of datetime strings
    datetime_list = create_datetime_range(
        t0=t0,
        tf=tf,
        dt=dt
    )

    # Select variable and concatenate files between t0 and tf
    vars_list = cs_string_to_list(var)
    cat_file = concat_mpas_output(
        stream=stream,
        datetime_list=datetime_list,
        data_dir=dir,
        vars_list=vars_list,
        )

    # Get lat, lon, vertlevel
    lat = float(lat)
    lon = float(lon)
    vertlevel = int(vertlevel)

    # Find grid cell
    nCells, ds = find_nCells_from_latlon(cat_file,lon=lon,lat=lat)

    # Plot
    ## Get again list of variables for plotting
    vars_list = cs_string_to_list(var)
    ## Get again list of datetimes, but abbreviated
    datetime_list = create_datetime_range(
        t0=t0,
        tf=tf,
        dt=dt,
        short_strings='y'
    )

    plt.figure('ts')
    for var in vars_list:
        # Try choosing nVertLevels
        try:
            y_series = ds[var].sel(nCells=nCells,nVertLevels=vertlevel).values
        except: 
            y_series = ds[var].sel(nCells=nCells).values
        plt.plot(datetime_list,y_series,linestyle='-',label=var)
    plt.tick_params(axis='both', which='major', labelsize=12)
    plt.xticks(np.arange(min(ds.Time), max(ds.Time)+1,1),
            rotation=45, ha='right')
    plt.grid(True)
    plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    plt.title(f"{stream} from {t0} to {tf}")
    plt.tight_layout()
    plt.savefig("time_series.png")
    plt.show()

def ts(t0,tf,dt,lat,lon,vertlevel,var,
       dir,stream='history',closest_value='euclidean'):
    '''
    Reads MPAS raw $stream output files from $dir and returns a 
    list of dates and times $datetime_list, a time series of $var ($y_series)
    at ($lat,$lon,$vertlevel) between $t0 and $tf with time step $dt, and
    a label $var for plotting
    
    INPUT: t0 (str) - initial time
           tf (str) - final time
           dt (str) - time step
           lat (str) - latitude of the grid cell
           lon (str) - longitude of the grid cell
           vertlevel (str) - vertical level (if applicable)
           var (str) - variable to be plotted
           dir (str) - directory of MPAS output files
           stream (str) - stream name
           closest_value (str) - method to find grid cell

    OUTPUT: datetime_list (list) - list of datetime strings (abbreviated for plotting)
            y_series (array) - time series for var
            var (str) - variable name (label)
    '''

    # Create datetime range and transform it into a list of datetime strings
    datetime_list = create_datetime_range(
        t0=t0,
        tf=tf,
        dt=dt
    )

    # Select variable and concatenate files between t0 and tf
    var_list = cs_string_to_list(var)
    cat_file = concat_mpas_output(
        stream=stream,
        datetime_list=datetime_list,
        data_dir=dir,
        vars_list=var_list,
        )

    # Get lat, lon, vertlevel
    lat = float(lat)
    lon = float(lon)
    vertlevel = int(vertlevel)

    # Find grid cell
    nCells, ds = find_nCells_from_latlon(cat_file,lon=lon,lat=lat)

    # Define time series
    try:
        y_series = ds[var].sel(nCells=nCells,nVertLevels=vertlevel).values
    except: 
        y_series = ds[var].sel(nCells=nCells).values
    
    # Define list of dates
    datetime_list = create_datetime_range(
        t0=t0,
        tf=tf,
        dt=dt,
        short_strings='y'
    )

    return datetime_list, y_series, var

def plot_ts(t,y,label=None,ofile=None):
    plt.figure('ts')
    if label is not None:
        plt.plot(t,y,linestyle='-',label=label)
        plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    else:
        plt.plot(t,y,linestyle='-')
    plt.tick_params(axis='both', which='major', labelsize=12)
    plt.xticks(rotation=45, ha='right')
    plt.grid(True)
    plt.tight_layout()
    if ofile is not None:
        plt.savefig(ofile)
    plt.show(block=False)

if __name__ == "__main__":
    # Create ArgumentParser object
    parser = argparse.ArgumentParser(description=
                                    'plotting time series for a list of variables at a' 
                                    +'particular point and time period.')
    # Input arguments
    parser.add_argument('--dir', default=os.getcwd(), type=str, 
                        help='data directory')
    parser.add_argument('--stream', default='history', type=str, 
                        help='stream')
    parser.add_argument('--var', default='swdnb, swupb', type=str, 
                        help='list of variables to plot (var1,var2,...)')
    parser.add_argument('--lat', default='-2.124375557901979', type=str, 
                        help='latitude')
    parser.add_argument('--lon', default='-59.003420487096946', type=str, 
                        help='longitude')
    parser.add_argument('--vertlevel', default=0, type=str, 
                        help='vertical level')
    parser.add_argument('--t0', type=str, 
                        help='initial datetime (YYYY-mm-dd HH:MM:SS)')
    parser.add_argument('--tf', type=str, 
                        help='final datetime (YYYY-mm-dd HH:MM:SS)')
    parser.add_argument('--dt', type=str, 
                        help='datetime step (seconds)')
    parser.add_argument('--closest_value', type=str, default='euclidean',
                        help='method to find grid cell from (lat,lon)')

    # Parse the command line arguments
    args = parser.parse_args()

    t0=args.t0
    tf=args.tf
    dt=args.dt
    lat=args.lat
    lon=args.lon
    vertlevel=args.vertlevel
    var=args.var
    dir=args.dir
    stream=args.stream
    closest_value=args.closest_value

    ts_vars_list(
        dir=dir,
        stream=stream,
        t0=t0,
        tf=tf,
        dt=dt,
        var=var,
        lat=lat,
        lon=lon,
        vertlevel=vertlevel
        )