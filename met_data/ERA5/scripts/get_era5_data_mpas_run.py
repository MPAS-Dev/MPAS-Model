#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Unified script for downloading ERA5 pressure-level and surface data for initializing
the MPAS-A model. Supports global and regional simulations.

The script automatically handles downloading data for specified date ranges and timesteps.
For global simulations, only the initial time step is downloaded. For regional simulations,
data is downloaded for all timesteps.

SST (sea surface temperature), land-sea mask, and sea-ice data can be optionally included
for all time steps.

Created by:
   Danilo Couto de Souza
   Universidade de São Paulo (USP)
   São Paulo, Brazil

Contact:
   danilo.oceano@gmail.com
"""

import os
import cdsapi
import pandas as pd
from datetime import datetime, timedelta

# Create the CDSAPI client
client = cdsapi.Client()

def download_era5_surface_data(date, time, area, target_filename):
    """
    Download surface data (sea-level) from ERA5 for a specific date and time.
    """
    date = pd.to_datetime(date).strftime('%Y-%m-%d %H:%M')
    request = {
        'product_type': ['reanalysis'],
        'variable': [
            '10m_u_component_of_wind', '10m_v_component_of_wind', '2m_dewpoint_temperature',
            '2m_temperature', 'land_sea_mask', 'mean_sea_level_pressure', 'sea_ice_cover',
            'sea_surface_temperature', 'skin_temperature', 'snow_depth', 'surface_pressure',
        ],
        'year': [date[:4]],
        'month': [date[5:7]],
        'day': [date[8:10]],
        'time': [time],
        'area': area,
        'format': 'grib',
        "download_format": "unarchived"
    }
    
    client.retrieve('reanalysis-era5-single-levels', request).download(target_filename)
    print(f"Downloaded surface data: {target_filename}")

def download_era5_pressure_data(date, time, area, target_filename):
    """
    Download pressure-level data from ERA5 for a specific date and time.
    """
    # Convert date to string format
    date = pd.to_datetime(date).strftime('%Y-%m-%d %H:%M')
    request = {
        'product_type': ['reanalysis'],
        'variable': [
            'geopotential', 'relative_humidity', 'specific_humidity', 'temperature',
            'u_component_of_wind', 'v_component_of_wind',
        ],
        'year': [date[:4]],
        'month': [date[5:7]],
        'day': [date[8:10]],
        'time': [time],
        'pressure_level': [
            '10', '20', '30', '50', '70', '100', '125', '150', '200', '225', '250', '300',
            '350', '400', '450', '500', '550', '600', '650', '700', '750', '800', '850',
            '900', '950', '1000',
        ],
        'area': area,
        'format': 'grib',
        "download_format": "unarchived"
    }

    print(f"Request details: {request}")
    client.retrieve('reanalysis-era5-pressure-levels', request).download(target_filename)
    print(f"Downloaded pressure data: {target_filename}")

def download_era5_sst_data(start_date, end_date, time_steps, area, target_filename):
    """
    Download SST, sea-ice cover, and land-sea mask data for a range of dates and all time steps.
    """
    request = {
        'product_type': 'reanalysis',
        'variable': ['land_sea_mask', 'sea_ice_cover', 'sea_surface_temperature'],
        'year': [start_date[:4], end_date[:4]],
        'month': [start_date[5:7], end_date[5:7]],
        'day': [start_date[8:10], end_date[8:10]],
        'time': time_steps,
        'area': area,
        'format': 'grib',
    }

    print(f"Request details: {request}")
    client.retrieve('reanalysis-era5-single-levels', request, target_filename)
    print(f"Downloaded SST data: {target_filename}")

def generate_time_steps(start_date, end_date, interval):
    """
    Generate a list of time steps for the given date range, based on the specified interval.
    """
    time_steps = []
    current_time = start_date.replace(hour=0, minute=0)
    while current_time <= end_date.replace(hour=23, minute=0):
        time_steps.append(current_time.strftime('%H:%M'))
        current_time += timedelta(hours=interval)
    return time_steps

def download_for_time_range(start_date, end_date, time_interval, area, output_dir, download_sst=False):
    """
    Downloads data for all time steps within a given date range for regional simulations.
    """
    time_steps = generate_time_steps(start_date, end_date, time_interval)
    for single_date in (start_date + timedelta(days=i) for i in range((end_date - start_date).days + 1)):
        date_str = single_date.strftime('%Y-%m-%d')
        for time in time_steps:
            target_filename_pl = f'{output_dir}/era5_pl_{date_str}_{time.replace(":", "")}.grib'
            target_filename_sl = f'{output_dir}/era5_sl_{date_str}_{time.replace(":", "")}.grib'
            download_era5_pressure_data(date_str, time, area, target_filename_pl)
            download_era5_surface_data(date_str, time, area, target_filename_sl)

    if download_sst:
        sst_filename = f'{output_dir}/era5_sst_{start_date.strftime("%Y%m%d")}-{end_date.strftime("%Y%m%d")}.grib'
        download_era5_sst_data(start_date, end_date, time_steps, area, sst_filename)

def download_for_single_timestep(start_date, end_date, time_interval, area, output_dir, download_sst=False):
    """
    Downloads data for a single timestep (for global simulations).
    """
    time = pd.to_datetime(start_date).strftime('%H:%M')  # Get the time in HH:MM format
    target_filename_pl = f'{output_dir}/era5_pl_{start_date}_{time.replace(":", "")}.grib'
    target_filename_sl = f'{output_dir}/era5_sl_{start_date}_{time.replace(":", "")}.grib'
    download_era5_pressure_data(start_date, time, area, target_filename_pl)
    download_era5_surface_data(start_date, time, area, target_filename_sl)

    if download_sst:
        time_steps = generate_time_steps(datetime.strptime(start_date, '%Y-%m-%d'), datetime.strptime(end_date, '%Y-%m-%d') + timedelta(hours=23), time_interval)
        sst_filename = f'{output_dir}/era5_sst_{start_date}.grib'
        download_era5_sst_data(start_date, end_date, time_steps, area, sst_filename)

if __name__ == "__main__":
    # Simulation setup:
    start_date = datetime.strptime('2014-01-25 00:00', '%Y-%m-%d %H:%M') # Simulation start date
    end_date = datetime.strptime('2014-06-02 00:00', '%Y-%m-%d %H:%M') # Simulation end date

    # Simulation type setup ('global' or 'regional')
    simulation_type = 'regional'  # Choose 'global' or 'regional'

    # Time interval in hours (e.g., 1, 2, 3) for downloading data
    # (Even for global simulations, this is used for SST data, if requested)
    time_interval = 6 

    # Download area
    area = [-90, -180, 90, 180]  # Example area [South, West, North, East]
    output_dir = '../DATA'  # Output directory

    # Ensure output directory exists
    os.makedirs(output_dir, exist_ok=True)

    # Choose whether to download SST data
    download_sst = False  # Set to True if SST data is needed, False otherwise

    if simulation_type == 'global':
        download_for_single_timestep(start_date, end_date, time_interval, area, output_dir, download_sst)
    elif simulation_type == 'regional':
        download_for_time_range(start_date, end_date, time_interval, area, output_dir, download_sst)
