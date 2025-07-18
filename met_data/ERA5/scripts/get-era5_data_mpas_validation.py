#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Unified script for downloading ERA5 pressure-level data for model validation.
This script is useful when previousl donwloaded ERA5 data only for the initial time step
and now you need to download data for all time steps.

Created by:
    Danilo Couto de Souza
    Universidade de São Paulo (USP)
    São Paulo, Brazil

Contact:
    danilo.oceano@gmail.com
"""
from datetime import datetime

from get_era5_data_mpas_run import download_for_time_range

if __name__ == "__main__":
    # Simulation setup:
    start_date = datetime.strptime('2014-01-25 00:00', '%Y-%m-%d %H:%M') # Simulation start date
    end_date = datetime.strptime('2014-06-02 00:00', '%Y-%m-%d %H:%M') # Simulation end date

    # Simulation type setup ('global' or 'regional')
    simulation_type = 'global'  # Choose 'global' or 'regional'

    # Time interval in hours (e.g., 1, 2, 3) for downloading data
    # (Even for global simulations, this is used for SST data, if requested)
    time_interval = 3 

    # Download area
    area = [-90, -180, 90, 180]  # Example area [South, West, North, East]
    output_dir = '../DATA'  # Output directory

    download_for_time_range(start_date, end_date, time_interval, area, output_dir, download_sst=False)
