#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
    Script for downloading the sea-level (sl) ERA5 data for initializing
the MPAS-A model. This script is called by the GetERA5_MPAS-RUN.sh.

"""

import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels',
    {
        'product_type':'reanalysis',
        'format':'grib',
        'variable':[
            '10m_u_component_of_wind','10m_v_component_of_wind','2m_dewpoint_temperature',
            '2m_temperature','land_sea_mask','mean_sea_level_pressure',
            'sea_ice_cover','sea_surface_temperature','skin_temperature',
            'snow_depth','soil_temperature_level_1','soil_temperature_level_2',
            'soil_temperature_level_3','soil_temperature_level_4','surface_pressure',
            'volumetric_soil_water_layer_1','volumetric_soil_water_layer_2','volumetric_soil_water_layer_3',
            'volumetric_soil_water_layer_4'
        ],
        'date':'DATE1',
        'area':'Nort/West/Sout/East',
        'time':'HH1',
    },
    'ERA5-DATE1-HH100-sl.grib')
