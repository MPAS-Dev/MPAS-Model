#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
    Script for downloading the pressure-level (pl) ERA5 data for initializing
the MPAS-A model. This script is called by the GetERA5_MPAS-RUN.sh.

"""

import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-pressure-levels',
    {
        'product_type':'reanalysis',
        'format':'grib',
        'pressure_level':[
            '1','2','3',
            '5','7','10',
            '20','30','50',
            '70','100','125',
            '150','175','200',
            '225','250','300',
            '350','400','450',
            '500','550','600',
            '650','700','750',
            '775','800','825',
            '850','875','900',
            '925','950','975',
            '1000'
        ],
        'date':'DATE1',
        'area':'Nort/West/Sout/East',
        'time':'HH1',
        'variable':[
            'geopotential','relative_humidity','specific_humidity',
            'temperature','u_component_of_wind','v_component_of_wind'
        ]
    },
    'ERA5-DATE1-HH100-pl.grib')
