#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 10 11:18:29 2022

@author: danilocoutodsouza
"""

import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels',
    {
        'product_type':'reanalysis',
        'format':'grib',
        'variable':['land_sea_mask','sea_ice_cover','sea_surface_temperature'],
        'date':'DATE1/DATE2',
        'area':'Nort/West/Sout/East',
        'time':'00/to/23/by/1',
    },
    'ERA5-DATE1-DATE2-sst.grib')
