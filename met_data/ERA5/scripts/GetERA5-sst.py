#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
    Script for downloading the sea surface temperature, sea-ice cover and the
land-sea mask from ERA5 data for creating surface update fields for running the
MPAS-A model. This script is called by the GetERA5_MPAS-RUN.sh.

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
