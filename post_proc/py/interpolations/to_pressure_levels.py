#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 19 08:59:48 2022

@author: danilocoutodsouza
"""

import xarray as xr

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
from netCDF4 import Dataset, num2date

from metpy.cbook import get_test_data
from metpy.interpolate import log_interpolate_1d
from metpy.plots import add_metpy_logo, add_timestamp
from metpy.units import units

file = '/Users/danilocoutodsouza/Documents/USP/MPAS/MPAS-BR/benchmarks/Catarina-physics/physics_suite/run.convection/latlon.nc'
data = xr.open_dataset(file)

lat = data.variables['latitude'][:]
lon = data.variables['longitude'][:]
# time = data.variables['Time']
# vtimes = num2date(time[:], time.units)
temperature = data.variables['theta'][:] * units.K
pressure = data.variables['pressure'][:] * units.Pa
hgt = data.variables['zgrid'][:] * units.m

plevs = [1000., 900., 800., 700.] * units.hPa


height, temp = log_interpolate_1d(plevs, pressure[1], hgt[:-1], temperature[1])


from wrf import getvar, interplevel

# Extract the Geopotential Height and Pressure (hPa) fields
z = getvar(file, "z")
p = getvar(file, "pressure")

# Compute the 500 MB Geopotential Height
ht_500mb = interplevel(z, p, 500.)