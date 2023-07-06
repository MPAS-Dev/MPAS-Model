#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct  6 20:21:32 2022

@author: danilocoutodsouza
"""

import os
import sys
import argparse

from netCDF4 import Dataset
import numpy as np

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import matplotlib.cm as cm

from mpas_patches import get_mpas_patches

import xarray as xr
import cartopy.crs as ccrs
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from cartopy.feature import NaturalEarthFeature, COASTLINE
from cartopy.feature import BORDERS

import numpy.ma as ma

import sys

def map_features(ax):
    ax.add_feature(COASTLINE)
    ax.add_feature(BORDERS, edgecolor='#383838')
    return ax

def Brazil_states(ax):    
    states = NaturalEarthFeature(category='cultural', scale='50m', facecolor='none',
                                  name='admin_1_states_provinces_lines')
    _ = ax.add_feature(states, edgecolor='#383838')
    
    cities = NaturalEarthFeature(category='cultural', scale='50m', facecolor='none',
                                  name='populated_places')
    _ = ax.add_feature(cities)
    
def grid_labels_params(ax):
    gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                      linewidth=1, color='gray', alpha=0.5,linestyle='--')
    gl.top_labels = False
    gl.right_labels = False
    gl.xlabel_style = {'size': 14, 'color': '#383838'}
    gl.ylabel_style = {'size': 14, 'color': '#383838'}
    ax.spines['geo'].set_edgecolor('#383838')
    gl.xformatter = LONGITUDE_FORMATTER
    gl.yformatter = LATITUDE_FORMATTER
    return ax

def convert_lon(xr,LonIndexer):
    """
    
    Convert longitudes from 0:360 range to -180:180
    Parameters
    ----------
    xr : xarray.DataArray 
        gridded data.
    LonIndexer : str
        corrdinate indexer used for longitude.
    Returns
    -------
    xr : xarray.DataArray 
        gridded data with longitude converted to desired format.
    """
    xr.coords[LonIndexer] = (xr.coords[LonIndexer] + 180) % 360 - 180
    xr = xr.sortby(xr[LonIndexer])
    return xr
    
parser = argparse.ArgumentParser()

parser.add_argument('file', 
                    type=str, 
                    help='''File you want to plot from''')
parser.add_argument('-v',
                    '--var', 
                    type=str,
                    default='Edge',
                    help='''Geometric variable you want to plot from that file''')

args = parser.parse_args()
variable = args.var
file = args.file

data = xr.open_dataset(file)

# Open the mesh using NetCDF4 Dataset.
mesh = Dataset(os.path.join(file), 'r')
print(mesh)
lat_var = "lat"+variable
lon_var = "lon"+variable
# Check to see the variable is in the mesh
if lat_var not in mesh.variables.keys(): 
    print("That variable was not found in this mpas mesh!", lat_var)
    print(mesh.variables.keys())
    sys.exit(-1)

if lon_var not in mesh.variables.keys(): 
    print("That variable was not found in this mpas mesh!", lon_var)
    print(mesh.variables.keys())
    sys.exit(-1)
    

# Pull the variable out of the mesh. Now we can manipulate it any way we choose
# do some 'post-processing' or other meteorological stuff
    
lats = mesh.variables['lat'+variable][:] * (180 / np.pi)
lons = mesh.variables['lon'+variable][:] * (180 / np.pi)

# Create or get the patch file for our current mesh
patchtype="edge"
patchtype="cell"
patch_collection = get_mpas_patches(mesh, type=patchtype, pickleFile=None)

print("Creating a plot ")
## Figure parameters ##
# projection
proj = ccrs.PlateCarree() 
# create figure
plt.close('all')
fig = plt.figure(constrained_layout=False,figsize=(18, 9), dpi=1000)
ax = fig.add_subplot(111, projection=proj)
ax.set_extent([-180.0, 180,-90.0, 90.0], crs=proj)

color_map = cm.gist_ncar
style = 'ggplot'

# decorators
grid_labels_params(ax)
Brazil_states(ax)
map_features(ax)

#patch_collection.set_array(lat[:])
patch_collection.set_linewidths(0.1)
patch_collection.set_linestyle("-")
patch_collection.set_edgecolors("blue")         # No Edge Colors
patch_collection.set_facecolors("white") 
patch_collection.set_antialiaseds(False)    # Blends things a little
patch_collection.set_snap(None)

# Now apply the patch_collection to our axis (ie plot it)
ax.add_collection(patch_collection)

plt.scatter(lons, lats, s=0.001, marker = 'x', color='r', zorder=5)


# Create the title as you see fit
plt.title(variable)
plt.style.use(style) # Set the style that we choose above

plt.savefig(variable+'.png', dpi=500)
patch_collection.remove()
plt.close(fig)