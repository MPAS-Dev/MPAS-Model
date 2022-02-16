''' 
File - mpas_plot_pressure.py
Author - Miles A. Curry (mcurry@ucar.edu)
Date - April 2019

This python file provides an example of plotting an MPAS field onto an MPAS
mesh. This plot produces pcolor like plots by created a collection of polygons
(MatPltLib patches) of the MPAS mesh. Thus, for large meshes this routine is
very slower.

However, this file has been provided with the `mpas_patches.py` which provides
a function `get_mpas_patches` that will autmoatcially produce such polygons as
a 'patch collection'. For large meshes, producing the patch collection is
costly and timely so `get_mpas_patches` will produce a python 'pickle' file, so
that it will only need to be produced once. This will greatly speed up the time
it takes to on subsequent visualiations.

Note: This 'clean' version of this example contains less comments and documentation,
but is the same as mpas_plot_pressure.py.

This file was created with great help and reference from:
* https://github.com/lmadaus/mpas_python

*Modified by P. Peixoto in Nov 2021 <ppeixoto@usp.br>
'''

import os
import sys
import argparse

from netCDF4 import Dataset
import numpy as np

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap

from mpas_patches import get_mpas_patches
    
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

# Open the NetCDF file and pull out the var at the given levels.
# Check to see if the mesh contains the variable
if not os.path.isfile(file):
    print("That file was not found :(")
    sys.exit(-1)

# Open the mesh using NetCDF4 Dataset.
mesh = Dataset(os.path.join(file), 'r')

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


# Normalize latitude and longitude
lons[lons > 180.0] = lons[lons > 180.] - 360.
lons[lons < -180.0] = lons[lons < -180.] + 360.

# Create or get the patch file for our current mesh
patch_collection = get_mpas_patches(mesh, pickleFile=None)

# Initalize Basemap
bmap = Basemap(projection='cyl', llcrnrlat=-90, urcrnrlat=90, llcrnrlon=-180, urcrnrlon=180, resolution='l')


color_map = cm.gist_ncar
style = 'ggplot'

print("Creating a plot ")

fig = plt.figure(figsize=(18, 9), dpi=800)
ax = plt.gca()


#bmap.drawcoastlines()

#bmap.drawparallels(range(-90, 90, 30), linewidth=1, labels=[1,0,0,0], color='b')
#bmap.drawmeridians(range(-180, 180, 45), linewidth=1, labels=[0,0,0,1], color='b', rotation=45)

#patch_collection.set_array(lat[:])
patch_collection.set_linewidths(0.1)
patch_collection.set_linestyle("-")
patch_collection.set_edgecolors("blue")         # No Edge Colors
patch_collection.set_facecolors("white") 
#patch_collection.set_antialiaseds(False)    # Blends things a little
#patch_collection.set_cmap(color_map)        # Select our color_map
patch_collection.set_snap(None)

# Now apply the patch_collection to our axis (ie plot it)
ax.add_collection(patch_collection)
#cbar = plt.colorbar(patch_collection)
#cbar.set_label('Pressure (Pa)')

lons, lats = bmap(lons, lats) #, marker = '.', color='r')

bmap.scatter(lons, lats, s=1, marker = '.', color='r', zorder=5)


# Create the title as you see fit
plt.title(variable)
plt.style.use(style) # Set the style that we choose above

plt.savefig(variable+'.png', dpi=500)
patch_collection.remove()
plt.close(fig)
