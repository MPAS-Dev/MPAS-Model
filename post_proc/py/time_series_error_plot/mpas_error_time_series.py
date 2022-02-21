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

This file was created with great help and reference from:
* https://github.com/lmadaus/mpas_python

* Edited by P. Peixoto on Feb 2022

'''

import os
import sys
import argparse
import numpy as np
from datetime import datetime, timedelta

from netCDF4 import Dataset

''' By default matplotlib will try to open a display windows of the plot, even
though sometimes we just want to save a plot. Somtimes this can cause the
program to crash if the display can't open. The two commands below makes it so
matplotlib doesn't try to open a window
'''
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

'''
cm = Color Map. Within the matplotlib.cm module will contain access to a number
of colormaps for a plot. A reference to colormaps can be found at:

    - https://matplotlib.org/examples/color/colormaps_reference.html
'''
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
                    default='pressure',
                    help='''Variable you want to plot from that file''')

args = parser.parse_args()
variable = args.var
file = args.file

# Open the NetCDF file and pull out the var at the given levels.
# Check to see if the mesh contains the variable
if not os.path.isfile(file):
    print("That file was not found :(")
    sys.exit(-1)

dir = variable
if not os.path.isdir(dir):
    os.makedirs(dir)

'''
Open the mesh using NetCDF4 Dataset.
'''
mesh = Dataset(os.path.join(file), 'r')

# Check to see the variable is in the mesh
if variable not in mesh.variables.keys(): 
    print("That variable was not found in this mpas mesh!")
    print(mesh.variables.keys())
    sys.exit(-1)

# Pull the variable out of the mesh. Now we can manipulate it any way we choose
# do some 'post-processing' or other meteorological stuff
var = mesh.variables[variable]

dims = var.dimensions
shap = var.shape
print(dims)
print(shap)
nlevels = shap[2]
ntimes = shap[0]
print("nlevels:", nlevels)

#Set time series periods
duration=mesh.__dict__['config_run_duration']
dys, time = duration.split("_")
t = datetime.strptime(time,"%H:%M:%S")
dur = timedelta(days=int(dys), hours=t.hour, minutes=t.minute, seconds=t.second)
t_ini = timedelta(days=0, hours=0, minutes=0, seconds=0)
delta = dur/(ntimes-1)
print(delta)

ts = []
for i in range(ntimes):
    t = t_ini+i*delta
    hours = t.total_seconds()/3600
    ts.append(hours)

f=open(dir+"/"+variable+"_errors.csv",'w')
np.savetxt(f, [ts], delimiter =", ",   fmt ='% s')

print(ts)
print()
#exit(1)

''' In this example, we will be plotting actual MPAS polygons. The
`get_mpas_patches` function will create a collection of patches for the current
mesh for us AND it will save it, so that later we do not need to create it
again (Because often time creation is very slow).

If you have a PickleFile someone where can supply it as the pickleFile argument
to the `get_mpas_patches` function.

Doing things this way is slower, as we will have to not only loop through
nCells, but also nEdges of all nCells.
'''
patch_collection = get_mpas_patches(mesh, pickleFile=None)

'''  Initialize Basemap

Basemap handles all things map projections. It can translate between one map
projection to another, drawcoastliens, draw latitude liens and a number of map
related things. I encourage you to check out this tutorial here:

    - https://basemaptutorial.readthedocs.io/en/latest/

As well as the official documentation

    - https://matplotlib.org/basemap/index.html
    
'''
bmap = Basemap(projection='cyl', 
               llcrnrlat=-90,
               urcrnrlat=90,
               llcrnrlon=0,
               urcrnrlon=360,
               resolution='l')


''' Colormaps can be choosen using MatPlotLib's colormaps collection. A
reference of the colormaps can be found below.:

- https://matplotlib.org/examples/color/colormaps_reference.html

We can also alter the styles of the plots we produce if we desire:

- https://matplotlib.org/gallery/style_sheets/style_sheets_reference.html

'''
#color_map = cm.gist_ncar
color_map = cm.bwr
style = 'ggplot'

'''
Make plots at vertical levels that is specified the range below, not this will
be vertical plots, 0, 1, 2, 3, and 4 and for all the times in this mesh file
(if there are any).
'''

print(mesh)

levels = range(nlevels)
times = range(ntimes)
print(len(levels))
refs = []
for l in levels:
    refs.append(var[0,:,l])

print(refs[0])



for l in levels:
    ts_error = []
    for t in times:

        ''' A figure is the final image that contains one or more axes. In this
        case we will produce three figures, all with three axes. Each figure is
        saved to its own file.
        '''

        print("Creating a plot of ", variable, " at ", l, " level and time", t, dir+"/"+variable+'_'+str(t)+'_'+str(l)+'.png')
        fig = plt.figure()
        ax = plt.gca()

        bmap.drawcoastlines()

        ''' Basemap allows latitude and longitude lines to be drawn with ease
        and much flexibility. The only thing that is required of you is to
        select the latitude or longitude lines you want respectivly. Everything
        else is optional.

        Easily select a range using python's `range` builtin. Range is a handy
        function that will create a list and is useful in loops and array
        creation. It is defined as:

            my_range = range(start, end, stride)

        Note, that this will not include end.

            my_range1 = range(2, 10, 2)  # [2, 4, 6, 8]
            my_range2 = range(3)         # [0, 1, 2]
            my_range3 = range(1, 3)      # [1, 2]

        '''
        bmap.drawparallels(range(-90, 90, 30), 
                           linewidth=1, 
                           labels=[1,0,0,0],
                           color='b')
        bmap.drawmeridians(range(0, 360, 45),
                          linewidth=1, 
                          labels=[0,0,0,1],
                          color='b',
                          rotation=45)


        ''' For plotting MPAS meshes, set the patch_color ro the variable that
        we are plotting: var. Here we are taking the 't' time and the 'l'
        level while pulling out the pressure values ie: `var[t,:,l]`

        
        We could also choose to write the following as:

            pressure = var[t,:,l]
            patch_collection.set_array(pressure)

        But I have choosen the way below for the brevity.
        '''
        error = var[t,:,l] - refs[l]       
        ts_error_tmp= np.sqrt(np.mean(error**2))
        ts_error.append(ts_error_tmp)
        #print(l, t, ts_error)
        vmax=np.max(error)
        vmin=np.min(error)
        patch_collection.set_array(error)
        #patch_collection.set_edgecolors("")         # No Edge Colors
        patch_collection.set_antialiaseds(True)    # Blends things a little
        patch_collection.set_cmap(color_map)        # Select our color_map
        #patch_collection.set_norm(None)
        patch_collection.set_clim(vmin, vmax)
        ''' Now apply the patch_collection to our axis '''
        ax.add_collection(patch_collection)

        '''
        Add a colorbar (if desired), and add a label to it. In this example the
        color bar will automatically be generated. See ll-plotting for a more
        advance colorbar example.

        https://matplotlib.org/api/colorbar_api.html
        '''
        plt.rcParams['axes.grid'] = False
        cbar = plt.colorbar(patch_collection, fraction=0.046, pad=0.04)
        cbar.set_label(variable)
        

        ''' Create the title as you see fit '''
        plt.title(variable+' at time '+str(t)+' and at level '+str(l))
        plt.style.use(style) # Set the style that we choose above

        ''' Save the file, remove the patch_collection, and close the figure.
        You'll need to always remove the patch_collection when generating
        plots on the same collection, else MPL will complain.
        '''
        plt.savefig(dir+"/"+variable+'_'+str(t)+'_'+str(l)+'error.png')
        patch_collection.remove()
        plt.close(fig)
        
    
    print( "ploting error time series")
    fig = plt.figure()
    ax = plt.gca()
    #print(ts, len(ts))
    #print(ts_error, len(ts_error))
    ax.plot(ts, ts_error)
    plt.title(variable+' at level '+str(l))
    plt.savefig(dir+"/"+variable+'_'+str(l)+'_ts_error.png')
    print( "-----------------------------------------")
    print( )

    np.savetxt(f, [ts_error], delimiter =", ",   fmt ='% s')

f.close()