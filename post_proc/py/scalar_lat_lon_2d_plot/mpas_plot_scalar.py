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

from mpas_patches import get_mpas_patches, plot_var_in_patch

parser = argparse.ArgumentParser()

parser.add_argument('file', 
                    type=str, 
                    help='''File you want to plot from''',
                    nargs='+'
                    )

parser.add_argument('-v',
                    '--var', 
                    type=str,
                    default='pressure',
                    help='''Variable you want to plot from that file''')


args = parser.parse_args()
variable = args.var

file = args.file[0]
compare = False
nFiles = len(args.file)
if nFiles>1:
    print(args.file)
    compare = True
    files = args.file

#Check ref file existence
if not os.path.isfile(file):
    print("That file was not found :(")
    sys.exit(-1)

#Create dir for output
dir = "figures/"+variable
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
vars = []
if nFiles>1:
    meshes = []
    for i, f in enumerate(files):
        meshes.append(Dataset(os.path.join(f), 'r'))
        vars.append(meshes[i].variables[variable])

dims = var.dimensions
shap = var.shape

nlevels = shap[2]
ntimes = shap[0]
print("nlevels:", nlevels)
if 'nEdges' in dims:
    patchtype="edge"
else:
    patchtype="cell"

if( variable == "u"):
    #plot assuming zonal wind, since otherwise it contains incomplete wind information
    cosangleedge=np.cos(mesh.variables['angleEdge'])
    cosangleedge[cosangleedge==0]=np.nan
    
#Get MPAS grid patches, for either cells or edges (triangles need to be implmented!!)
patch_collection = get_mpas_patches(mesh, type=patchtype, pickleFile=None)


'''
Select levels and time instants for plotting
'''
levels = [1] #range(nlevels)
times = range(ntimes)
for l in levels:
    for t in times:

        if nFiles==1:
            title = variable+' at time '+str(t)+' and at level '+str(l)
            outfile = dir+"/"+variable+'_'+str(t)+'_'+str(l)+'.png'
            var_tmp = var[t,:,l]
        else:
            title = variable+' diff at time '+str(t)+' and at level '+str(l)
            outfile = dir+"/"+variable+'_diff_'+str(t)+'_'+str(l)+'.png'
            var_tmp = vars[1][t,:,l]-var[t,:,l]

        print("Creating a plot of ", title, outfile)
        #Plot variables in patches
        print(var_tmp)
        if(variable=="u"):
            #Try to write the zonal velocity
            var_tmp = var_tmp/cosangleedge
            
        plot_var_in_patch(var_tmp, patch_collection, variable, title, outfile)
            



