#!/usr/bin/env python
"""
This script culls all cells except for a lat/lon patch
"""

from __future__ import absolute_import, division, print_function, \
    unicode_literals

import os
import os.path
import subprocess
import numpy
#from optparse import OptionParser
import xarray

from mpas_tools import conversion
from mpas_tools.io import write_netcdf

# may add input later
# parser = OptionParser()
# parser.add_option("-p", "--geom_data_path", type="string", dest="path",
#                   default="geometric_data",
#                   help="Path to the geometric_data from the geometric_features"
#                        " repository.")
# options, args = parser.parse_args()

minlat = numpy.deg2rad(30.)
maxlat = numpy.deg2rad(40.)
minlon = numpy.deg2rad(30.)
maxlon = numpy.deg2rad(40.)

# required for compatibility with MPAS
netcdfFormat = 'NETCDF3_64BIT'

# Create the land mask based on the land coverage, i.e. coastline data.
dsBaseMesh = xarray.open_dataset('base_mesh.nc')

# These are like pointers -- no copies are made
# each is an xarray.DataArray
lat = dsBaseMesh.latCell
lon = dsBaseMesh.lonCell

# This is a bool type xarray.DataArray
landMask = numpy.logical_and(numpy.logical_and(lat >= minlat, lat <= maxlat),
                             numpy.logical_and(lon >= minlon, lon <= maxlon))

# Convert logical to int
landMask = landMask.astype(int)

dsLandMask = xarray.Dataset()
dsLandMask['cullCell'] = landMask

dsCulledMesh = conversion.cull(dsBaseMesh, dsMask=dsLandMask,
                               graphInfoFileName='culled_graph.info')
write_netcdf(dsCulledMesh, 'culled_mesh.nc', format=netcdfFormat)

args = ['paraview_vtk_field_extractor.py',
        '--ignore_time',
        '-d', 'maxEdges=',
        '-v', 'allOnCells',
        '-f', 'culled_mesh.nc',
        '-o', 'culled_mesh_vtk']
print("running", ' '.join(args))
subprocess.check_call(args, env=os.environ.copy())
