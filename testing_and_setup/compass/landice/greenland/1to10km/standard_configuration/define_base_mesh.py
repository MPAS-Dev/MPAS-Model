#!/usr/bin/env python
from __future__ import absolute_import, division, print_function, \
    unicode_literals

import numpy as np
import netCDF4
import jigsawpy

def cellWidthVsXY():
   f = netCDF4.Dataset('/Users/mhoffman/Documents/greenland_geometry/jkennedy_20190227/greenland_1km_2019_02_11.epsg3413.nc','r')

   x1 = f.variables['x1'][:].data
   y1 = f.variables['y1'][:].data
   thk = f.variables['thk'][0,:,:].data
   cell_width = 20000.0 * np.ones(thk.shape)

   xx0=x1.min(); xx1=x1.max();
   yy0=y1.min(); yy1=y1.max();
   geom_points = np.array([ # list of xy "node" coordinates
       ((xx0, yy0), 0),
       ((xx1, yy0), 0),
       ((xx1, yy1), 0),
       ((xx0, yy1), 0)],
       dtype=jigsawpy.jigsaw_msh_t.VERT2_t)

   geom_edges = np.array([    # list of "edges" between nodes
       ((0, 1), 0),
       ((1, 2), 0),
       ((2, 3), 0),
       ((3, 0), 0)],
       dtype=jigsawpy.jigsaw_msh_t.EDGE2_t)

   return cell_width.astype('float64'), x1.astype('float64'), y1.astype('float64'), geom_points, geom_edges
