#!/usr/bin/env python
"""
% Create cell width for this mesh on a regular latitude-longitude grid.
% Outputs:
%    cellWidth - m x n array, entries are desired cell width in km
%    lon - longitude, vector of length m, with entries between -180 and 180, degrees
%    lat - latitude, vector of length n, with entries between -90 and 90, degrees
"""
from __future__ import absolute_import, division, print_function, \
    unicode_literals

import numpy as np


def cellWidthVsLatLon():

    ddeg = 10
    constantCellWidth = 60

    lat = np.arange(-90, 90.01, ddeg)
    lon = np.arange(-180, 180.01, ddeg)

    cellWidth = constantCellWidth * np.ones((lat.size, lon.size))
    return cellWidth, lon, lat
