#!/usr/bin/env python
'''
This script creates an initial condition file for MPAS-Ocean.
'''
# import packages {{{
import os
import shutil
import numpy as np
import netCDF4 as nc
from netCDF4 import Dataset
import argparse
# }}}


def main():
    # {{{
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--input_file', dest='input_file',
                        default='base_mesh.nc',
                        help='Input file, containing base mesh'
                        )
    parser.add_argument('-o', '--output_file', dest='output_file',
                        default='initial_state.nc',
                        help='Output file, containing initial variables'
                        )
    parser.add_argument('-L', '--nVertLevels', dest='nVertLevels',
                        default=3,
                        type=int,
                        help='Number of vertical levels'
                        )
    parser.add_argument('-H',
        '--thicknessAllLayers',
        dest='thicknessAllLayers',
        default=1000,
        type=float,
        help='thickness of each layer, [m]')
    thicknessAllLayers = parser.parse_args().thicknessAllLayers
    nVertLevels = parser.parse_args().nVertLevels

    input_file = parser.parse_args().input_file
    output_file = parser.parse_args().output_file
    shutil.copy2(input_file, output_file)
    ds = Dataset(output_file, 'a', format='NETCDF3_64BIT_OFFSET')

    #vertical_init(ds, thicknessAllLayers, nVertLevels)
    tracer_init(ds, thicknessAllLayers)
    #velocity_init(ds)
    #coriolis_init(ds)
    #others_init(ds)

    ds.close()
# }}}


def vertical_init(ds, thicknessAllLayers, nVertLevels):
    # {{{

    # create new variables # {{{
    ds.createDimension('nVertLevels', nVertLevels)
    refLayerThickness = ds.createVariable(
        'refLayerThickness', np.float64, ('nVertLevels',))
    maxLevelCell = ds.createVariable('maxLevelCell', np.int32, ('nCells',))
    refBottomDepth = ds.createVariable(
        'refBottomDepth', np.float64, ('nVertLevels',))
    refZMid = ds.createVariable('refZMid', np.float64, ('nVertLevels',))
    bottomDepth = ds.createVariable('bottomDepth', np.float64, ('nCells',))
    bottomDepthObserved = ds.createVariable(
        'bottomDepthObserved', np.float64, ('nCells',))
    layerThickness = ds.createVariable(
        'layerThickness', np.float64, ('Time', 'nCells', 'nVertLevels',))
    restingThickness = ds.createVariable(
        'restingThickness', np.float64, ('nCells', 'nVertLevels',))
    vertCoordMovementWeights = ds.createVariable(
        'vertCoordMovementWeights', np.float64, ('nVertLevels',))
    # }}}
    ssh = ds.createVariable(
        'ssh', np.float64, ('Time', 'nCells',))

    # evenly spaced vertical grid
    refLayerThickness[:] = thicknessAllLayers
    # make first layer deep to avoid z^2 derivative problems near zero.
    refLayerThickness[0] = 1000

    nVertLevels = len(ds.dimensions['nVertLevels'])
    nCells = len(ds.dimensions['nCells'])
    lonCell = ds.variables['lonCell']
    latCell = ds.variables['latCell']

    latCenterDeg = 0.0 # center point in degrees
    lonCenterDeg = 180.0 # center point in degrees
    sphere_radius = 6371220.
    GaussianWidth = 100e3/ sphere_radius
    
    latCenter = np.deg2rad(latCenterDeg)
    lonCenter = np.deg2rad(lonCenterDeg)

    for iCell in range(0, nCells):
        # Halversine formula for distance
        d = np.sin((latCell[iCell] - latCenter)/2)**2 \
            + np.cos(latCenter)*np.cos(latCell[iCell]) \
            * np.sin((lonCell[iCell] - lonCenter)/2)**2
        ssh[0, iCell] = 1.0*np.exp(-0.5*(d/GaussianWidth)**2.0)
        layerThickness[0,iCell,:]= (thicknessAllLayers + ssh[0,iCell])/nVertLevels
    # Create other variables from refLayerThickness
    refBottomDepth[0] = refLayerThickness[0]
    refZMid[0] = -0.5 * refLayerThickness[0]
    for k in range(1, nVertLevels):
        refBottomDepth[k] = refBottomDepth[k - 1] + refLayerThickness[k]
        refZMid[k] = -refBottomDepth[k - 1] - 0.5 * refLayerThickness[k]
    vertCoordMovementWeights[:] = 1.0

    # flat bottom, no bathymetry
    maxLevelCell[:] = nVertLevels
    bottomDepth[:] = refBottomDepth[nVertLevels - 1]
    bottomDepthObserved[:] = refBottomDepth[nVertLevels - 1]
    for k in range(nVertLevels):
        restingThickness[:, k] = refLayerThickness[k]
# }}}


def tracer_init(ds, thicknessAllLayers):
    S0 = 35 # constant salinity in psu
    T0 = 10 # constant temperature in C

    h = thicknessAllLayers
    # create new variables # {{{
    #temperature = ds.createVariable(
    #    'temperature', np.float64, ('Time', 'nCells', 'nVertLevels',))
    #salinity = ds.createVariable(
    #    'salinity', np.float64, ('Time', 'nCells', 'nVertLevels',))
    #tracer1 = ds.createVariable(
    #    'tracer1', np.float64, ('Time', 'nCells', 'nVertLevels',))
    #tracer2 = ds.createVariable(
    #    'tracer2', np.float64, ('Time', 'nCells', 'nVertLevels',))
    tracer3 = ds.createVariable(
        'tracer3', np.float64, ('Time', 'nCells', 'nVertLevels',))
    # }}}

    # obtain dimensions and mesh variables # {{{
    nVertLevels = len(ds.dimensions['nVertLevels'])
    nCells = len(ds.dimensions['nCells'])
    lonCell = ds.variables['lonCell']
    latCell = ds.variables['latCell']
    # For periodic domains, the max cell coordinate is also the domain width
    Lx = max(lonCell)
    Ly = max(latCell)
    refZMid = ds.variables['refZMid']
    refBottomDepth = ds.variables['refBottomDepth']
    H = max(refBottomDepth)
    # }}}

    for iCell in range(0, nCells):
        x = lonCell[iCell]
        y = latCell[iCell]
        for k in range(0, nVertLevels):
            z = refZMid[k]

            #salinity[0, iCell, k] = S0
            #temperature[0,iCell,k] = T0
            #tracer1[0,iCell,k] = 0.0
            #tracer2[0,iCell,k] = 0.0
            tracer3[0,iCell,k] = np.sin(6*x)*np.cos(4*y)*np.sin(2*z/H*2.0*np.pi)
    #salinity = S0
    #temperature = T0
    #tracer1 = 0.0
    #tracer2 = 0.0

def velocity_init(ds):
    # {{{
    normalVelocity = ds.createVariable(
        'normalVelocity', np.float64, ('Time', 'nEdges', 'nVertLevels',))
    normalVelocity[:] = 0.0
# }}}


def coriolis_init(ds):
    # {{{
    fEdge = ds.createVariable('fEdge', np.float64, ('nEdges',))
    fEdge[:] = 0.0
    fVertex = ds.createVariable('fVertex', np.float64, ('nVertices',))
    fVertex[:] = 0.0
    fCell = ds.createVariable('fCell', np.float64, ('nCells',))
    fCell[:] = 0.0
# }}}


def others_init(ds):
    # {{{
    surfaceStress = ds.createVariable(
        'surfaceStress', np.float64, ('Time', 'nEdges',))
    surfaceStress[:] = 0.0
    atmosphericPressure = ds.createVariable(
        'atmosphericPressure', np.float64, ('Time', 'nCells',))
    atmosphericPressure[:] = 0.0
    boundaryLayerDepth = ds.createVariable(
        'boundaryLayerDepth', np.float64, ('Time', 'nCells',))
    boundaryLayerDepth[:] = 0.0


    # obtain dimensions and mesh variables # {{{
    nVertLevels = len(ds.dimensions['nVertLevels'])
    nCells = len(ds.dimensions['nCells'])
    lonCell = ds.variables['lonCell']
    latCell = ds.variables['latCell']
    # For periodic domains, the max cell coordinate is also the domain width
    Lx = max(lonCell)
    Ly = max(latCell)
    refZMid = ds.variables['refZMid']
    refBottomDepth = ds.variables['refBottomDepth']
    H = max(refBottomDepth)

# }}}


if __name__ == '__main__':
    # If called as a primary module, run main
    main()

# vim: foldmethod=marker ai ts=4 sts=4 et sw=4 ft=python
