#!/usr/bin/env python
#
#  Basic script to creat spherical grids for MPAS-Atmosphere
#  by Pedro S. Peixoto Dec 2021 <ppeixoto@usp.br>
#
#  Based on 
#  http://mpas-dev.github.io/MPAS-Tools/stable/mesh_creation.html#spherical-meshes
#  and Jigsaw scripts: https://github.com/dengwirda/jigsaw-python/tree/master/tests
#  
# Pre-requisites:
# 0) Install the conda enviroment MPAS-Tools
# 1) Get the requirements https://github.com/pedrospeixoto/MPAS-Tools/blob/master/conda_package/dev-spec.txt
# 2) Create enviroment
#     $ conda config --add channels conda-forge
#     $ conda create --name mpas-tools --file dev-spec.txt 
# 3) Install the mpas-tools pack
#     $ conda install mpas_tools
# 4) Use it with $ conda activate mpas-tools
#
# - To install jigsaw use conda https://github.com/dengwirda/jigsaw-geo-python/ )
 
import numpy as np
import argparse
import os
import numpy as np
import subprocess
import matplotlib.pyplot as plt

#from mpas_tools.ocean import build_spherical_mesh
#from scipy import interpolate

import jigsawpy as jig

def cellWidthVsLatLon(r=70):
    """
    Create cell width array for this mesh on a regular latitude-longitude grid.
    
    Input
    ---------
    r : float
        constant desired cell width resolution in km

    Returns
    -------
    cellWidth : ndarray
        m x n array of cell width in km
    lon : ndarray
        longitude in degrees (length n and between -180 and 180)
    lat : ndarray
        longitude in degrees (length m and between -90 and 90)
    """
    dlat = r/1000 #Make the lat-lon grid ~ 10x finer than resolution at equator, where 1deg ~ 100km
    dlon = dlat
    constantCellWidth = r  #in km

    nlat = int(180./dlat) + 1
    nlon = int(360./dlon) + 1

    lat = np.linspace(-90., 90., nlat)
    lon = np.linspace(-180., 180., nlon)

    cellWidth = constantCellWidth * np.ones((lat.size, lon.size))
    return cellWidth, lon, lat


def localrefVsLatLon(r=12,l=150, radius_low=50, transition_radius=600,
                     earth_radius=6371.0e3, p=False):
    """
    Create cell width array for this mesh on a locally refined latitude-longitude grid.
    Input
    ---------
    h : float
        grid spacing for high resolution area in km
    l : float
        grid spacing for low resolution area in km
    radius_low : float
        radius of influence of low resolution area in km
    transition_radius : float
        radius of the transition zone between high and low resolution in km 
        
    Returns
    -------
    cellWidth : ndarray
        m x n array of cell width in km
    lon : ndarray
        longitude in degrees (length n and between -180 and 180)
    lat : ndarray
        longitude in degrees (length m and between -90 and 90)
    """
    dlat = 0.5 #Make the lat-lon grid ~ 10x finer than resolution at equator, where 1deg ~ 100km
    dlon = dlat
    constantCellWidth = r  #in km
    print("Trying to set grid spacing of high resolution zone to approximately\
: "+str(constantCellWidth))

    nlat = int(180./dlat) + 1
    nlon = int(360./dlon) + 1

    lat = np.linspace(-90., 90., nlat)
    lon = np.linspace(-180., 180., nlon)

    lons, lats = np.meshgrid(lon, lat)
    #Calculate distances to center (lat=0,lon=0)
    dists = latlon_to_distance_center(lons, lats)

    if p:
        h = plt.contourf(lons, lats, dists)
        plt.axis('scaled')
        plt.colorbar()
        plt.show()

    #Parameters
    #------------------------------

    # Radius (in km) of high resolution area
    maxdist = radius_low
    print("Radius of high resolution area set approximately to: "+str(maxdist))

    distance = transition_radius/10
    print("Transition zone from high to low resolution set approximately to: "+\
          str(transition_radius))
    # (increase_of_resolution) / (distance)
    slope = 10./distance
    # Gammas
    gammas = l
    print("Global grid spacing set to approximately: "+str(gammas))
    
    # distance (in km) of transition zone belt: ratio / slope
    maxepsilons = 10000.
    epsilons = gammas/slope
    
    if(epsilons > maxepsilons):
        print("Transition zone too wide: set to 10,000 km")
        epsilons = maxepsilons
    
    # ## If radius of transition zone is not provided, try to find best value
    # if not(transition_radius):
    #     # distance (in km) of transition zone belt: ratio / slope
    #     maxepsilons = 10000.
    #     epsilons = gammas/slope
        
    #     if(epsilons > maxepsilons):
    #         epsilons = maxepsilons
    #     print("Transition zone radius not provided. Value set to: "+str(epsilons))
    # else:
    #     epsilons = transition_radius
    #     print("Transition zone radius provided: "+str(epsilons))


    # initialize with resolution = r (min resolution)
    resolution = constantCellWidth * np.ones(np.shape(dists))    

    # point in transition zone
    transition_zone = (dists > maxdist) & (dists <= maxdist + epsilons)
    sx = (dists - maxdist ) * slope
    transition_values = constantCellWidth + sx
    resolution = np.where(transition_zone, transition_values, resolution)

    # further points
    far_from_center = (dists > maxdist + epsilons)
    resolution[far_from_center] += epsilons * slope
    
    if p:
        h = plt.contourf(lons, lats, resolution, cmap="viridis", levels=100)
        plt.axis('scaled')
        plt.colorbar()
        plt.show()

    print(np.min(resolution), np.max(resolution))

    cellWidth = resolution #constantCellWidth * np.ones((lat.size, lon.size))
    
    return cellWidth, lon, lat

def density_function_dists(dists, slope=None, gammas=None, maxdist=None,
                           maxepsilons=None):

    epsilons = gammas/slope
    if epsilons > maxepsilons:
        epsilons = maxepsilons

    # initialize with resolution = 1
    resolution = np.ones(np.shape(dists))

    # point in transition zone
    transition_zone = (dists > maxdist) & (dists <= maxdist + epsilons)
    sx = (dists -maxdist ) *slope
    transition_values = 1.0 + sx
    resolution = np.where(transition_zone, transition_values, resolution)

    # further points
    far_from_center = (dists > maxdist + epsilons)
    resolution[far_from_center] += epsilons * slope

    # convert to density
    dens_f = 1 / resolution**2
    return dens_f


def latlon_to_distance_center(lon, lat):
    lon, lat = map(np.radians, [lon, lat])

    haver_formula = np.sin(lat / 2.0) ** 2 + \
                    np.cos(lat) * np.sin(lon / 2.0) ** 2

    dists = 2 * np.arcsin(np.sqrt(haver_formula)) * 6367
    return dists




def jigsaw_gen_sph_grid(cellWidth, x, y, earth_radius=6371.0e3,
    basename="mesh" ):

    """
    A function for building a jigsaw spherical mesh
    Parameters
    ----------
    cellWidth : ndarray
        The size of each cell in the resulting mesh as a function of space
    x, y : ndarray
        The x and y coordinates of each point in the cellWidth array (lon and
        lat for spherical mesh)
    on_sphere : logical, optional
        Whether this mesh is spherical or planar
    earth_radius : float, optional
        Earth radius in meters
    """
    # Authors
    # -------
    #by P. Peixoto in Dec 2021
    # based on MPAS-Tools file from Mark Petersen, Phillip Wolfram, Xylar Asay-Davis 

    
    # setup files for JIGSAW
    opts = jig.jigsaw_jig_t()
    opts.geom_file = basename+'.msh'
    opts.jcfg_file = basename+'.jig'
    opts.mesh_file = basename+'-MESH.msh'
    opts.hfun_file = basename+'-HFUN.msh'

    # save HFUN data to file
    hmat = jig.jigsaw_msh_t()
    
    hmat.mshID = 'ELLIPSOID-GRID'
    hmat.xgrid = np.radians(x)
    hmat.ygrid = np.radians(y)
    hmat.value = cellWidth
    jig.savemsh(opts.hfun_file, hmat)

    # define JIGSAW geometry
    geom = jig.jigsaw_msh_t()
    geom.mshID = 'ELLIPSOID-MESH'
    geom.radii = earth_radius*1e-3*np.ones(3, float)
    jig.savemsh(opts.geom_file, geom)

    # build mesh via JIGSAW!
    opts.hfun_scal = 'absolute'
    opts.hfun_hmax = float("inf")
    opts.hfun_hmin = 0.0
    opts.mesh_dims = +2  # 2-dim. simplexes
    opts.mesh_iter = 500000
    opts.optm_qlim = 0.9375
    opts.optm_qtol = 1.0e-6
    opts.optm_iter = 500000
    opts.verbosity = +1
    jig.savejig(opts.jcfg_file, opts)
    
    #Call jigsaw
    process = subprocess.call(['jigsaw', opts.jcfg_file])

    return opts.mesh_file 

def jigsaw_gen_icos_grid(basename="mesh", level=4):

    # setup files for JIGSAW
    opts = jig.jigsaw_jig_t()
    icos = jig.jigsaw_msh_t()
    geom = jig.jigsaw_msh_t()

    opts.geom_file = basename+'.msh'
    opts.jcfg_file = basename+'.jig'
    opts.mesh_file = basename+'-MESH.msh'

    geom.mshID = "ellipsoid-mesh"
    geom.radii = np.full(3, 1.000E+000, dtype=geom.REALS_t)
        
    jig.savemsh(opts.geom_file, geom)

    opts.hfun_hmax = +1.
    opts.mesh_dims = +2                 # 2-dim. simplexes
    opts.optm_iter = +512
    opts.optm_qtol = +1.0E-06
    
    jig.cmd.icosahedron(opts, level, icos)    

    return opts.mesh_file 
