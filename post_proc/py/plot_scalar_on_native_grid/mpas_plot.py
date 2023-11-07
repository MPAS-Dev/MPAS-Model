#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script to plot sclar fields on native MPAS grids

Originally From: ??
Edited: Danilo  <danilo.oceano@gmail.com>  in 2023
Last edited: Nov 2023 by P. Peixoto (ppeixoto@usp.br)

"""

import math
import os

import xarray as xr
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from geopy.distance import distance

import argparse

from tqdm import tqdm

def add_mpas_mesh_variables(ds, full=True, **kwargs):
    for v in ds.data_vars:
        if v not in derived_variables:
            continue

        newvs = derived_variables[v]

        for newv in newvs:
            if newv in ds:
                #print(newv + ' already here')
                continue

            if 'lat' in v or 'lon' in v:
                ds[newv] = xr.apply_ufunc(np.rad2deg, ds[v])
                ds[newv] = ds[newv].where(ds[newv] <= 180.0, ds[newv] - 360.0)
                ds[newv].attrs['units'] = 'degrees'

            elif newv == 'area':
                radius_circle = ds.attrs.get('sphere_radius', 1.0)
                if radius_circle == 1:
                    # need to correct to earth radius
                    correction_rad_earth = 6371220.0
                else:
                    correction_rad_earth = 1

                ds[newv] = (ds[v] / 10 ** 6) * correction_rad_earth**2
                ds[newv].attrs['units'] = 'km^2 (assuming areaCell in m^2)'
                ds[newv].attrs['long_name'] = 'Area of the cell in km^2'

            elif newv == 'resolution':
                radius_circle = ds.attrs.get('sphere_radius', 1.0)
                if radius_circle == 1.0:
                    #print('need to correct to earth radius!!')
                    correction_rad_earth = 6371220.0
                else:
                    correction_rad_earth = 1

                # km^2 (assuming areaCell in m^2)
                area = (ds[v] / 10 ** 6) * correction_rad_earth**2

                ds[newv] = 2 * (xr.apply_ufunc(np.sqrt, area / math.pi))
                ds[newv].attrs['units'] = 'km'
                ds[newv].attrs['long_name'] = 'Resolution of the cell (approx)'

    return ds


def open_mpas_file(file, **kwargs):

    ds = xr.open_dataset(file)

    ds = add_mpas_mesh_variables(ds, **kwargs)

    return ds

def start_cartopy_map_axis(zorder=1):

    ax = plt.axes(projection=ccrs.PlateCarree())  # projection type
    
    add_cartopy_details(ax, zorder=zorder)

    return ax

def add_cartopy_details(ax, zorder=1):

    #Country boarders
    #ax.add_feature(cfeature.BORDERS, linestyle=':', zorder=zorder, linewidth=0.1)

    #Coastlines
    ax.coastlines(resolution='10m', zorder=zorder+1, linewidth=0.1)

    # Reference gridlines
    gl = ax.gridlines(draw_labels=True, alpha=0.5, linestyle='--',
                      zorder=zorder+2, linewidth=0.5)
    gl.top_labels = False
    gl.right_labels = False
    
def set_plot_kwargs(da=None, list_darrays=None, **kwargs):
    plot_kwargs = {k: v for k, v in kwargs.items()
                   if k in ['cmap', 'vmin', 'vmax']
                   and v is not None}
    
    if 'cmap' not in plot_kwargs:
        plot_kwargs['cmap'] = 'Spectral'

    vmin = plot_kwargs.get('vmin', None)
    if vmin is None:
        if da is not None:
            vmin = da.min().values   
            #vmin = np.min(da)
        elif list_darrays is not None:
            vmin = np.min([v.min() for v in list_darrays if v is not None])

    if vmin is not None:
        plot_kwargs['vmin'] = vmin

    vmax = plot_kwargs.get('vmax', None)
    if vmax is None:
        if da is not None:
            vmax = da.max().values   
            #vmax = np.max(da)
        elif list_darrays is not None:
            vmax = np.max([v.max() for v in list_darrays if v is not None])

    if vmax is not None:
        plot_kwargs['vmax'] = vmax

    return plot_kwargs

def colorvalue(val, da, vmin=None, vmax=None, cmap='Spectral'):
    """
    Given a value and the range max, min, it returns the associated
    color of the desired cmap.
    :param val: float
    :param da: xarray data
    :param vmin: float (default None)
    :param vmax: float (default None)
    :param cmap: str
    :return: cm(norm_val): color
    """
    
    # Get a colormap instance, defaulting to rc values if name is None.
    cm = mpl.colormaps[cmap] #cm.get_cmap(cmap, None)
    if vmin is None:
        vmin = da.min().values #xr.DataArray.min().values  # min value of the array
    if vmax is None:
        vmax = da.max().values #xr.DataArray.max().values  # max value of the array

    if vmin == vmax:
        # A class which, when called, linearly normalizes data into the
        # [0.0, 1.0] interval.
        norm_val = mpl.colors.Normalize(vmin=vmin - 1, vmax=vmax + 1, clip=True)(val)
    else:
        norm_val = mpl.colors.Normalize(vmin=vmin, vmax=vmax, clip=True)(val)
        
    return cm(norm_val)

def plot_cells_mpas(da, ds, ax, **plot_kwargs):
    # da: specific xarray to be plotted (time/level filtered)
    # ds: general xarray with grid structure, require for grid propreties

    # ax = start_cartopy_map_axis()
    print("Generating grid plot and plotting variable. This may take a while...")
    for cell in tqdm(ds['nCells'].values):

        value = da.sel(nCells=cell)

        vertices = ds['verticesOnCell'].sel(nCells=cell).values
        num_sides = int(ds['nEdgesOnCell'].sel(nCells=cell))
        
        if 0 in vertices[:num_sides]:
            # Border cell
            continue

        # Cel indexation in MPAS starts in 1 (saved in verticesOnCell), 
        #  but for indexing in XARRAY starts with 0 (so -1 the indexes)
        vertices = vertices[:num_sides] - 1
        
        lats = ds['latitudeVertex'].sel(nVertices=vertices)
        lons = ds['longitudeVertex'].sel(nVertices=vertices)
        
        #Set color
        maxval = da.max().values
        minval = da.min().values
        color = colorvalue(value, da, vmin=minval, vmax=maxval)
        
        # Check if there are polygons at the boarder of the map (+/- 180 longitude)
        # Shift +360 deg the negative longitude
        if max(lons) > 170 and min(lons) < -170 :
            lons = xr.where(lons >= 170.0, lons - 360.0, lons)
            
        ax.fill(lons, lats, edgecolor='grey', linewidth=0.1, facecolor=color)
        
    return

def plot_dual_mpas(da, ds, ax, **plot_kwargs):

    #Loop over all Voronoi cell vertices - which are triangle circumcentres
    print("Generating grid plot and plotting variable. This may take a while...")
    for vertex in tqdm(ds['nVertices'].values):

        #Triangle value
        value = da.sel(nVertices=vertex)

        #The triangle is formed by connecting 3 cell nodes
        cells = ds['cellsOnVertex'].sel(nVertices=vertex).values
        
        if 0 in cells:
            # Border triangle
            continue

        #Indexing given from mpas starts in 1, so adjust to start in 0
        cells = cells - 1
        lats = ds['latitude'].sel(nCells=cells)
        lons = ds['longitude'].sel(nCells=cells)
        
        #Set color
        maxval = da.max().values
        minval = da.min().values        
        color = colorvalue(value, da, vmin=minval, vmax=maxval)
        
        # Check if there are polygons at the boarder of the map (+/- 180 longitude)
        # Shift +360 deg the negative longitude
        if max(lons) > 170 and min(lons) < -170 :
            lons = xr.where(lons >= 170.0, lons - 360.0, lons)

        # Plot polygons and variable
        ax.fill(lons, lats, edgecolor='grey', linewidth=0.1, facecolor=color)
    
    return
        
def add_colorbar(axs, fig=None, label=None, **plot_kwargs):
    if fig is None:
        fig = plt.gcf()

    try:
        x = axs[0, 0]
    except:
        try:
            x = axs[0]
            n = len(axs)
        except:
            axs = np.array([axs]).reshape([1, 1])
        else:
            axs = axs.reshape([n, 1])

    cbar = fig.colorbar(
        mpl.cm.ScalarMappable(
            norm=mpl.colors.Normalize(vmin=plot_kwargs['vmin'],
                                      vmax=plot_kwargs['vmax'], clip=True),
            cmap=plot_kwargs['cmap']),
        ax=axs[:, :], shrink=0.6)
    cbar.ax.locator_params(nbins=10)
    if label is not None:
        cbar.set_label(label)

    return

def close_plot(fig=None, size_fig=None, pdf=None, outfile=None,
               force_show=False):

    if size_fig is None:
        size_fig = [10, 8]

    if fig is None:
        fig = plt.gcf()
    fig.set_size_inches(size_fig)

    if outfile is not None:
        plt.savefig(outfile, dpi=800)

    if pdf is not None:
        pdf.savefig(fig, dpi=800)

    if (outfile is None and pdf is None) or force_show:
        plt.show()

    plt.close()


def plot_mpas_darray(ds, vname, time=None, level=None, ax=None, outfile=None, title=None, **kwargs):
    
    ## plot_mpas_darray
    da = ds[vname]
    
    if 'Time' in da.dims:
        print()    
        if time not in da['Time'].values:
            print("Proposed time slice not available:", time," Timesteps:", da['Time'].values)
            print("  Setting time slice to zero.")
            time = 0
        else:
            print('Selecting time slice '+ str(time) + '.')

        da = da.isel({'Time': time})      

    
    if 'nVertLevels' in da.dims:
        print()    
        if level not in da['nVertLevels'].values:
            print("Proposed vertical level slice not available.", level," Levels:", da['nVertLevels'].values)
            print("  Setting level to zero.")
            level = 0
        else:
            print('Selecting vertical level '+ str(time) + '.')

        da = da.isel({'nVertLevels': level})      
    print("\n Data to be plotted")    
    print(da)
    print()

    ax.set_extent([-180.0, 180,-90.0, 90.0], crs=ccrs.PlateCarree())
    
    plot_kwargs = set_plot_kwargs(da=da, **kwargs)
    
    if 'nCells' in da.dims: #Plot on Voronoi cells
        plot_cells_mpas(da, ds, ax, **plot_kwargs)

    elif 'nVertices' in da.dims: #Plot on Triangles
        plot_dual_mpas(da, ds, ax, **plot_kwargs)

    else: # TO DO: Implement ploting edge quantities
        print('WARNING  Impossible to plot!')
    
        
    if title == None:
        title = vname
    
    ax.set_title(title)
    
    add_colorbar(ax, label=vname + ' (' + da.attrs.get('units', '') + ')', **plot_kwargs)

    return 

def view_mpas_mesh(mpas_grid_file, outfile=None,
                            vname='resolution',
                            time=None,
                            level=None,
                            **kwargs):
    
    ds = open_mpas_file(mpas_grid_file)

    if vname not in ds.data_vars:
        print('Unplottable Data Array ' + vname)
        print('Available variables:', list(ds.keys()))
        return

    units = ds[vname].attrs.get('units', '')
    ncells = str(len(ds[vname].values.flatten()))
    name = os.path.basename(mpas_grid_file)
    print(vname, units, ncells, name, ds[vname])

    ax = start_cartopy_map_axis(zorder=2)

    tit = vname + ': ' + name + ' (' + str(ncells) + ')'
    if time is not None:
        tit = tit + " Timestep="+str(time)
    if level is not None:
        tit = tit + " Level="+str(level)
                
    plot_mpas_darray(ds, vname, time=time, level=level, ax=ax, title=tit)
    
    close_plot(outfile=outfile)
        
    
if __name__ == "__main__":
    
    derived_variables = {
        'latCell': ['latitude'],
        'lonCell': ['longitude'],
        'latVertex': ['latitudeVertex'],
        'lonVertex': ['longitudeVertex'],
        'areaCell': ['area', 'resolution'],
    }
    
    parser = argparse.ArgumentParser(
    description=__doc__,
    formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument(
        "-f", "--infile", type=str, required=True,
        help="Name of an MPAS grid or data file (.nc)",
    )
    
    parser.add_argument(
        "-o", "--outfile", type=str, default=None,
        help="File to save the MPAS plot",
    )
    parser.add_argument(
        "-v", "--var", type=str, default='resolution',
        help="Variable to be plotted",
    )

    parser.add_argument(
        "-l", "--level", type=int, default=None,
        help="Vertical level",
    )

    parser.add_argument(
        "-t", "--time", type=int, default=None,
        help="Time step",
    )

    args = parser.parse_args()
    

    if not os.path.exists(args.infile):
        raise IOError('File does not exist: ' + args.infile)
    
    view_mpas_mesh(args.infile, outfile=args.outfile, time=args.time, level=args.level, vname=args.var)