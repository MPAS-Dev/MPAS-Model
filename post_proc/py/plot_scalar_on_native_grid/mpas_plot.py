#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script to plot sclar fields on native MPAS grids

Originally From: ??
Edited: Danilo  <danilo.oceano@gmail.com>  in 2023
Last edited: Nov 2023 by P. Peixoto (ppeixoto@usp.br)
Last edited: Nov 2023 by F.A.V.B. Alves (fbalves@usp.br)
Last edited: Mar 2024 by G. Torres Mendonça (guilherme.torresmendonca@ime.usp.br)

"""

import math
import os

import xarray as xr
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs

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
    
def set_plot_kwargs(da=None, clip=False, list_darrays=None, **kwargs):
    plot_kwargs = {k: v for k, v in kwargs.items()
                   if k in ['cmap', 'vmin', 'vmax']
                   and v is not None}
    
    if 'cmap' not in plot_kwargs:
        plot_kwargs['cmap'] = 'Spectral'

    vmin = plot_kwargs.get('vmin', None)
    if vmin is None:
        if da is not None:
            vmin,_ = get_vim_vmax(da,clip)   
            #vmin = np.min(da)
        elif list_darrays is not None:
            vmin = np.min([v.min() for v in list_darrays if v is not None])

    if vmin is not None:
        plot_kwargs['vmin'] = vmin

    vmax = plot_kwargs.get('vmax', None)
    if vmax is None:
        if da is not None:
            _,vmax = get_vim_vmax(da,clip)   
            #vmax = np.max(da)
        elif list_darrays is not None:
            vmax = np.max([v.max() for v in list_darrays if v is not None])

    if vmax is not None:
        plot_kwargs['vmax'] = vmax

    return plot_kwargs

def get_vim_vmax(da,clip=False):
    #Returns good vmin and vmax values to plot da field

    truemaxval = da.max().values
    trueminval = da.min().values

    # Use same range for negative and positive values (zero will always have the same color)
    if (trueminval <= 0) and (truemaxval > 0):
        if truemaxval > abs(trueminval):
            slice = da.values[ da.values > 0]
        else:
            slice = -da.values[ da.values <= 0]
        sigma = np.std(slice)
        m = np.mean(slice)
        if clip:
            maxval = min(truemaxval,m+4*sigma)
        else:
            maxval = truemaxval
        minval = -maxval
 
    # Clip values at mean + 4*std so we still get good color resolution if the solution has a small number of very large numbers
    elif (truemaxval >= 0):
        minval = trueminval
        sigma = np.std(da.values)
        m = np.mean(da.values)
        if clip:
            maxval = min(truemaxval,m+4*sigma)
        else:
            maxval = truemaxval
    else:
        maxval = truemaxval
        sigma = np.std(da.values)
        m = np.mean(da.values)
        if clip:
            minval = max(trueminval,m-4*sigma)
        else:
            minval = trueminval

    return minval, maxval
 
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

def plot_cells_mpas(da, ds, ax, plotEdge=True, gridfile=None, **plot_kwargs):
    # da: specific xarray to be plotted (time/level filtered)
    # ds: general xarray with grid structure, require for grid propreties
    # plotEdge: wether the cell edge should be visible or not. For high-resolution grids figure looks better if plotEdge=False

    # GTM: check if grid properties needed for plotting are in ds
    grid_properties = ['verticesOnCell', 'nEdgesOnCell']

    if set(grid_properties).issubset(set(ds.keys())):
        print (f"{grid_properties} found in dataset.")
        ds_grid = ds
    else:
        print (f"{grid_properties} not found in dataset. "+ 
               "Trying to recover them from additional grid file.")
        try:
            # Open additional grid file
            ds_grid = open_mpas_file(gridfile)
            if set(grid_properties).issubset(set(ds_grid.keys())):
                print (f"{grid_properties} found in additional grid file.")
        except:
            raise RuntimeError(f"Recovery of {grid_properties} failed.")

    # ax = start_cartopy_map_axis()
    print("Generating grid plot and plotting variable. This may take a while...")
    for cell in tqdm(ds['nCells'].values):

        value = da.sel(nCells=cell)

        # GTM: now grid properties are taken from ds_grid, defined above
        vertices = ds_grid['verticesOnCell'].sel(nCells=cell).values
        num_sides = int(ds_grid['nEdgesOnCell'].sel(nCells=cell))
    
        if 0 in vertices[:num_sides]:
            # Border cell
            continue

        # Cel indexation in MPAS starts in 1 (saved in verticesOnCell), 
        #  but for indexing in XARRAY starts with 0 (so -1 the indexes)
        vertices = vertices[:num_sides] - 1
    
        lats = ds_grid['latitudeVertex'].sel(nVertices=vertices)
        lons = ds_grid['longitudeVertex'].sel(nVertices=vertices)
        
        #Set color
        color = colorvalue(value, da, vmin=plot_kwargs['vmin'], vmax=plot_kwargs['vmax'])
        
        # Check if there are polygons at the boarder of the map (+/- 180 longitude)
        # Shift +360 deg the negative longitude
        if max(lons) > 170 and min(lons) < -170 :
            lons = xr.where(lons >= 170.0, lons - 360.0, lons)

        if plotEdge:
            edgecolor = 'grey'
            lw = 0.1
        else:
            edgecolor = None   
            lw = None

        ax.fill(lons, lats, edgecolor=edgecolor, linewidth=lw, facecolor=color)
        
    return

def plot_dual_mpas(da, ds, ax, plotEdge=True, **plot_kwargs):

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
        color = colorvalue(value, da, vmin=plot_kwargs['vmin'], vmax=plot_kwargs['vmax'])
        
        # Check if there are polygons at the boarder of the map (+/- 180 longitude)
        # Shift +360 deg the negative longitude
        if max(lons) > 170 and min(lons) < -170 :
            lons = xr.where(lons >= 170.0, lons - 360.0, lons)

        if plotEdge:
            edgecolor = 'grey'
            lw = 0.1
        else:
            edgecolor = None   
            lw = None

        # Plot polygons and variable
        ax.fill(lons, lats, edgecolor=edgecolor, linewidth=lw, facecolor=color)
    
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


def plot_mpas_darray(ds, vname, time=None, level=None, ax=None, outfile=None, 
                     title=None, plotEdge=True, clip=False, gridfile=None, **kwargs):
    
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
    
    plot_kwargs = set_plot_kwargs(da=da, clip=clip, **kwargs)
    
    if 'nCells' in da.dims: #Plot on Voronoi cells
        plot_cells_mpas(da, ds, ax, plotEdge, gridfile=gridfile, **plot_kwargs)

    elif 'nVertices' in da.dims: #Plot on Triangles
        plot_dual_mpas(da, ds, ax, plotEdge, **plot_kwargs)

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
                            plotEdge=True,
                            clip=False,
                            gridfile=None,
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
                
    plot_mpas_darray(ds, vname, time=time, level=level, ax=ax, 
                     title=tit, plotEdge=plotEdge, clip=clip,
                     gridfile=gridfile)
    
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

    parser.add_argument(
        "-g", "--grid", type=str, default='yes',
        help="Draw grid edges: yes or no",
    )

    parser.add_argument(
        "-c", "--clip", type=str, default='no',
        help="Clip values grater than (expected_val + 4*std): yes or no",
    )

    # GTM: option to supply additional file containing grid properties of infile
    parser.add_argument(
        "-gf", "--gridfile", type=str, default=None,
        help="Name of additional file that contains grid properties of MPAS"
        + " infile (.nc; for use only when infile does not contain these properties)",
    )

    args = parser.parse_args()
    

    if not os.path.exists(args.infile):
        raise IOError('File does not exist: ' + args.infile)

    if args.grid in ['no', 'No', 'N', 'n']:
        plotEdge=False
    else:
        plotEdge=True

    if args.clip in ['yes', 'Yes', 'Y', 'y']:
        clip=True
    else:
        clip=False

    view_mpas_mesh(args.infile, outfile=args.outfile, time=args.time, 
                   level=args.level, vname=args.var, plotEdge=plotEdge,
                   clip=clip, gridfile=args.gridfile)