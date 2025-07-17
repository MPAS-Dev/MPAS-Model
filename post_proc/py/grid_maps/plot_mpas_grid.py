#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
plot_mpas_grid.py

Script for visualizing MPAS native grid, displaying grid cells on a map.
It allows for plotting variables such as resolution, area, latitude, and longitude of the grid cells
and vertices.
It has options for zooming into a specific region and saving the plot to a file.

author: Danilo Couto de Souza
date: 2025-07-17
contact: danilo.oceano@gmail.com
"""

import math
import os
import argparse
import numpy as np
import xarray as xr
import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature

def add_mpas_mesh_variables(ds, derived_variables, **kwargs):
    """
    Adds derived variables to the MPAS grid, such as latitude, longitude, area, etc.
    """
    for v in ds.data_vars:
        if v not in derived_variables:
            continue

        newvs = derived_variables[v]
        for newv in newvs:
            if newv in ds:
                continue

            if 'lat' in v or 'lon' in v:
                ds[newv] = xr.apply_ufunc(np.rad2deg, ds[v])
                ds[newv] = ds[newv].where(ds[newv] <= 180.0, ds[newv] - 360.0)
                ds[newv].attrs['units'] = 'degrees'

            elif newv == 'area':
                radius_circle = ds.attrs.get('sphere_radius', 1.0)
                correction_rad_earth = 6371220.0 if radius_circle == 1 else 1
                ds[newv] = (ds[v] / 10 ** 6) * correction_rad_earth**2
                ds[newv].attrs['units'] = 'km^2 (assuming areaCell in m^2)'

            elif newv == 'resolution':
                radius_circle = ds.attrs.get('sphere_radius', 1.0)
                correction_rad_earth = 6371220.0 if radius_circle == 1 else 1
                area = (ds[v] / 10 ** 6) * correction_rad_earth**2
                ds[newv] = 2 * (xr.apply_ufunc(np.sqrt, area / math.pi))
                ds[newv].attrs['units'] = 'km'

    return ds


def open_mpas_file(file, derived_variables, **kwargs):
    """
    Opens and processes an MPAS file.
    """
    try:
        ds = xr.open_dataset(file)
        return add_mpas_mesh_variables(ds, derived_variables, **kwargs)
    except Exception as e:
        raise IOError(f"Failed to open file {file}: {e}")


def set_plot_kwargs(da=None, **kwargs):
    """
    Sets the plotting parameters.
    """
    plot_kwargs = {k: v for k, v in kwargs.items() if v is not None}

    if 'cmap' not in plot_kwargs:
        plot_kwargs['cmap'] = 'turbo_r'  # Default colormap

    vmin, vmax = kwargs.get('vmin', None), kwargs.get('vmax', None)
    if vmin is None:
        vmin = np.min(da) if da is not None else None
    if vmax is None:
        vmax = np.max(da) if da is not None else None

    plot_kwargs['vmin'] = vmin
    plot_kwargs['vmax'] = vmax

    return plot_kwargs


def colorvalue(val, cmap='Spectral', vmin=None, vmax=None):
    """
    Returns the color associated with a value within a given range.
    """
    cm = mpl.colormaps[cmap]
    norm_val = mpl.colors.Normalize(vmin=vmin, vmax=vmax, clip=True)(val)
    return cm(norm_val)


def plot_cells_mpas(ds, vname, ax, **plot_kwargs):
    """
    Plots cells of the MPAS grid on the map.
    """
    for i, cell in enumerate(ds['nCells'].values):
        value = ds[vname].sel(nCells=cell)

        vals = ds['verticesOnCell'].sel(nCells=cell).values
        num_sides = int(ds['nEdgesOnCell'].sel(nCells=cell))
        vals = vals[:num_sides] - 1
        lats = ds['latitudeVertex'].sel(nVertices=vals)
        lons = ds['longitudeVertex'].sel(nVertices=vals)
    
        color = colorvalue(value, plot_kwargs['cmap'], 
                           vmin=plot_kwargs['vmin'], vmax=plot_kwargs['vmax'])
        
        if all(j for j in lons >= -179) and all(j for j in lons <= 179):
            ax.fill(lons, lats, edgecolor=None, linewidth=0.0, facecolor=color)


def close_plot(fig=None, size_fig=None, pdf=None, outfile=None, force_show=False):
    """
    Closes the plot and saves it to a file if necessary.
    """
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


def plot_mpas_darray(ds, vname, ax=None, outfile=None, **kwargs):
    """
    Plots a specific variable from an MPAS dataset on a map.
    """
    if vname not in ds.data_vars:
        print(f"Unplottable Data Array {vname}")
        print(ds)
        return
    
    da = ds[vname]
    for coord in ['time', 'lev']:
        if coord in da.dims:
            print(f'Selecting first slice for {coord}.')
            da = da.isel({coord: 0})

    # Apply zoom here if provided
    zoom_extent = kwargs.get('zoom_extent', [-180.0, 180, -90.0, 90.0])
    ax.set_extent(zoom_extent, crs=ccrs.PlateCarree())

    plot_kwargs = set_plot_kwargs(da=da, **kwargs)
    
    if 'nCells' in ds[vname].dims:
        plot_cells_mpas(ds, vname, ax, **plot_kwargs)
    else:
        print('WARNING: Impossible to plot: no nCells dimension.')
    
    units = da.attrs.get('units', '')
    title = kwargs.get('title', '')
    title = title.replace('<VAR>', vname).replace('<UNITS>', units)
    ax.set_title(title)

    # Add coastlines and gridlines
    ax.add_feature(cfeature.COASTLINE, linewidth=0.5)
    ax.gridlines(draw_labels=True, linewidth=0.5, color='gray', alpha=0.5, linestyle='--')

    # Add colorbar
    label=f'{vname} ({units})'
    norm=mpl.colors.Normalize(vmin=plot_kwargs['vmin'], vmax=plot_kwargs['vmax'], clip=True)
    cbar = plt.colorbar(
        mpl.cm.ScalarMappable(
            norm=norm, cmap=plot_kwargs['cmap']), ax=ax, shrink=0.6
            )
    cbar.ax.locator_params(nbins=10)
    if label is not None:
        cbar.set_label(label)


def view_mpas_mesh(mpas_grid_file, outfile=None, vname='resolution', **kwargs):
    """
    Visualizes an MPAS grid file and plots a specific variable.
    """
    derived_variables = {
        'latCell': ['latitude'],
        'lonCell': ['longitude'],
        'latVertex': ['latitudeVertex'],
        'lonVertex': ['longitudeVertex'],
        'areaCell': ['area', 'resolution'],
    }

    ds = open_mpas_file(mpas_grid_file, derived_variables)
    
    ncells = str(len(ds[vname].values.flatten()))
    name = os.path.basename(mpas_grid_file)
    
    ax = plt.axes(projection=ccrs.PlateCarree())  # Projection type
    plot_kwargs = set_plot_kwargs(da=ds[vname], **kwargs)
    
    tit = f'{vname}: {name} ({ncells})'
    plot_mpas_darray(ds, vname, ax=ax, title=tit, **plot_kwargs)
    
    close_plot(outfile=outfile)


# Main execution block
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Visualize MPAS grids")
    parser.add_argument("-g", "--grid", type=str, default=None, help="MPAS grid file (e.g., grid.nc)")
    parser.add_argument("-o", "--outfile", type=str, default=None, help="File to save the MPAS plot")
    
    # Zoom arguments
    parser.add_argument("-lat_min", type=float, default=None, help="Minimum latitude for zoom")
    parser.add_argument("-lat_max", type=float, default=None, help="Maximum latitude for zoom")
    parser.add_argument("-lon_min", type=float, default=None, help="Minimum longitude for zoom")
    parser.add_argument("-lon_max", type=float, default=None, help="Maximum longitude for zoom")
    
    args = parser.parse_args()

    # Determine zoom extent
    zoom_extent = [
        args.lon_min if args.lon_min is not None else -180,
        args.lon_max if args.lon_max is not None else 180,
        args.lat_min if args.lat_min is not None else -90,
        args.lat_max if args.lat_max is not None else 90,
    ]

    if not os.path.exists(args.grid):
        raise IOError(f'File does not exist: {args.grid}')

    view_mpas_mesh(args.grid, outfile=args.outfile, zoom_extent=zoom_extent)

    print(f"MPAS grid visualization completed for {args.grid}")
    if args.outfile:
        print(f"Output saved to {args.outfile}")
    else:
        print("No output file specified, displaying plot on screen.")
