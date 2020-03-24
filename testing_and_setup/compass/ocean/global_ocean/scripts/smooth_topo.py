#!/usr/bin/env python
"""
A script for smoothing bathymetry at the resolution of the MPAS mesh before
it is resampled to MPAS cell centers
"""

import argparse
import xarray
import numpy
from mpas_tools.io import write_netcdf
from pyremap import LatLonGridDescriptor, Remapper
from progressbar import ProgressBar, Percentage, Bar, ETA


def remap_widths_to_topo_grid(dsWidth, dsTopo, mpiTasks):
    """
    Remap widths from the lon/lat grid used to create the base mesh to the
    topography grid

    Parameters
    ----------
    dsWidth : xarray.Dataset
        Dataset contain the widths of cells as a function of lon/lat

    dsTopo : xarray.Dataset
        Dataset with the lat/lon for topography

    mpiTasks : int
        The number of MPI tasks to use for remapping

    Returns
    -------
    dsWidth : xarray.Dataset
        The same cell widths remapped to the lon/lat of teh topography grid
    """
    _add_degrees(dsTopo)
    _add_degrees(dsWidth)
    inDescriptor = LatLonGridDescriptor.read(ds=dsWidth)
    outDescriptor = LatLonGridDescriptor.read(ds=dsTopo)
    mappingFileName = 'map_{}_to_{}_bilinear.nc'.format(inDescriptor.meshName,
                                                        outDescriptor.meshName)

    remapper = Remapper(inDescriptor, outDescriptor, mappingFileName)
    remapper.build_mapping_file(method='bilinear', mpiTasks=mpiTasks)

    dsWidth = remapper.remap(dsWidth, renormalizationThreshold=0.01)
    return dsWidth


def compute_smooth_weights(dsWidth, dsTopo, smoothing, iters,
                           earth_radius=6371.229):
    """
    Remap widths from the lon/lat grid used to create the base mesh to the
    topography grid

    Parameters
    ----------
    dsWidth : xarray.Dataset
        Dataset contain the widths (in km) of cells as a function of lon/lat

    dsTopo : xarray.Dataset
        Dataset with the lat/lon for topography

    smoothing : float
        Number of MPAS cell widths to smooth

    iters : int
        Number of iterations of smoothing

    earth_radius : float
        Radius of the Earth in km

    Returns
    -------
    weights_x, weights_y : xarray.DataArray
        The weights to be used for smoothing at each location
    """

    # assume constant dlon and dlat
    dlon = dsTopo.lon[1].values - dsTopo.lon[0].values
    dlat = dsTopo.lat[1].values - dsTopo.lat[0].values

    dx = earth_radius*numpy.deg2rad(dlon)*numpy.cos(numpy.deg2rad(dsTopo.lat))
    dy = earth_radius*numpy.deg2rad(dlat)

    cell_width_x = numpy.minimum(smoothing*dsWidth.cellWidth/dx,
                                 len(dsTopo.lon.values))
    cell_width_y = smoothing*dsWidth.cellWidth/dy

    # "magic" numbers found by curve fitting to smoothing of a discrete delta
    # function over various numbers of iterations and with various weights
    A = 2.65
    p_iter = 0.5
    p_width = 2.25

    factor = (1./(A*iters**p_iter))**p_width

    weights_x = numpy.minimum(factor*cell_width_x**p_width, 1.0)
    weights_y = numpy.minimum(factor*cell_width_y**p_width, 1.0)

    return weights_x, weights_y


def smooth_topo(dsTopo, iters, weights_x, weights_y, mask_field, threshold):
    """
    Remap widths from the lon/lat grid used to create the base mesh to the
    topography grid

    Parameters
    ----------
    dsTopo : xarray.Dataset
        Topography dataset

    iters : int
        Number of iterations of smoothing

    weights_x, weights_y : xarray.DataArray
        The weights to be used for smoothing at each location

    mask_field : str
        A field in ``input_file`` that contains a mask for where ocean
        (vs. land or grounded ice) is present

    threshold : float
        Fraction below which normalizaiton by the mask is not performed and
        the result is instead masked out

    Returns
    -------
    dsTopoSmooth : xarray.Dataset
        Smoothed topography dataset
    """

    dims = dsTopo.lon.dims + dsTopo.lat.dims
    variables = []
    for var in dsTopo.data_vars:
        if all([dim in dsTopo[var].dims for dim in dims]):
            variables.append(var)

    ocean_frac = dsTopo[mask_field]
    mask = ocean_frac > threshold
    ocean_frac = ocean_frac.where(mask, other=0.0)

    renorm = ocean_frac.where(mask, other=1.0)
    renorm = (1.0/renorm).where(mask, other=0.0)

    for var in variables:
        if 'mask' in var:
            # needs to be renormalized to only the ocean part
            dsTopo[var] = renorm*dsTopo[var]

    after_x = _smooth_x(ocean_frac, weights_x)
    after_xy = _smooth_y(after_x, weights_y)
    renorm = after_xy.where(mask, other=1.0)
    renorm = (1.0/renorm).where(mask, other=0.0)

    print('Smoothing topography:')
    pbar = ProgressBar(widgets=[Percentage(), Bar(), ETA()],
                       maxval=iters*len(variables)).start()

    dsTopoSmooth = dsTopo
    for outer in range(iters):
        for inner, var in enumerate(variables):
            da = _smooth_x(ocean_frac*dsTopoSmooth[var], weights_x)
            dsTopoSmooth[var] = renorm*_smooth_y(da, weights_y)
            pbar.update(outer*len(variables) + inner + 1)

    pbar.finish()

    for var in variables:
        if 'mask' in var:
            # back to including the ocean fraction
            dsTopo[var] = ocean_frac*dsTopo[var]

    return dsTopoSmooth


def smooth_topo_from_cell_widths(input_file='topography.nc',
                                 cell_width_file='cellWidthVsLatLon.nc',
                                 output_file='topography_smoothed.nc',
                                 include_cavities=False, threshold=0.01,
                                 iters=100, smoothing=1.0, mpi_tasks=1):
    """
    Smooth the topographic dataset based on the widths of cells on the MPAS
    mesh.

    Parameters
    ----------
    input_file : str
        Input topography file

    cell_width_file : str
        Cell widths file from base_mesh

    output_file : str
        Output topography file after smoothing

    include_cavities : bool
        Whether to include ice-shelf cavities in the ocean domain

    threshold : float
        Fraction below which normalizaiton by the mask is not performed and
        the result is instead masked out

    iters : int
        Number of iterations of smoothing

    smoothing : float
        Number of MPAS cell widths to smooth

    mpi_tasks : int
        Number of MPI tasks to ues for remapping
    """

    dsTopo = xarray.open_dataset(input_file)
    if 'x' in dsTopo.dims:
        dsTopo = dsTopo.rename({'x': 'lon'})
    if 'y' in dsTopo.dims:
        dsTopo = dsTopo.rename({'y': 'lat'})
    dsTopo = dsTopo.set_coords(['lon', 'lat'])

    if include_cavities:
        grounded_mask = dsTopo.grounded_mask
        dsTopo['floating_mask'] = dsTopo.ice_mask - grounded_mask
        dsTopo = dsTopo.drop_vars(['ice_mask', 'grounded_mask', 'water_column'])
    else:
        dsTopo['ocean_mask'] = numpy.minimum(1.0 - dsTopo.ice_mask,
                                             dsTopo.ocean_mask)
        dsTopo = dsTopo[['bathymetry', 'ocean_mask']]
        grounded_mask = None

    dsWidth = xarray.open_dataset(cell_width_file)
    dsWidth = remap_widths_to_topo_grid(dsWidth, dsTopo, mpi_tasks)

    weights_x, weights_y = compute_smooth_weights(dsWidth, dsTopo, smoothing,
                                                  iters)

    mask_field = 'ocean_mask'
    dsTopoSmooth = smooth_topo(dsTopo, iters, weights_x, weights_y, mask_field,
                               threshold)

    if include_cavities:
        dsTopoSmooth['grounded_mask'] = grounded_mask
        dsTopoSmooth['ice_mask'] = grounded_mask + dsTopoSmooth.floating_mask
    write_netcdf(dsTopoSmooth, output_file, format='NETCDF3_64BIT')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--input_file', dest='input_file',
                        default='topography.nc',
                        help='Input topography file')
    parser.add_argument('-w', '--cell_width_file', dest='cell_width_file',
                        default='cellWidthVsLatLon.nc',
                        help='Cell widths file from base_mesh')
    parser.add_argument('-o', '--output_file', dest='output_file',
                        default='topography_smoothed.nc',
                        help='Output topography file after smoothing')
    parser.add_argument('--include_cavities', dest='include_cavities',
                        action='store_true',
                        help='Whether to include ice-shelf cavities in the '
                             'ocean domain')
    parser.add_argument('--iters', dest='iters', type=int, default=100,
                        help='Number of iterations of smoothing')
    parser.add_argument('-s', '--smoothing', dest='smoothing', type=float,
                        default=1.0,
                        help='Number of MPAS cell widths to smooth')
    parser.add_argument('--mpi_tasks', dest='mpi_tasks', type=int,
                        default=1,
                        help='Number of MPI tasks to ues for remapping')
    parser.add_argument('--threshold', dest='threshold', type=float,
                        default=0.01,
                        help='Fraction below which normalizaiton by the mask '
                             'is not performed')

    args = parser.parse_args()

    smooth_topo_from_cell_widths(args.input_file, args.cell_width_file,
                                 args.output_file, args.include_cavities,
                                 args.threshold, args.iters, args.smoothing,
                                 args.mpi_tasks)


def _smooth_x(da, weights_x):
    """ Smooth the data array in the x direction, accounting for periodicity """
    da_p = da.roll(lon=1, roll_coords=False)
    da_m = da.roll(lon=-1, roll_coords=False)

    da = weights_x*(da_m + da_p) + da
    return da


def _smooth_y(da, weights_y):
    """ Smooth the data array in the x direction, accounting for periodicity """
    da_p = da.shift(lat=1, fill_value=0.)
    da_m = da.shift(lat=-1, fill_value=0.)

    da = weights_y*(da_m + da_p) + da
    return da


def _add_degrees(ds):
    """
    Modifies the dataset to add "degrees" as the "units" for lon and lat

    Parameters
    ----------
    ds : xarray.Dataset
        Dataset with lon/lat
    """
    for coord in ['lon', 'lat']:
        if 'units' not in ds[coord].attrs:
            ds[coord].attrs['units'] = 'degrees'


if __name__ == '__main__':
    main()
