#!/usr/bin/env python3
"""
Trim MPAS history files for ECT ensemble processing.

Extracts a single time slice, removes excluded variables, strips most
static mesh geometry, and applies lossless NetCDF4 deflation (zlib).

PyCECT requires three area-weighting variables (areaCell, dvEdge,
areaTriangle) to compute global means. These static variables are
preserved even though they lack a Time dimension.

The ECT exclusion list path is configured via ECT_EXCLUDED_VARS in ci-config.env
and passed via --exclude-file.

Usage:
    python3 trim_history.py input.nc output.nc --tslice -1 --exclude-file excluded_vars.txt
"""

import argparse
import os
import sys

import netCDF4 as nc

# Static variables PyCECT reads for area-weighted global means
# (see pyEnsLib.py generate_global_mean_for_summary_MPAS, lines 745-758)
PYCECT_REQUIRED_STATIC = {'areaCell', 'dvEdge', 'areaTriangle'}


def trim_history(infile, outfile, tslice, exclude_vars=None):
    exclude = set(exclude_vars or [])

    with nc.Dataset(infile, 'r') as src, nc.Dataset(outfile, 'w', format='NETCDF4') as dst:
        ntime = src.dimensions['Time'].size
        if tslice < 0:
            tslice = ntime + tslice
        if tslice < 0 or tslice >= ntime:
            print(f"ERROR: tslice={tslice} out of range for {ntime} time slice(s) in {infile}")
            sys.exit(1)
        dst.setncatts({k: src.getncattr(k) for k in src.ncattrs()})

        # Copy ALL dimensions — PyCECT checks nCells/nEdges/nVertices
        for dname, dim in src.dimensions.items():
            if dname == 'Time':
                dst.createDimension(dname, 1)
            else:
                dst.createDimension(dname, len(dim))

        # Identify variables to keep:
        #   1. Time-varying variables not in the exclude list
        #   2. Static variables required by PyCECT for area weighting
        keep_dynamic = {}
        keep_static = {}
        for name, var in src.variables.items():
            if name in exclude:
                continue
            if 'Time' in var.dimensions:
                keep_dynamic[name] = var
            elif name in PYCECT_REQUIRED_STATIC:
                keep_static[name] = var

        kept = 0

        # Write static variables (no time slicing needed)
        for name, var in keep_static.items():
            outvar = dst.createVariable(name, var.dtype, var.dimensions)
            outvar.setncatts({k: var.getncattr(k) for k in var.ncattrs()})
            outvar[:] = var[:]
            kept += 1

        # Write time-varying variables (extract single time slice, compress)
        for name, var in keep_dynamic.items():
            dims = var.dimensions
            use_zlib = var.size > 1000
            outvar = dst.createVariable(
                name, var.dtype, dims,
                zlib=use_zlib, complevel=1)
            outvar.setncatts({k: var.getncattr(k) for k in var.ncattrs()})

            tidx = dims.index('Time')
            slices = [slice(None)] * len(dims)
            slices[tidx] = slice(tslice, tslice + 1)
            outvar[:] = var[tuple(slices)]
            kept += 1

        skipped = len(src.variables) - kept

    in_size = os.path.getsize(infile) / 1048576
    out_size = os.path.getsize(outfile) / 1048576
    print(f"Kept {kept} variables ({len(keep_dynamic)} dynamic + "
          f"{len(keep_static)} static), dropped {skipped}, "
          f"tslice={tslice}, {in_size:.0f}MB -> {out_size:.0f}MB")


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Trim MPAS history files for ECT')
    parser.add_argument('input', help='Input history file')
    parser.add_argument('output', help='Output trimmed file')
    parser.add_argument('--tslice', type=int, default=-1,
                        help='Time slice index to extract (negative counts from end, e.g. -1 = last)')
    parser.add_argument('--exclude-file',
                        help='File listing variable names to exclude')
    args = parser.parse_args()

    exclude = []
    if args.exclude_file:
        with open(args.exclude_file) as f:
            exclude = [line.strip() for line in f
                       if line.strip() and not line.startswith('#')]

    trim_history(args.input, args.output, args.tslice, exclude)
