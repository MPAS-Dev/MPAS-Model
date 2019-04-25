#!/usr/bin/env python
#  $id$
"""
Make gsw_mod_saar_data.f from the current gsw_data_v3_0.nc. This is a developer
utility and not a part of the public distribution, but its end-product is.
Note that it generates gsw_saar_data.c but will not overwrite it if it exists.
General concept: we don't want end-users of this distribution to require having
netcdf installed, nor do we want to incur the I/O overhead every time this
library is used. So we simply generate static data from the netcdf file.
"""
import math, os, sys
from netCDF4 import Dataset

def float2string(val, sformat, addcomma):
    if math.isnan(val):
        str_val = "9e90_r8"
    else:
        str_val = sformat % val
        if str_val.find(".") < 0 and str_val.find("e") < 0 :
            str_val += "."
        str_val += "_r8"
    if addcomma:
        str_val += ", "
    return str_val;

def write_variable_real(var_name, dims, v):
    ndims = len(dims)
    # list dimensions in reverse order (nz,ny,nx)
    if ndims == 1:
        fortran_dims = "(%s)" % v.dimensions[0]
    elif ndims == 2:
        fortran_dims = "(%s,%s)" % v.dimensions[::-1]
    elif ndims == 3:
        fortran_dims = "(%s,%s,%s)" % v.dimensions[::-1]

    out.write("real (r8), dimension%s :: %s\n" % (fortran_dims, var_name))

    buf = "  "
    maxlen = 78
    numformat = "%.17g"
    if ndims == 1:
        out.write("data %s / &\n" % var_name)
        lastx = dims[0]-1
#
#       The following construct (and variations below) transfer the
#       netcdf variable into a memory-resident buffer all at once.
#       Anything else is not advised.
#
        vv = v[:]
        for val, x in [(vv[cx],cx) for cx in range(dims[0])]:
            sval = float2string(val,numformat,(x != lastx))
            if len(buf)+len(sval) > maxlen:
                out.write(buf+"&\n")
                buf = "  "
            buf += sval
        if buf:
            out.write(buf+" &\n")
        out.write("  /\n\n")
    elif ndims == 2:
        out.write("data %s / &\n" % var_name)
        lastx = dims[0]-1
        lasty = dims[1]-1
        vv = v[:][:]
        for x in range(dims[0]):
            for val,y in [(vv[x][cy],cy) for cy in range(dims[1])]:
                sval = float2string(val,numformat,(x != lastx or y != lasty))
                if len(buf)+len(sval) > maxlen:
                    out.write(buf+"&\n")
                    buf = "  "
                buf += sval
        if buf:
            out.write(buf+" &\n")
        out.write("  /\n\n")
    else:
#
#       For 3d real arrays we construct separate data statements for short
#       array sections (one row per statement) to avoid continuation line
#       limits (gFortran is unlimited, but iFort has a 511 line limit).
#
        lastz = dims[2]-1
        vv = v[:][:][:]
        for x in range(dims[0]):
            for y in range(dims[1]):
                out.write("data %s(:,%d,%d) / &\n" % (var_name, y+1, x+1))
                for val,z in [(vv[x][y][cz],cz) for cz in range(dims[2])]:
                    sval = float2string(val,numformat,(z != lastz))
                    if len(buf)+len(sval) > maxlen:
                        out.write(buf+"&\n")
                        buf = "  "
                    buf += sval
                if buf:
                    out.write(buf+" &\n")
                    buf = "  "
                out.write("  /\n")
        out.write("\n")

def write_variable_int(var_name, dims, v):
    ndims = len(dims)
    # list dimensions in reverse order (nz,ny,nx)
    if ndims == 1:
        fortran_dims = "(%s)" % v.dimensions[0]
    elif ndims == 2:
        fortran_dims = "(%s,%s)" % v.dimensions[::-1]
    elif ndims == 3:
        fortran_dims = "(%s,%s,%s)" % v.dimensions[::-1]

    out.write("integer, dimension%s :: %s\n" % (fortran_dims, var_name))
    out.write("data %s / &\n" % var_name)

    buf = "  "
    maxlen = 78
    nan = "999"
    if ndims == 1:
        lastx = dims[0]-1
#
#       The following construct (and variations below) transfer the
#       netcdf variable into a memory-resident buffer all at once.
#       Anything else is not advised.
#
        vv = v[:]
        for val, x in [(vv[cx],cx) for cx in range(dims[0])]:
            if math.isnan(val):
                sval = nan
            else:
                sval = "%d" % val
            if x != lastx:
                sval += ", "
            if len(buf)+len(sval) > maxlen:
                out.write(buf+"&\n")
                buf = "  "
            buf += sval
    elif ndims == 2:
        lastx = dims[0]-1
        lasty = dims[1]-1
        vv = v[:][:]
        for x in range(dims[0]):
            for val,y in [(vv[x][cy],cy) for cy in range(dims[1])]:
                if math.isnan(val):
                    sval = nan
                else:
                    sval = "%d" % val
                if x != lastx or y != lasty:
                    sval += ", "
                if len(buf)+len(sval) > maxlen:
                    out.write(buf+"&\n")
                    buf = "  "
                buf += sval
    else:
        lastx = dims[0]-1
        lasty = dims[1]-1
        lastz = dims[2]-1
        vv = v[:][:][:]
        for x in range(dims[0]):
            for y in range(dims[1]):
                for val,z in [(vv[x][y][cz],cz) for cz in range(dims[2])]:
                    if math.isnan(val):
                        sval = nan
                    else:
                        sval = "%d" % val
                    if x != lastx or y != lasty or z != lastz:
                        sval += ", "
                    if len(buf)+len(sval) > maxlen:
                        out.write(buf+"&\n")
                        buf = "  "
                    buf += sval
    if buf:
        out.write(buf+" &\n")
    out.write("  /\n\n")

rootgrp = Dataset('gsw_data_v3_0.nc', 'r')
v = rootgrp.variables
d = rootgrp.dimensions

nx = len(d['nx'])
ny = len(d['ny'])
nz = len(d['nz'])

version_date = rootgrp.version_date
version_number = rootgrp.version_number

vars_real = [["p_ref", "", [nz]], ["lats_ref", "", [ny]],
        ["longs_ref", "", [nx]], ["saar_ref", "SAAR_ref", [nx,ny,nz]],
        ["delta_sa_ref", "deltaSA_ref", [nx,ny,nz]]]

vars_int = [["ndepth_ref", "", [nx,ny]]]

try:
    fd = os.open("gsw_mod_saar_data.f90", os.O_CREAT|os.O_EXCL|os.O_RDWR, 0644)
except:
    print str(sys.exc_info()[1])
    print "Will not overwrite gsw_mod_saar_data.f90. Exiting."
    sys.exit(1)
out = os.fdopen(fd, "w")
out.write("!\n!**  $Id$\n!**  Extracted from gsw_data_v3_0.nc\n!\n")
out.write("""
!==========================================================================
module gsw_mod_saar_data
!==========================================================================

use gsw_mod_kinds

implicit none

integer, dimension(4) :: deli = (/0,1,1,0/), delj = (/0,0,1,1/)

integer, parameter :: npan = 6
real (r8), dimension(npan) :: longs_pan, lats_pan

data longs_pan /260.00_r8, 272.59_r8, 276.50_r8, 278.65_r8, 280.73_r8, 292.0_r8/
data  lats_pan / 19.55_r8,  13.97_r8,   9.60_r8,   8.10_r8,   9.33_r8,   3.4_r8/

""")

out.write("integer, parameter :: nx = %d\n"% nx)
out.write("integer, parameter :: ny = %d\n"% ny)
out.write("integer, parameter :: nz = %d\n"% nz)
out.write("\n")
out.write("character(*), parameter :: gsw_version_date = \"%s\"\n" % version_date)
out.write("character(*), parameter :: gsw_version_number = \"%s\"\n\n" % version_number)

for var_label, var_name, dims in [var for var in vars_real]:
    if not var_name:
        var_name = var_label
    write_variable_real(var_label, dims, v[var_name])

for var_label, var_name, dims in [var for var in vars_int]:
    if not var_name:
        var_name = var_label
    write_variable_int(var_label, dims, v[var_name])

out.write("""
contains

    subroutine gsw_get_version (version_date, version_number)

    implicit none

    character(*), intent(out) :: version_date, version_number

    version_date = gsw_version_date
    version_number = gsw_version_number

    end subroutine gsw_get_version

end module gsw_mod_saar_data
!--------------------------------------------------------------------------
""")

out.close()
sys.exit(0)
