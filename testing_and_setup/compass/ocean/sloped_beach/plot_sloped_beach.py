#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset
from scipy import spatial
from scipy import interpolate
plt.switch_backend('agg')

# Read exact solution from init mode
nc_fid = Dataset("exact_solution.nc", "r")
x = nc_fid.variables["xTransect"][:]
y = np.copy(x)*0.0 + 25.0
ssh_exact = nc_fid.variables["sshExact"][:]
nc_fid.close()

# Read MPAS-O solution from forward mode
nc_fid = Dataset("output.nc", "r")
xCell = nc_fid.variables["xCell"][:]
yCell = nc_fid.variables["yCell"][:]
ssh = nc_fid.variables["ssh"][:]
nc_fid.close()

# Interpolate MPAS-O solution onto exact solution transect
xyCell = np.vstack((xCell,yCell)).T
interp = interpolate.LinearNDInterpolator(xyCell,ssh[-1,:])
xy = np.vstack((x,y)).T
ssh_mpas = interp(xy)

# Plot comparison
fig = plt.figure(figsize=(15,5))
l1, = plt.plot(x,ssh_exact)
l2, = plt.plot(x,ssh_mpas)
plt.xlabel('x')
plt.ylabel('sea surface height')
plt.legend((l1,l2),('exact solution','MPAS-Ocean'))
plt.tight_layout()
plt.savefig('ssh_comparison.png',bbox_inches='tight')
plt.close()


