#!/usr/bin/env python

import xarray as xr
import matplotlib.pyplot as plt

# render statically by default
plt.switch_backend('agg')

if __name__ == "__main__":

  ds = xr.open_dataset('output.nc')
  u = ds.velocityZonal.isel(Time=-1).mean('nCells').values*100.
  z = ds.refBottomDepth.values
  plt.plot(u,z, 'k-')
  plt.xlabel('Velocity (cm/s)')
  plt.ylabel('Depth (m)')
  plt.gca().invert_yaxis()
  plt.savefig('velocity_profile.png')


