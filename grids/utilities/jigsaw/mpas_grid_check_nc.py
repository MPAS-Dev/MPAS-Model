#!/usr/bin/env python
#
#  Basic script to convert netcdf grids from ocean to atmosphere
#  by Pedro S. Peixoto Feb 2022 <ppeixoto@usp.br>
#


import netCDF4 as nc

fn = 'mpas-a-test/mpas-a-test_mpas.nc'
ds = nc.Dataset(fn)

print(ds)
vars1=[]
for var in ds.variables:
    print(var)
    vars1.append(var)

print("-----------------------")
fn2 = '../../grids/x1.10242/x1.10242.grid.nc'
ds2 = nc.Dataset(fn2)

print(ds2)
vars2=[]
for var in ds2.variables:
    print(var)
    vars2.append(var)
    
dif = list(set(vars2)-set(vars1))
print(dif)
dif = list(set(vars1)-set(vars2))
print(dif)

print(ds['xVertex'][:])
print(ds2['xVertex'][:])


print("-----------------------")
fn3 = 'mpas-a-test/mpas-a-test_triangles.nc'
ds3 = nc.Dataset(fn3)
print(ds3)
print(ds3['xVertex'][1:100])
