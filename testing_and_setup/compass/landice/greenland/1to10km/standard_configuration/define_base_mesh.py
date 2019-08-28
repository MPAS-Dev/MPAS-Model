#!/usr/bin/env python
from __future__ import absolute_import, division, print_function, \
    unicode_literals

import numpy as np
import netCDF4
import jigsawpy
from matplotlib import pyplot as plt

def cellWidthVsXY():

   # get needed fields from GIS dataset
   f = netCDF4.Dataset('greenland_8km_2017_06_27.epsg3413.nc','r')
   f.set_auto_mask(False) # disable masked arrays

   x1 = f.variables['x1'][:]
   y1 = f.variables['y1'][:]
   thk = f.variables['thk'][0,:,:]
   topg = f.variables['topg'][0,:,:]
   vx = f.variables['vx'][0,:,:]
   vy = f.variables['vy'][0,:,:]

   # subset data - optional
   step=1
   x1=x1[::step]
   y1=y1[::step]
   thk=thk[::step, ::step]
   topg=topg[::step, ::step]
   vx=vx[::step, ::step]
   vy=vy[::step, ::step]


   dx = x1[1] - x1[0] # assumed constant and equal in x and y
   nx = len(x1)
   ny = len(y1)

   sz = thk.shape

   # define extent of region to mesh
   xx0=x1.min(); xx1=x1.max();
   yy0=y1.min(); yy1=y1.max();
   geom_points = np.array([ # list of xy "node" coordinates
       ((xx0, yy0), 0),
       ((xx1, yy0), 0),
       ((xx1, yy1), 0),
       ((xx0, yy1), 0)],
       dtype=jigsawpy.jigsaw_msh_t.VERT2_t)

   geom_edges = np.array([    # list of "edges" between nodes
       ((0, 1), 0),
       ((1, 2), 0),
       ((2, 3), 0),
       ((3, 0), 0)],
       dtype=jigsawpy.jigsaw_msh_t.EDGE2_t)

   # flood fill to remove island, icecaps, etc.
   searchedMask = np.zeros(sz)
   floodMask = np.zeros(sz)
   iStart=sz[0]//2
   jStart=sz[1]//2
   floodMask[iStart,jStart]=1

   neighbors=np.array([[1,0], [-1,0], [0,1], [0,-1]])

   lastSearchList = np.ravel_multi_index([[iStart],[jStart]], sz, order='F')

   # flood fill -------------------
   cnt = 0
   while len(lastSearchList) > 0:
       #print(cnt)
       cnt += 1
       newSearchList = np.array([], dtype='i')

       for iii in range(len(lastSearchList)):
           [i, j] = np.unravel_index(lastSearchList[iii], sz, order='F')
           # search neighbors
           for n in neighbors:
               ii=i+n[0]; jj=j+n[1];  # subscripts to neighbor
               if searchedMask[ii,jj] == 0:  # only consider unsearched neighbors
                   searchedMask[ii,jj] = 1  # mark as searched

                   if thk[ii, jj] > 0.0:
                       floodMask[ii,jj] = 1  # mark as ice
                       newSearchList = np.append(newSearchList, np.ravel_multi_index([[ii],[jj]], sz, order='F')[0]) # add to list of newly found  cells
       lastSearchList = newSearchList
   # optional - plot flood mask
   #plt.pcolor(floodMask)
   #plt.show()

   # apply flood fill
   thk[floodMask==0] = 0.0
   vx[floodMask==0] = 0.0
   vy[floodMask==0] = 0.0


   # make masks -------------------
   neighbors=np.array([[1,0], [-1,0], [0,1], [0,-1], [1,1], [-1,1], [1,-1], [-1,-1]])

   iceMask = thk>0.0
   #groundedMask = thk > (-1028.0/910.0 * topg)
   #floatingMask = np.logical_and(thk < (-1028.0/910.0 * topg), thk>0.0)
   marginMask = np.zeros(sz, dtype='i')
   for n in neighbors:
       marginMask = np.logical_or(marginMask, np.logical_not(np.roll(iceMask, n, axis=[0,1])))
   marginMask = np.logical_and(marginMask, iceMask) # where ice exists and neighbors non-ice locations
   # optional - plot mask
   #plt.pcolor(marginMask); plt.show()


   # calc dist to margin -------------------
   [XPOS,YPOS] = np.meshgrid(x1,y1);
   distToEdge = np.zeros(sz)

   # -- KEY PARAMETER: how big of a search 'box' (one-directional) to use.
   # Bigger number makes search slower, but if too small, the transition zone
   # could get truncated.
   # (could automatically set this from maxDist variables used in next section.)
   windowSize = 100.0e3;
   # ---

   d = int(np.ceil(windowSize / dx))
   #print(windowSize, d)
   rng = np.arange(-1*d, d, dtype='i')
   maxdist = float(d) * dx

   #ind = np.where( np.ravel(thk, order='F')>0 )[0]  # just look over areas with ice
   ind = np.where( np.ravel(thk, order='F')>=0 )[0]  # do it everywhere
   for iii in range(len(ind)):
       [i, j] = np.unravel_index(ind[iii], sz, order='F')

       irng = i+rng; jrng = j+rng;

       # only keep indices in the grid
       irng = irng[np.nonzero(np.logical_and(irng>=0, irng<ny))]
       jrng = jrng[np.nonzero(np.logical_and(jrng>=0, jrng<nx))]

       dist2Here = ((XPOS[np.ix_(irng,jrng)]-x1[j])**2 + (YPOS[np.ix_(irng,jrng)]-y1[i])**2)**0.5
       dist2Here[marginMask[np.ix_(irng,jrng)]==0] = maxdist
       distToEdge[i,j] = dist2Here.min()
   # optional - plot distance calculation
   #plt.pcolor(distToEdge/1000.0); plt.colorbar(); plt.show()


   # now create cell spacing function -------
   speed = (vx**2+vy**2)**0.5
   lspd = np.log10( speed )
   # threshold
   #ls_min = 0
   #ls_max = 3
   #lspd(lspd<ls_min) = ls_min
   #lspd(lspd>ls_max) = ls_max

   # make dens fn mapping from log speed to cell spacing
   minSpac=1.0
   maxSpac=10.0
   highLogSpeed=2.5
   lowLogSpeed=0.75
   spacing = np.interp(lspd, [lowLogSpeed, highLogSpeed], [maxSpac, minSpac], left=maxSpac, right=minSpac)
   spacing[thk==0.0]=minSpac
   #plt.pcolor(spacing); plt.colorbar(); plt.show()

   # make dens fn mapping for dist to margin
   minSpac=1.0
   maxSpac=10.0
   highDist=100.0 * 1000.0 # m
   lowDist=10.0 * 1000.0
   spacing2 = np.interp(distToEdge, [lowDist, highDist], [minSpac, maxSpac], left=minSpac, right=maxSpac)
   spacing2[thk==0.0]=minSpac
   plt.pcolor(spacing2); plt.colorbar(); plt.show()

   # merge two cell spacing methods
   cell_width = np.minimum(spacing, spacing2)*1000.0
   cell_width[np.logical_and(thk==0.0, distToEdge>50.0e3)] = maxSpac*1000.0 # put coarse res far out in non-ice area to keep mesh smaller in the part we are going to cull anyway (speeds up whole process)
   plt.pcolor(cell_width); plt.colorbar(); plt.show()

   #cell_width = 20000.0 * np.ones(thk.shape)

   return cell_width.astype('float64'), x1.astype('float64'), y1.astype('float64'), geom_points, geom_edges
