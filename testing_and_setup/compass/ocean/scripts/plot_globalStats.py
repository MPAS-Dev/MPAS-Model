#!/usr/bin/env python
'''
This script plots the global stats variables given as command-line arguments
'''
from __future__ import absolute_import, division, print_function, \
    unicode_literals

import numpy
from netCDF4 import Dataset

from optparse import OptionParser
import os
import sys
import glob

import matplotlib.pyplot as plt


plt.switch_backend('Agg')

parser = OptionParser()

parser.add_option("--out_dir", type="string", default='globalStatsPlots',
                  dest="out_dir")
parser.add_option("--iteration", type="int", default=-1, dest="iteration")

options, args = parser.parse_args()

varNames = args

if(len(args) < 1):
    print("usage: plot_globalStats.py <variable_name1> [<variable_name2> ...]")
    print("where <variable_name1>, etc. are variables in a globalStats file")
    sys.exit(1)

try:
    os.makedirs(options.out_dir)
except OSError:
    pass

inFolder = '.'
inFiles = glob.glob('{}/analysis_members/globalStats*.nc'.format(inFolder))
if(len(inFiles) == 0):
    print("Error: files not found in {}".format(inFolder))
    sys.exit(1)
inFiles.sort()

fields = []
for varIndex in range(len(varNames)):
    fields.append(numpy.empty(0))
times = numpy.empty(0)
for inFileName in inFiles:
    inFile = Dataset(inFileName, 'r')

    localTime = inFile.variables['daysSinceStartOfSim']
    for varIndex in range(len(varNames)):
        fieldLocal = numpy.array(inFile.variables[varNames[varIndex]])
        fields[varIndex] = numpy.append(fields[varIndex], fieldLocal)

    times = numpy.append(times, localTime)

if(times[-1] < 1 / 24.):
    timeUnit = 's'
    times *= 3600. * 24.
elif(times[-1] < 1.):
    timeUnit = 'hrs'
    times *= 24.
elif(times[-1] < 365):
    timeUnit = 'days'
else:
    timeUnit = 'yrs'
    times /= 365

for varIndex in range(len(varNames)):
    plt.figure(varIndex + 1)
    plt.plot(times, fields[varIndex])

for varIndex in range(len(varNames)):
    plt.figure(varIndex + 1)
    plt.xlabel('time ({})'.format(timeUnit))
    plt.title(varNames[varIndex])
    if(options.iteration >= 0):
        fileName = '{}/{}{02d}.png'.format(options.out_dir, varNames[varIndex],
                                           options.iteration)
    else:
        fileName = '{}/{}.png'.format(options.out_dir, varNames[varIndex])
    plt.savefig(fileName)
    plt.close()
