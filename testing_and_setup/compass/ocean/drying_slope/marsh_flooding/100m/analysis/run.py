#!/usr/bin/env python

# This script was generated from setup_testcases.py as part of a config file

import sys
import os
import shutil
import glob
import subprocess


dev_null = open('/dev/null', 'w')

# Run command is:
# ./comparison.py
subprocess.check_call(['./comparison.py'])

# Run command is:
# ./movie.py
subprocess.check_call(['./movie.py'])

# Run command is:
# paraview_vtk_field_extractor.py -f output1.nc -o vtk_output1 -v allOnCells -d
# maxEdges=0 nVertLevels=0:10 --combine
subprocess.check_call(['paraview_vtk_field_extractor.py', '-f', 'output1.nc',
                       '-o', 'vtk_output1', '-v', 'allOnCells', '-d',
                       'maxEdges=0', 'nVertLevels=0:10', '--combine'])

# Run command is:
# paraview_vtk_field_extractor.py -f output2.nc -o vtk_output2 -v allOnCells -d
# maxEdges=0 nVertLevels=0:10 --combine
subprocess.check_call(['paraview_vtk_field_extractor.py', '-f', 'output2.nc',
                       '-o', 'vtk_output2', '-v', 'allOnCells', '-d',
                       'maxEdges=0', 'nVertLevels=0:10', '--combine'])
