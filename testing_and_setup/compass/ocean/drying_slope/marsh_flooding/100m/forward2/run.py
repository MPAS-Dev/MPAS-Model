#!/usr/bin/env python

# This script was generated from setup_testcases.py as part of a config file

import sys
import os
import shutil
import glob
import subprocess


dev_null = open('/dev/null', 'w')

# Run command is:
# gpmetis graph.info 4
subprocess.check_call(['gpmetis', 'graph.info', '4'])
print("\n")
print("     *****************************")
print("     ** Starting model run step **")
print("     *****************************")
print("\n")
os.environ['OMP_NUM_THREADS'] = '1'

# Run command is:
# mpirun -n 4 ./ocean_model -n namelist.ocean -s streams.ocean
subprocess.check_call(['mpirun', '-n', '4', './ocean_model', '-n',
                       'namelist.ocean', '-s', 'streams.ocean'])
print("\n")
print("     *****************************")
print("     ** Finished model run step **")
print("     *****************************")
print("\n")
