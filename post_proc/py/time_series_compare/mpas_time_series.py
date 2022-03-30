''' 

Plot evolution of several time series from MPAS output

P. Peixoto on Feb 2022 <ppeixoto@usp.br>

'''

import os
import sys
import argparse
import numpy as np
import glob

from datetime import datetime, timedelta

from netCDF4 import Dataset

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap

from mpas_patches import get_mpas_patches
    
parser = argparse.ArgumentParser()

parser.add_argument('dir', 
                    type=str, 
                    help='''Directory of benchmarks''')
parser.add_argument('-v',
                    '--var', 
                    type=str,
                    default='pressure',
                    help='''Variable you want to plot from that file''')

args = parser.parse_args()
variable = args.var
benchdir = args.dir

if not os.path.isdir(benchdir):
    print("That bench dir was not found :(")
    sys.exit(-1)

dir = benchdir+"/pp_time_series"
if not os.path.isdir(dir):
    os.makedirs(dir)

#get all existing benchmark 
benchs = glob.glob(benchdir+"/run.*")
benchnames = [ os.path.split(x)[1] for x in benchs ]
benchnames_clean = [ os.path.split(x)[1].replace("run.", "") for x in benchs ]

print( "ploting time series")
fig = plt.figure(figsize=(20, 12))
ax = plt.gca()

for ib, b in enumerate(benchs):
    print(ib,b)

    data = Dataset(b+"/out.nc", 'r')

    # Check to see the variable is in the mesh
    if variable not in data.variables.keys(): 
        print("That variable was not found in this mpas mesh!")
        print(data.variables.keys())
        sys.exit(-1)

    var = data.variables[variable]

    dims = var.dimensions
    shap = var.shape
    print(dims)
    print(shap)
    nlevels = shap[2]
    ntimes = shap[0]
    print("nlevels:", nlevels)

    #Set time series periods
    duration = data.__dict__['config_run_duration']
    dys, time = duration.split("_")
    t = datetime.strptime(time,"%H:%M:%S")
    dur = timedelta(days=int(dys), hours=t.hour, minutes=t.minute, seconds=t.second)
    t_ini = timedelta(days=0, hours=0, minutes=0, seconds=0)
    delta = dur/(ntimes-1)
    print(delta)

    if (True): #if (benchnames_clean[ib].endswith("visc4smag_0.0") ):
        ts = []
        ys = []
        for i in range(ntimes):
            t = t_ini+i*delta
            hours = t.total_seconds()/3600
            ts.append(hours)
            var_agg = var[i]
            var_agg = np.sqrt(np.sum(np.sum(np.square(var_agg))))
            ys.append(var_agg)
            print(t,var_agg)
	    

        ax.plot(ts, ys, label=benchnames_clean[ib] )

    


#print(ts, len(ts))
#print(ts_error, len(ts_error))
#plt.legend(loc="upper left")
ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))

plt.xlabel("Hours")
plt.ylabel( "RMS "+variable )
plt.title(variable)
plt.tight_layout()
plt.savefig(dir+"/"+variable+'_ts.png')
print( "-----------------------------------------")
print( )

