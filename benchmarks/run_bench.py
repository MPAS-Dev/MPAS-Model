#-------------------------------------------------
#  Script to generate namelists and stream for MPAS
#  Author: P. Peixoto <ppeixoto@usp.br>
#  Last update: Mar 2022
# --------------------------------------------------
import f90nml
import os
import argparse
import subprocess
import sys
import shutil
import glob
import time 
import mpas_benchmarks as bench

# Get args: init or run core
args = bench.call_parser()

#Workspace
work_dir = os.getenv('MPAS_DIR')
b_name = args.name
#b_dir = work_dir+"/benchmarks/"+b_name
b_dir = work_dir+"/benchmarks/"+args.bdir
print(b_dir)

os.environ['OMP_NUM_THREADS'] = "2"

#Init_atmosphere setup
if args.init:
    benchs = glob.glob(b_dir+"/init*")
    mainexec = "./init_atmosphere_model"
else:
    #Make sure the init test exists!
    benchs = glob.glob(b_dir+"/run.*") 
    mainexec = "./atmosphere_model"

print("---------------------------")
print(" Running :", benchs)
print("---------------------------")
for b in benchs:
    print()
    print(" Running :", b)
    print()
    
    p = subprocess.Popen(mainexec, stdout = subprocess.PIPE, stderr=subprocess.PIPE, cwd=b, shell=False)

    time.sleep(1)
    try:
        outfile = bench.find("log.*out", b)[0]  
        print(outfile)   
        follow = bench.LogFollower(open(outfile))

        while p.poll() is None:
            for line in follow:
                print(line)
    except:
        print("Couldn't reach log file, check manually, just in case")   
    
        
    


