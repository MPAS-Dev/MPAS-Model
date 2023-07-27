# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    run_bench-real.py                                  :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: Danilo <danilo.oceano@gmail.com>           +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2022/11/01 12:00:00 by Danilo            #+#    #+#              #
#    Updated: 2023/07/26 18:00:19 by Danilo           ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

#-------------------------------------------------------------------------------
#  Script to generate namelists and stream for MPAS for real cases
#  Based on P.Peixoto's run_bench.py script
#-------------------------------------------------------------------------------

import os
import sys
import subprocess
import glob
import time 
import mpas_benchmarks_RealCase as bench

# Get args: init or run core
args = bench.call_parser()

#Workspace
work_dir = os.getenv('MPAS_DIR')
if work_dir is None:
    print('MPAS_DIR not on environment! Set it and try again')
    sys.exit(1)
b_name = args.name
b_dir = work_dir+"/benchmarks/"+args.bdir
print(b_dir)

#Init_atmosphere setup
if args.run:
    #Make sure the init test exists!
    benchs = glob.glob(b_dir+"/run.*") 
    mainexec = "mpiexec -n "+str(args.threads)+" ./atmosphere_model"
else:
    benchs = glob.glob(b_dir+"/init*")
    mainexec = "./init_atmosphere_model"


print("---------------------------")
print(" Running :", benchs)
print("---------------------------")
for b in benchs:
    print()
    print(" Running :", b)
    print()
    
    # Time model run: get initial time 
    start_time = time.time()
    
    p = subprocess.Popen(mainexec, stdout = subprocess.PIPE,
                          stderr=subprocess.PIPE, cwd=b, shell=True)

    time.sleep(5)
    try:
        outfile = bench.find("log.*out", b)[0]  
        print(outfile)   
        follow = bench.LogFollower(open(outfile))

        while p.poll() is None:
            for line in follow:
                print(line)
    except:
        print("Couldn't reach log file, check manually, just in case")   
    
    # Duration of model run 
    lenght = (time.time() - start_time)/60/60
    print("--- %s hours for running model ---" % (lenght))
    
    # Write time required for model run into text file
    f = open(b+'/run_duration', 'w' )
    f.write(str(lenght)+' hours')
    f.close()
    
    # wait process to finish before starting a new one    
    p.wait()    

