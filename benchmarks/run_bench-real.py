#-------------------------------------------------
#  Script to generate namelists and stream for MPAS
#  Author: P. Peixoto <ppeixoto@usp.br>
#  Last update: Mar 2022
# --------------------------------------------------
import os
import subprocess
import glob
import time 
import mpas_benchmarks_RealCase as bench

# Get args: init or run core
args = bench.call_parser()

#Workspace
work_dir = os.getenv('MPAS_DIR')
b_name = args.name
#b_dir = work_dir+"/benchmarks/"+b_name
b_dir = work_dir+"/benchmarks/"+args.bdir
print(b_dir)

if args.threads:
    cores = args.threads
    print("Running with "+cores+" threads")
    os.environ['OMP_NUM_THREADS'] = cores
else:
    os.environ['OMP_NUM_THREADS'] ="2"
    
#Init_atmosphere setup
if args.run:
    #Make sure the init test exists!
    benchs = glob.glob(b_dir+"/run.*") 
    mainexec = "mpiexec -n "+str(cores)+" ./atmosphere_model"
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
    
    
    print('---------------')
    print('trying to run: '+mainexec)
    
    p = subprocess.Popen(mainexec.split(' '), stdout = subprocess.PIPE,
                          stderr=subprocess.PIPE, cwd=b, shell=True)

    time.sleep(1)
    try:
        outfile = bench.find("log.*out", b)[0]  
        print(outfile)   
        follow = bench.LogFollower(open(outfile))

        # while p.poll() is None:
        #     for line in follow:
        #         print(line)
    except:
        print("Couldn't reach log file, check manually, just in case")   
    
    # Duration of model run 
    lenght = (time.time() - start_time)/60/60
    print("--- %s hours for running model ---" % (lenght))
    
    # Write time required for model run into text file
    f = open(b+'/run_duration', 'w' )
    f.write(str(lenght)+' hours')
    f.close()
        
    

