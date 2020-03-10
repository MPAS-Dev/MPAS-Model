#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=4:00:00
#SBATCH --job-name=marsh_flooding
#SBATCH --output=marsh_flooding.o%j
#SBATCH --error=marsh_flooding.e%j
#SBATCH --qos=interactive
#SBATCH -A w19_mpascoastal
###SBATCH --mail-user=caozd999@lanl.gov
#SBATCH --mail-type=ALL

#mlintel
module purge
echo "loading modules git, anaconda, intel, openmpi, netcdf, pnetcdf, pio for grizzly"
module load git
module use /usr/projects/climate/SHARED_CLIMATE/modulefiles/all/
module unload python
source /usr/projects/climate/SHARED_CLIMATE/anaconda_envs/load_latest_compass.sh
module load intel/17.0.1 openmpi/1.10.5 netcdf/4.4.1 parallel-netcdf/1.5.0 pio/1.7.2
echo "done"

# in forward model directory (or in top-level directory for case)
python run_test.py

