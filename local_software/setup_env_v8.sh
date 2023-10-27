#! /bin/bash

#
# This is a template to set up environment variables correctly
#
#  Updated for MPAS version 8 on Oct 2023 by P. Peixoto
#
#  Copy a local version for each machine
# ----------------------------

BASEDIR="`pwd`/local_software"
#BASEDIR=SCRIPTDIR

if [ ! -d "$BASEDIR" ]; then
        echo
	echo "********************************************************"
        echo "ERROR: Execute this script only from the root directory"
        echo "   source local_software/setup_env.sh"
	echo "********************************************************"
        echo
	return
fi

# MPI
# conda install -c conda-forge openmpi

# netcdf
# conda install -c conda-forge netcdf4

# pnetcdf
#conda install -c "e3sm/label/compass" libpnetcdf

# you should need either netcdf or pnetcdf, not both

# System based installs 
export LD_LIBRARY_PATH=`nc-config --libdir`:$LD_LIBRARY_PATH
export NETCDF_PATH=`nc-config --prefix`
export PNETCDF_PATH=`pnetcdf-config --prefix`

# FOR MPAS
export MPAS_DIR=`pwd`
export NETCDF=$NETCDF_PATH
export PNETCDF=$PNETCDF_PATH
#export MPAS_EXTERNAL_LIBS="-L${NETCDF_PATH}/lib -lhdf5_hl -lhdf5 -ldl -lz"
#export MPAS_EXTERNAL_INCLUDES="-I${NETCDF_PATH}/include"
export MPAS_EXTERNAL_LIBS="-L${PNETCDF_PATH}/lib -lhdf5_hl -lhdf5 -ldl -lz"
export MPAS_EXTERNAL_INCLUDES="-I${PNETCDF_PATH}/include"
#export MPAS_EXTERNAL_LIBS="-L${NETCDF_PATH}/lib -L${PNETCDF_PATH}/lib -lhdf5_hl -lhdf5 -ldl -lz"
#export MPAS_EXTERNAL_INCLUDES="-I${NETCDF_PATH}/include -I${PNETCDF_PATH}/include"

echo "Environment variables loaded"
