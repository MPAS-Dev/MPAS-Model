#!/bin/sh -f
# J. Cipriani - 02/2017

# environment variables / modules needed for MPAS compilation

export MPAS_ROOT=`pwd`
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

# get Linux distribution 
os_name=`uname`
distribution=`uname -r`

echo "Linux distribution = $distribution"

export serial_FC

module purge
module load ibm_smpi/default 
module load pgi/19.4
module load cmake/3.7.0

# Path to libnetcdf.a
# PIO, NETCDF and PNETCDF are set in the .bash_profile
export PNETCDF_PATH=$PNETCDF
export CMAKE=cmake
export F77=pgf90
export serial_FC=pgf90
export FC=pgf90
export F90=pgf90
export CC=pgcc
export CXX=pgc++
export MPICC=mpicc
export MPIF77=mpif90
export MPIF90=mpif90
export MPIFC=mpif90
export target=pgi
export USE_PIO2_MPAS=true
export CFLAGS="-q64 -O3 -DIBMR2Fortran"
export CPP="pgcc -E"
export CXXFLAGS="-q64 -O3"
export CXXCPP="pgc++ -E"
export FFLAGS="-q64 -O3"
export CUDA_LAUNCH_BLOCKING=0
export OMPI_CC=pgcc
export OMPI_CXX=pgc++
export OMPI_FC=pgf90
export OMPI_F90=pgf90
export OMPI_F77=pgf90

    
echo "******************"
echo "(2) Building MPAS"
echo "******************"


cd $MPAS_ROOT

# edit 1 MPAS file ...

if [[ ${USE_PIO2_MPAS} == "false" ]]; then
    sed -i -e 's/.*netcdf.inc.*/   !include "netcdf.inc"/' src/framework/mpas_io.F
fi

export MPAS_EXTERNAL_LIBS="-L$NETCDF/lib -lhdf5_hl -lhdf5 -lz"
export MPAS_EXTERNAL_INCLUDES="-I${NETCDF}/include" 

# 1st:  init_atmosphere
make clean CORE=init_atmosphere
make ${target} CORE=init_atmosphere PRECISION=single USE_PIO2=${USE_PIO2_MPAS}

# 2nd:  atmosphere
make clean CORE=atmosphere
make ${target} CORE=atmosphere PRECISION=single USE_PIO2=${USE_PIO2_MPAS}

mkdir logs
mkdir data
mkdir run
mkdir geocache


echo "**************************"
echo "(3) Building metis-5.1.0"
echo "**************************"

cd $MPAS_ROOT

rm -rf metis-5.1.0
tar -xvzf metis-5.1.0.tar.gz

export CC=gcc
cd metis-5.1.0
make config
make

echo "*****************************"
echo "(4) Building double_to_float"
echo "*****************************"

cd $MPAS_ROOT/dbl2flt/
${serial_FC} -o double_to_float_grid double_to_float_grid.f90 -I${NETCDF}/include -L${NETCDF}/lib -lnetcdff -lnetcdf


echo "*****************************"
echo "(5) Building grid_rotate"
echo "*****************************"

cd $MPAS_ROOT/grid_rotate/
cp Makefile.${target} Makefile
make clean
make
echo "DONE BUILDING EVERYTHING FOR MPAS ... "


exit
