#!/bin/sh -f


export MPAS_ROOT=`pwd`
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

export serial_FC
export NETCDF=/usr/local/
export HDF5_DIR=/usr/local
export PNETCDF=$MPAS_ROOT/libraries
export PNETCDF_PATH=$MPAS_ROOT/libraries
export PIO=$MPAS_ROOT/libraries
export serial_FC=gfortran-5
export FC=gfortran-5
export CC=gcc-5
export CXX=g++-5
export OMPI_CC=$CC
export OMPI_CXX=$CXX
export OMPI_FC=$FC
export MPICC=mpicc
export MPIF77=mpifort
export MPIF90=mpifort
export MPIFC=mpifort
export target=gfortran
export USE_PIO2_MPAS=true
cd $MPAS_ROOT/software


echo "******************"
echo "(2) Building MPAS"
echo "******************"

cd $MPAS_ROOT

#export MPAS_EXTERNAL_LIBS="-L/usr/lib64 -ldl -lz -L${HDF5_DIR}/lib -lhdf5_hl -lhdf5"
#export MPAS_EXTERNAL_INCLUDES="-I${HDF5_DIR}/include -I/usr/include" 

# 1st:  init_atmosphere
make clean CORE=init_atmosphere
make ${target} CORE=init_atmosphere PRECISION=single USE_PIO2=${USE_PIO2_MPAS}

# 2nd:  atmosphere
#make clean CORE=atmosphere
#make ${target} CORE=atmosphere PRECISION=single USE_PIO2=${USE_PIO2_MPAS}

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
make clean
make FFLAGS=-ffree-line-length-none

echo "DONE BUILDING EVERYTHING FOR MPAS ... "

exit
