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

if [[ $distribution == "3.0.101-0.46.1_1.0502.8871-cray_ari_s" ]]; then

    module swap PrgEnv-cray PrgEnv-intel
    module load cray-netcdf
    module load cray-parallel-netcdf
    module load cray-hdf5
    module load craype-broadwell
    module load cray-mpich

    # Path to libnetcdf.a
    export NETCDF=$NETCDF_DIR
    export PNETCDF=$PARALLEL_NETCDF_DIR
    export PNETCDF_PATH=$PARALLEL_NETCDF_DIR
    export PIO=$MPAS_ROOT/software/PIO_install
    export CMAKE_PATH=/mnt/lustre/shared_data/cmake-install/bin
    export F77=ftn
    export serial_FC=ftn
    export FC=ftn
    export CC=cc
    export CXX=CC
    export MPICC=cc
    export MPIF77=ftn
    export MPIF90=ftn
    export MPIFC=ftn
    export target=intel-nersc
    export USE_PIO2_MPAS=true

    echo "*************************"
    echo "(1) Building Parallel IO"
    echo "*************************"

    rm -rf $PIO
    mkdir $PIO
    cd $MPAS_ROOT/software
    rm -rf ParallelIO/
    rm -rf PIO_build/

    git clone git@github.com:NCAR/ParallelIO.git ./ParallelIO
    cd ParallelIO
    export PIOSRC=`pwd`
    cd ..
    mkdir PIO_build
    cd PIO_build
    $CMAKE_PATH/cmake -DNetCDF_C_PATH=$NETCDF -DNetCDF_Fortran_PATH=$NETCDF -DPnetCDF_PATH=$PNETCDF -DHDF5_PATH=$HDF5_DIR \
        -DCMAKE_INSTALL_PREFIX=$PIO -DPIO_ENABLE_TIMING=OFF $PIOSRC
    make
    make install

elif [[ $distribution == "2.6.32-279.14.1.el6.x86_64" ]]; then

    # Path to libnetcdf.a
    export NETCDF=/share/apps/netcdf4
    export HDF5_DIR=/share/apps/hdf5
    export PNETCDF=$MPAS_ROOT/libraries
    export PNETCDF_PATH=$MPAS_ROOT/libraries
    export PIOSRC=$MPAS_ROOT/software/ParallelIO-pio1_9_23/pio
    export PIO=$MPAS_ROOT/software/pio-1.9.23
    export CMAKE_PATH=$MPAS_ROOT/software/cmake-install/bin
    export serial_FC=ifort
    export FC=mpif90
    export CC=mpicc
    export MPICC=mpicc
    export MPIF77=mpifort
    export MPIF90=mpifort
    export MPIFC=mpifort
    export target=ifort
    export USE_PIO2_MPAS=false

    cd $MPAS_ROOT/software

    echo "************************************"
    echo "(0a) Untarring cmake & building ... "
    echo "************************************"

    rm -rf cmake-3.7.2
    tar -xvf cmake-3.7.2.tar  # -->  cmake-3.7.2

    cd cmake-3.7.2/
    ./bootstrap --prefix=$MPAS_ROOT/software/cmake-install
    make
    make install

    cd $MPAS_ROOT/software

    echo "*****************************"
    echo "(0b) Building Parallel netCDF"
    echo "*****************************"

    rm -rf parallel-netcdf-1.7.0
    tar -xvzf parallel-netcdf-1.7.0.tar.gz  # -->  parallel-netcdf-1.7.0

    cd parallel-netcdf-1.7.0
    make clean
    ./configure --prefix=$MPAS_ROOT/libraries
    make
    make install

    echo "*************************"
    echo "(1) Building Parallel IO"
    echo "*************************"

    rm -rf $PIO
    mkdir $PIO

    cd $PIO
    ${CMAKE_PATH}/cmake -DNETCDF_C_DIR=$NETCDF -DNETCDF_Fortran_DIR=$NETCDF -DPNETCDF_DIR=$PNETCDF -DCMAKE_VERBOSE_MAKEFILE=1 $PIOSRC
    # need to edit the CMakeCache.txt file --> with a sed command
    sed -i -e "s/.*PIO_DEFINITIONS.*/PIO_DEFINITIONS:STRING=-D_NETCDF;-D_PNETCDF;-DUSEMPIIO;-D_NOUSEMCT;-D_USEBOX;-DNO_MPIMOD/" CMakeCache.txt
    make

    
elif [[ $os_name == "Darwin" ]]; then
    # Mac (Darwin version 16.6.0)

    # Path to libnetcdf.a
    export NETCDF=/usr/local/
    export HDF5_DIR=/usr/local
    export PNETCDF=$MPAS_ROOT/libraries
    export PNETCDF_PATH=$MPAS_ROOT/libraries
    export PIOSRC=$MPAS_ROOT/software/ParallelIO-pio1_9_23/pio
    export PIO=$MPAS_ROOT/software/pio-1.9.23
    export CMAKE_PATH=/usr/local/bin
    export serial_FC=gfortran-6
    export FC=gfortran-6
    export CC=gcc-6
    export OMPI_CC=$CC
    export OMPI_FC=$FC
    export MPICC=mpicc
    export MPIF77=mpifort
    export MPIF90=mpifort
    export MPIFC=mpifort
    export target=gfortran
    export USE_PIO2_MPAS=false

    cd $MPAS_ROOT/software

    echo "************************************"
    echo "(0a) Untarring cmake & building ... "
    echo "************************************"

    # Skip cmake, we'll use the one installed by brew instead for mac

    #rm -rf cmake-3.7.2
    #tar -xvf cmake-3.7.2.tar  # -->  cmake-3.7.2
#
#    cd cmake-3.7.2/
#    ./bootstrap --prefix=$MPAS_ROOT/software/cmake-install
#    make
#    make install

    cd $MPAS_ROOT/software

    echo "*****************************"
    echo "(0b) Building Parallel netCDF"
    echo "*****************************"

    rm -rf parallel-netcdf-1.7.0
    tar -xvzf parallel-netcdf-1.7.0.tar.gz  # -->  parallel-netcdf-1.7.0

    cd parallel-netcdf-1.7.0
    make clean
    ./configure --prefix=$MPAS_ROOT/libraries
    make
    make install

    echo "*************************"
    echo "(1) Building Parallel IO"
    echo "*************************"

    rm -rf $PIO
    mkdir $PIO

    cd $PIO
    ${CMAKE_PATH}/cmake -DNETCDF_C_DIR=$NETCDF -DNETCDF_Fortran_DIR=$NETCDF -DPNETCDF_DIR=$PNETCDF -DCMAKE_VERBOSE_MAKEFILE=1 $PIOSRC
    # need to edit the CMakeCache.txt file --> with a sed command
    sed -i -e "s/.*PIO_DEFINITIONS.*/PIO_DEFINITIONS:STRING=-D_NETCDF;-D_PNETCDF;-DUSEMPIIO;-D_NOUSEMCT;-D_USEBOX;-DNO_MPIMOD/" CMakeCache.txt
    make


fi


echo "******************"
echo "(2) Building MPAS"
echo "******************"


cd $MPAS_ROOT

# edit 1 MPAS file ...

if [[ ${USE_PIO2_MPAS} == "false" ]]; then
    sed -i -e 's/.*netcdf.inc.*/   !include "netcdf.inc"/' src/framework/mpas_io.F
fi

export MPAS_EXTERNAL_LIBS="-L/usr/lib64 -ldl -lz -L${HDF5_DIR}/lib -lhdf5_hl -lhdf5"
export MPAS_EXTERNAL_INCLUDES="-I${HDF5_DIR}/include -I/usr/include" 

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

export CC=gcc
rm -rf metis-5.1.0
tar -xvzf metis-5.1.0.tar.gz

export PATH=$CMAKE_PATH:$PATH
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
if [[ $os_name == "Darwin" ]]; then
    make FFLAGS=-ffree-line-length-none
else
    make
fi
echo "DONE BUILDING EVERYTHING FOR MPAS ... "


exit
