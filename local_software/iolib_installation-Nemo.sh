#!/usr/bin/env bash

#
# Sources for all libraries used in this script can be found at
# Updated by P. Peixoto on Feb 2022 based on mduda's original script
# updated on Sep 2022 to download and install gcc-8
# updated by F.A.V.B. Alves on June 2023 to address some issues when system's gcc version is >=13
# Updated by D. C. de Souza on July 2023

BASEDIR=$PWD/

echo "Script for MPAS pre-requisites"
echo "-----------------------------"
echo " Set to work with GCC 8, GFORTRAN 8"

all=false
packtoinstall="pio"
echo "installing $packinstall"

while getopts i:a flag
do
    case "${flag}" in
        i) packtoinstall=${OPTARG};;
    esac
done

if [[ $BASEDIR == *"local_software"* ]]; then
    echo "We are in the right directory!" $BASEDIR
else
    echo "Please run this script inside local_software folder of MPAS"
    echo "in the main MPAS folder, run:"
    echo "mkdir local_software"
    echo "copy script to this folder and run it from there"
    exit 1
fi

if [[ ! -d "gccsources" ]]; then
    #Download data
    echo "Downloading data from gcc"
    URL=http://mirrors.concertpass.com/gcc/releases/gcc-8.5.0/gcc-8.5.0.tar.gz
    mkdir gccsources
    cd gccsources
    wget $URL 
    cd ..
else
    echo "Seems like we already have the gcc source in local dir!"
fi

if [[ ! -d "people/duda/files/mpas" ]]; then
    #Download data
    echo "Downloading data from ncar"
    URL=http://www2.mmm.ucar.edu/people/duda/files/mpas/
    wget $URL -np -nH -r -N -c -e robots=off
    ln -s people/duda/files/mpas/sources ncarsources
else
    echo "Seems like we already have the ncar sources in local dir!"
fi

echo
echo "Installing packages from source:" $packtoinstall

# Where to find sources for libraries

export LIBSRC=$BASEDIR/ncarsources
export LIBSRC_GCC=$BASEDIR/gccsources/

# Where to install libraries
export LIBBASE=$BASEDIR/libs #/scratch/duda/mpas-libs-gnu8.3.0
# Installing gcc on the same directory as the other libs is giving error
export LIBBASEGCC=$BASEDIR/libs-gcc

# Compilers
export SERIAL_FC=gfortran
export SERIAL_F77=gfortran
export SERIAL_CC=gcc
export SERIAL_CXX=g++
export MPI_FC=mpifort
export MPI_F77=mpifort
export MPI_CC=mpicc
export MPI_CXX=mpic++


export CC=$SERIAL_CC
export CXX=$SERIAL_CXX
export F77=$SERIAL_F77
export FC=$SERIAL_FC
#export F90=$SERIAL_FC
unset F90  # required to install mpich (God knows why...)
unset F90FLAGS  # required to install mpich (God knows why...)
export CFLAGS="-g"
export FFLAGS="-g -fbacktrace"                          
export FCFLAGS="-g -fbacktrace"
export F77FLAGS="-g -fbacktrace"                       

########################################
# GCC-8
########################################

if [[ $packtoinstall = "gcc" ]] ; then
   tar xzvf ${LIBSRC_GCC}/gcc-8.5.0.tar.gz
   cd gcc-8.5.0
   ./contrib/download_prerequisites
   ./configure --prefix=${LIBBASEGCC} --enable-languages=c,c++,fortran --disable-multilib
   make -j 54
   retVal=$?
   # @FAVBA I couldn't compile gcc8.5.0 with gcc13 unless I disabled some options. If compilation fails, tries compiling with nls and libsanitizer disabled
   if [ $retVal -ne 0 ]; then
      ./configure --prefix=${LIBBASEGCC} --enable-languages=c,c++,fortran --disable-multilib --disable-nls --disable-libsanitizer
      make -j 55
      retVal2=$?
      [ $retVal2 -ne 0 ] && echo "Compilation of gcc 8.5.0 failed" && exit $retVal2
   fi
   #make check
   make install
   #make testing
   cd ..
   rm -rf gcc-8.5.0
   #exit 1
fi


# LIBBASE: /p1-nemo/danilocs/mpas/MPAS-BRv8/local_software//libs
# BASEDIR: /p1-nemo/danilocs/mpas/MPAS-BRv8/local_software/
export PATH=${LIBBASE}/bin:$PATH
export PATH=${LIBBASEGCC}/bin:$PATH
export LD_LIBRARY_PATH=${LIBBASE}/lib/:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${LIBBASE}/lib64/:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${LIBBASEGCC}/lib/gcc/x86_64-pc-linux-gnu/:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${LIBBASEGCC}/lib64/:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${LIBBASEGCC}/lib:$LD_LIBRARY_PATH

#######################################
#Check if gcc-8 and gfortran-8 are in the system
########################################
if gcc --version | grep -q "8."; then
    if gfortran --version | grep -q "8."; then
	    echo "gcc-8 and gfortran-8 seems to be here. Great!"
    else
	    echo "Please install gfortran-8"
	    exit 1
    fi
else 
    echo "Please install gcc-8 and gfortran-8"
    echo "sudo apt-get install gcc-8"
    echo "sudo apt-get install gfortran-8"
    echo "or install from source"
    exit 1
fi


########################################
# MPICH
########################################

if [ $packtoinstall = "all" ] || [ $packtoinstall = "mpich" ] ; then
   tar xzvf ${LIBSRC}/mpich-4.1.2.tar.gz
   cd mpich-4.1.2
   ./configure --prefix=${LIBBASE}
   make -j 50
   #make check
   make install
   #make testing
   cd ..
   rm -rf mpich-4.1.2
   #exit 1
fi


########################################
# zlib 
########################################
if [ $packtoinstall = "all" ] || [ $packtoinstall = "zlib" ] ; then
    tar xzvf ${LIBSRC}/zlib-1.2.11.tar.gz
    cd zlib-1.2.11
    ./configure --prefix=${LIBBASE} --static
    make -j 4
    make install
    cd ..
    rm -rf zlib-1.2.11
    #exit 1
fi

########################################
# HDF5 
########################################
if [ $packtoinstall = "all" ] || [ $packtoinstall = "hdf5" ] ; then
    tar xjvf ${LIBSRC}/hdf5-1.10.5.tar.bz2
    cd hdf5-1.10.5
    export FC=$MPI_FC
    export CC=$MPI_CC
    export CXX=$MPI_CXX
    ./configure --prefix=${LIBBASE} --enable-parallel --with-zlib=${LIBBASE} --disable-shared
    make -j 4
    #make check
    make install
    cd ..
    rm -rf hdf5-1.10.5
    #exit 1
fi

########################################
# Parallel-netCDF - use mpich!!!
########################################
if [ $packtoinstall = "all" ] || [ $packtoinstall = "pnetcdf" ] ; then
    tar xzvf ${LIBSRC}/pnetcdf-1.11.2.tar.gz
    cd pnetcdf-1.11.2
    export CC=$SERIAL_CC
    export CXX=$SERIAL_CXX
    export F77=$SERIAL_F77
    export FC=$SERIAL_FC
    export MPICC=$MPI_CC
    export MPICXX=$MPI_CXX
    export MPIF77=$MPI_F77
    export MPIF90=$MPI_FC
    ### Will also need gcc in path
    ./configure --prefix=${LIBBASE}
    make -j 4
    #make check
    #make ptest
    #make testing
    make install
    cd ..
    rm -rf pnetcdf-1.11.2
    #exit 1
fi
export PNETCDF=${LIBBASE}

########################################
# netCDF (C library)
########################################
if [ $packtoinstall = "all" ] || [ $packtoinstall = "netcdf" ] ; then
    tar xzvf ${LIBSRC}/netcdf-c-4.7.0.tar.gz
    cd netcdf-c-4.7.0
    export CPPFLAGS="-I${LIBBASE}/include"
    export LDFLAGS="-L${LIBBASE}/lib"
    export LIBS="-lhdf5_hl -lhdf5 -lz -ldl"
    export CC=$MPI_CC
    ./configure --prefix=${LIBBASE} --disable-dap --enable-netcdf4 --enable-pnetcdf --enable-cdf5 --enable-parallel-tests --disable-shared
    make -j 4 
    make check
    make install

    cd ..
    rm -rf netcdf-c-4.7.0
    #exit 1
fi

export NETCDF=${LIBBASE}

########################################
# netCDF (Fortran interface library)
########################################
if [ $packtoinstall = "all" ] || [ $packtoinstall = "netcdf-fortran" ] ; then
    tar xzvf ${LIBSRC}/netcdf-fortran-4.4.5.tar.gz
    cd netcdf-fortran-4.4.5
    export FC=$MPI_FC
    export F77=$MPI_F77
    export LIBS="-lnetcdf ${LIBS}"
    ./configure --prefix=${LIBBASE} --enable-parallel-tests --disable-shared
    make -j 4
    make check
    make install
    cd ..
    rm -rf netcdf-fortran-4.4.5
    #exit 1
fi

########################################
# PIO
########################################
if [ $packtoinstall = "all" ] || [ $packtoinstall = "pio" ] ; then
    git clone https://github.com/NCAR/ParallelIO
    cd ParallelIO
    git checkout -b pio-2.4.4 pio2_4_4
    export PIOSRC=`pwd`
    cd ..
    mkdir pio
    cd pio
    export CC=$MPI_CC
    export FC=$MPI_FC
    cmake -DNetCDF_C_PATH=$NETCDF -DPIO_ENABLE_DOC=OFF -DNetCDF_Fortran_PATH=$NETCDF -DPnetCDF_PATH=$PNETCDF -DHDF5_PATH=$NETCDF -DCMAKE_INSTALL_PREFIX=$LIBBASE -DPIO_USE_MALLOC=ON -DCMAKE_VERBOSE_MAKEFILE=1 -DPIO_ENABLE_TIMING=OFF $PIOSRC
    make -j 50
    retVal=$?
    # @FAVBA newer cmake version (mine was v3.26.4) didn't work with gcc 8.5.0 for me. If cmake configuration fails, tries configuring using 'autoreconf' and 'configure'
    if [ $retVal -ne 0 ]; then
	make clean
        autoreconf -if
	    LIBRARY_PATH=$LIBBASE/lib CPATH=$LIBBASE/include ./configure --prefix=${LIBBASE} --disable-timing --enable-fortran
	    LIBRARY_PATH=$LIBBASE/lib CPATH=$LIBBASE/include make
    fi
    #make check
    make install
    cd ..
    rm -rf pio ParallelIO
fi

export PIO=$LIBBASE

########################################
# Other environment vars needed by MPAS
########################################
export MPAS_EXTERNAL_LIBS="-L${LIBBASE}/lib -lhdf5_hl -lhdf5 -ldl -lz"
export MPAS_EXTERNAL_INCLUDES="-I${LIBBASE}/include"

echo "MPAS env variables exported!"
