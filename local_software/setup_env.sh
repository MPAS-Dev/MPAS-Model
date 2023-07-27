#! /bin/bash

#
# This is a template to set up environment variables correctly
#


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


export PATH="$BASEDIR/libs/bin:$PATH"
export PKG_CONFIG_PATH="$BASEDIR/libs/lib/pkgconfig:$PKG_CONFIG_PATH"

export LD_LIBRARY_PATH="$BASEDIR/libs/lib:$LD_LIBRARY_PATH"

export LIBSRC=$BASEDIR/ncarsources
export LIBBASE=$BASEDIR/libs #/scratch/duda/mpas-libs-gnu8.3.0


export NETCDF_PATH=${LIBBASE}
export PNETCDF_PATH=${LIBBASE}
export PIO_PATH=${LIBBASE}


host='nemo'
if [[ $host == mac* ]]; then
	echo "DETECTED MAC CLUSTER (AMD), LOADING STUFF"

	source /etc/profile.d/modules.sh

	module unload intel
	module unload mpi.intel
	module load gcc/4.8
	module load mpi.intel/5.1_gcc
	module load binutils

	# setup compile stuff
	#For GNU
	export FC=gfortran
	export F77=gfortran
	export F90=gfortran
	export CC=gcc

	#For Intel - gnarg
	#export FC=ifort
	#export F77=ifort
	#export F90=ifort
	#export CC=icc

	#MPI
	export MPIFC=mpif90
	export MPIF90=mpif90
	export MPIF77=mpif77
	export MPICC=mpicc
	
	
elif [[ $host == bgq* ]]; then

	echo "DETECTED BLUE GENE CLUSTER, LOADING STUFF"

	#module unload gcc-bgq/4.4.7
	module load xl
	module load mpi/xl

	export NETCDF_PATH=/opt/apps/netcdf/4.3.2
	export PNETCDF_PATH=/opt/apps/pnetcdf/1.6.0

	export PATH="$NETCDF_PATH/bin:$PATH"
	export PATH="$PNETCDF_PATH/bin:$PATH"
	export PKG_CONFIG_PATH="$NETCDF_PATH/lib/pkgconfig:$PKG_CONFIG_PATH"

	export LD_LIBRARY_PATH="$NETCDF_PATH/lib:$LD_LIBRARY_PATH"
	export LD_LIBRARY_PATH="$PNETCDF_PATH/lib:$LD_LIBRARY_PATH"


	# setup compile stuff
	#For xl
	export FC=xlf_r
	export F77=xlf_r
	export F90=xlf90_r
	export CC=xlc_r
	export CPP=cpp
	export CXX=xlC_r

	#MPI
	export MPIFC=mpixlf2003_r
	export MPIF90=mpixlf90_r
	export MPIF77=mpixlf77_r
	export MPICC=mpixlc_r
	
elif [[ $host == ppeixoto* ]]; then
	
	echo "Detected a local pedro peixoto machine, loading enviroment..."
	export SERIAL_FC=gfortran-8
	export SERIAL_F77=gfortran-8
	export SERIAL_CC=gcc-8
	export SERIAL_CXX=g++-8
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

elif [[ $host == ybytu* ]]; then
	
	echo "Detected a labmap cluster ybytu, loading enviroment..."
	export SERIAL_FC=gfortran-8
	export SERIAL_F77=gfortran-8
	export SERIAL_CC=gcc-8
	export SERIAL_CXX=g++-8
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


elif [[ $host == nemo ]]; then

        echo "Detected MASTER's nemo machine, loading enviroment..."
        export MPI_FC=mpifort
        export MPI_F77=mpifort
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

        BASEDIR=$PWD/local_software/
        export LIBSRC_GCC=$BASEDIR/gccsources/
        export LIBBASE=$BASEDIR/libs
        export LIBBASEGCC=$BASEDIR/libs-gcc
       
        export PATH=${LIBBASE}/bin:$PATH
        export PATH=${LIBBASEGCC}/bin:$PATH
        export LD_LIBRARY_PATH=${LIBBASE}/lib/:$LD_LIBRARY_PATH
        export LD_LIBRARY_PATH=${LIBBASE}/lib64/:$LD_LIBRARY_PATH
        export LD_LIBRARY_PATH=${LIBBASEGCC}/lib/gcc/x86_64-pc-linux-gnu/:$LD_LIBRARY_PATH
        export LD_LIBRARY_PATH=${LIBBASEGCC}/lib64/:$LD_LIBRARY_PATH
        export LD_LIBRARY_PATH=${LIBBASEGCC}/lib:$LD_LIBRARY_PATH

        export PATH=${PWD}/MPAS-Tools/mesh_tools/grid_rotate:${PATH}

else
	echo "********************************************************"
	echo "****************** ENVIRONMENT UNKNOWN *****************"
	echo "********************************************************"
	return
fi


# FOR MPAS
export MPAS_DIR=`pwd`
export NETCDF=$NETCDF_PATH
export PNETCDF=$PNETCDF_PATH
export PIO=$PIO_PATH
export MPAS_EXTERNAL_LIBS="-L${LIBBASE}/lib -lhdf5_hl -lhdf5 -ldl -lz"
export MPAS_EXTERNAL_INCLUDES="-I${LIBBASE}/include"

echo "Environment variables loaded"
