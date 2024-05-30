MODEL_FORMULATION =

ifneq "${MPAS_SHELL}" ""
        SHELL = ${MPAS_SHELL}
endif

dummy:
	( $(MAKE) error )

gnu:   # BUILDTARGET GNU Fortran, C, and C++ compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpicxx" \
	"FC_SERIAL = gfortran" \
	"CC_SERIAL = gcc" \
	"CXX_SERIAL = g++" \
	"FFLAGS_PROMOTION = -fdefault-real-8 -fdefault-double-8" \
	"FFLAGS_OPT = -std=f2008 -O3 -ffree-line-length-none -fconvert=big-endian -ffree-form" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -std=f2008 -g -ffree-line-length-none -fconvert=big-endian -ffree-form -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow" \
	"CFLAGS_DEBUG = -g" \
	"CXXFLAGS_DEBUG = -g" \
	"LDFLAGS_DEBUG = -g" \
	"FFLAGS_OMP = -fopenmp" \
	"CFLAGS_OMP = -fopenmp" \
	"FFLAGS_ACC =" \
	"CFLAGS_ACC =" \
	"PICFLAG = -fPIC" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"OPENACC = $(OPENACC)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

xlf:   # BUILDTARGET IBM XL compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpifort" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpic++" \
	"FC_SERIAL = xlf2003_r" \
	"CC_SERIAL = xlc_r" \
	"CXX_SERIAL = xlc++_r" \
	"FFLAGS_PROMOTION = -qrealsize=8" \
	"FFLAGS_OPT = -O3 -qufmt=be -WF,-qnotrigraph" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -O0 -g -C -qufmt=be -WF,-qnotrigraph" \
	"CFLAGS_DEBUG = -O0 -g" \
	"CXXFLAGS_DEBUG = -O0 -g" \
	"LDFLAGS_DEBUG = -O0 -g" \
	"FFLAGS_OMP = -qsmp=omp" \
	"CFLAGS_OMP = -qsmp=omp" \
	"PICFLAG = -qpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

xlf-summit-omp-offload:   # BUILDTARGET IBM XL compilers w/OpenMP offloading on ORNL Summit
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpiCC" \
	"FC_SERIAL = xlf90_r" \
	"CC_SERIAL = xlc_r" \
	"CXX_SERIAL = xlc++_r" \
	"FFLAGS_PROMOTION = -qrealsize=8" \
	"FFLAGS_OPT = -g -qfullpath -qmaxmem=-1 -qphsinfo -qzerosize -qfree=f90 -qxlf2003=polymorphic -qspillsize=2500 -qextname=flush -O2 -qstrict -Q" \
	"CFLAGS_OPT = -g -qfullpath -qmaxmem=-1 -qphsinfo -O3" \
	"CXXFLAGS_OPT = -g -qfullpath -qmaxmem=-1 -qphsinfo -O3" \
	"LDFLAGS_OPT = -Wl,--relax -Wl,--allow-multiple-definition -qsmp -qoffload -lcudart -L$(CUDA_DIR)/lib64" \
	"FFLAGS_GPU = -qsmp -qoffload" \
	"LDFLAGS_GPU = -qsmp -qoffload -lcudart -L$(CUDA_DIR)/lib64" \
	"FFLAGS_DEBUG = -O0 -g -qinitauto=7FF7FFFF -qflttrap=ov:zero:inv:en" \
	"CFLAGS_DEBUG = -O0 -g" \
	"CXXFLAGS_DEBUG = -O0 -g" \
	"LDFLAGS_DEBUG = -O0 -g" \
	"FFLAGS_OMP = -qsmp=omp" \
	"CFLAGS_OMP = -qsmp=omp" \
	"PICFLAG = -qpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"OPENMP_OFFLOAD = $(OPENMP_OFFLOAD)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI -DFORTRAN_SAME -DCPRIBM -DLINUX" )

ftn:   # BUILDTARGET Cray compilers
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"CXX_PARALLEL = CC" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = cc" \
	"CXX_SERIAL = CC" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -i4 -gopt -O2 -Mvect=nosse -Kieee -convert big_endian" \
	"CFLAGS_OPT = -fast" \
	"CXXFLAGS_OPT = -fast" \
	"LDFLAGS_OPT = " \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -mp" \
	"FFLAGS_ACC =" \
	"CFLAGS_ACC =" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"OPENACC = $(OPENACC)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

titan-cray:   # BUILDTARGET (deprecated) Cray compilers with options for ORNL Titan
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = gcc" \
	"FFLAGS_PROMOTION = -default64" \
	"FFLAGS_OPT = -s integer32 -O3 -f free -N 255 -em -ef" \
	"CFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_OMP = " \
	"CFLAGS_OMP = " \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

nvhpc:   # BUILDTARGET NVIDIA HPC SDK
	( $(MAKE) all \
	"FC_PARALLEL = mpifort" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpic++" \
	"FC_SERIAL = nvfortran" \
	"CC_SERIAL = nvc" \
	"CXX_SERIAL = nvc++" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -gopt -O4 -byteswapio -Mfree" \
	"CFLAGS_OPT = -gopt -O3" \
	"CXXFLAGS_OPT = -gopt -O3" \
	"LDFLAGS_OPT = -gopt -O3" \
	"FFLAGS_DEBUG = -O0 -g -Mbounds -Mchkptr -byteswapio -Mfree -Ktrap=divz,fp,inv,ovf -traceback" \
	"CFLAGS_DEBUG = -O0 -g -traceback" \
	"CXXFLAGS_DEBUG = -O0 -g -traceback" \
	"LDFLAGS_DEBUG = -O0 -g -Mbounds -Ktrap=divz,fp,inv,ovf -traceback" \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -mp" \
	"FFLAGS_ACC = -Mnofma -acc -gpu=cc70,cc80 -Minfo=accel" \
	"CFLAGS_ACC =" \
	"PICFLAG = -fpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"OPENACC = $(OPENACC)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI -DCPRPGI" )

pgi:   # BUILDTARGET PGI compiler suite
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpicxx" \
	"FC_SERIAL = pgf90" \
	"CC_SERIAL = pgcc" \
	"CXX_SERIAL = pgc++" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -O3 -byteswapio -Mfree" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -O0 -g -Mbounds -Mchkptr -byteswapio -Mfree -Ktrap=divz,fp,inv,ovf -traceback" \
	"CFLAGS_DEBUG = -O0 -g -traceback" \
	"CXXFLAGS_DEBUG = -O0 -g -traceback" \
	"LDFLAGS_DEBUG = -O0 -g -Mbounds -Ktrap=divz,fp,inv,ovf -traceback" \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -mp" \
	"FFLAGS_ACC = -Mnofma -acc -Minfo=accel" \
	"CFLAGS_ACC =" \
	"PICFLAG = -fpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"OPENACC = $(OPENACC)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI -DCPRPGI" )

pgi-summit:   # BUILDTARGET PGI compiler suite w/OpenACC options for ORNL Summit
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpicxx" \
	"FC_SERIAL = pgf90" \
	"CC_SERIAL = pgcc" \
	"CXX_SERIAL = pgc++" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -g -O3 -byteswapio -Mfree" \
	"CFLAGS_OPT = -O3 " \
	"CXXFLAGS_OPT = -O3 " \
	"LDFLAGS_OPT = -O3 " \
	"FFLAGS_ACC = -acc -Minfo=accel -ta=tesla:cc70,cc60,deepcopy,nollvm " \
	"CFLAGS_ACC = -acc -Minfo=accel -ta=tesla:cc70,cc60,deepcopy,nollvm "  \
	"FFLAGS_DEBUG = -O0 -g -Mbounds -Mchkptr -byteswapio -Mfree -Ktrap=divz,fp,inv,ovf -traceback" \
	"CFLAGS_DEBUG = -O0 -g -traceback" \
	"CXXFLAGS_DEBUG = -O0 -g -traceback" \
	"LDFLAGS_DEBUG = -O0 -g -Mbounds -Ktrap=divz,fp,inv,ovf -traceback" \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -mp" \
	"PICFLAG = -fpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"OPENACC = $(OPENACC)" \
	"CPPFLAGS = -DpgiFortran -D_MPI -DUNDERSCORE" )

pgi-nersc:   # BUILDTARGET (deprecated) PGI compilers on NERSC machines
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"CXX_PARALLEL = CC" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = cc" \
	"CXX_SERIAL = CC" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -O3 -byteswapio -Mfree" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -mp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI -DCPRPGI" )

pgi-llnl:   # BUILDTARGET (deprecated) PGI compilers on LLNL machines
	( $(MAKE) all \
	"FC_PARALLEL = mpipgf90" \
	"CC_PARALLEL = pgcc" \
	"CXX_PARALLEL = mpipgcxx" \
	"FC_SERIAL = pgf90" \
	"CC_SERIAL = pgcc" \
	"CXX_SERIAL = pgc++" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -i4 -g -O2 -byteswapio" \
	"CFLAGS_OPT = -fast" \
	"CXXFLAGS_OPT = -fast" \
	"LDFLAGS_OPT = " \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -mp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI -DCPRPGI" )

ifort:   # BUILDTARGET Intel Fortran, C, and C++ compiler suite
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpicxx" \
	"FC_SERIAL = ifort" \
	"CC_SERIAL = icc" \
	"CXX_SERIAL = icpc" \
	"FFLAGS_PROMOTION = -real-size 64" \
	"FFLAGS_OPT = -O3 -convert big_endian -free -align array64byte" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -g -convert big_endian -free -check all -fpe0 -traceback" \
	"CFLAGS_DEBUG = -g -traceback" \
	"CXXFLAGS_DEBUG = -g -traceback" \
	"LDFLAGS_DEBUG = -g -fpe0 -traceback" \
	"FFLAGS_OMP = -qopenmp" \
	"CFLAGS_OMP = -qopenmp" \
	"PICFLAG = -fpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

ifort-scorep:   # BUILDTARGET Intel compiler suite with ScoreP profiling library
	( $(MAKE) all \
	"FC_PARALLEL = scorep --compiler mpif90" \
	"CC_PARALLEL = scorep --compiler mpicc" \
	"CXX_PARALLEL = scorep --compiler mpicxx" \
	"FC_SERIAL = ifort" \
	"CC_SERIAL = icc" \
	"CXX_SERIAL = icpc" \
	"FFLAGS_PROMOTION = -real-size 64" \
	"FFLAGS_OPT = -O3 -g -convert big_endian -free -align array64byte" \
	"CFLAGS_OPT = -O3 -g" \
	"CXXFLAGS_OPT = -O3 -g" \
	"LDFLAGS_OPT = -O3 -g" \
	"FFLAGS_DEBUG = -g -convert big_endian -free -check all -fpe0 -traceback" \
	"CFLAGS_DEBUG = -g -traceback" \
	"CXXFLAGS_DEBUG = -g -traceback" \
	"LDFLAGS_DEBUG = -g -fpe0 -traceback" \
	"FFLAGS_OMP = -qopenmp" \
	"CFLAGS_OMP = -qopenmp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

ifort-gcc:   # BUILDTARGET Intel Fortran compiler and GNU C/C++ compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpicxx" \
	"FC_SERIAL = ifort" \
	"CC_SERIAL = gcc" \
	"CXX_SERIAL = g++" \
	"FFLAGS_PROMOTION = -real-size 64" \
	"FFLAGS_OPT = -O3 -convert big_endian -free -align array64byte" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -g -convert big_endian -free -check all -fpe0 -traceback" \
	"CFLAGS_DEBUG = -g" \
	"CXXFLAGS_DEBUG = -g" \
	"LDFLAGS_DEBUG = -g -fpe0 -traceback" \
	"FFLAGS_OMP = -qopenmp" \
	"CFLAGS_OMP = -fopenmp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

intel-mpi:   # BUILDTARGET Intel compiler suite with Intel MPI library
	( $(MAKE) all \
	"FC_PARALLEL = mpiifort" \
	"CC_PARALLEL = mpiicc" \
	"CXX_PARALLEL = mpiicpc" \
	"FC_SERIAL = ifort" \
	"CC_SERIAL = icc" \
	"CXX_SERIAL = icpc" \
	"FFLAGS_PROMOTION = -real-size 64" \
	"FFLAGS_OPT = -O3 -convert big_endian -free -align array64byte" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -g -convert big_endian -free -CU -CB -check all -fpe0 -traceback" \
	"CFLAGS_DEBUG = -g -traceback" \
	"CXXFLAGS_DEBUG = -g -traceback" \
	"LDFLAGS_DEBUG = -g -fpe0 -traceback" \
	"FFLAGS_OMP = -qopenmp" \
	"CFLAGS_OMP = -qopenmp" \
	"PICFLAG = -fpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

gfortran:   # BUILDTARGET GNU Fortran, C, and C++ compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpicxx" \
	"FC_SERIAL = gfortran" \
	"CC_SERIAL = gcc" \
	"CXX_SERIAL = g++" \
	"FFLAGS_PROMOTION = -fdefault-real-8 -fdefault-double-8" \
	"FFLAGS_OPT = -O3 -ffree-line-length-none -fconvert=big-endian -ffree-form" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -g -ffree-line-length-none -fconvert=big-endian -ffree-form -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow" \
	"CFLAGS_DEBUG = -g" \
	"CXXFLAGS_DEBUG = -g" \
	"LDFLAGS_DEBUG = -g" \
	"FFLAGS_OMP = -fopenmp" \
	"CFLAGS_OMP = -fopenmp" \
	"FFLAGS_ACC =" \
	"CFLAGS_ACC =" \
	"PICFLAG = -fPIC" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"OPENACC = $(OPENACC)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

gfortran-clang:   # BUILDTARGET GNU Fortran compiler with LLVM clang/clang++ compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc -cc=clang" \
	"CXX_PARALLEL = mpicxx -cxx=clang++" \
	"FC_SERIAL = gfortran" \
	"CC_SERIAL = clang" \
	"CXX_SERIAL = clang++" \
	"FFLAGS_PROMOTION = -fdefault-real-8 -fdefault-double-8" \
	"FFLAGS_OPT = -O3 -m64 -ffree-line-length-none -fconvert=big-endian -ffree-form" \
	"CFLAGS_OPT = -O3 -m64" \
	"CXXFLAGS_OPT = -O3 -m64" \
	"LDFLAGS_OPT = -O3 -m64" \
	"FFLAGS_DEBUG = -g -m64 -ffree-line-length-none -fconvert=big-endian -ffree-form -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow" \
	"CFLAGS_DEBUG = -g -m64" \
	"CXXFLAGS_DEBUG = -O3 -m64" \
	"LDFLAGS_DEBUG = -g -m64" \
	"FFLAGS_OMP = -fopenmp" \
	"CFLAGS_OMP = -fopenmp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

g95:   # BUILDTARGET (deprecated) G95 Fortran compiler with GNU C/C++ compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpif90" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpicxx" \
	"FC_SERIAL = g95" \
	"CC_SERIAL = gcc" \
	"CXX_SERIAL = g++" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -O3 -ffree-line-length-huge -fendian=big" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_OMP = -fopenmp" \
	"CFLAGS_OMP = -fopenmp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

pathscale-nersc:   # BUILDTARGET (deprecated) Pathscale compilers on NERSC machines
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"CXX_PARALLEL = CC" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = cc" \
	"CXX_SERIAL = CC" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -O3 -freeform -extend-source" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -mp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

cray-nersc:   # BUILDTARGET (deprecated) Cray compilers on NERSC machines
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"CXX_PARALLEL = CC" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = cc" \
	"CXX_SERIAL = CC" \
	"FFLAGS_PROMOTION = -default64" \
	"FFLAGS_OPT = -O3 -f free" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_OMP = " \
	"CFLAGS_OMP = " \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

gnu-nersc:   # BUILDTARGET (deprecated) GNU compilers on NERSC machines
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"CXX_PARALLEL = CC" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = cc" \
	"CXX_SERIAL = CC" \
	"FFLAGS_PROMOTION = -fdefault-real-8 -fdefault-double-8" \
	"FFLAGS_OPT = -O3 -m64 -ffree-line-length-none -fconvert=big-endian -ffree-form" \
	"CFLAGS_OPT = -O3 -m64" \
	"CXXFLAGS_OPT = -O3 -m64" \
	"LDFLAGS_OPT = -O3 -m64" \
	"FFLAGS_DEBUG = -g -m64 -ffree-line-length-none -fconvert=big-endian -ffree-form" \
	"CFLAGS_DEBUG = -g -m64" \
	"CXXFLAGS_DEBUG = -g -m64" \
	"LDFLAGS_DEBUG = -g -m64" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"SERIAL = $(SERIAL)" \
	"USE_PAPI = $(USE_PAPI)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI $(FILE_OFFSET) $(ZOLTAN_DEFINE)" )

intel-nersc:   # BUILDTARGET (deprecated) Intel compilers on NERSC machines
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"CXX_PARALLEL = CC" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = cc" \
	"CXX_SERIAL = CC" \
	"FFLAGS_PROMOTION = -real-size 64" \
	"FFLAGS_OPT = -O3 -convert big_endian -free -align array64byte" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_OMP = -qopenmp" \
	"CFLAGS_OMP = -qopenmp" \
	"FFLAGS_DEBUG = -real-size 64 -g -convert big_endian -free -check all -gen-interfaces -warn interfaces -traceback" \
	"CFLAGS_DEBUG = -g -traceback" \
	"CXXFLAGS_DEBUG = -g -traceback" \
	"LDFLAGS_DEBUG = -g -traceback" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

bluegene:   # BUILDTARGET (deprecated) IBM XL compilers on BlueGene/Q systems
	( $(MAKE) all \
	"FC_PARALLEL = mpixlf95_r" \
	"CC_PARALLEL = mpixlc_r" \
	"CXX_PARALLEL = mpixlcxx_r" \
	"FC_SERIAL = bgxlf95_r" \
	"CC_SERIAL = bgxlc_r" \
	"CXX_SERIAL = bgxlc++_r" \
	"FFLAGS_PROMOTION = -qrealsize=8" \
	"FFLAGS_OPT = -O2 -g" \
	"CFLAGS_OPT = -O2 -g" \
	"CXXFLAGS_OPT = -O2 -g" \
	"LDFLAGS_OPT = -O2 -g" \
	"FFLAGS_DEBUG = -O0 -g -C -qinitalloc -qinitauto" \
	"CFLAGS_DEBUG = -O0 -g" \
	"CXXFLAGS_DEBUG = -O0 -g" \
	"LDFLAGS_DEBUG = -O0 -g" \
	"FFLAGS_OMP = -qsmp=omp" \
	"CFLAGS_OMP = -qsmp=omp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

llvm:   # BUILDTARGET LLVM flang, clang, and clang++ compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpifort" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpic++" \
	"FC_SERIAL = flang" \
	"CC_SERIAL = clang" \
	"CXX_SERIAL = clang++" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -O3 -g -Mbyteswapio -Mfreeform" \
	"CFLAGS_OPT = -O3 -g" \
	"CXXFLAGS_OPT = -O3 -g" \
	"LDFLAGS_OPT = -O3 -g" \
	"FFLAGS_DEBUG = -O0 -g -Mbounds -Mchkptr -Mbyteswapio -Mfreeform -Mstandard" \
	"CFLAGS_DEBUG = -O0 -g -Weverything" \
	"CXXFLAGS_DEBUG = -O0 -g -Weverything" \
	"LDFLAGS_DEBUG = -O0 -g" \
	"FFLAGS_OMP = -mp" \
	"CFLAGS_OMP = -fopenmp" \
	"PICFLAG = -fpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

nag:   # BUILDTARGET NAG Fortran compiler and GNU C/C++ compilers
	( $(MAKE) all \
	"FC_PARALLEL = mpifort" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpic++" \
	"FC_SERIAL = nagfor" \
	"CC_SERIAL = gcc" \
	"CXX_SERIAL = g++" \
	"FFLAGS_PROMOTION = -r8" \
	"FFLAGS_OPT = -free -mismatch -O3 -convert=big_ieee" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -free -mismatch -O0 -g -C -convert=big_ieee" \
	"CFLAGS_DEBUG = -O0 -g -Wall -pedantic" \
	"CXXFLAGS_DEBUG = -O0 -g -Wall -pedantic" \
	"LDFLAGS_DEBUG = -O0 -g -C" \
	"FFLAGS_OMP = -qsmp=omp" \
	"CFLAGS_OMP = -qsmp=omp" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI -DUNDERSCORE -DNAG_COMPILER" )

cray:   # BUILDTARGET Cray Programming Environment
	( $(MAKE) all \
	"FC_PARALLEL = ftn" \
	"CC_PARALLEL = cc" \
	"CXX_PARALLEL = CC" \
	"FC_SERIAL = ftn" \
	"CC_SERIAL = cc" \
	"CXX_SERIAL = CC" \
	"FFLAGS_PROMOTION = -sreal64" \
	"FFLAGS_OPT = -Ofast -ffree" \
	"CFLAGS_OPT =  -Ofast" \
	"CXXFLAGS_OPT = -Ofast" \
	"LDFLAGS_OPT =  -Ofast -hbyteswapio" \
	"FFLAGS_DEBUG = -eD -O0 -ffree" \
	"CFLAGS_DEBUG = -O0 -g -Weverything" \
	"CXXFLAGS_DEBUG = -O0 -g -Weverything" \
	"LDFLAGS_DEBUG = -eD -O0 -hbyteswapio" \
	"FFLAGS_OMP = -homp" \
	"CFLAGS_OMP = -fopenmp" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

intel:   # BUILDTARGET Intel oneAPI Fortran, C, and C++ compiler suite
	( $(MAKE) all \
	"FC_PARALLEL = mpifort" \
	"CC_PARALLEL = mpicc" \
	"CXX_PARALLEL = mpic++" \
	"FC_SERIAL = ifx" \
	"CC_SERIAL = icx" \
	"CXX_SERIAL = icpx" \
	"FFLAGS_PROMOTION = -real-size 64" \
	"FFLAGS_OPT = -O3 -convert big_endian -free -align array64byte" \
	"CFLAGS_OPT = -O3" \
	"CXXFLAGS_OPT = -O3" \
	"LDFLAGS_OPT = -O3" \
	"FFLAGS_DEBUG = -g -convert big_endian -free -check all -fpe0 -traceback" \
	"CFLAGS_DEBUG = -g -traceback" \
	"CXXFLAGS_DEBUG = -g -traceback" \
	"LDFLAGS_DEBUG = -g -fpe0 -traceback" \
	"FFLAGS_OMP = -qopenmp" \
	"CFLAGS_OMP = -qopenmp" \
	"PICFLAG = -fpic" \
	"BUILD_TARGET = $(@)" \
	"CORE = $(CORE)" \
	"DEBUG = $(DEBUG)" \
	"USE_PAPI = $(USE_PAPI)" \
	"OPENMP = $(OPENMP)" \
	"CPPFLAGS = $(MODEL_FORMULATION) -D_MPI" )

CPPINCLUDES =
FCINCLUDES =
LIBS =

ifneq "$(PIO)" ""
#
# Regardless of PIO library version, look for a lib subdirectory of PIO path
# NB: PIO_LIB is used later, so we don't just set LIBS directly
#
ifneq ($(wildcard $(PIO)/lib), )
	PIO_LIB = $(PIO)/lib
else
	PIO_LIB = $(PIO)
endif
LIBS = -L$(PIO_LIB)

#
# Regardless of PIO library version, look for an include subdirectory of PIO path
#
ifneq ($(wildcard $(PIO)/include), )
	CPPINCLUDES += -I$(PIO)/include
	FCINCLUDES += -I$(PIO)/include
else
	CPPINCLUDES += -I$(PIO)
	FCINCLUDES += -I$(PIO)
endif

#
# Depending on PIO version, libraries may be libpio.a, or libpiof.a and libpioc.a
# Keep open the possibility of shared libraries in future with, e.g., .so suffix
#
# Check if libpio.* exists and link -lpio if so, but we make an exception for
# libpio.settings (a file added in PIO2), which is not a library to link
ifneq ($(wildcard $(PIO_LIB)/libpio\.*), )
	# Makefiles don't support "and" operators so we have nested "if" instead
	ifneq "$(wildcard $(PIO_LIB)/libpio\.*)" "$(PIO_LIB)/libpio.settings"
		LIBS += -lpio
	endif
endif

ifneq ($(wildcard $(PIO_LIB)/libpiof\.*), )
	LIBS += -lpiof
endif
ifneq ($(wildcard $(PIO_LIB)/libpioc\.*), )
	LIBS += -lpioc
endif
ifneq ($(wildcard $(PIO_LIB)/libgptl\.*), )
	LIBS += -lgptl
endif

else # Not using PIO, using SMIOL
	LIBS += -L$(PWD)/src/external/SMIOL -lsmiolf -lsmiol
	FCINCLUDES += -I$(PWD)/src/external/SMIOL
endif

ifneq "$(NETCDF)" ""
ifneq ($(wildcard $(NETCDF)/lib), )
	NETCDFLIBLOC = lib
endif
ifneq ($(wildcard $(NETCDF)/lib64), )
	NETCDFLIBLOC = lib64
endif
	CPPINCLUDES += -I$(NETCDF)/include
	FCINCLUDES += -I$(NETCDF)/include
	LIBS += -L$(NETCDF)/$(NETCDFLIBLOC)
	NCLIB = -lnetcdf
	NCLIBF = -lnetcdff
	ifneq ($(wildcard $(NETCDF)/$(NETCDFLIBLOC)/libnetcdff.*), ) # CHECK FOR NETCDF4
		LIBS += $(NCLIBF)
	endif # CHECK FOR NETCDF4
	ifneq "$(NETCDFF)" ""
		FCINCLUDES += -I$(NETCDFF)/include
		LIBS += -L$(NETCDFF)/$(NETCDFLIBLOC)
		LIBS += $(NCLIBF)
	endif
	LIBS += $(NCLIB)
endif


ifneq "$(PNETCDF)" ""
ifneq ($(wildcard $(PNETCDF)/lib), )
	PNETCDFLIBLOC = lib
endif
ifneq ($(wildcard $(PNETCDF)/lib64), )
	PNETCDFLIBLOC = lib64
endif
	CPPINCLUDES += -I$(PNETCDF)/include
	FCINCLUDES += -I$(PNETCDF)/include
	LIBS += -L$(PNETCDF)/$(PNETCDFLIBLOC) -lpnetcdf
endif

ifneq "$(LAPACK)" ""
        LIBS += -L$(LAPACK)
        LIBS += -llapack
        LIBS += -lblas
endif

RM = rm -f
CPP = cpp -P -traditional
RANLIB = ranlib

ifdef CORE

ifneq ($(wildcard src/core_$(CORE)), ) # CHECK FOR EXISTENCE OF CORE DIRECTORY

ifneq ($(wildcard src/core_$(CORE)/build_options.mk), ) # Check for build_options.mk
include src/core_$(CORE)/build_options.mk
else # ELSE Use Default Options
EXE_NAME=$(CORE)_model
NAMELIST_SUFFIX=$(CORE)
endif

override CPPFLAGS += -DMPAS_NAMELIST_SUFFIX=$(NAMELIST_SUFFIX)
override CPPFLAGS += -DMPAS_EXE_NAME=$(EXE_NAME)

else # ELSE CORE DIRECTORY CHECK

report_builds: all

endif # END CORE DIRECTORY CHECK

ifeq "$(DEBUG)" "true"

ifndef FFLAGS_DEBUG
	FFLAGS=$(FFLAGS_OPT)
	CFLAGS=$(CFLAGS_OPT)
	CXXFLAGS=$(CXXFLAGS_OPT)
	LDFLAGS=$(LDFLAGS_OPT)
	DEBUG_MESSAGE="Debug flags are not defined for this compile group. Defaulting to Optimized flags"
else # FFLAGS_DEBUG IF
	FFLAGS=$(FFLAGS_DEBUG)
	CFLAGS=$(CFLAGS_DEBUG)
	CXXFLAGS=$(CXXFLAGS_DEBUG)
	LDFLAGS=$(LDFLAGS_DEBUG)
	override CPPFLAGS += -DMPAS_DEBUG
	DEBUG_MESSAGE="Debugging is on."
endif # FFLAGS_DEBUG IF

else # DEBUG IF
	FFLAGS=$(FFLAGS_OPT)
	CFLAGS=$(CFLAGS_OPT)
	CXXFLAGS=$(CXXFLAGS_OPT)
	LDFLAGS=$(LDFLAGS_OPT)
	DEBUG_MESSAGE="Debugging is off."
endif # DEBUG IF

FC=$(FC_PARALLEL)
CC=$(CC_PARALLEL)
CXX=$(CXX_PARALLEL)
SFC=$(FC_SERIAL)
SCC=$(CC_SERIAL)
PARALLEL_MESSAGE="Parallel version is on."

ifeq "$(OPENMP)" "true"
	FFLAGS += $(FFLAGS_OMP)
	CFLAGS += $(CFLAGS_OMP)
	CXXFLAGS += $(CFLAGS_OMP)
	override CPPFLAGS += "-DMPAS_OPENMP"
	LDFLAGS += $(FFLAGS_OMP)
endif #OPENMP IF

ifeq "$(OPENACC)" "true"
        FFLAGS += $(FFLAGS_ACC)
        CFLAGS += $(CFLAGS_ACC)
        CXXFLAGS += $(CFLAGS_ACC)
        override CPPFLAGS += "-DMPAS_OPENACC"
        LDFLAGS += $(FFLAGS_ACC)
endif #OPENACC IF

ifeq "$(OPENMP_OFFLOAD)" "true"
	FFLAGS += $(FFLAGS_GPU)
	CFLAGS += $(FFLAGS_GPU)
	CXXFLAGS += $(FFLAGS_GPU)
	override CPPFLAGS += "-DMPAS_OPENMP_OFFLOAD"
	LDFLAGS += $(LDFLAGS_GPU)
endif #OPENMP_OFFLOAD IF

ifneq (,$(filter-out double single,$(PRECISION)))
$(error PRECISION should be "", "single", or "double"; received value "$(PRECISION)")
endif
ifeq "$(PRECISION)" "double"
	FFLAGS += $(FFLAGS_PROMOTION)
	PRECISION_MESSAGE="MPAS was built with default double-precision reals."
else
$(if $(PRECISION),$(info NOTE: PRECISION=single is unnecessary, single is the default))
	CFLAGS += "-DSINGLE_PRECISION"
	CXXFLAGS += "-DSINGLE_PRECISION"
	override CPPFLAGS += "-DSINGLE_PRECISION"
	PRECISION_MESSAGE="MPAS was built with default single-precision reals."
endif #PRECISION IF

ifeq "$(USE_PAPI)" "true"
	CPPINCLUDES += -I$(PAPI)/include -D_PAPI
	FCINCLUDES += -I$(PAPI)/include
	LIBS += -L$(PAPI)/lib -lpapi
	PAPI_MESSAGE="Papi libraries are on."
else # USE_PAPI IF
	PAPI_MESSAGE="Papi libraries are off."
endif # USE_PAPI IF

# Only if this Makefile was invoked from a compiler target should we check that PICFLAG is set
ifneq "$(FC_SERIAL)" ""
ifeq "$(SHAREDLIB)" "true"
ifneq "$(PICFLAG)" ""
	FFLAGS += $(PICFLAG)
	CFLAGS += $(PICFLAG)
	CXXFLAGS += $(PICFLAG)
	LDFLAGS += $(PICFLAG)
	SHAREDLIB_MESSAGE="Position-independent code was generated."
else
$(error Position-independent code was requested but PIC flags are not available. Please add PIC flags for the '$(BUILD_TARGET)' target)
endif
else
	SHAREDLIB_MESSAGE="Position-dependent code was generated."
endif
endif

ifdef TIMER_LIB
ifeq "$(TIMER_LIB)" "tau"
	override TAU=true
	TIMER_MESSAGE="TAU is being used for the timer interface"
endif

ifeq "$(TIMER_LIB)" "gptl"
	override CPPFLAGS += -DMPAS_GPTL_TIMERS
	override FCINCLUDES += -I${GPTL}/include
	override LIBS += -L${GPTL}/lib -lgptl
	TIMER_MESSAGE="GPTL is being used for the timer interface"
endif

ifeq "$(TIMER_LIB)" ""
	override CPPFLAGS += -DMPAS_NATIVE_TIMERS
	TIMER_MESSAGE="The native timer interface is being used"
endif

else # else ifdef $(TIMER_LIB)

	override CPPFLAGS += -DMPAS_NATIVE_TIMERS
	TIMER_MESSAGE="The native timer interface is being used"

endif # endif ifdef $(TIMER_LIB)

ifeq "$(TAU)" "true"
	LINKER=tau_f90.sh
	CPPINCLUDES += -DMPAS_TAU -DMPAS_TAU_TIMERS
	TAU_MESSAGE="TAU Hooks are on."
else
	LINKER=$(FC)
	TAU_MESSAGE="TAU Hooks are off."
endif

ifeq "$(GEN_F90)" "true"
	override CPPFLAGS += -Uvector
	GEN_F90_MESSAGE="MPAS generated and was built with intermediate .f90 files."
else
	override GEN_F90=false
	GEN_F90_MESSAGE="MPAS was built with .F files."
endif

ifeq "$(OPENMP)" "true"
	OPENMP_MESSAGE="MPAS was built with OpenMP enabled."
else
	OPENMP_MESSAGE="MPAS was built without OpenMP support."
endif

ifeq "$(OPENMP_OFFLOAD)" "true"
	OPENMP_OFFLOAD_MESSAGE="MPAS was built with OpenMP-offload GPU support enabled."
else
	OPENMP_OFFLOAD_MESSAGE="MPAS was built without OpenMP-offload GPU support."
endif

ifeq "$(OPENACC)" "true"
	OPENACC_MESSAGE="MPAS was built with OpenACC accelerator support enabled."
else
	OPENACC_MESSAGE="MPAS was built without OpenACC accelerator support."
endif

ifneq ($(wildcard .mpas_core_*), ) # CHECK FOR BUILT CORE

ifneq ($(wildcard .mpas_core_$(CORE)), ) # CHECK FOR SAME CORE AS ATTEMPTED BUILD.
	override AUTOCLEAN=false
	CONTINUE=true
else
	LAST_CORE=`cat .mpas_core_*`

ifeq "$(AUTOCLEAN)" "true" # CHECK FOR CLEAN PRIOR TO BUILD OF A NEW CORE.
	CONTINUE=true
	AUTOCLEAN_MESSAGE="Infrastructure was cleaned prior to building ."
else
	CONTINUE=false
endif # END OF AUTOCLEAN CHECK

endif # END OF CORE=LAST_CORE CHECK

else

	override AUTOCLEAN=false
	CONTINUE=true
endif # END IF BUILT CORE CHECK

ifneq ($(wildcard namelist.$(NAMELIST_SUFFIX)), ) # Check for generated namelist file.
	NAMELIST_MESSAGE="A default namelist file (namelist.$(NAMELIST_SUFFIX).defaults) has been generated, but namelist.$(NAMELIST_SUFFIX) has not been modified."
else
	NAMELIST_MESSAGE="A default namelist file (namelist.$(NAMELIST_SUFFIX).defaults) has been generated and copied to namelist.$(NAMELIST_SUFFIX)."
endif

ifneq ($(wildcard streams.$(NAMELIST_SUFFIX)), ) # Check for generated streams file.
	STREAM_MESSAGE="A default streams file (streams.$(NAMELIST_SUFFIX).defaults) has been generated, but streams.$(NAMELIST_SUFFIX) has not been modified."
else
	STREAM_MESSAGE="A default streams file (streams.$(NAMELIST_SUFFIX).defaults) has been generated and copied to streams.$(NAMELIST_SUFFIX)."
endif


ifeq "$(findstring clean, $(MAKECMDGOALS))" "clean" # CHECK FOR CLEAN TARGET
	override AUTOCLEAN=false
endif # END OF CLEAN TARGET CHECK

VER=$(shell git describe --dirty 2> /dev/null)
#override CPPFLAGS += -DMPAS_GIT_VERSION=$(VER)

ifeq "$(findstring v, $(VER))" "v"
	override CPPFLAGS += -DMPAS_GIT_VERSION=$(VER)
else
	override CPPFLAGS += -DMPAS_GIT_VERSION="unknown"
endif # END OF GIT DESCRIBE VERSION

####################################################
# Section for adding external libraries and includes
####################################################
ifdef MPAS_EXTERNAL_LIBS
	override LIBS += $(MPAS_EXTERNAL_LIBS)
endif
ifdef MPAS_EXTERNAL_INCLUDES
	override CPPINCLUDES += $(MPAS_EXTERNAL_INCLUDES)
	override FCINCLUDES += $(MPAS_EXTERNAL_INCLUDES)
endif
ifdef MPAS_EXTERNAL_CPPFLAGS
	override CPPFLAGS += $(MPAS_EXTERNAL_CPPFLAGS)
endif
####################################################

override CPPFLAGS += -DMPAS_BUILD_TARGET=$(BUILD_TARGET)

ifeq ($(wildcard src/core_$(CORE)), ) # CHECK FOR EXISTENCE OF CORE DIRECTORY

all: core_error
clean: core_error

else

ifeq ($(wildcard src/core_$(CORE)/build_options.mk), ) # Check for build_options.mk
report_builds:
	@echo "CORE=$(CORE)"
endif

ifeq "$(CONTINUE)" "true"
all: mpas_main
else
all: clean_core
endif

endif


openmp_test:
ifeq "$(OPENMP)" "true"
	@echo "Testing compiler for OpenMP support"
	@echo "#include <omp.h>" > conftest.c; echo "int main() { int n = omp_get_num_threads(); return 0; }" >> conftest.c; $(SCC) $(CFLAGS) -o conftest.out conftest.c || \
		(echo "$(SCC) does not support OpenMP - see INSTALL in top-level directory for more information"; rm -fr conftest.*; exit 1)
	@echo "#include <omp.h>" > conftest.c; echo "int main() { int n = omp_get_num_threads(); return 0; }" >> conftest.c; $(CC) $(CFLAGS) -o conftest.out conftest.c || \
		(echo "$(CC) does not support OpenMP - see INSTALL in top-level directory for more information"; rm -fr conftest.*; exit 1)
	@echo "#include <omp.h>" > conftest.cpp; echo "int main() { int n = omp_get_num_threads(); return 0; }" >> conftest.cpp; $(CXX) $(CFLAGS) -o conftest.out conftest.cpp || \
		(echo "$(CXX) does not support OpenMP - see INSTALL in top-level directory for more information"; rm -fr conftest.*; exit 1)
	@echo "program test; use omp_lib; integer n; n = OMP_GET_NUM_THREADS(); stop 0; end program" > conftest.f90; $(SFC) $(FFLAGS) -o conftest.out conftest.f90 || \
		(echo "$(SFC) does not support OpenMP - see INSTALL in top-level directory for more information"; rm -fr conftest.*; exit 1)
	@echo "program test; use omp_lib; integer n; n = OMP_GET_NUM_THREADS(); stop 0; end program" > conftest.f90; $(FC) $(FFLAGS) -o conftest.out conftest.f90 || \
		(echo "$(FC) does not support OpenMP - see INSTALL in top-level directory for more information"; rm -fr conftest.*; exit 1)
	@rm -fr conftest.*
endif


openacc_test:
ifeq "$(OPENACC)" "true"
	@#
	@# First ensure that both FFLAGS_ACC and CFLAGS_ACC are not blank
	@# If these are not set for a target, then OpenACC most likely cannot compile
	@#
	@echo "Checking if FFLAGS_ACC and CFLAGS_ACC are defined for [$(BUILD_TARGET)]..."
	@( if ([ -z "$(FFLAGS_ACC)" ] && [ -z "$(CFLAGS_ACC)" ]); then \
	      echo "*********************************************************"; \
	      echo "ERROR: OPENACC=true was specified, but [$(BUILD_TARGET)] build target does not seem to support OpenACC:"; \
	      echo "    FFLAGS_ACC and CFLAGS_ACC are both undefined for [$(BUILD_TARGET)] in the top-level Makefile."; \
	      echo "Please set these variables to appropriate OpenACC compilation flags in the [$(BUILD_TARGET)] target to enable OpenACC support."; \
	      echo "*********************************************************"; exit 1; \
	   else \
	      echo "=> FFLAGS_ACC or CFLAGS_ACC are defined"; \
	   fi )

	@#
	@# Create test C and Fortran programs that look for OpenACC header file and parallelize a loop
	@#
	@printf "#include <openacc.h>\n\
	        &int main(){\n\
	        &    int n_devs=acc_get_num_devices( acc_device_default );\n\
	        &    int i,n=0;\n\
	        &    #pragma acc kernels\n\
	        &    for (i=0; i<10; i++)\n\
	        &      n=n+i;\n\
	        &    return 0;\n\
	        &}\n" | sed 's/^ *&//' > openacc.c
	@printf "program openacc\n\
	        &    use openacc\n\
	        &    integer :: i,n=0,n_devs=0\n\
	        &    n_devs=acc_get_num_devices( acc_device_default )\n\
	        &    !\$$acc kernels\n\
	        &    do i=0,10\n\
	        &      n=n+i\n\
	        &    end do\n\
	        &    !\$$acc end kernels\n\
	        &end program\n" | sed 's/^ *&//' > openacc.f90

	@#
	@# See whether the test programs can be compiled
	@#
	@echo "Checking [$(BUILD_TARGET)] compilers for OpenACC support..."
	@( $(SCC) openacc.c $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o openacc_c.out > openacc_c.log 2>&1; \
	   if [ $$? -eq 0 ]; then \
	       echo "=> $(SCC) can compile test OpenACC program"; \
	   else \
	       echo "*********************************************************"; \
	       echo "ERROR: Test OpenACC C program could not be compiled by $(SCC)."; \
	       echo "Following compilation command failed with errors:" ; \
	       echo "$(SCC) openacc.c $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o openacc_c.out"; \
	       echo ""; \
	       echo "Test program openacc.c and output openacc_c.log have been left"; \
	       echo "in the top-level MPAS directory for further debugging"; \
	       echo "*********************************************************"; \
	      rm -f openacc.f90 openacc_[cf].out openacc_f.log; exit 1; \
	   fi )
	@( $(CC) openacc.c $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o openacc_c.out > openacc_c.log 2>&1; \
	   if [ $$? -eq 0 ] ; then \
	       echo "=> $(CC) can compile test OpenACC program"; \
	   else \
	       echo "*********************************************************"; \
	       echo "ERROR: Test OpenACC C program could not be compiled by $(CC)."; \
	       echo "Following compilation command failed with errors:" ; \
	       echo "$(CC) openacc.c $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o openacc_c.out"; \
	       echo ""; \
	       echo "Test program openacc.c and output openacc_c.log have been left"; \
	       echo "in the top-level MPAS directory for further debugging"; \
	       echo "*********************************************************"; \
	      rm -f openacc.f90 openacc_[cf].out openacc_f.log; exit 1; \
	   fi )
	@( $(CXX) openacc.c $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o openacc_c.out > openacc_c.log 2>&1; \
	   if [ $$? -eq 0 ] ; then \
	       echo "=> $(CXX) can compile test OpenACC program"; \
	   else \
	       echo "*********************************************************"; \
	       echo "ERROR: Test OpenACC C program could not be compiled by $(CXX)."; \
	       echo "Following compilation command failed with errors:" ; \
	       echo "$(CXX) openacc.c $(CPPINCLUDES) $(CFLAGS) $(LDFLAGS) -o openacc_c.out"; \
	       echo ""; \
	       echo "Test program openacc.c and output openacc_c.log have been left"; \
	       echo "in the top-level MPAS directory for further debugging"; \
	       echo "*********************************************************"; \
	      rm -f openacc.f90 openacc_[cf].out openacc_f.log; exit 1; \
	   fi )
	@( $(SFC) openacc.f90 $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) -o openacc_f.out > openacc_f.log 2>&1; \
	   if [ $$? -eq 0 ] ; then \
	       echo "=> $(SFC) can compile test OpenACC program"; \
	   else \
	       echo "*********************************************************"; \
	       echo "ERROR: Test OpenACC Fortran program could not be compiled by $(SFC)."; \
	       echo "Following compilation command failed with errors:" ; \
	       echo "$(SFC) openacc.f90 $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) -o openacc_f.out"; \
	       echo ""; \
	       echo "Test program openacc.f90 and output openacc_f.log have been left"; \
	       echo "in the top-level MPAS directory for further debugging"; \
	       echo "*********************************************************"; \
	      rm -f openacc.c openacc_[cf].out openacc_c.log; exit 1; \
	   fi )
	@( $(FC) openacc.f90 $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) -o openacc_f.out > openacc_f.log 2>&1; \
	   if [ $$? -eq 0 ] ; then \
	       echo "=> $(FC) can compile test OpenACC program"; \
	   else \
	       echo "*********************************************************"; \
	       echo "ERROR: Test OpenACC Fortran program could not be compiled by $(FC)."; \
	       echo "Following compilation command failed with errors:" ; \
	       echo "$(FC) openacc.f90 $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) -o openacc_f.out"; \
	       echo ""; \
	       echo "Test program openacc.f90 and output openacc_f.log have been left"; \
	       echo "in the top-level MPAS directory for further debugging"; \
	       echo "*********************************************************"; \
	       rm -f openacc.c openacc_[cf].out openacc_c.log; exit 1; \
	   fi )

	@rm -f openacc.c openacc.f90 openacc_[cf].out openacc_[cf].log
endif # OPENACC eq true


pio_test: openmp_test openacc_test
	@#
	@# PIO_VERS will be set to:
	@#  0 if no working PIO library was detected (and .piotest.log will contain error messages)
	@#  1 if a PIO 1.x library was detected
	@#  2 if a PIO 2.x library was detected
	@#
	$(info Checking for a working PIO library...)
ifneq "$(USE_PIO2)" ""
	$(info *** Note: The USE_PIO2 option has been deprecated and will be ignored.)
endif
	$(eval PIO_VERS := $(shell $\
		rm -f .piotest.log; $\
		printf "program pio1\n$\
		        &   use pio\n$\
		        &   use pionfatt_mod\n$\
		        &   integer, parameter :: MPAS_IO_OFFSET_KIND = PIO_OFFSET\n$\
		        &   integer, parameter :: MPAS_INT_FILLVAL = NF_FILL_INT\n$\
		        &   type (Var_desc_t) :: field_desc\n$\
		        &   integer (kind=MPAS_IO_OFFSET_KIND) :: frame_number\n$\
		        &   call PIO_setframe(field_desc, frame_number)\n$\
		        end program\n" | sed 's/&/ /' > pio1.f90; $\
		$\
		printf "program pio2\n$\
		        &   use pio\n$\
		        &   integer, parameter :: MPAS_IO_OFFSET_KIND = PIO_OFFSET_KIND\n$\
		        &   integer, parameter :: MPAS_INT_FILLVAL = PIO_FILL_INT\n$\
		        &   type (file_desc_t) :: pio_file\n$\
		        &   type (Var_desc_t) :: field_desc\n$\
		        &   integer (kind=MPAS_IO_OFFSET_KIND) :: frame_number\n$\
		        &   call PIO_setframe(pio_file, field_desc, frame_number)\n$\
		        end program\n" | sed 's/&/ /' > pio2.f90; $\
		$\
		$(FC) pio1.f90 -o pio1.x $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) $(LIBS) > /dev/null 2>&1; $\
		pio1_status=$$?; $\
		$\
		$(FC) pio2.f90 -o pio2.x $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) $(LIBS) > /dev/null 2>&1; $\
		pio2_status=$$?; $\
		$\
		if [ $$pio1_status -ne 0 -a $$pio2_status -ne 0 ]; then $\
		    printf "0"; $\
		    printf "*********************************************************\n" > .piotest.log; $\
		    printf "ERROR: Could not detect a working PIO library!\n" >> .piotest.log; $\
		    printf "\n" >> .piotest.log; $\
		    printf "Both of the following commands to compile a test program\n" >> .piotest.log; $\
		    printf "failed with errors:\n" >> .piotest.log; $\
		    printf "\n" >> .piotest.log; $\
		    printf "$(FC) pio1.f90 -o pio1.x $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) $(LIBS)\n" >> .piotest.log; $\
		    printf "\n" >> .piotest.log; $\
		    printf "$(FC) pio2.f90 -o pio2.x $(FCINCLUDES) $(FFLAGS) $(LDFLAGS) $(LIBS)\n" >> .piotest.log; $\
		    printf "\n" >> .piotest.log; $\
		    printf "The pio1.f90 and pio2.f90 test programs have been left in\n" >> .piotest.log; $\
		    printf "the top-level MPAS directory for further debugging.\n" >> .piotest.log; $\
		    printf "*********************************************************\n" >> .piotest.log; $\
		elif [ $$pio1_status -eq 0 ]; then $\
		    printf "1"; $\
		    rm -f pio[12].f90 pio[12].x; $\
		elif [ $$pio2_status -eq 0 ]; then $\
		    printf "2"; $\
		    rm -f pio[12].f90 pio[12].x; $\
		fi $\
	))
	$(if $(findstring 1,$(PIO_VERS)), $(eval IO_MESSAGE = "Using the PIO 1.x library."), )
	$(if $(findstring 1,$(PIO_VERS)), $(info PIO 1.x detected.))
	$(if $(findstring 2,$(PIO_VERS)), $(eval override CPPFLAGS += -DUSE_PIO2), )
	$(if $(findstring 2,$(PIO_VERS)), $(eval IO_MESSAGE = "Using the PIO 2.x library."), )
	$(if $(findstring 2,$(PIO_VERS)), $(info PIO 2.x detected.))
	@#
	@# A .piotest.log file exists iff no working PIO library was detected
	@#
	@if [ -f .piotest.log ]; then \
	    cat .piotest.log; \
	    rm -f .piotest.log; \
	    exit 1; \
	fi


mpi_f08_test:
	@#
	@# MPAS_MPI_F08 will be set to:
	@#  0 if no mpi_f08 module support was detected
	@#  1 if the MPI library provides an mpi_f08 module
	@#
	$(info Checking for mpi_f08 support...)
	$(eval MPAS_MPI_F08 := $(shell $\
		printf "program main\n$\
		        &   use mpi_f08, only : MPI_Init, MPI_Comm\n$\
		        &   integer :: ierr\n$\
		        &   type (MPI_Comm) :: comm\n$\
		        &   call MPI_Init(ierr)\n$\
		        end program main\n" | sed 's/&/ /' > mpi_f08.f90; $\
		$\
		$(FC) mpi_f08.f90 -o mpi_f08.x $(FFLAGS) $(LDFLAGS) > /dev/null 2>&1; $\
		mpi_f08_status=$$?; $\
		rm -f mpi_f08.f90 mpi_f08.x; $\
		if [ $$mpi_f08_status -eq 0 ]; then $\
		    printf "1"; $\
		else $\
		    printf "0"; $\
		fi $\
	))
	$(if $(findstring 0,$(MPAS_MPI_F08)), $(eval MPI_F08_MESSAGE = "Using the mpi module."), )
	$(if $(findstring 0,$(MPAS_MPI_F08)), $(info No working mpi_f08 module detected; using mpi module.))
	$(if $(findstring 1,$(MPAS_MPI_F08)), $(eval override CPPFLAGS += -DMPAS_USE_MPI_F08), )
	$(if $(findstring 1,$(MPAS_MPI_F08)), $(eval MPI_F08_MESSAGE = "Using the mpi_f08 module."), )
	$(if $(findstring 1,$(MPAS_MPI_F08)), $(info mpi_f08 module detected.))

ifneq "$(PIO)" ""
MAIN_DEPS = openmp_test openacc_test pio_test mpi_f08_test
override CPPFLAGS += "-DMPAS_PIO_SUPPORT"
else
MAIN_DEPS = openmp_test openacc_test mpi_f08_test
IO_MESSAGE = "Using the SMIOL library."
override CPPFLAGS += "-DMPAS_SMIOL_SUPPORT"
endif


mpas_main: $(MAIN_DEPS)
ifeq "$(AUTOCLEAN)" "true"
	$(RM) .mpas_core_*
endif
	cd src; $(MAKE) FC="$(FC)" \
                 CC="$(CC)" \
                 CXX="$(CXX)" \
                 SFC="$(SFC)" \
                 SCC="$(SCC)" \
                 LINKER="$(LINKER)" \
                 CFLAGS="$(CFLAGS)" \
                 CXXFLAGS="$(CXXFLAGS)" \
                 FFLAGS="$(FFLAGS)" \
                 LDFLAGS="$(LDFLAGS)" \
                 RM="$(RM)" \
                 CPP="$(CPP)" \
                 CPPFLAGS="$(CPPFLAGS)" \
                 LIBS="$(LIBS)" \
                 CPPINCLUDES="$(CPPINCLUDES)" \
                 FCINCLUDES="$(FCINCLUDES)" \
                 CORE="$(CORE)"\
                 AUTOCLEAN="$(AUTOCLEAN)" \
                 GEN_F90="$(GEN_F90)" \
                 NAMELIST_SUFFIX="$(NAMELIST_SUFFIX)" \
                 EXE_NAME="$(EXE_NAME)"

	@echo "$(EXE_NAME)" > .mpas_core_$(CORE)
	if [ -e src/$(EXE_NAME) ]; then mv src/$(EXE_NAME) .; fi
	( cd src/core_$(CORE); $(MAKE) ROOT_DIR="$(PWD)" post_build )
	@echo "*******************************************************************************"
	@echo $(PRECISION_MESSAGE)
	@echo $(DEBUG_MESSAGE)
	@echo $(PARALLEL_MESSAGE)
	@echo $(MPI_F08_MESSAGE)
	@echo $(PAPI_MESSAGE)
	@echo $(TAU_MESSAGE)
	@echo $(OPENMP_MESSAGE)
	@echo $(OPENMP_OFFLOAD_MESSAGE)
	@echo $(OPENACC_MESSAGE)
	@echo $(SHAREDLIB_MESSAGE)
ifeq "$(AUTOCLEAN)" "true"
	@echo $(AUTOCLEAN_MESSAGE)
endif
	@echo $(GEN_F90_MESSAGE)
	@echo $(TIMER_MESSAGE)
	@echo $(IO_MESSAGE)
	@echo "*******************************************************************************"
clean:
	cd src; $(MAKE) clean RM="$(RM)" CORE="$(CORE)"
	$(RM) .mpas_core_*
	$(RM) $(EXE_NAME)
	$(RM) namelist.$(NAMELIST_SUFFIX).defaults
	$(RM) streams.$(NAMELIST_SUFFIX).defaults
core_error:
	@echo ""
	@echo "*******************************************************************************"
	@echo "     The directory src/core_$(CORE) does not exist."
	@echo "     $(CORE) is not a valid core choice."
	@echo "*******************************************************************************"
	@echo ""
	exit 1
error: errmsg

clean_core:
	@echo ""
	@echo "*******************************************************************************"
	@echo " The MPAS infrastructure is currently built for the $(LAST_CORE) core."
	@echo " Before building the $(CORE) core, please do one of the following."
	@echo ""
	@echo ""
	@echo " To remove the $(LAST_CORE)_model executable and clean the MPAS infrastructure, run:"
	@echo "      make clean CORE=$(LAST_CORE)"
	@echo ""
	@echo " To preserve all executables except $(CORE)_model and clean the MPAS infrastructure, run:"
	@echo "      make clean CORE=$(CORE)"
	@echo ""
	@echo " Alternatively, AUTOCLEAN=true can be appended to the make command to force a clean,"
	@echo " build a new $(CORE)_model executable, and preserve all other executables."
	@echo ""
	@echo "*******************************************************************************"
	@echo ""
	exit 1

else # CORE IF

all: error
clean: error
error: errmsg
	@echo "************ ERROR ************"
	@echo "No CORE specified. Quitting."
	@echo "************ ERROR ************"
	@echo ""
	exit 1

endif # CORE IF

errmsg:
	@echo ""
	@echo "Usage: $(MAKE) target CORE=[core] [options]"
	@echo ""
	@echo "Available Targets:"
	@grep BUILDTARGET Makefile | grep -v grep | sed -e 's/#[[:blank:]]*BUILDTARGET[[:blank:]]*/#/' | sed -e 's/:[[:blank:]]*#/:#/' | sed -e 's/://' | awk 'BEGIN {FS="#"}{printf ("    %-15s - %s\n", $$1, $$2)}'
	@echo ""
	@echo "Availabe Cores:"
	@cd src; ls -d core_* | grep ".*" | sed "s/core_/    /g"
	@echo ""
	@echo "Available Options:"
	@echo "    DEBUG=true    - builds debug version. Default is optimized version."
	@echo "    USE_PAPI=true - builds version using PAPI for timers. Default is off."
	@echo "    TAU=true      - builds version using TAU hooks for profiling. Default is off."
	@echo "    AUTOCLEAN=true    - forces a clean of infrastructure prior to build new core."
	@echo "    GEN_F90=true  - Generates intermediate .f90 files through CPP, and builds with them."
	@echo "    TIMER_LIB=opt - Selects the timer library interface to be used for profiling the model. Options are:"
	@echo "                    TIMER_LIB=native - Uses native built-in timers in MPAS"
	@echo "                    TIMER_LIB=gptl - Uses gptl for the timer interface instead of the native interface"
	@echo "                    TIMER_LIB=tau - Uses TAU for the timer interface instead of the native interface"
	@echo "    OPENMP=true   - builds and links with OpenMP flags. Default is to not use OpenMP."
	@echo "    OPENACC=true  - builds and links with OpenACC flags. Default is to not use OpenACC."
	@echo "    PRECISION=double - builds with default double-precision real kind. Default is to use single-precision."
	@echo "    SHAREDLIB=true - generate position-independent code suitable for use in a shared library. Default is false."
	@echo ""
	@echo "Ensure that NETCDF, PNETCDF, PIO, and PAPI (if USE_PAPI=true) are environment variables"
	@echo "that point to the absolute paths for the libraries."
	@echo ""
ifdef CORE
	exit 1
endif

