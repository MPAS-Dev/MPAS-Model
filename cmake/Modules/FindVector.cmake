# Version 0.6 Increment by 0.1 every change
#
# Authors:
#    Bob Robey, Los Alamos National Laboratory, brobey@lanl.gov
#    Daniel Dunning, Los Alamos, National Laboratory, ddunning@lanl.gov
#
# Operation tested on
#    Intel Skylake with clang/8.0.1 gcc/9.1.0 intel/19.0.4 pgi/18.10
#    AMD-Epyc with with clang/8.0.1 gcc/9.1.0 intel/19.0.4 pgi/18.10
#    ARM with clang/8.0.0 gcc/9.1.0 ThunderX2CN99/RHEL/7/gcc-8.2.0/armpl/19.2.0
#    Power9 with clang/8.0.0 gcc/9.1.0 ibm/xlc-16.1.1.3-xlf-16.1.1.3 pgi/19.3
#
#    Main output flags
#       VECTOR_<LANG>_FLAGS          -- All flags set plus turning on vectorization
#       VECTOR_NOVEC_<LANG>_FLAGS    -- All flags set same as vectorization, but with vectorization off
#       VECTOR_<LANG>_VERBOSE        -- Turn on verbose messages when compiling for vectorization feedback
#    Component flags
#       VECTOR_ALIASING_<LANG>_FLAGS -- Stricter aliasing option to help auto-vectorization
#       VECTOR_ARCH_<LANG>_FLAGS     -- Set to compile for architecture that it is on
#       VECTOR_FPMODEL_<LANG>_FLAGS  -- Set so that Kahan sum does not get optimized out (unsafe optimizations)
#       VECTOR_NOVEC_<LANG>_OPT      -- Turn off vectorization for debugging and performance measurement
#       VECTOR_VEC_<LANG>_OPTS       -- Turn on vectorization
#
#    Main output flags are build from component flags by the following rule
#       set(VECTOR_BASE_<LANG>_FLAGS "${VECTOR_ALIASING_<LANG>_FLAGS} ${VECTOR_ARCH_<LANG>_FLAGS} ${VECTOR_FPMODEL_<LANG>_FLAGS}")
#       set(VECTOR_NOVEC_<LANG>_FLAGS "${VECTOR_BASE_<LANG>_FLAGS} ${VECTOR_NOVEC_<LANG>_FLAGS}")
#       set(VECTOR_<LANG>_FLAGS "${VECTOR_BASE_<LANG>_FLAGS} ${VECTOR_<LANG>_FLAGS} ${VECTOR_OPENMP_SIMD_<LANG>_FLAGS}")
#
#    Using in CMakeLists:
#
#    These lines setup for turning on verbosity with cmake -DCMAKE_VECTOR_VERBOSE
#
#       if (CMAKE_VECTOR_VERBOSE)
#           set(VECTOR_C_FLAGS "${VECTOR_C_FLAGS} ${VECTOR_C_VERBOSE}")
#           set(VECTOR_CXX_FLAGS "${VECTOR_CXX_FLAGS} ${VECTOR_CXX_VERBOSE}")
#       endif (CMAKE_VECTOR_VERBOSE)
#
#    Vectorization or vector verbosity can be set for individual files
#
#       set_source_files_properties(<target> PROPERTIES COMPILE_FLAGS ${VECTOR_C_FLAGS})
#
#    Can be added to compile flags for all files
#
#       set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${VECTOR_C_FLAGS")

include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)
include(CheckFortranCompilerFlag)

# Set vectorization flags for a few compilers
if(CMAKE_C_COMPILER_LOADED)
    if ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang") # using Clang
        set(VECTOR_ALIASING_C_FLAGS "${VECTOR_ALIASING_C_FLAGS} -fstrict-aliasing")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
           set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -march=native -mtune=native")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
            set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -mcpu=powerpc64le")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
            set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -march=native -mtune=native")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_OPENMP_SIMD_C_FLAGS "${VECTOR_OPENMP_SIMD_C_FLAGS} -fopenmp-simd")
        set(VECTOR_C_OPTS "${VECTOR_C_OPTS} -fvectorize")
        set(VECTOR_C_FPOPTS "${VECTOR_C_FPOPTS} -fno-math-errno")
        set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT} -fno-vectorize")
        set(VECTOR_C_VERBOSE "${VECTOR_C_VERBOSE} -Rpass=loop-vectorize -Rpass-missed=loop-vectorize -Rpass-analysis=loop-vectorize")

    elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU") # using GCC
        set(VECTOR_ALIASING_C_FLAGS "${VECTOR_ALIASING_C_FLAGS} -fstrict-aliasing")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -march=native -mtune=native")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
            set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -mcpu=powerpc64le")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
            set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -march=native -mtune=native")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_OPENMP_SIMD_C_FLAGS "${VECTOR_OPENMP_SIMD_C_FLAGS} -fopenmp-simd")
        set(VECTOR_C_OPTS "${VECTOR_C_OPTS} -ftree-vectorize")
        set(VECTOR_C_FPOPTS "${VECTOR_C_FPOPTS} -fno-trapping-math -fno-math-errno")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            if ("${CMAKE_C_COMPILER_VERSION}" VERSION_GREATER "7.9.0")
                set(VECTOR_C_OPTS "${VECTOR_C_OPTS} -mprefer-vector-width=512")
            endif ("${CMAKE_C_COMPILER_VERSION}" VERSION_GREATER "7.9.0")
            if ("${CMAKE_C_COMPILER_VERSION}" VERSION_LESS "9.0.0")
                message(STATUS "Use a GCC compiler version 9.0.0 or greater to get effective vectorization")
            endif ("${CMAKE_C_COMPILER_VERSION}" VERSION_LESS "9.0.0")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT} -fno-tree-vectorize")
        set(VECTOR_C_VERBOSE "${VECTOR_C_VERBOSE} -fopt-info-vec-optimized -fopt-info-vec-missed -fopt-info-loop-optimized -fopt-info-loop-missed")

    elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "Intel") # using Intel C
        set(VECTOR_ALIASING_C_FLAGS "${VECTOR_ALIASING_C_FLAGS} -ansi-alias")
        set(VECTOR_FPMODEL_C_FLAGS "${VECTOR_FPMODEL_C_FLAGS} -fp-model:precise")

        set(VECTOR_OPENMP_SIMD_C_FLAGS "${VECTOR_OPENMP_SIMD_C_FLAGS} -qopenmp-simd")
        set(VECTOR_C_OPTS "${VECTOR_C_OPTS} -march=native -mtune=native -restrict -xHOST -vecabi=cmdtarget")
        if ("${CMAKE_C_COMPILER_VERSION}" VERSION_GREATER "17.0.4")
            set(VECTOR_C_OPTS "${VECTOR_C_OPTS} -qopt-zmm-usage=high")
        endif ("${CMAKE_C_COMPILER_VERSION}" VERSION_GREATER "17.0.4")
        set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT} -march=native -mtune=native -no-vec")
        set(VECTOR_C_VERBOSE "${VECTOR_C_VERBOSE} -qopt-report=5 -qopt-report-phase=openmp,loop,vec")

    elseif (CMAKE_C_COMPILER_ID MATCHES "PGI")
        set(VECTOR_ALIASING_C_FLAGS "${VECTOR_ALIASING_C_FLAGS} -alias=ansi")
        set(VECTOR_OPENMP_SIMD_C_FLAGS "${VECTOR_OPENMP_SIMD_C_FLAGS} -Mvect=simd")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            # PGI is converting over to a LLVM based compiler on x86_64. To enable, add -Mllvm
            #    and then prepend the llvm pgi compiler path something like below. This is important
            #    when using OpenMP and also adds OpenMP 4.5 support
            # module load pgi/18.10
            # export PATH="/projects/opt/centos7/pgi/linux86-64-llvm/18.10/bin:${PATH}"
            if ("${CMAKE_C_COMPILER_VERSION}" VERSION_GREATER "18.6")
                execute_process(COMMAND pgcc --version COMMAND grep LLVM COMMAND wc -l OUTPUT_VARIABLE PGI_VERSION_OUTPUT OUTPUT_STRIP_TRAILING_WHITESPACE)
                if ("${PGI_VERSION_OUTPUT}" STREQUAL "1")
                    set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -Mllvm")
                endif ("${PGI_VERSION_OUTPUT}" STREQUAL "1")
            endif ("${CMAKE_C_COMPILER_VERSION}" VERSION_GREATER "18.6")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT} -Mnovect ")
        set(VECTOR_C_VERBOSE "${VECTOR_C_VERBOSE} -Minfo=loop,inline,vect")

    elseif (CMAKE_C_COMPILER_ID MATCHES "MSVC")
        set(VECTOR_C_OPTS "${VECTOR_C_OPTS}" " ")

        set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT}" " ")
        set(VECTOR_C_VERBOSE "${VECTOR_C_VERBOSE} -Qvec-report:2")

    elseif (CMAKE_C_COMPILER_ID MATCHES "XL")
        set(VECTOR_ALIASING_C_FLAGS "${VECTOR_ALIASING_C_FLAGS} -qalias=restrict")
        set(VECTOR_FPMODEL_C_FLAGS "${VECTOR_FPMODEL_C_FLAGS} -qstrict")
        set(VECTOR_ARCH_C_FLAGS "${VECTOR_ARCH_C_FLAGS} -qhot -qarch=auto -qtune=auto")

        set(CMAKE_VEC_C_FLAGS "${CMAKE_VEC_FLAGS} -qsimd=auto")
        set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT} -qsimd=noauto")
        # "long vector" optimizations
        #set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT} -qhot=novector")
        set(VECTOR_C_VERBOSE "${VECTOR_C_VERBOSE} -qreport")

    elseif (CMAKE_C_COMPILER_ID MATCHES "Cray")
        set(VECTOR_ALIASING_C_FLAGS "${VECTOR_ALIASING_C_FLAGS} -h restrict=a")
        set(VECTOR_C_OPTS "${VECTOR_C_OPTS} -h vector=3")
  
        set(VECTOR_NOVEC_C_OPT "${VECTOR_NOVEC_C_OPT} -h vector=0")
        set(VECTOR_C_VERBOSE "${VECTOR_C_VERBOSE} -h msgs -h negmsgs -h list=a")

    endif()

    CHECK_C_COMPILER_FLAG("${VECTOR_OPENMP_SIMD_C_FLAGS}" HAVE_OPENMP_SIMD)
    if (HAVE_OPENMP_SIMD)
       add_definitions(-D_OPENMP_SIMD)
    else (HAVE_OPENMP_SIMD)
       unset(VECTOR_OPENMP_SIMD_C_FLAGS)
    endif (HAVE_OPENMP_SIMD)

    set(VECTOR_BASE_C_FLAGS "${VECTOR_ALIASING_C_FLAGS} ${VECTOR_ARCH_C_FLAGS} ${VECTOR_FPMODEL_C_FLAGS}")
    set(VECTOR_NOVEC_C_FLAGS "${VECTOR_BASE_C_FLAGS} ${VECTOR_NOVEC_C_OPT}")
    set(VECTOR_C_FLAGS "${VECTOR_BASE_C_FLAGS} ${VECTOR_C_OPTS} ${VECTOR_C_FPOPTS} ${VECTOR_OPENMP_SIMD_C_FLAGS}")

    mark_as_advanced(VECTOR_C_FLAGS
                     VECTOR_NOVEC_C_FLAGS
                     VECTOR_C_VERBOSE
                     VECTOR_ALIASING_C_FLAGS
                     VECTOR_ARCH_C_FLAGS
                     VECTOR_FPMODEL_C_FLAGS
                     VECTOR_NOVEC_C_OPT
                     VECTOR_VEC_C_OPTS
                     VECTOR_VEC_C_FPOPTS)

    message(STATUS  "Setting Vector C flags to: ${VECTOR_C_FLAGS}")
    message(STATUS  "Setting Vector C No-Vector flags to: ${VECTOR_NOVEC_C_FLAGS}")
    message(STATUS  "Setting Vector C Verbose flags to: ${VECTOR_C_VERBOSE}")

endif(CMAKE_C_COMPILER_LOADED)

#Start CXX Flags
if(CMAKE_CXX_COMPILER_LOADED)
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang") # using Clang
        set(VECTOR_ALIASING_CXX_FLAGS "${VECTOR_ALIASING_CXX_FLAGS} -fstrict-aliasing")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
           set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -march=native -mtune=native")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
            set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -mcpu=powerpc64le")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
            set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -march=native -mtune=native")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_OPENMP_SIMD_CXX_FLAGS "${VECTOR_OPENMP_SIMD_CXX_FLAGS} -fopenmp-simd")
        set(VECTOR_CXX_OPTS "${VECTOR_CXX_OPTS} -fvectorize")
        set(VECTOR_CXX_FPOPTS "${VECTOR_CXX_FPOPTS} -fno-math-errno")
        set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT} -fno-vectorize")
        set(VECTOR_CXX_VERBOSE "${VECTOR_CXX_VERBOSE} -Rpass=loop-vectorize -Rpass-missed=loop-vectorize -Rpass-analysis=loop-vectorize")

    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU") # using GCC
        set(VECTOR_ALIASING_CXX_FLAGS "${VECTOR_ALIASING_CXX_FLAGS} -fstrict-aliasing")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -march=native -mtune=native")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
            set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -mcpu=powerpc64le")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
            set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -march=native -mtune=native")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_OPENMP_SIMD_CXX_FLAGS "${VECTOR_OPENMP_SIMD_CXX_FLAGS} -fopenmp-simd")
        set(VECTOR_CXX_OPTS "${VECTOR_CXX_OPTS} -ftree-vectorize")
        set(VECTOR_CXX_FPOPTS "${VECTOR_CXX_FPOPTS} -fno-trapping-math -fno-math-errno")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            if ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_GREATER "7.9.0")
                set(VECTOR_CXX_OPTS "${VECTOR_CXX_OPTS} -mprefer-vector-width=512")
            endif ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_GREATER "7.9.0")
            if ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_LESS "9.0.0")
                message(STATUS "Use a GCC compiler version 9.0.0 or greater to get effective vectorization")
            endif ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_LESS "9.0.0")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT} -fno-tree-vectorize")
        set(VECTOR_CXX_VERBOSE "${VECTOR_CXX_VERBOSE} -fopt-info-vec-optimized -fopt-info-vec-missed -fopt-info-loop-optimized -fopt-info-loop-missed")

    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel") # using Intel C
        set(VECTOR_ALIASING_CXX_FLAGS "${VECTOR_ALIASING_CXX_FLAGS} -ansi-alias")
        set(VECTOR_FPMODEL_CXX_FLAGS "${VECTOR_FPMODEL_CXX_FLAGS} -fp-model:precise")

        set(VECTOR_OPENMP_SIMD_CXX_FLAGS "${VECTOR_OPENMP_SIMD_CXX_FLAGS} -qopenmp-simd")
        set(VECTOR_CXX_OPTS "${VECTOR_CXX_OPTS} -march=native -mtune=native -restrict -xHOST -vecabi=cmdtarget")
        if ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_GREATER "17.0.4")
            set(VECTOR_CXX_OPTS "${VECTOR_CXX_OPTS} -qopt-zmm-usage=high")
        endif ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_GREATER "17.0.4")
        set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT} -march=native -mtune=native -no-vec")
        set(VECTOR_CXX_VERBOSE "${VECTOR_CXX_VERBOSE} -qopt-report=5 -qopt-report-phase=openmp,loop,vec")

    elseif (CMAKE_CXX_COMPILER_ID MATCHES "PGI")
        set(VECTOR_ALIASING_CXX_FLAGS "${VECTOR_ALIASING_CXX_FLAGS} -alias=ansi")
        set(VECTOR_OPENMP_SIMD_CXX_FLAGS "${VECTOR_OPENMP_SIMD_CXX_FLAGS} -Mvect=simd")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            if ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_GREATER "18.6")
                execute_process(COMMAND pgc++ --version COMMAND grep LLVM COMMAND wc -l OUTPUT_VARIABLE PGI_VERSION_OUTPUT OUTPUT_STRIP_TRAILING_WHITESPACE)
                if ("${PGI_VERSION_OUTPUT}" STREQUAL "1")
                    set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -Mllvm")
                endif ("${PGI_VERSION_OUTPUT}" STREQUAL "1")
            endif ("${CMAKE_CXX_COMPILER_VERSION}" VERSION_GREATER "18.6")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT} -Mnovect ")
        set(VECTOR_CXX_VERBOSE "${VECTOR_CXX_VERBOSE} -Minfo=loop,inline,vect")

    elseif (CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
        set(VECTOR_CXX_OPTS "${VECTOR_CXX_OPTS}" " ")

        set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT}" " ")
        set(VECTOR_CXX_VERBOSE "${VECTOR_CXX_VERBOSE} -Qvec-report:2")

    elseif (CMAKE_CXX_COMPILER_ID MATCHES "XL")
        set(VECTOR_ALIASING_CXX_FLAGS "${VECTOR_ALIASING_CXX_FLAGS} -qalias=restrict")
        set(VECTOR_FPMODEL_CXX_FLAGS "${VECTOR_FPMODEL_CXX_FLAGS} -qstrict")
        set(VECTOR_ARCH_CXX_FLAGS "${VECTOR_ARCH_CXX_FLAGS} -qhot -qarch=auto -qtune=auto")

        set(CMAKE_VEC_CXX_FLAGS "${CMAKE_VEC_FLAGS} -qsimd=auto")
        set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT} -qsimd=noauto")
        # "long vector" optimizations
        #set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT} -qhot=novector")
        set(VECTOR_CXX_VERBOSE "${VECTOR_CXX_VERBOSE} -qreport")

    elseif (CMAKE_CXX_COMPILER_ID MATCHES "Cray")
        set(VECTOR_ALIASING_CXX_FLAGS "${VECTOR_ALIASING_CXX_FLAGS} -h restrict=a")
        set(VECTOR_CXX_OPTS "${VECTOR_CXX_OPTS} -h vector=3")
  
        set(VECTOR_NOVEC_CXX_OPT "${VECTOR_NOVEC_CXX_OPT} -h vector=0")
        set(VECTOR_CXX_VERBOSE "${VECTOR_CXX_VERBOSE} -h msgs -h negmsgs -h list=a")

    endif()

    CHECK_CXX_COMPILER_FLAG("${VECTOR_OPENMP_SIMD_CXX_FLAGS}" HAVE_OPENMP_SIMD)
    if (HAVE_OPENMP_SIMD)
       add_definitions(-D_OPENMP_SIMD)
    else (HAVE_OPENMP_SIMD)
       unset(VECTOR_OPENMP_SIMD_CXX_FLAGS)
    endif (HAVE_OPENMP_SIMD)

    set(VECTOR_BASE_CXX_FLAGS "${VECTOR_ALIASING_CXX_FLAGS} ${VECTOR_ARCH_CXX_FLAGS} ${VECTOR_FPMODEL_CXX_FLAGS}")
    set(VECTOR_NOVEC_CXX_FLAGS "${VECTOR_BASE_CXX_FLAGS} ${VECTOR_NOVEC_CXX_OPT}")
    set(VECTOR_CXX_FLAGS "${VECTOR_BASE_CXX_FLAGS} ${VECTOR_CXX_OPTS} ${VECTOR_CXX_FPOPTS} ${VECTOR_OPENMP_SIMD_CXX_FLAGS}")

    mark_as_advanced(VECTOR_CXX_FLAGS
                     VECTOR_NOVEC_CXX_FLAGS
                     VECTOR_CXX_VERBOSE
                     VECTOR_ALIASING_CXX_FLAGS
                     VECTOR_ARCH_CXX_FLAGS
                     VECTOR_FPMODEL_CXX_FLAGS
                     VECTOR_NOVEC_CXX_OPT
                     VECTOR_VEC_CXX_OPTS
                     VECTOR_VEC_CXX_FPOPTS)

   message(STATUS  "Setting Vector CXX flags to: ${VECTOR_CXX_FLAGS}")
   message(STATUS  "Setting Vector CXX No-Vector flags to: ${VECTOR_NOVEC_CXX_FLAGS}")
   message(STATUS  "Setting Vector CXX Verbose flags to: ${VECTOR_CXX_VERBOSE}")

endif(CMAKE_CXX_COMPILER_LOADED)

# Start Fortran flags
if(CMAKE_Fortran_COMPILER_LOADED)
    if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Clang") # using Clang
        set(VECTOR_ALIASING_Fortran_FLAGS "${VECTOR_ALIASING_Fortran_FLAGS} -fstrict-aliasing")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
           set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -march=native -mtune=native")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
            set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -mcpu=powerpc64le")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
            set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -march=native -mtune=native")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_OPENMP_SIMD_Fortran_FLAGS "${VECTOR_OPENMP_SIMD_Fortran_FLAGS} -fopenmp-simd")
        set(VECTOR_Fortran_OPTS "${VECTOR_Fortran_OPTS} -fvectorize")
        set(VECTOR_Fortran_FPOPTS "${VECTOR_Fortran_FPOPTS} -fno-math-errno")
        set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT} -fno-vectorize")
        set(VECTOR_Fortran_VERBOSE "${VECTOR_Fortran_VERBOSE} -Rpass=loop-vectorize -Rpass-missed=loop-vectorize -Rpass-analysis=loop-vectorize")

    elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU") # using GCC
        set(VECTOR_ALIASING_Fortran_FLAGS "${VECTOR_ALIASING_Fortran_FLAGS} -fstrict-aliasing")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -march=native -mtune=native")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
            set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -mcpu=powerpc64le")
        elseif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
            set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -march=native -mtune=native")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_OPENMP_SIMD_Fortran_FLAGS "${VECTOR_OPENMP_SIMD_Fortran_FLAGS} -fopenmp-simd")
        set(VECTOR_Fortran_OPTS "${VECTOR_Fortran_OPTS} -ftree-vectorize")
        set(VECTOR_Fortran_FPOPTS "${VECTOR_Fortran_FPOPTS} -fno-trapping-math -fno-math-errno")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            if ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "7.9.0")
                set(VECTOR_Fortran_OPTS "${VECTOR_Fortran_OPTS} -mprefer-vector-width=512")
            endif ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "7.9.0")
            if ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_LESS "9.0.0")
                message(STATUS "Use a GCC compiler version 9.0.0 or greater to get effective vectorization")
            endif ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_LESS "9.0.0")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT} -fno-tree-vectorize")
        set(VECTOR_Fortran_VERBOSE "${VECTOR_Fortran_VERBOSE} -fopt-info-vec-optimized -fopt-info-vec-missed -fopt-info-loop-optimized -fopt-info-loop-missed")

    elseif ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel") # using Intel C
        set(VECTOR_ALIASING_Fortran_FLAGS "${VECTOR_ALIASING_Fortran_FLAGS} -ansi-alias")
        set(VECTOR_FPMODEL_Fortran_FLAGS "${VECTOR_FPMODEL_Fortran_FLAGS} -fp-model:precise")

        set(VECTOR_OPENMP_SIMD_Fortran_FLAGS "${VECTOR_OPENMP_SIMD_Fortran_FLAGS} -qopenmp-simd")
        set(VECTOR_Fortran_OPTS "${VECTOR_Fortran_OPTS} -march=native -mtune=native -xHOST -vecabi=cmdtarget")
        if ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "17.0.4")
            set(VECTOR_Fortran_OPTS "${VECTOR_Fortran_OPTS} -qopt-zmm-usage=high")
        endif ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "17.0.4")
        set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT} -march=native -mtune=native -no-vec")
        set(VECTOR_Fortran_VERBOSE "${VECTOR_Fortran_VERBOSE} -qopt-report=5 -qopt-report-phase=openmp,loop,vec")

    elseif (CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
        set(VECTOR_ALIASING_Fortran_FLAGS "${VECTOR_ALIASING_Fortran_FLAGS}")
        set(VECTOR_OPENMP_SIMD_Fortran_FLAGS "${VECTOR_OPENMP_SIMD_Fortran_FLAGS} -Mvect=simd")
        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            if ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "18.6")
                execute_process(COMMAND pgfortran --version COMMAND grep LLVM COMMAND wc -l OUTPUT_VARIABLE PGI_VERSION_OUTPUT OUTPUT_STRIP_TRAILING_WHITESPACE)
                if ("${PGI_VERSION_OUTPUT}" STREQUAL "1")
                    set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -Mllvm")
                endif ("${PGI_VERSION_OUTPUT}" STREQUAL "1")
            endif ("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_GREATER "18.6")
        endif ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")

        set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT} -Mnovect ")
        set(VECTOR_Fortran_VERBOSE "${VECTOR_Fortran_VERBOSE} -Minfo=loop,inline,vect")

    elseif (CMAKE_Fortran_COMPILER_ID MATCHES "MSVC")
        set(VECTOR_Fortran_OPTS "${VECTOR_Fortran_OPTS}" " ")

        set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT}" " ")
        set(VECTOR_Fortran_VERBOSE "${VECTOR_Fortran_VERBOSE} -Qvec-report:2")

    elseif (CMAKE_Fortran_COMPILER_ID MATCHES "XL")
        set(VECTOR_ALIASING_Fortran_FLAGS "${VECTOR_ALIASING_Fortran_FLAGS} -qalias=restrict")
        set(VECTOR_FPMODEL_Fortran_FLAGS "${VECTOR_FPMODEL_Fortran_FLAGS} -qstrict")
        set(VECTOR_ARCH_Fortran_FLAGS "${VECTOR_ARCH_Fortran_FLAGS} -qhot -qarch=auto -qtune=auto")

        set(CMAKE_VEC_Fortran_FLAGS "${CMAKE_VEC_FLAGS} -qsimd=auto")
        set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT} -qsimd=noauto")
        # "long vector" optimizations
        #set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT} -qhot=novector")
        set(VECTOR_Fortran_VERBOSE "${VECTOR_Fortran_VERBOSE} -qreport")

    elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
        set(VECTOR_ALIASING_Fortran_FLAGS "${VECTOR_ALIASING_Fortran_FLAGS} -h restrict=a")
        set(VECTOR_Fortran_OPTS "${VECTOR_Fortran_OPTS} -h vector=3")
  
       set(VECTOR_NOVEC_Fortran_OPT "${VECTOR_NOVEC_Fortran_OPT} -h vector=0")
       set(VECTOR_Fortran_VERBOSE "${VECTOR_Fortran_VERBOSE} -h msgs -h negmsgs -h list=a")

    endif()

    CHECK_FORTRAN_COMPILER_FLAG("${VECTOR_OPENMP_SIMD_Fortran_FLAGS}" HAVE_OPENMP_SIMD)
    if (HAVE_OPENMP_SIMD)
       add_definitions(-D_OPENMP_SIMD)
    else (HAVE_OPENMP_SIMD)
       unset(VECTOR_OPENMP_SIMD_Fortran_FLAGS)
    endif (HAVE_OPENMP_SIMD)

    set(VECTOR_BASE_Fortran_FLAGS "${VECTOR_ALIASING_Fortran_FLAGS} ${VECTOR_ARCH_Fortran_FLAGS} ${VECTOR_FPMODEL_Fortran_FLAGS}")
    set(VECTOR_NOVEC_Fortran_FLAGS "${VECTOR_BASE_Fortran_FLAGS} ${VECTOR_NOVEC_Fortran_OPT}")
    set(VECTOR_Fortran_FLAGS "${VECTOR_BASE_Fortran_FLAGS} ${VECTOR_Fortran_OPTS} ${VECTOR_Fortran_FPOPTS} ${VECTOR_OPENMP_SIMD_Fortran_FLAGS}")

    mark_as_advanced(VECTOR_Fortran_FLAGS
                     VECTOR_NOVEC_Fortran_FLAGS
                     VECTOR_Fortran_VERBOSE
                     VECTOR_ALIASING_Fortran_FLAGS
                     VECTOR_ARCH_Fortran_FLAGS
                     VECTOR_FPMODEL_Fortran_FLAGS
                     VECTOR_NOVEC_Fortran_OPT
                     VECTOR_VEC_Fortran_OPTS
                     VECTOR_VEC_Fortran_FPOPTS)

    message(STATUS  "Setting Vector Fortran flags to: ${VECTOR_Fortran_FLAGS}")
    message(STATUS  "Setting Vector Fortran No-Vector flags to: ${VECTOR_NOVEC_Fortran_FLAGS}")
    message(STATUS  "Setting Vector Fortran Verbose flags to: ${VECTOR_Fortran_VERBOSE}")

endif(CMAKE_Fortran_COMPILER_LOADED)
