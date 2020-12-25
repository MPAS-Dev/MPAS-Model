macro(print_all_variables)
    message(STATUS "print_all_variables------------------------------------------{")
    get_cmake_property(_variableNames VARIABLES)
    foreach (_variableName ${_variableNames})
        message(STATUS "${_variableName}=${${_variableName}}")
    endforeach()
    message(STATUS "print_all_variables------------------------------------------}")
endmacro()

cmake_minimum_required(VERSION 3.0.0)

project (MPAS-Model C CXX Fortran)
# The version number.
set (MPAS-Model_VERSION_MAJOR 1)
set (MPAS-Model_VERSION_MINOR 1)
set (MPAS-Model_VERSION "${MPAS-Model_VERSION_MAJOR}.${MPAS-Model_VERSION_MINOR}")

cmake_policy(SET CMP0057 NEW)
if (POLICY CMP0074)
   cmake_policy(SET CMP0074 OLD)
   set(CMAKE_POLICY_DEFAULT_CMP0074 OLD)
endif()
if (POLICY CMP0075)
   set(CMAKE_POLICY_DEFAULT_CMP0075 OLD)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules")
set(CASEROOT ${CMAKE_MODULE_PATH})

# Set defaults for build
if (NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE RelWithDebInfo)
   #set(CMAKE_BUILD_TYPE Release)
endif(NOT CMAKE_BUILD_TYPE)

option (CMAKE_USE_OPENMP     "Enable OpenMP"           OFF)
option (CMAKE_USE_PAPI       "Enable PAPI profiling"   OFF)
option (CMAKE_USE_TAU        "Enable TAU profiling"    OFF)
option (CMAKE_VECTOR_NOVEC   "Disable vectorization"   OFF)
option (CMAKE_VECTOR_VERBOSE "Turn on vector reports"  OFF)

set(CMAKE_VERBOSE_MAKEFILE TRUE)

if( DEFINED ENV{PE_ENV} )
  set( CMAKE_SYSTEM_NAME CrayLinuxEnvironment CACHE STRING "description")
  #set( MPIEXEC "aprun" CACHE STRING "description")
endif()

# Need MPI for thirdparty build
find_package(MPI)

# turns off GPTL timing build
set(PIO_ENABLE_TIMING OFF CACHE BOOL "Enable GPTL timing")
set(PIO_C_EXTRA_LIBRARIES ${MPI_C_LIBRARIES})
set(PIO_C_EXTRA_INCLUDE_DIRS ${MPI_C_INCLUDE_PATH})
set(PIO_Fortran_EXTRA_INCLUDE_DIRS ${MPI_Fortran_INCLUDE_PATH})
set(PIO_Fortran_EXTRA_LIBRARIES ${MPI_Fortran_LIBRARIES})
if ( DEFINED ENV{PNETCDF} )
   set(PNETCDF $ENV{PNETCDF})
endif()
if ( DEFINED ENV{NETCDF_ROOT} )
   set(NETCDF $ENV{NETCDF_ROOT})
endif()
add_subdirectory(scorpio)
set(PIO_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/scorpio/src/flib)
set(INCLUDES "${INCLUDES};${PIO_INCLUDE_DIRS}")

if (NOT DEFINED NetCDF_C_LIBRARY)
   include(ExternalProject)
   include(${CMAKE_SOURCE_DIR}/thirdparty/NETCDF-C.cmake)
   set(NetCDF_C_HAS_VARN true CACHE BOOL "")
   set(NetCDF_Fortran_HAS_VARN true CACHE BOOL "")
   # turns off the detection of NetCDF. We've set these in the third-party build.
   set(WITH_NETCDF OFF CACHE BOOL "Require the use of NetCDF")
   add_dependencies(pioc netcdf_c_4_7_4)
endif()

if (NOT DEFINED PnetCDF_C_LIBRARY)
   include(ExternalProject)
   include(${CMAKE_SOURCE_DIR}/thirdparty/PNETCDF_parallel.cmake)
   set(PnetCDF_C_HAS_VARN true CACHE BOOL "")
   set(PnetCDF_Fortran_HAS_VARN true CACHE BOOL "")
   # turns off the detection of NetCDF. We've set these in the third-party build.
   set(WITH_PNETCDF OFF CACHE BOOL "Require the use of NetCDF")
   add_dependencies(pioc pnetcdf_1_12_1_parallel)
endif()

set(CORES ocean)
set(STANDALONE_BUILD YES)
set(NAMELIST_SUFFIX ocean)
set(EXE_NAME ocean_model)
set(USE_ESMF_LIB YES)

set(CMAKE_EXE_LINKER_FLAGS "${LDFLAGS}")

set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -g")
set(CMAKE_C_FLAGS_RELWITHDEBINFO       "-O3 -g")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO     "-O3 -g")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_C_FLAGS_RELEASE       "-O3")
set(CMAKE_CXX_FLAGS_RELEASE     "-O3")

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -m64 -ffree-line-length-none -fconvert=big-endian -ffree-form")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fbound-check -fbacktrace -ffpe-trap=invalid,zero,overflow")
    set(FFLAGS_PROMOTION "-fdefault-real-8 -fdefault-double-8")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Clang")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8 -ffree-line-length-none -fconvert=big-endian -ffree-form")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fbound-check -fbacktrace -ffpe-trap=invalid,zero,overflow")
    set(FFLAGS_PROMOTION "-fdefault-real-8 -fdefault-double-8")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -free -align array64byte")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -CU -CB -check all -fpe0 -traceback")
    set(FFLAGS_PROMOTION "-real-size 64")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -byteswapio -Mfree")
    set(CMAKE_Fortran_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Mbounds -Mchkptr -Ktrap=divz,fp,inv,ovf -traceback")
    set(FFLAGS_PROMOTION "-r8")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i4 -Kieee -convert big_endian")
    set(FFLAGS_PROMOTION "-r8")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "XL")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qufmt=be -WF,-qnotrigraph")
    set(FFLAGS_PROMOTION "-qrealsize=8")
endif()

if (CMAKE_C_COMPILER_ID MATCHES "GNU")
elseif (CMAKE_C_COMPILER_ID MATCHES "Intel")
  set(CMAKE_C_FLAGS "")
elseif (CMAKE_C_COMPILER_ID MATCHES "PGI")
elseif (CMAKE_C_COMPILER_ID MATCHES "Cray")
elseif (CMAKE_C_COMPILER_ID MATCHES "XL")
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Intel")
  set(CMAKE_CXX_FLAGS "")
elseif (CMAKE_CXX_COMPILER_ID MATCHES "PGI")
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Cray")
elseif (CMAKE_CXX_COMPILER_ID MATCHES "XL")
endif()

find_package(Vector)

if (CMAKE_VECTOR_NOVEC)
    set(VECTOR_C_FLAGS "${VECTOR_NOVEC_C_FLAGS}")
    set(VECTOR_CXX_FLAGS "${VECTOR_NOVEC_CXX_FLAGS}")
    set(VECTOR_Fortran_FLAGS "${VECTOR_NOVEC_Fortran_FLAGS}")
endif (CMAKE_VECTOR_NOVEC)

if (CMAKE_VECTOR_VERBOSE)
    set(VECTOR_C_FLAGS "${VECTOR_C_FLAGS} ${VECTOR_C_VERBOSE}")
    set(VECTOR_CXX_FLAGS "${VECTOR_CXX_FLAGS} ${VECTOR_CXX_VERBOSE}")
    set(VECTOR_Fortran_FLAGS "${VECTOR_Fortran_FLAGS} ${VECTOR_Fortran_VERBOSE}")
endif (CMAKE_VECTOR_VERBOSE)

set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} ${VECTOR_C_FLAGS}")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${VECTOR_CXX_FLAGS}")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${VECTOR_Fortran_FLAGS}")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} ${VECTOR_C_FLAGS}")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} ${VECTOR_CXX_FLAGS}")
set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} ${VECTOR_Fortran_FLAGS}")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${VECTOR_NOVEC_C_FLAGS}")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${VECTOR_NOVEC_CXX_FLAGS}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${VECTOR_NOVEC_Fortran_FLAGS}")

if (CMAKE_USE_OPENMP)
  find_package(OpenMP)
  if (OPENMP_FOUND)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_Fortran_FLAGS}")
  else (OPENMP_FOUND)
      set(CMAKE_USE_OPENMP Off)
  endif (OPENMP_FOUND)
endif (CMAKE_USE_OPENMP)
message(STATUS "CMAKE_USE_OPENMP is ${CMAKE_USE_OPENMP}")

if (CMAKE_USE_PAPI)
  find_package(PAPI)
  if (PAPI_FOUND)
      include_directories(${PAPI_INCLUDE_DIRS})
  else (PAPI_FOUND)
      set(CMAKE_USE_PAPI Off)
  endif (PAPI_FOUND)
endif (CMAKE_USE_PAPI)
message(STATUS "CMAKE_USE_PAPI is ${CMAKE_USE_PAPI}")

if (CMAKE_USE_TAU)
  find_package(TAU)
  if (TAU_FOUND)
      include_directories(${TAU_INCLUDE_DIRS})
  else (TAU_FOUND)
      set(CMAKE_USE_TAU Off)
  endif (TAU_FOUND)
endif (CMAKE_USE_TAU)
message(STATUS "CMAKE_USE_TAU is ${CMAKE_USE_TAU}")

if (MPI_FOUND)
  list(APPEND Defines _MPI)
endif (MPI_FOUND)
execute_process(COMMAND git describe --dirty ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE MPAS_GIT_VERSION)
list(APPEND Defines MPAS_GIT_VERSION=${MPAS_GIT_VERSION})
list(APPEND Defines MPAS_NAMELIST_SUFFIX=${NAMELIST_SUFFIX})
list(APPEND Defines MPAS_EXE_NAME=${EXE_NAME})
list(APPEND Defines MPAS_NATIVE_TIMERS)
list(APPEND Defines CORE_OCEAN)
list(APPEND Defines USE_PIO2)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FFLAGS_PROMOTION}")

add_subdirectory(src/external/ezxml)
set(EZXML_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/external/ezxml)
set(INCLUDES "${INCLUDES};${EZXML_INCLUDE_DIRS}")
set(EZXML_LIBRARIES ${CMAKE_BINARY_DIR}/src/external/ezxml/libezxml.a)

add_subdirectory(src/tools/input_gen)
add_subdirectory(src/tools/registry)

add_subdirectory(src/external/esmf_time_f90)
set(ESMF_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/src/external/esmf_time_f90)
set(INCLUDES "${INCLUDES};${ESMF_INCLUDE_DIRS}")

if (MPI_FOUND)
    set(INCLUDES "${INCLUDES};${MPI_Fortran_INCLUDE_PATH}")
endif (MPI_FOUND)

set(LIBRARIES ${LIBRARIES} esmf_time)
if (MPI_FOUND)
    set(LIBRARIES ${LIBRARIES} ${MPI_C_LIBRARIES})
endif (MPI_FOUND)
if (PAPI_FOUND)
    set(LIBRARIES ${LIBRARIES} ${PAPI_LIBRARIES})
endif (PAPI_FOUND)
if (TAU_FOUND)
    set(LIBRARIES ${LIBRARIES} ${TAU_LIBRARIES})
endif (TAU_FOUND)

add_subdirectory(src/framework)
set(FRAMEWORK_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/src/framework)
set(INCLUDES "${INCLUDES};${FRAMEWORK_INCLUDE_DIRS}")
add_subdirectory(src/operators)
set(OPERATORS_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/src/operators)
set(CORE_OCEAN_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/src/core_ocean)
add_subdirectory(src/core_ocean)

set(INCLUDES "${INCLUDES};${CMAKE_BINARY_DIR}/PnetCDF/include;${CMAKE_BINARY_DIR}/src")

set(LIBRARIES ${LIBRARIES} piof)
if (MPI_FOUND)
   set(LIBRARIES ${LIBRARIES} ${MPI_Fortran_LIBRARIES})
endif (MPI_FOUND)

add_executable(ocean_model src/driver/mpas.F src/driver/mpas_subdriver.F)
set_target_properties(ocean_model PROPERTIES COMPILE_DEFINITIONS "${Defines}")

target_include_directories(ocean_model PRIVATE ${INCLUDES} ${CORE_OCEAN_INCLUDE_DIRS})
target_link_libraries(ocean_model PRIVATE dycore framework ops ezxml)
target_link_libraries(ocean_model PRIVATE ${LIBRARIES})
set_property(TARGET ocean_model PROPERTY LINKER_LANGUAGE Fortran)
add_dependencies(ocean_model gen_namelists gen_streams)

add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/nightly_ocean_test_suite.py
     COMMAND sed ARGS 
       -e '/^forward/s!FULL_PATH_TO_MPAS_MODEL_REPO!${CMAKE_BINARY_DIR}/default_inputs!'
       -e '/^init/s!FULL_PATH_TO_MPAS_MODEL_REPO!${CMAKE_BINARY_DIR}/default_inputs!'
       -e '/^model/s!FULL_PATH_TO_MPAS_MODEL_REPO!${CMAKE_BINARY_DIR}!'
       -e '/^mpas_model/s!FULL_PATH_TO_MPAS_MODEL_REPO!${CMAKE_SOURCE_DIR}!'
       -e '/^mpas_model/ageometric_data = /turquoise/usr/projects/climate/mpeterse/repos/geometric_features/master/geometric_data'
       -e '/FULL_PATH_TO_LOCAL_MESH_DATABASE/s!FULL_PATH_TO_LOCAL_MESH_DATABASE!/usr/projects/regionalclimate/COMMON_MPAS/ocean/grids/mesh_database!'
       -e '/FULL_PATH_TO_LOCAL_INITIAL_CONDITION_DATABASE/s!FULL_PATH_TO_LOCAL_INITIAL_CONDITION_DATABASE!/usr/projects/regionalclimate/COMMON_MPAS/ocean/grids/initial_condition_database!'
       -e '/FULL_PATH_TO_BATHYMETRY_DATABASE/s!FULL_PATH_TO_BATHYMETRY_DATABASE!/usr/projects/regionalclimate/COMMON_MPAS/ocean/!'
       -e '/forward/s!namelist.ocean!namelist.ocean.forward!'
       -e '/init/s!namelist.ocean!namelist.ocean.init!'
       general.config.ocean > ${CMAKE_BINARY_DIR}/general.config.ocean.sys
     COMMAND ./manage_regression_suite.py ARGS -t ocean/regression_suites/nightly.xml 
        -f ${CMAKE_BINARY_DIR}/general.config.ocean.sys -s -m runtime_definitions/srun.xml --work_dir ${CMAKE_BINARY_DIR}
     WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/testing_and_setup/compass )

add_custom_target(gen_test_suite DEPENDS ${CMAKE_BINARY_DIR}/nightly_ocean_test_suite.py)

add_custom_command(OUTPUT case_outputs/Global_Ocean_240km_-_Init_Test
     COMMAND ./nightly_ocean_test_suite.py
     WORKING_DIRECTORY ${CMAKE_BINARY_DIR} 
     DEPENDS ${CMAKE_BINARY_DIR}/nightly_ocean_test_suite.py)

add_custom_target(regression_tests DEPENDS case_outputs)

add_custom_command(OUTPUT cleanup.stamp
     COMMAND rm ARGS -rf ocean case_outputs nightly_ocean_test_suite.py general.config.ocean.sys
     COMMAND ${CMAKE_COMMAND} ARGS -E touch cleanup.stamp
     WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )

add_custom_target(test_cleanup DEPENDS cleanup.stamp)

