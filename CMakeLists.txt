cmake_minimum_required(VERSION 3.13)
cmake_policy(SET CMP0076 NEW)
project(reg C Fortran)


#
# Define build options
#
set(MPAS_CORE "None" CACHE STRING "The MPAS core to build. Possible options: atmosphere init_atmosphere")
set(MPAS_COMPILER "None" CACHE STRING "The MPAS core to build. Possible options: gnu intel nvhpc")
set(MPAS_PRECISION "double" CACHE STRING "The default precision for real values. Possible options: single double")

#
# Set core to build and associated preprocessing definitions
#
if(MPAS_CORE STREQUAL "atmosphere")
    set(MPAS_EXE_NAME "atmosphere_model")
    set(MPAS_CORE_DEF "CORE_ATMOSPHERE")
elseif(MPAS_CORE STREQUAL "init_atmosphere")
    set(MPAS_EXE_NAME "init_atmosphere_model")
    set(MPAS_CORE_DEF "CORE_INIT_ATMOSPHERE")
else()
    message(FATAL_ERROR "Please define MPAS_CORE as one of: atmosphere init_atmosphere")
endif()

add_executable(${MPAS_EXE_NAME})
target_compile_definitions(${MPAS_EXE_NAME} PUBLIC ${MPAS_CORE_DEF})


#
# Set compilation macros that are invariant
#
target_compile_definitions(${MPAS_EXE_NAME} PUBLIC _MPI)


#
# Set compiler and compiler options
#
if(MPAS_COMPILER STREQUAL gnu)
    set(CMAKE_C_COMPILER mpicc)
    set(CMAKE_Fortran_COMPILER mpif90)
    set(CMAKE_Fortran_FLAGS "-ffree-line-length-none")
    set(PROMOTION_FLAGS "-fdefault-real-8 -fdefault-double-8")
    set(BYTESWAP_FLAGS "-fconvert=big-endian")
elseif(MPAS_COMPILER STREQUAL intel)
    set(CMAKE_C_COMPILER mpicc)
    set(CMAKE_Fortran_COMPILER mpif90)
    set(PROMOTION_FLAGS "-real-size 64")
    set(BYTESWAP_FLAGS "-convert big_endian")
elseif(MPAS_COMPILER STREQUAL nvhpc)
    set(CMAKE_C_COMPILER mpicc)
    set(CMAKE_Fortran_COMPILER mpifort)
    set(PROMOTION_FLAGS "-r8")
    set(BYTESWAP_FLAGS "-byteswapio")
else()
    message(FATAL_ERROR "Please define MPAS_COMPILER as one of: gnu intel nvhpc")
endif()

set_property(TARGET ${MPAS_EXE_NAME} PROPERTY Fortran_FORMAT FREE)
set_property(TARGET ${MPAS_EXE_NAME} PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)


#
# Set precision for real values
#
if(MPAS_PRECISION STREQUAL "single")
    target_compile_definitions(${MPAS_EXE_NAME} PUBLIC SINGLE_PRECISION)
else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${PROMOTION_FLAGS}")
endif()


#
# Set flags to handle endianness of binary files
#
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${BYTESWAP_FLAGS}")


#
# Locate external libraries: netCDF, parallel-netCDF, PIO
#
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/" ${CMAKE_MODULE_PATH})


find_package(NetCDF REQUIRED)
find_package(HDF5 REQUIRED)
find_package(PNETCDF REQUIRED)
find_package(PIO)


#
# If the PIO library was found, use PIO; otherwise use SMIOL
#
get_property(PIO_VERSION GLOBAL PROPERTY PIO_VERSION)
if(PIO_VERSION GREATER_EQUAL 1)
    target_compile_definitions(${MPAS_EXE_NAME} PUBLIC MPAS_PIO_SUPPORT)

    #
    # Set compilation macros to select appropriate PIO API
    #
    if(PIO_VERSION STREQUAL "2")
        target_compile_definitions(${MPAS_EXE_NAME} PUBLIC USE_PIO2)
    endif()
    set(IO_LIBRARIES ${PIO_LIBRARIES})
    set(IO_INCLUDES_DIRS ${PIO_INCLUDE_DIRS})
else()
    target_compile_definitions(${MPAS_EXE_NAME} PUBLIC MPAS_SMIOL_SUPPORT)
endif()


#
# Process everything in the src/ subdirectory
#
add_subdirectory(src)


#
# Add include paths and libraries from packages that are internally built by MPAS
#
get_property(MPAS_INTERNAL_INCLUDE_DIRS GLOBAL PROPERTY MPAS_INTERNAL_INCLUDE_DIRS)
get_property(MPAS_INTERNAL_LIBRARIES GLOBAL PROPERTY MPAS_INTERNAL_LIBRARIES)
target_include_directories(${MPAS_EXE_NAME} PRIVATE ${MPAS_INTERNAL_INCLUDE_DIRS})
target_link_libraries(${MPAS_EXE_NAME} PRIVATE ${MPAS_INTERNAL_LIBRARIES})

#
# Add external include paths and libraries
#
target_include_directories(${MPAS_EXE_NAME}
                           PUBLIC
                              ${IO_INCLUDE_DIRS}
                              ${PNETCDF_INCLUDE_DIRS}
                              ${HDF5_INCLUDE_DIRS}
                              ${NETCDF_INCLUDE_DIRS}
                          )

target_link_libraries(${MPAS_EXE_NAME}
                      PUBLIC
                         ${IO_LIBRARIES}
                         ${NetCDF_LIBRARIES}
                         ${HDF5_LIBRARIES}
                         ${PNETCDF_LIBRARIES}
                         ${CMAKE_DL_LIBS}
                     )
