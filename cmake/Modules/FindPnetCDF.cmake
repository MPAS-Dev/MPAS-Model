# FindPnetCDF.cmake
#
# Copyright UCAR 2020
#
# Find PnetCDF: A Parallel I/O Library for NetCDF File Access
# https://parallel-netcdf.github.io/
#
# Components available for query:
#  C - Has C support
#  CXX - Has CXX support
#  Fortran - Has Fortran support
#  NetCDF4 - Has NetCDF4 output support
#  GPTL - Has profiling support with GPTL enabled
#  Threads - Has thread safety enabled
#
# Variables provided:
#  PnetCDF_FOUND - True if PnetCDFL was found
#  PnetCDF_CONFIG_EXE - pnetcdf-config executable if found
#  PnetCDF_VERSION - Version of installed PnetCDF
#  PnetCDF_BIN_DIR - PnetCDF binary directory
#  PnetCDF_DEBUG - True if PnetCDF is built in debug mode
#
# Targets provided:
#  PnetCDF::PnetCDF_Fortran - Fortran interface target
#  PnetCDF::PnetCDF_C - C interface target
#  PnetCDF::PnetCDF_CXX - CXX interface target
#
# Functions provided:
#  pnetcdf_get_config(ret_var flags) - Call `pnetcdf-config` with flags and set ret_var with output on execution success.
#
#
# This module requires the `pnetcdf-config` executable to detect the directories and compiler and linker flags
# necessary for the PnetCDF::PnetCDF target.  To control where PnetCDF is found:
# * Option 1: Set an environment or cmake variable `PnetCDF_ROOT` to the install prefix for PnetCDF (e.g. /usr/local)
# * Option 2: Set an environment or cmake variable `PnetCDF_CONFIG_EXE` to the full path to the `pnetcdf-config`
#              (e.g., /usr/local/bin/pnetcdf-config)
#

find_program(PnetCDF_CONFIG_EXE NAMES pnetcdf-config PATH_SUFFIXES bin bin64 PATHS
             $ENV{PnetCDF_CONFIG_EXE} ${PnetCDF_ROOT} $ENV{PnetCDF_ROOT} ${PNETCDF_ROOT} $ENV{PNETCDF_ROOT})
message(DEBUG "[FindPnetCDF] Using PnetCDF_CONFIG_EXE:${PnetCDF_CONFIG_EXE}")

# pnetcdf_get_config(ret_var flags...)
#  Get the output of pnetcdf-config
#  Args:
#   ret_var: return variable name
#   flags: flags to pass to pnetcdf-config
function(pnetcdf_get_config ret_var pcflags)
    execute_process(COMMAND ${PnetCDF_CONFIG_EXE} ${pcflags} OUTPUT_VARIABLE _out RESULT_VARIABLE _ret OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(_ret EQUAL 0)
        separate_arguments(_out)
        set(${ret_var} ${_out} PARENT_SCOPE)
    else()
        set(${ret_var} "" PARENT_SCOPE)
    endif()
endfunction()

## Find libraries and paths, and determine found components
if(EXISTS ${PnetCDF_CONFIG_EXE})
    #Use pnetcdf-config to find the prefix, flags, directories, executables, and libraries
    pnetcdf_get_config(PnetCDF_VERSION --version)
    string(REGEX MATCH "([0-9.]+)" PnetCDF_VERSION "${PnetCDF_VERSION}") #Match only version actual number

    pnetcdf_get_config(PnetCDF_PREFIX --prefix)
    pnetcdf_get_config(PnetCDF_CXX_FOUND --has-c++)
    pnetcdf_get_config(PnetCDF_Fortran_FOUND --has-fortran)
    pnetcdf_get_config(PnetCDF_NetCDF4_FOUND --netcdf4)
    pnetcdf_get_config(PnetCDF_GPTL_FOUND --profiling)
    pnetcdf_get_config(PnetCDF_Threads_FOUND --thread-safe)
    pnetcdf_get_config(PnetCDF_DEBUG --debug)
    pnetcdf_get_config(PnetCDF_INCLUDE_DIR --includedir)
    pnetcdf_get_config(PnetCDF_LIB_DIR --libdir)

    #Translate boolean variables from pnetcdf-config enabled/disabled to True/False
    foreach(_var IN ITEMS PnetCDF_CXX_FOUND PnetCDF_Fortran_FOUND PnetCDF_NetCDF4_FOUND PnetCDF_GPTL_FOUND PnetCDF_Threads_FOUND PnetCDF_DEBUG)
        if( ${_var} MATCHES "(enabled)|([Yy][Ee][Ss])")
            set(${_var} True)
        else()
            set(${_var} False)
        endif()
    endforeach()

    find_path(PnetCDF_MODULE_DIR NAMES pnetcdf.mod HINTS ${PnetCDF_PREFIX} ${PnetCDF_INCLUDE_DIR}
              PATH_SUFFIXES include include/pnetcdf module module/pnetcdf lib/pnetcdf/module NO_DEFAULT_PATH)
    if(PnetCDF_Fortran_FOUND AND NOT EXISTS ${PnetCDF_MODULE_DIR})
        message(WARNING "[PnetCDF] pnetcdf-config --has-fortran=yes, but could not find pnetcdf.mod.  Set PnetCDF_MODULE_DIR to path containing pnetcdf.mod")
        set(PnetCDF_Fortran_FOUND NO)
    endif()

    if(PnetCDF_INCLUDE_DIR AND PnetCDF_LIB_DIR)
        set(PnetCDF_C_FOUND True)
    endif()

    find_path(PnetCDF_BIN_DIR NAMES pnetcdf-config PATH_SUFFIXES bin PATHS ${PnetCDF_PREFIX} NO_DEFAULT_PATH)
    find_library(PnetCDF_LIBRARY NAMES pnetcdf PATH_SUFFIXES lib lib64 PATHS ${PnetCDF_PREFIX} NO_DEFAULT_PATH)
    #Hide non-documented cache variables reserved for internal/advanced usage
    mark_as_advanced( PnetCDF_MODULE_DIR PnetCDF_LIBRARY )
endif()

## Debugging output
message(DEBUG "[FindPnetCDF] PnetCDF_CONFIG_EXE: ${PnetCDF_CONFIG_EXE}")
message(DEBUG "[FindPnetCDF] PnetCDF_VERSION: ${PnetCDF_VERSION}")
message(DEBUG "[FindPnetCDF] PnetCDF_C_FOUND: ${PnetCDF_C_FOUND}")
message(DEBUG "[FindPnetCDF] PnetCDF_CXX_FOUND: ${PnetCDF_CXX_FOUND}")
message(DEBUG "[FindPnetCDF] PnetCDF_Fortran_FOUND: ${PnetCDF_Fortran_FOUND}")
message(DEBUG "[FindPnetCDF] PnetCDF_NetCDF4_FOUND: ${PnetCDF_NetCDF4_FOUND}")
message(DEBUG "[FindPnetCDF] PnetCDF_GPTL_FOUND: ${PnetCDF_GPTL_FOUND}")
message(DEBUG "[FindPnetCDF] PnetCDF_Threads_FOUND: ${PnetCDF_Threads_FOUND}")
message(DEBUG "[FindPnetCDF] PnetCDF_DEBUG: ${PnetCDF_DEBUG}")
message(DEBUG "[FindPnetCDF] PnetCDF_PREFIX: ${PnetCDF_PREFIX}")
message(DEBUG "[FindPnetCDF] PnetCDF_BIN_DIR: ${PnetCDF_BIN_DIR}")
message(DEBUG "[FindPnetCDF] PnetCDF_INCLUDE_DIR: ${PnetCDF_INCLUDE_DIR}")
message(DEBUG "[FindPnetCDF] PnetCDF_MODULE_DIR: ${PnetCDF_MODULE_DIR}")
message(DEBUG "[FindPnetCDF] PnetCDF_LIB_DIR: ${PnetCDF_LIB_DIR}")

## Check package has been found correctly
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  PnetCDF
  REQUIRED_VARS
    PnetCDF_CONFIG_EXE
    PnetCDF_PREFIX
  VERSION_VAR
    PnetCDF_VERSION
  HANDLE_COMPONENTS
)
message(DEBUG "[FindPnetCDF] PnetCDF_FOUND: ${PnetCDF_FOUND}")

## Create targets
set(_new_components)

# PnetCDF::PnetCDF_Fortran imported interface target
if(PnetCDF_Fortran_FOUND AND NOT TARGET PnetCDF::PnetCDF_Fortran)
    add_library(PnetCDF::PnetCDF_Fortran INTERFACE IMPORTED)
    set_target_properties(PnetCDF::PnetCDF_Fortran PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${PnetCDF_INCLUDE_DIR}
                                                              INTERFACE_LINK_DIRECTORIES ${PnetCDF_LIB_DIR})
    if(PnetCDF_MODULE_DIR AND NOT PnetCDF_MODULE_DIR STREQUAL PnetCDF_INCLUDE_DIR )
        set_property(TARGET PnetCDF::PnetCDF_Fortran APPEND PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${PnetCDF_MODULE_DIR})
    endif()
    set(_new_components 1)
    target_link_libraries(PnetCDF::PnetCDF_Fortran INTERFACE -lpnetcdf)
endif()

# PnetCDF::PnetCDF_C imported interface target
if(PnetCDF_C_FOUND AND NOT TARGET PnetCDF::PnetCDF_C)
    add_library(PnetCDF::PnetCDF_C INTERFACE IMPORTED)
    set_target_properties(PnetCDF::PnetCDF_C PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${PnetCDF_INCLUDE_DIR}
                                                        INTERFACE_LINK_DIRECTORIES ${PnetCDF_LIB_DIR})
    set(_new_components 1)
endif()

# PnetCDF::PnetCDF_CXX imported interface target
if(PnetCDF_CXX_FOUND AND NOT TARGET PnetCDF::PnetCDF_CXX)
    add_library(PnetCDF::PnetCDF_CXX INTERFACE IMPORTED)
    set_target_properties(PnetCDF::PnetCDF_CXX PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${PnetCDF_INCLUDE_DIR}
                                                          INTERFACE_LINK_DIRECTORIES ${PnetCDF_LIB_DIR})
    set(_new_components 1)
endif()

## Print status
if(${CMAKE_FIND_PACKAGE_NAME}_FOUND AND NOT ${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY AND _new_components)
    message( STATUS "Find${CMAKE_FIND_PACKAGE_NAME}:" )
    message( STATUS "  - ${CMAKE_FIND_PACKAGE_NAME}_VERSION [${${CMAKE_FIND_PACKAGE_NAME}_VERSION}]")
    message( STATUS "  - ${CMAKE_FIND_PACKAGE_NAME}_PREFIX [${${CMAKE_FIND_PACKAGE_NAME}_PREFIX}]")
    set(_found_comps)
    foreach( _comp IN ITEMS Fortran C CXX NetCDF4 GPTL Threads )
        if( ${CMAKE_FIND_PACKAGE_NAME}_${_comp}_FOUND )
            list(APPEND _found_comps ${_comp})
        endif()
    endforeach()
    message( STATUS "  - ${CMAKE_FIND_PACKAGE_NAME} Components Found: ${_found_comps}")
    unset(_found_comps)
endif()
unset(_new_components)
