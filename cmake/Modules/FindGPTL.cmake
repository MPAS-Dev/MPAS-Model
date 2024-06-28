# FindGPTL.cmake
#
# Copyright UCAR 2020
#
# Find the GPTL: General Purpose Timing Library (https://jmrosinski.github.io/GPTL/)
#
# This find module sets the following variables and targets:
#
# Variables:
#  GPTL_FOUND - True if GPTL was found
#  GPTL_VERSION_STRING - Version of installed GPTL
#  GPTL_BIN_DIR - GPTL binary directory
#  GPTL_HAS_PKG_CONFIG - GPTL was found with installed `gptl.pc` and pkg-config.  This indicates full support
#                        for compiler and linker flags as exported by GPTL.
# Targets:
#  GPTL::GPTL - Imported interface target to pass to target_link_libraries()
#
# NOTE: This find modules uses `pkg-config` to locate GPTL and glean the appropriate flags, directories,
# and link dependency ordering.  For this to work, both a `pkg-config` executable and a `gptl.pc`
# config file need to be found.
# * To find the `pkg-config` executable, ensure it is on your PATH.
#   * For non-standard locations the official CMake FindPkgConfig uses Cmake variable `PKG_CONFIG_EXECUTABLE`
#     or environment variable `PKG_CONFIG`. See: https://cmake.org/cmake/help/latest/module/FindPkgConfig.html
# * To find `gptl.pc` ensure it is on the (colon-separated) directories listed in standard pkg-config
#   environment variable `PKG_CONFIG_PATH`.
#    * See: https://linux.die.net/man/1/pkg-config
# * A working GPTL pkg-config install can be confirmed on the command line, e.g.,
#   ```
#   $ pkg-config --modversion gptl
#   8.0.2
#   ```
# To set a non-standard location for GPTL, ensure the correct `gptl.pc` pkg config file is found first
# on the environment's `PKG_CONFIG_PATH`. This can be checked with the pkg-config executable, e.g.,
#  ```
#  $ pkg-config --variable=prefix gptl
#  /usr/local
#  ```
# Only when pkg-config is not supported or available, GPTL will be searched by the standard CMake search procedures.
# Set environment or CMake variable GPTL_ROOT to control this search.  The GPTL_ROOT variable will have no effect
# if GPTL_HAS_PKG_CONFIG=True.
#

find_package(PkgConfig QUIET)
if(PKG_CONFIG_FOUND)
    message(DEBUG "[FindGPTL] Using PKG_CONFIG_EXECUTABLE:${PKG_CONFIG_EXECUTABLE}")
endif()

#Helper:
#check_pkg_config(ret_var pcname pcflags...)
# Check if pcname is known to pkg-config
# Returns:
#  Boolean: true if ${pcname}.pc file is found by pkg-config).
# Args:
#  ret_var: return variable name.
#  pcname: pkg-config name to look for (.pc file)
function(check_pkg_config ret_var pcname)
    if(NOT PKG_CONFIG_FOUND OR NOT EXISTS ${PKG_CONFIG_EXECUTABLE})
        set(${ret_var} False PARENT_SCOPE)
    else()
        execute_process(COMMAND ${PKG_CONFIG_EXECUTABLE} --exists ${pcname} RESULT_VARIABLE _found)
        if(_found EQUAL 0)
            set(${ret_var} True PARENT_SCOPE)
        else()
            set(${ret_var} False PARENT_SCOPE)
        endif()
    endif()
endfunction()

#Helper:
#get_pkg_config(ret_var pcname pcflags...)
# Get the output of pkg-config
# Args:
#  ret_var: return variable name
#  pcname: pkg-config name to look for (.pc file)
#  pcflags: pkg-config flags to pass
function(get_pkg_config ret_var pcname pcflags)
    execute_process(COMMAND ${PKG_CONFIG_EXECUTABLE} ${ARGN} ${pcname} ${pcflags} OUTPUT_VARIABLE _out RESULT_VARIABLE _ret OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(_ret EQUAL 0)
        separate_arguments(_out)
        set(${ret_var} ${_out} PARENT_SCOPE)
    else()
        set(${ret_var} "" PARENT_SCOPE)
    endif()
endfunction()

check_pkg_config(GPTL_HAS_PKG_CONFIG gptl)
if(GPTL_HAS_PKG_CONFIG)
    #Use pkg-config to find the prefix, flags, directories, executables, and libraries
    get_pkg_config(GPTL_VERSION_STRING gptl --modversion)
    get_pkg_config(GPTL_PREFIX gptl --variable=prefix)
    get_pkg_config(GPTL_INCLUDE_DIR gptl --cflags-only-I)
    if(EXISTS GPTL_INCLUDE_DIR)
        string(REGEX REPLACE "-I([^ ]+)" "\\1;" GPTL_INCLUDE_DIR ${GPTL_INCLUDE_DIR}) #Remove -I
    else()
        find_path(GPTL_INCLUDE_DIR NAMES gptl.h PATH_SUFFIXES include include/gptl PATHS ${GPTL_PREFIX} NO_DEFAULT_PATH)
    endif()
    find_path(GPTL_MODULE_DIR NAMES gptl.mod PATH_SUFFIXES include include/gptl module module/gptl PATHS ${GPTL_PREFIX} NO_DEFAULT_PATH)
    get_pkg_config(GPTL_COMPILE_OPTIONS gptl --cflags-only-other)
    get_pkg_config(GPTL_LINK_LIBRARIES gptl --libs-only-l)
    get_pkg_config(GPTL_LINK_DIRECTORIES gptl --libs-only-L)
    if(GPTL_LINK_DIRECTORIES)
        string(REGEX REPLACE "-L([^ ]+)" "\\1;" GPTL_LINK_DIRECTORIES ${GPTL_LINK_DIRECTORIES}) #Remove -L
    endif()
    get_pkg_config(GPTL_LINK_OPTIONS gptl --libs-only-other)
    find_library(GPTL_LIBRARY NAMES gptl PATH_SUFFIXES lib lib64 PATHS ${GPTL_PREFIX} NO_DEFAULT_PATH)
    find_path(GPTL_BIN_DIR NAMES gptl_avail PATH_SUFFIXES bin PATHS ${GPTL_PREFIX} NO_DEFAULT_PATH)
else()
    #Attempt to find GPTL without pkg-config as last resort.
    message(WARNING "\
FindGPTL: The `pkg-config` executable was not found. Ensure it is on your path or set \
environment variable PKG_CONFIG to your pkg-config executable. \
Attempting to find GPTL without pkg-config support may cause some required compiler and linker options to be unset.")

    find_path(GPTL_INCLUDE_DIR NAMES gptl.h PATH_SUFFIXES include include/gptl)
    find_path(GPTL_MODULE_DIR NAMES gptl.mod PATH_SUFFIXES include include/gptl module module/gptl)
    find_library(GPTL_LIBRARY NAMES gptl PATH_SUFFIXES lib lib64)
    find_path(GPTL_BIN_DIR NAMES gptl_avail PATH_SUFFIXES bin)
endif()

#Hide non-documented cache variables reserved for internal/advanced usage
mark_as_advanced( GPTL_INCLUDE_DIR
                  GPTL_MODULE_DIR
                  GPTL_LIBRARY )

#Debugging output
message(DEBUG "[FindGPTL] GPTL_FOUND: ${GPTL_FOUND}")
message(DEBUG "[FindGPTL] GPTL_VERSION_STRING: ${GPTL_VERSION_STRING}")
message(DEBUG "[FindGPTL] GPTL_HAS_PKG_CONFIG: ${GPTL_HAS_PKG_CONFIG}")
message(DEBUG "[FindGPTL] GPTL_PREFIX: ${GPTL_PREFIX}")
message(DEBUG "[FindGPTL] GPTL_BIN_DIR: ${GPTL_BIN_DIR}")
message(DEBUG "[FindGPTL] GPTL_INCLUDE_DIR: ${GPTL_INCLUDE_DIR}")
message(DEBUG "[FindGPTL] GPTL_MODULE_DIR: ${GPTL_MODULE_DIR}")
message(DEBUG "[FindGPTL] GPTL_LIBRARY: ${GPTL_LIBRARY}")
message(DEBUG "[FindGPTL] GPTL_LINK_LIBRARIES: ${GPTL_LINK_LIBRARIES}")
message(DEBUG "[FindGPTL] GPTL_LINK_DIRECTORIES: ${GPTL_LINK_DIRECTORIES}")
message(DEBUG "[FindGPTL] GPTL_LINK_OPTIONS: ${GPTL_LINK_OPTIONS}")

#Check package has been found correctly
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
  GPTL
  REQUIRED_VARS
    GPTL_LIBRARY
    GPTL_INCLUDE_DIR
    GPTL_MODULE_DIR
    GPTL_BIN_DIR
  VERSION_VAR
    GPTL_VERSION_STRING
)

#Create GPTL::GPTL imported interface target
if(GPTL_FOUND AND NOT TARGET GPTL::GPTL)
    add_library(GPTL::GPTL INTERFACE IMPORTED)
    set_property(TARGET GPTL::GPTL PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${GPTL_INCLUDE_DIR})
    if(GPTL_MODULE_DIR)
        set_property(TARGET GPTL::GPTL APPEND PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${GPTL_MODULE_DIR})
    endif()
    if(GPTL_COMPILE_OPTIONS)
        set_property(TARGET GPTL::GPTL PROPERTY INTERFACE_COMPILE_OPTIONS ${GPTL_COMPILE_OPTIONS})
    endif()
    if(GPTL_LINK_DIRECTORIES)
        set_property(TARGET GPTL::GPTL PROPERTY INTERFACE_LINK_DIRECTORIES ${GPTL_LINK_DIRECTORIES})
    endif()
    if(GPTL_LINK_OPTIONS)
        set_property(TARGET GPTL::GPTL PROPERTY INTERFACE_LINK_OPTIONS ${GPTL_LINK_OPTIONS})
    endif()
    if(GPTL_LINK_LIBRARIES)
        set_property(TARGET GPTL::GPTL PROPERTY INTERFACE_LINK_LIBRARIES ${GPTL_LINK_LIBRARIES})
    else()
        set_property(TARGET GPTL::GPTL PROPERTY INTERFACE_LINK_LIBRARIES ${GPTL_LIBRARY})
        get_filename_component(_lib_dir ${GPTL_LIBRARY} DIRECTORY)
        set_property(TARGET GPTL::GPTL APPEND PROPERTY INTERFACE_LINK_DIRECTORIES ${_lib_dir})
        unset(_lib_dir)
    endif()
endif()
