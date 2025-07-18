## MPAS-Model
cmake_minimum_required(VERSION 3.12)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/Functions/MPAS_Functions.cmake)
get_mpas_version(MPAS_VERSION)
project(MPAS LANGUAGES C Fortran VERSION ${MPAS_VERSION} DESCRIPTION "MPAS - Model for Prediction Across Scales")

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})
include(GNUInstallDirs)

# Options
set(MPAS_ALL_CORES atmosphere init_atmosphere)
set(MPAS_CORES atmosphere CACHE STRING "MPAS cores to build. Options: ${MPAS_ALL_CORES}")
if(MPAS_CORES MATCHES " ") #Convert strings separated with spaces to CMake list separated with ';'
    string(REPLACE " " ";" MPAS_CORES ${MPAS_CORES})
    set(MPAS_CORES ${MPAS_CORES} CACHE STRING "MPAS cores to build. Options: ${MPAS_ALL_CORES}" FORCE)
endif()
option(DO_PHYSICS "Use built-in physics schemes." TRUE)
option(MPAS_DOUBLE_PRECISION "Use double precision 64-bit Floating point." TRUE)
option(MPAS_PROFILE "Enable GPTL profiling" OFF)
option(MPAS_OPENMP "Enable OpenMP" OFF)
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(MPAS_USE_PIO "Build with PIO I/O library" OFF)

message(STATUS "[OPTION] MPAS_CORES: ${MPAS_CORES}")
message(STATUS "[OPTION] MPAS_DOUBLE_PRECISION: ${MPAS_DOUBLE_PRECISION}")
message(STATUS "[OPTION] MPAS_PROFILE: ${MPAS_PROFILE}")
message(STATUS "[OPTION] MPAS_OPENMP: ${MPAS_OPENMP}")
message(STATUS "[OPTION] BUILD_SHARED_LIBS: ${BUILD_SHARED_LIBS}")
message(STATUS "[OPTION] MPAS_USE_PIO: ${MPAS_USE_PIO}")

# Build product output locations
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Set default build type to RelWithDebInfo
if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting default build type to Release.  Specify CMAKE_BUILD_TYPE to override.")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "CMake Build type" FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Detect MPAS git version
if(NOT MPAS_GIT_VERSION)
    find_package(Git QUIET)
    if(GIT_FOUND)
        execute_process(COMMAND ${GIT_EXECUTABLE} describe --dirty
                        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
                        OUTPUT_VARIABLE _mpas_git_version
                        ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
    else()
        set(_mpas_git_version "Unknown")
    endif()
    set(MPAS_GIT_VERSION ${_mpas_git_version} CACHE STRING "MPAS-Model git version")
endif()

### Dependencies
find_package(OpenMP COMPONENTS Fortran)

# use mpifort options with nvfortran
if(CMAKE_Fortran_COMPILER_ID MATCHES NVHPC)
    find_program(CMAKE_Fortran_COMPILER_MPI NAMES mpifort REQUIRED)
    message(VERBOSE "CMAKE_Fortran_COMPILER_MPI: ${CMAKE_Fortran_COMPILER_MPI}")
    set(MPI_Fortran_COMPILER mpifort)
endif()
find_package(MPI REQUIRED COMPONENTS Fortran)

find_package(PnetCDF REQUIRED COMPONENTS Fortran)
if(MPAS_USE_PIO)
    find_package(PIO REQUIRED COMPONENTS Fortran C)
    find_package(NetCDF REQUIRED COMPONENTS Fortran C)
endif()
if(MPAS_PROFILE)
    find_package(GPTL REQUIRED)
endif()

# Find C pre-processor
if(CMAKE_C_COMPILER_ID MATCHES GNU)
    find_program(CPP_EXECUTABLE NAMES cpp REQUIRED)
    set(CPP_EXTRA_FLAGS -traditional)
elseif(CMAKE_C_COMPILER_ID MATCHES "(Apple)?Clang" )
    find_program(CPP_EXECUTABLE NAMES cpp REQUIRED)
elseif(CMAKE_C_COMPILER_ID MATCHES "NVHPC" )
    find_program(CPP_EXECUTABLE NAMES cpp REQUIRED)
else()
    message(STATUS "Unknown compiler: ${CMAKE_C_COMPILER_ID}")
    set(CPP_EXECUTABLE ${CMAKE_C_COMPILER})
endif()

## Common Variables

# Fortran module output directory for build interface
set(MPAS_MODULE_DIR ${PROJECT_NAME}/module/${CMAKE_Fortran_COMPILER_ID}/${CMAKE_Fortran_COMPILER_VERSION})
# Install Fortran module directory
install(DIRECTORY ${CMAKE_BINARY_DIR}/${MPAS_MODULE_DIR}/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/${MPAS_MODULE_DIR}/)

# Location of common subdriver module compiled by each cores
set(MPAS_MAIN_SRC  ${CMAKE_CURRENT_SOURCE_DIR}/src/driver/mpas.F)
set(MPAS_SUBDRIVER_SRC  ${CMAKE_CURRENT_SOURCE_DIR}/src/driver/mpas_subdriver.F)

## Create targets
add_subdirectory(src/external/ezxml) # Target: MPAS::external::ezxml
if(NOT MPAS_USE_PIO)
    add_subdirectory(src/external/SMIOL) # Target: MPAS::external::smiol
endif()
if(ESMF_FOUND)
  message(STATUS "Configure MPAS for external ESMF")
  add_definitions(-DMPAS_EXTERNAL_ESMF_LIB -DMPAS_NO_ESMF_INIT)
  add_library(${PROJECT_NAME}::external::esmf ALIAS esmf)
else()
  message(STATUS "Configure MPAS for internal ESMF")
  add_subdirectory(src/external/esmf_time_f90) # Target: MPAS::external::esmf_time
endif()
add_subdirectory(src/tools/input_gen) # Targets: namelist_gen, streams_gen
add_subdirectory(src/tools/registry) # Targets: mpas_parse_<core_name>
add_subdirectory(src/framework) # Target: MPAS::framework
add_subdirectory(src/operators) # Target: MPAS::operators

foreach(_core IN LISTS MPAS_CORES)
    add_subdirectory(src/core_${_core}) # Target: MPAS::core::<core_name>
endforeach()

### Package config
include(CMakePackageConfigHelpers)

# Build-tree target exports
export(EXPORT ${PROJECT_NAME}ExportsExternal NAMESPACE ${PROJECT_NAME}::external:: FILE ${PROJECT_NAME}-targets-external.cmake)
export(EXPORT ${PROJECT_NAME}Exports NAMESPACE ${PROJECT_NAME}:: FILE ${PROJECT_NAME}-targets.cmake)
export(EXPORT ${PROJECT_NAME}ExportsCore NAMESPACE ${PROJECT_NAME}::core:: FILE ${PROJECT_NAME}-targets-core.cmake)

# CMake Config file install location
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})
# Install MPAS-supplied Find<Pkg>.cmake modules for use by downstream CMake dependencies
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules DESTINATION ${CONFIG_INSTALL_DESTINATION})

## <pkgname>-config.cmake: build-tree
# Variables to export for use from build-tree
set(BINDIR ${CMAKE_BINARY_DIR}/bin)
set(CORE_DATADIR_ROOT ${CMAKE_BINARY_DIR}/${PROJECT_NAME})
set(CMAKE_MODULE_INSTALL_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
string(TOLOWER ${PROJECT_NAME} PROJECT_NAME_LOWER)
configure_package_config_file(cmake/PackageConfig.cmake.in ${PROJECT_NAME_LOWER}-config.cmake
                              INSTALL_DESTINATION .
                              INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}
                              PATH_VARS BINDIR CORE_DATADIR_ROOT CMAKE_MODULE_INSTALL_PATH)

## <pkgname>-config.cmake: install-tree
# Variables to export for use from install-tree
set(BINDIR ${CMAKE_INSTALL_BINDIR})
set(CORE_DATADIR_ROOT ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})
set(CMAKE_MODULE_INSTALL_PATH ${CONFIG_INSTALL_DESTINATION}/Modules)
configure_package_config_file(cmake/PackageConfig.cmake.in install/${PROJECT_NAME_LOWER}-config.cmake
                              INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION}
                              PATH_VARS BINDIR CORE_DATADIR_ROOT CMAKE_MODULE_INSTALL_PATH)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/install/${PROJECT_NAME_LOWER}-config.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})

## <pkgname>-config-version.cmake
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME_LOWER}-config-version.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME_LOWER}-config-version.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})

## package-targets.cmake and package-targets-<build-type>.cmake
install(EXPORT ${PROJECT_NAME}ExportsExternal NAMESPACE ${PROJECT_NAME}::external::
        FILE ${PROJECT_NAME_LOWER}-targets-external.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(EXPORT ${PROJECT_NAME}Exports NAMESPACE ${PROJECT_NAME}::
        FILE ${PROJECT_NAME_LOWER}-targets.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(EXPORT ${PROJECT_NAME}ExportsCore NAMESPACE ${PROJECT_NAME}::core::
        FILE ${PROJECT_NAME_LOWER}-targets-core.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})
