## MPAS-Model
cmake_minimum_required(VERSION 3.12)
project(MPAS LANGUAGES C Fortran VERSION 8.0 DESCRIPTION "MPAS - Model for Prediction Across Scales")
### Global Configuration

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})
include(GNUInstallDirs)

# Options
set(MPAS_ALL_CORES atmosphere init_atmosphere landice ocean seaice sw test)
set(MPAS_CORES atmosphere CACHE STRING "MPAS cores to build. Options: ${MPAS_ALL_CORES}")
if(MPAS_CORES MATCHES " ") #Convert strings separated with spaces to CMake list separated with ';'
    string(REPLACE " " ";" MPAS_CORES ${MPAS_CORES})
    set(MPAS_CORES ${MPAS_CORES} CACHE STRING "MPAS cores to build. Options: ${MPAS_ALL_CORES}" FORCE)
endif()
option(MPAS_DOUBLE_PRECISION "Use double precision 64-bit Floating point." TRUE)
option(MPAS_PROFILE "Enable GPTL profiling" OFF)
option(MPAS_OPENMP "Enable OpenMP" OFF)
option(BUILD_SHARED_LIBS "Build shared libraries" ON)

message(STATUS "[OPTION] MPAS_CORES: ${MPAS_CORES}")
message(STATUS "[OPTION] MPAS_DOUBLE_PRECISION: ${MPAS_DOUBLE_PRECISION}")
message(STATUS "[OPTION] MPAS_PROFILE: ${MPAS_PROFILE}")
message(STATUS "[OPTION] MPAS_OPENMP: ${MPAS_OPENMP}")
message(STATUS "[OPTION] BUILD_SHARED_LIBS: ${BUILD_SHARED_LIBS}")

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
find_package(MPI REQUIRED COMPONENTS Fortran)
find_package(NetCDF REQUIRED COMPONENTS Fortran C)
find_package(PnetCDF REQUIRED COMPONENTS Fortran)
find_package(PIO REQUIRED COMPONENTS Fortran C)
if(MPAS_PROFILE)
    find_package(GPTL REQUIRED)
endif()

# Find C pre-processor
if(CMAKE_C_COMPILER_ID MATCHES GNU)
    find_program(CPP_EXECUTABLE NAMES cpp REQUIRED)
    set(CPP_EXTRA_FLAGS -traditional)
elseif(CMAKE_C_COMPILER_ID MATCHES "(Apple)?Clang" )
    find_program(CPP_EXECUTABLE NAMES cpp REQUIRED)
else()
    message(STATUS "Unknown compiler: ${CMAKE_C_COMPILER_ID}")
    set(CPP_EXECUTABLE ${CMAKE_C_COMPILER})
endif()

### Macros: Common target configuration

##
# mpas_fortran_target( <target-name> )
#
# Fortran configuration and options common to all MPAS Fortran targets
#
# * Installs common Fortan modules to a per-compiler-version directory
# * General Fortran formatting and configuration options
# * Per-compiler configuration and options
#   * MPAS_DOUBLE_PRECISION related flags
#
# Args:
#  <target_name> - The name of the target to prepare
#
macro(mpas_fortran_target _tgt)
    # Fortran modules include path
    set_target_properties(${_tgt} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MPAS_MODULE_DIR})
    target_include_directories(${_tgt} INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MPAS_MODULE_DIR}>
                                                 $<INSTALL_INTERFACE:${MPAS_MODULE_DIR}>)
    #Relocatable, portable, runtime dynamic linking
    set_target_properties(${_tgt} PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")

    # Global Fortran configuration
    set_target_properties(${_tgt} PROPERTIES Fortran_FORMAT FREE)
    target_compile_definitions(${_tgt} PRIVATE _MPI=1)
    target_compile_definitions(${_tgt} PRIVATE USE_PIO2=1)
    if(NOT MPAS_DOUBLE_PRECISION)
      target_compile_definitions(${_tgt} PRIVATE "-DSINGLE_PRECISION")
    endif()

    # Enable OpenMP support
    if(MPAS_OPENMP)
        target_link_libraries(${_tgt} PUBLIC OpenMP::OpenMP_Fortran)
    endif()

    # Compiler-specific options and flags
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        target_compile_options(${_tgt} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-ffree-line-length-none>)
        target_compile_options(${_tgt} PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-fconvert=big-endian>)
        if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
            target_compile_options(${_tgt} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fallow-argument-mismatch>)
            target_compile_options(${_tgt} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fallow-invalid-boz>)
        endif()
        if(MPAS_DOUBLE_PRECISION)
            target_compile_options(${_tgt} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-real-8> $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-double-8>)
        endif()
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        target_compile_options(${_tgt} PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-convert big_endian>)
        target_compile_options(${_tgt} PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-align array64byte>)
        if(MPAS_DOUBLE_PRECISION)
            target_compile_options(${_tgt} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-real-size 64>)
        endif()
    endif()
endmacro()

##
# mpas_core_target(CORE <core-name> TARGET <cmake-target-name> INCLUDE <file1.inc, ...> )
#
# Common configuration and properties for `MPAS::core::<core_name>` targets.
# * Calls mpas_fortran_target() for common Fortran target configuration.
# * Installs Fortran modules to a per-core directory and adds target include directories
#   appropriate for build and install trees.
# * XML Processing, parsing and generation of includes, namelists and streams
#   * Each core uses a core-specific parser executable
# * Links to MPAS::framework and MPAS::operators
# * Exports MPAS::core::<core_name> target alias for use by external dependencies
# * Installs core libraries modules and generated files.
#
#  Args:
#   CORE - Name of core
#   TARGET - Name of core_target (without namespace)
#   INCLUDES - List of generated include files
#
macro(mpas_core_target)
    cmake_parse_arguments(_ARG "" "CORE;TARGET" "INCLUDES" ${ARGN})

    mpas_fortran_target(${_ARG_TARGET})

    set_property(TARGET ${_ARG_TARGET} APPEND PROPERTY SOURCES ${MPAS_SUBDRIVER_SRC})

    string(TOUPPER "${_ARG_TARGET}" _TGT)
    set_target_properties(${_ARG_TARGET} PROPERTIES OUTPUT_NAME mpas_${_ARG_CORE})

    #Fortran modules output location
    set(CORE_MODULE_DIR ${MPAS_MODULE_DIR}/${_ARG_TARGET})
    set_target_properties(${_ARG_TARGET} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${CORE_MODULE_DIR})
    target_include_directories(${_ARG_TARGET} INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${CORE_MODULE_DIR}>
                                                        $<INSTALL_INTERFACE:${CORE_MODULE_DIR}>)

    #MPAS Specific option
    target_compile_definitions(${_ARG_TARGET} PRIVATE ${_TGT}=1)

    #Generated includes are included from either ./inc/ or ./ so we create a symlink in the build directory
    #To handle the inc/ variety (sw, test, seaice) uniformly with the ./ variety (atmosphere, init_atmosphere)
    add_custom_target(${_ARG_CORE}_include_link ALL
        COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/inc)
    add_dependencies(${_ARG_TARGET} ${_ARG_CORE}_include_link)
    target_include_directories(${_ARG_TARGET} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)

    #Core-independent library dependencies
    target_link_libraries(${_ARG_TARGET} PUBLIC ${PROJECT_NAME}::framework)
    target_link_libraries(${_ARG_TARGET} PUBLIC ${PROJECT_NAME}::operators)

    #Define alias for external use
    add_library(${PROJECT_NAME}::core::${_ARG_CORE} ALIAS ${_ARG_TARGET})

    #Create main executable
    add_executable(mpas_${_ARG_CORE} ${MPAS_MAIN_SRC})
    mpas_fortran_target(mpas_${_ARG_CORE})
    target_link_libraries(mpas_${_ARG_CORE} PUBLIC ${PROJECT_NAME}::core::${_ARG_CORE})

    #Per-core generated output and tables directory location
    set(CORE_DATADIR ${CMAKE_BINARY_DIR}/${PROJECT_NAME}/${_ARG_TARGET})
    file(MAKE_DIRECTORY ${CORE_DATADIR})

    #Process registry and generate includes, namelists, and streams
add_custom_command(OUTPUT Registry_processed.xml
                       COMMAND ${CPP_EXECUTABLE} -E -P ${CPP_EXTRA_FLAGS} -DDO_PHYSICS ${CMAKE_CURRENT_SOURCE_DIR}/Registry.xml > Registry_processed.xml
                       COMMENT "CORE ${_ARG_CORE}: Pre-Process Registry"
                       DEPENDS Registry.xml)
    add_custom_command(OUTPUT ${_ARG_INCLUDES}
                       COMMAND mpas_parse_${_ARG_CORE} Registry_processed.xml
                       COMMENT "CORE ${_ARG_CORE}: Parse Registry"
                       DEPENDS mpas_parse_${_ARG_CORE} Registry_processed.xml)
    add_custom_command(OUTPUT namelist.${_ARG_CORE}
                       WORKING_DIRECTORY ${CORE_DATADIR}
                       COMMAND mpas_namelist_gen ${CMAKE_CURRENT_BINARY_DIR}/Registry_processed.xml namelist.${_ARG_CORE} in_defaults=true
                       COMMENT "CORE ${_ARG_CORE}: Generate Namelist"
                       DEPENDS mpas_namelist_gen Registry_processed.xml)
    add_custom_command(OUTPUT streams.${_ARG_CORE}
                       WORKING_DIRECTORY ${CORE_DATADIR}
                       COMMAND mpas_streams_gen ${CMAKE_CURRENT_BINARY_DIR}/Registry_processed.xml streams.${_ARG_CORE} stream_list.${_ARG_CORE}. listed
                       COMMENT "CORE ${_ARG_CORE}: Generate Streams"
                       DEPENDS mpas_streams_gen Registry_processed.xml)
    add_custom_target(gen_${_ARG_CORE} DEPENDS ${_ARG_INCLUDES} namelist.${_ARG_CORE} streams.${_ARG_CORE})
    add_dependencies(${_ARG_TARGET} gen_${_ARG_CORE})

    #Install data and target library and executable
    install(DIRECTORY ${CORE_DATADIR}/ DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/${_ARG_TARGET}
            FILES_MATCHING PATTERN "namelist.*" PATTERN "streams.*" PATTERN "stream_list.*" )
    install(TARGETS ${_ARG_TARGET} EXPORT ${PROJECT_NAME}ExportsCore
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
    install(TARGETS mpas_${_ARG_CORE}
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endmacro()


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
string(TOLOWER ${PROJECT_NAME} _project_name_lower)
configure_package_config_file(cmake/PackageConfig.cmake.in ${_project_name_lower}-config.cmake
                              INSTALL_DESTINATION .
                              INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}
                              PATH_VARS BINDIR CORE_DATADIR_ROOT CMAKE_MODULE_INSTALL_PATH)

## <pkgname>-config.cmake: install-tree
# Variables to export for use from install-tree
set(BINDIR ${CMAKE_INSTALL_BINDIR})
set(CORE_DATADIR_ROOT ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})
set(CMAKE_MODULE_INSTALL_PATH ${CONFIG_INSTALL_DESTINATION}/Modules)
configure_package_config_file(cmake/PackageConfig.cmake.in install/${_project_name_lower}-config.cmake
                              INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION}
                              PATH_VARS BINDIR CORE_DATADIR_ROOT CMAKE_MODULE_INSTALL_PATH)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/install/${_project_name_lower}-config.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})

## <pkgname>-config-version.cmake
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/${_project_name_lower}-config-version.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${_project_name_lower}-config-version.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})

## package-targets.cmake and package-targets-<build-type>.cmake
install(EXPORT ${PROJECT_NAME}ExportsExternal NAMESPACE ${PROJECT_NAME}::external::
        FILE ${_project_name_lower}-targets-external.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(EXPORT ${PROJECT_NAME}Exports NAMESPACE ${PROJECT_NAME}::
        FILE ${_project_name_lower}-targets.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(EXPORT ${PROJECT_NAME}ExportsCore NAMESPACE ${PROJECT_NAME}::core::
        FILE ${_project_name_lower}-targets-core.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})
