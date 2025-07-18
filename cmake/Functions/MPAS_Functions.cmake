##
# get_mpas_version( <mpas_version> )
#
# Extracts the MPAS-Model project's version from the README.md file.
# The extracted version is a string following the format "X.Y.Z", where
# "X", "Y", and "Z" correspond to the major, minor, and patch versions
# respectively.
#
# Precondition:
# * README.md file needs to be in the current source directory.
# * README.md file should contain the project version formatted
#   as "MPAS-vX.Y.Z".
#
# Postcondition:
# * If a match is found, <output_var> will contain the version string,
#   else it will be empty.
#
# Args:
# <mpas_version> - The name of the variable that will hold the extracted version
#                string.
#
# Example usage:
# get_mpas_version(MPAS_VERSION)
# message("MPAS Version: ${MPAS_VERSION}")
##
function(get_mpas_version mpas_version)
    file(READ "${CMAKE_CURRENT_SOURCE_DIR}/README.md" readme_contents)
    string(REGEX MATCH "MPAS-v([0-9]+\\.[0-9]+\\.[0-9]+)" _ ${readme_contents})
    set(${mpas_version} ${CMAKE_MATCH_1} PARENT_SCOPE)
endfunction()

##
# get_git_version( <git_version> )
# Extracts the current Git version of the project.
# <git_version> will contain the Git version string.
# Example usage:
# get_git_version(GIT_VERSION)
# message("Git Version: ${GIT_VERSION}")
##


function(get_git_version git_version)
    execute_process(
            COMMAND git describe --tags --always
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            RESULT_VARIABLE RESULT
            OUTPUT_VARIABLE GIT_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    if(NOT RESULT EQUAL 0)
        message(WARNING "Failed to get Git version!")
    endif()
    set(${git_version} ${GIT_VERSION} PARENT_SCOPE
    )
endfunction()


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

function(mpas_fortran_target target)
    # Fortran modules include path
    set_target_properties(${target} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MPAS_MODULE_DIR})
    target_include_directories(${target} INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MPAS_MODULE_DIR}>
            $<INSTALL_INTERFACE:${MPAS_MODULE_DIR}>)
    #Relocatable, portable, runtime dynamic linking
    set_target_properties(${target} PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")

    # Global Fortran configuration
    set_target_properties(${target} PROPERTIES Fortran_FORMAT FREE)
    if(MPAS_USE_PIO)
        set(MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS
            USE_PIO2=1
        )
    else()
        set(MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS
            MPAS_SMIOL_SUPPORT=1
        )
    endif()
    list(APPEND MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS _MPI=1)
    # Enable OpenMP support
    if(MPAS_OPENMP)
        target_link_libraries(${target} PUBLIC OpenMP::OpenMP_Fortran)
    endif()

    # Compiler-specific options and flags
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        list(APPEND MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                $<$<COMPILE_LANGUAGE:Fortran>:-ffree-line-length-none>
        )
        list(APPEND MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PUBLIC
                $<$<COMPILE_LANGUAGE:Fortran>:-fconvert=big-endian>
        )

        if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
            list(APPEND MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                    $<$<COMPILE_LANGUAGE:Fortran>:-fallow-argument-mismatch>
                    $<$<COMPILE_LANGUAGE:Fortran>:-fallow-invalid-boz>
            )
        endif()
        if(MPAS_DOUBLE_PRECISION)
            list(APPEND MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                    $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-real-8> $<$<COMPILE_LANGUAGE:Fortran>:-fdefault-double-8>
            )
        else()
            list(APPEND MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS SINGLE_PRECISION)
        endif()
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        list(APPEND MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PUBLIC
                $<$<COMPILE_LANGUAGE:Fortran>:-align array64byte>
                $<$<COMPILE_LANGUAGE:Fortran>:-convert big_endian>
        )
        if(MPAS_DOUBLE_PRECISION)
            list(APPEND MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                $<$<COMPILE_LANGUAGE:Fortran>:-real-size 64>
            )
        else()
            list(APPEND MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS SINGLE_PRECISION)
        endif()
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES NVHPC)

        list(APPEND MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS
            $<$<COMPILE_LANGUAGE:Fortran>:-DCPRPGI -DMPAS_NAMELIST_SUFFIX=atmosphere -DMPAS_EXE_NAME=atmosphere_model>
            $<$<COMPILE_LANGUAGE:Fortran>:-DMPAS_OPENACC -DSINGLE_PRECISION -DMPAS_BUILD_TARGET=nvhpc>
        )
        list(APPEND MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
            $<$<COMPILE_LANGUAGE:Fortran>: -Mnofma -acc -gpu=math_uniform,cc70,cc80 -Minfo=accel -byteswapio>
        )
        message(VERBOSE "${target} options: ${MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE}")
        message(VERBOSE "${target} defines: ${MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS}")
    endif()
    target_compile_definitions(${target} PRIVATE ${MPAS_FORTRAN_TARGET_COMPILE_DEFINITIONS})
    target_compile_options(${target} PRIVATE ${MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE})
    target_compile_options(${target} PUBLIC ${MPAS_FORTRAN_TARGET_COMPILE_OPTIONS_PUBLIC})
endfunction()


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
function(mpas_core_target)
    cmake_parse_arguments(ARG "" "CORE;TARGET" "INCLUDES" ${ARGN})

    mpas_fortran_target(${ARG_TARGET})

    set_property(TARGET ${ARG_TARGET} APPEND PROPERTY SOURCES ${MPAS_SUBDRIVER_SRC})

    string(TOUPPER "${ARG_TARGET}" TARGET)
    set_target_properties(${ARG_TARGET} PROPERTIES OUTPUT_NAME mpas_${ARG_CORE})

    #Fortran modules output location
    set(CORE_MODULE_DIR ${MPAS_MODULE_DIR}/${ARG_TARGET})
    set_target_properties(${ARG_TARGET} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${CORE_MODULE_DIR})
    target_include_directories(${ARG_TARGET} INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${CORE_MODULE_DIR}>
            $<INSTALL_INTERFACE:${CORE_MODULE_DIR}>)

    #MPAS Specific option
    target_compile_definitions(${ARG_TARGET} PRIVATE ${TARGET}=1)

    #Generated includes are included from either ./inc/ or ./ so we create a symlink in the build directory
    #To handle the inc/ variety (sw, test, seaice) uniformly with the ./ variety (atmosphere, init_atmosphere)
    add_custom_target(${ARG_CORE}_include_link ALL
            COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/inc)
    add_dependencies(${ARG_TARGET} ${ARG_CORE}_include_link)
    target_include_directories(${ARG_TARGET} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)

    #Core-independent library dependencies
    target_link_libraries(${ARG_TARGET} PUBLIC ${PROJECT_NAME}::operators ${PROJECT_NAME}::framework)

    #Define alias for external use
    add_library(${PROJECT_NAME}::core::${ARG_CORE} ALIAS ${ARG_TARGET})

    #Create main executable
    add_executable(mpas_${ARG_CORE} ${MPAS_MAIN_SRC})
    mpas_fortran_target(mpas_${ARG_CORE})
    target_link_libraries(mpas_${ARG_CORE} PUBLIC ${PROJECT_NAME}::core::${ARG_CORE})

    #Per-core generated output and tables directory location
    set(CORE_DATADIR ${CMAKE_BINARY_DIR}/${PROJECT_NAME}/${ARG_TARGET})
    file(MAKE_DIRECTORY ${CORE_DATADIR})

    #Process registry and generate includes, namelists, and streams
    get_git_version(git_version)
    string(TOUPPER ${ARG_CORE} ARG_CORE_UPPER)
    set(CPP_EXTRA_FLAGS ${CPP_EXTRA_FLAGS} -DCORE_${ARG_CORE_UPPER} -DMPAS_NAMELIST_SUFFIX=${ARG_CORE} -DMPAS_EXE_NAME=mpas_${ARG_CORE} -DMPAS_GIT_VERSION=${git_version} -DMPAS_BUILD_TARGET=${CMAKE_Fortran_COMPILER_ID})
    message("CPP_EXTRA_FLAGS: ${CPP_EXTRA_FLAGS}")
    if (${DO_PHYSICS})
        set(CPP_EXTRA_FLAGS ${CPP_EXTRA_FLAGS} -DDO_PHYSICS)
    endif()

add_custom_command(OUTPUT Registry_processed.xml
            COMMAND ${CPP_EXECUTABLE} -E -P ${CPP_EXTRA_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/Registry.xml > Registry_processed.xml
            COMMENT "CORE ${ARG_CORE}: Pre-Process Registry"
            DEPENDS Registry.xml)
    add_custom_command(OUTPUT ${ARG_INCLUDES}
            COMMAND mpas_parse_${ARG_CORE} Registry_processed.xml ${CPP_EXTRA_FLAGS}
            COMMENT "CORE ${ARG_CORE}: Parse Registry"
            DEPENDS mpas_parse_${ARG_CORE} Registry_processed.xml)
    add_custom_command(OUTPUT namelist.${ARG_CORE}
            WORKING_DIRECTORY ${CORE_DATADIR}
            COMMAND mpas_namelist_gen ${CMAKE_CURRENT_BINARY_DIR}/Registry_processed.xml namelist.${ARG_CORE} in_defaults=true
            COMMENT "CORE ${ARG_CORE}: Generate Namelist"
            DEPENDS mpas_namelist_gen Registry_processed.xml)
    add_custom_command(OUTPUT streams.${ARG_CORE}
            WORKING_DIRECTORY ${CORE_DATADIR}
            COMMAND mpas_streams_gen ${CMAKE_CURRENT_BINARY_DIR}/Registry_processed.xml streams.${ARG_CORE} stream_list.${ARG_CORE}. listed
            COMMENT "CORE ${ARG_CORE}: Generate Streams"
            DEPENDS mpas_streams_gen Registry_processed.xml)
    add_custom_target(gen_${ARG_CORE} DEPENDS ${ARG_INCLUDES} namelist.${ARG_CORE} streams.${ARG_CORE})
    add_dependencies(${ARG_TARGET} gen_${ARG_CORE})

    #Install data and target library and executable
    install(DIRECTORY ${CORE_DATADIR}/ DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/${ARG_TARGET}
            FILES_MATCHING PATTERN "namelist.*" PATTERN "streams.*" PATTERN "stream_list.*" )
    install(TARGETS ${ARG_TARGET} EXPORT ${PROJECT_NAME}ExportsCore
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
    install(TARGETS mpas_${ARG_CORE}
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endfunction()

##
# set_MPAS_DEBUG_flag( <target> )
#
# Sets the MPAS_DEBUG compile definition for a given target when the build type is Debug.
#
# Args:
# <target> - The target for which the compile definition will be set
#
# Usage example:
# set_MPAS_DEBUG_flag(TARGET)
# This will define MPAS_DEBUG for the target TARGET during a Debug build
##
function(set_MPAS_DEBUG_flag target)
    if(CMAKE_BUILD_TYPE MATCHES Debug)
        target_compile_definitions(${target} PRIVATE MPAS_DEBUG)
    endif()
endfunction()
