cmake_minimum_required(VERSION 3.9)
cmake_policy(SET CMP0057 NEW)

project(MPAS C CXX Fortran)

if (SMP_PRESENT)
  set(THREADDIR "threads")
  set(compile_threaded TRUE)
else()
  set(THREADDIR "nothreads")
  set(compile_threaded FALSE)
endif()

include(${CASEROOT}/Macros.cmake)

set(EXE_NAME ${CORE}_model)
set(NAMELIST_SUFFIX ${CORE})

# Map the ESM component corresponding to each MPAS core
if (CORE STREQUAL "ocean")
  set(COMPONENT "ocn")
elseif(CORE STREQUAL "landice")
  set(COMPONENT "glc")
elseif(CORE STREQUAL "seaice")
  set(COMPONENT "ice")
endif()

if (USE_ESMF_LIB)
  set(ESMFDIR "esmf")
else()
  set(ESMFDIR "noesmf")
endif()

set(INSTALL_SHAREDPATH "$ENV{INSTALL_SHAREDPATH}")

# Set up CPPDEFS
set(FILE_OFFSET "-DOFFSET64BIT")
set(CPPDEFS "${CPPDEFS} -DMPAS_NO_LOG_REDIRECT -DMPAS_NO_ESMF_INIT -DMPAS_ESM_SHR_CONST -DMPAS_PERF_MOD_TIMERS ${MODEL_FORMULATION} ${FILE_OFFSET} ${ZOLTAN_DEFINE} -D_MPI -DMPAS_NAMELIST_SUFFIX=${NAMELIST_SUFFIX} -DMPAS_EXE_NAME=${EXE_NAME}")
if (DEBUG)
  set(CPPDEFS "${CPPDEFS} -DMPAS_DEBUG")
endif()
if (compile_threaded)
  set(CPPDEFS "${CPPDEFS} -DMPAS_OPENMP")
endif()

set(INCLUDES "-I${INSTALL_SHAREDPATH}/include -I${INSTALL_SHAREDPATH}/${COMP_INTERFACE}/${ESMFDIR}/${NINST_VALUE}/csm_share -I${NETCDF_PATH}/include -I${INSTALL_SHAREDPATH}/pio -I${PNETCDF_PATH}/include -I${CMAKE_CURRENT_SOURCE_DIR}/external/ezxml -I${CMAKE_BINARY_DIR}/framework -I${CMAKE_BINARY_DIR}/operators")

# build_options.mk stuff handled here
if (CORE STREQUAL "ocean")
  set(CPPDEFS "${CPPDEFS} -DCORE_OCEAN")
  set(INCLUDES "${INCLUDES} -I${CMAKE_BINARY_DIR}/core_ocean/BGC -I${CMAKE_BINARY_DIR}/core_ocean/shared -I${CMAKE_BINARY_DIR}/core_ocean/analysis_members -I${CMAKE_BINARY_DIR}/core_ocean/cvmix -I${CMAKE_BINARY_DIR}/core_ocean/mode_forward -I${CMAKE_BINARY_DIR}/core_ocean/mode_analysis -I${CMAKE_BINARY_DIR}/core_ocean/mode_init")
elseif (CORE STREQUAL "seaice")
  set(CPPDEFS "${CPPDEFS} -DCORE_SEAICE -Dcoupled -DCCSMCOUPLED")
  set(INCLUDES "${INCLUDES} -I${CMAKE_BINARY_DIR}/core_seaice/column -I${CMAKE_BINARY_DIR}/core_seaice/shared -I${CMAKE_BINARY_DIR}/core_seaice/analysis_members -I${CMAKE_BINARY_DIR}/core_seaice/model_forward")
endif()


# Set key CMake variables
set(CMAKE_Fortran_FLAGS "${FFLAGS} ${INCLUDES}")
set(CMAKE_C_FLAGS "${CFLAGS} ${INCLUDES}")
set(CMAKE_CXX_FLAGS "${CXXFLAGS} ${INCLUDES}")
set(CMAKE_VERBOSE_MAKEFILE TRUE)

set(CMAKE_C_COMPILER ${MPICC})
set(CMAKE_CXX_COMPILER ${MPICXX})
set(CMAKE_Fortran_COMPILER ${MPIFC})
set(CMAKE_EXE_LINKER_FLAGS "${LDFLAGS}")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/../../lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/../../lib)

add_library(${COMPONENT})
separate_arguments(CPPDEFS_LIST UNIX_COMMAND "${CPPDEFS}")
separate_arguments(INCLUDES_LIST UNIX_COMMAND "${INCLUDES}")
target_compile_definitions(${COMPONENT} PRIVATE ${CPPDEFS_LIST})

# Gather sources

# externals
set(RAW_SOURCES external/ezxml/ezxml.c)

# framework
list(APPEND RAW_SOURCES
  framework/mpas_kind_types.F
  framework/mpas_framework.F
  framework/mpas_timer.F
  framework/mpas_timekeeping.F
  framework/mpas_constants.F
  framework/mpas_attlist.F
  framework/mpas_hash.F
  framework/mpas_sort.F
  framework/mpas_block_decomp.F
  framework/mpas_block_creator.F
  framework/mpas_dmpar.F
  framework/mpas_abort.F
  framework/mpas_decomp.F
  framework/mpas_threading.F
  framework/mpas_io.F
  framework/mpas_io_streams.F
  framework/mpas_bootstrapping.F
  framework/mpas_io_units.F
  framework/mpas_stream_manager.F
  framework/mpas_stream_list.F
  framework/mpas_forcing.F
  framework/mpas_c_interfacing.F
  framework/random_id.c
  framework/pool_hash.c
  framework/mpas_derived_types.F
  framework/mpas_domain_routines.F
  framework/mpas_field_routines.F
  framework/mpas_pool_routines.F
  framework/xml_stream_parser.c
  framework/regex_matching.c
  framework/mpas_field_accessor.F
  framework/mpas_log.F
)

# operators
list(APPEND RAW_SOURCES
  operators/mpas_vector_operations.F
  operators/mpas_matrix_operations.F
  operators/mpas_tensor_operations.F
  operators/mpas_rbf_interpolation.F
  operators/mpas_vector_reconstruction.F
  operators/mpas_spline_interpolation.F
  operators/mpas_tracer_advection_helpers.F
  operators/mpas_tracer_advection_mono.F
  operators/mpas_tracer_advection_std.F
  operators/mpas_geometry_utils.F
)

# Make build tools
set(CMAKE_C_COMPILER ${SCC})
add_executable(streams_gen tools/input_gen/streams_gen.c tools/input_gen/test_functions.c external/ezxml/ezxml.c)
target_compile_definitions(streams_gen PRIVATE ${CPPDEFS_LIST})
target_compile_options(streams_gen PRIVATE "-Uvector")

add_executable(namelist_gen tools/input_gen/namelist_gen.c tools/input_gen/test_functions.c external/ezxml/ezxml.c)
target_compile_definitions(namelist_gen PRIVATE ${CPPDEFS_LIST})
target_compile_options(namelist_gen PRIVATE "-Uvector")

add_executable(parse tools/registry/parse.c tools/registry/dictionary.c tools/registry/gen_inc.c tools/registry/fortprintf.c tools/registry/utility.c external/ezxml/ezxml.c)
target_compile_definitions(parse PRIVATE ${CPPDEFS_LIST})
target_compile_options(parse PRIVATE "-Uvector")
set(CMAKE_C_COMPILER ${MPICC})

set(CORE_BLDDIR ${CMAKE_BINARY_DIR}/core_${CORE})
if (NOT EXISTS ${CORE_BLDDIR})
  file(MAKE_DIRECTORY ${CORE_BLDDIR})
endif()

set(CORE_INPUT_DIR ${CORE_BLDDIR}/default_inputs)
if (NOT EXISTS ${CORE_INPUT_DIR})
  file(MAKE_DIRECTORY ${CORE_INPUT_DIR})
endif()

# Make .inc files
add_custom_command (
  OUTPUT ${CORE_BLDDIR}/Registry_processed.xml
  COMMAND cpp -P -traditional ${CPPDEFS_LIST} -Uvector
  ${CMAKE_CURRENT_SOURCE_DIR}/core_${CORE}/Registry.xml > Registry_processed.xml
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/core_${CORE}/Registry.xml
  WORKING_DIRECTORY ${CORE_BLDDIR}
)

set(INC_DIR ${CORE_BLDDIR}/inc)
if (NOT EXISTS ${INC_DIR})
  file(MAKE_DIRECTORY ${INC_DIR})
endif()

add_custom_command(
  OUTPUT ${INC_DIR}/core_variables.inc
  COMMAND ${CMAKE_BINARY_DIR}/parse < ${CORE_BLDDIR}/Registry_processed.xml
  DEPENDS parse ${CORE_BLDDIR}/Registry_processed.xml
  WORKING_DIRECTORY ${INC_DIR}
)

# Macro for handling nl and st gen
macro(handle_nl_st_gen)
  foreach(NL_GEN_ARG IN LISTS NL_GEN_ARGS)
    separate_arguments(ITEMS UNIX_COMMAND "${NL_GEN_ARG}")
    list(GET ITEMS 0 ITEM)
    list(APPEND INPUTS ${ITEM})
    add_custom_command(
      OUTPUT ${CORE_INPUT_DIR}/${ITEM}
      COMMAND ${CMAKE_BINARY_DIR}/namelist_gen ${CORE_BLDDIR}/Registry_processed.xml ${NL_GEN_ARG}
      DEPENDS namelist_gen ${CORE_BLDDIR}/Registry_processed.xml
      WORKING_DIRECTORY ${CORE_INPUT_DIR}
    )
  endforeach()

  foreach(ST_GEN_ARG IN LISTS ST_GEN_ARGS)
    separate_arguments(ITEMS UNIX_COMMAND "${ST_GEN_ARG}")
    list(GET ITEMS 0 ITEM)
    list(APPEND INPUTS ${ITEM})
    add_custom_command(
      OUTPUT ${CORE_INPUT_DIR}/${ITEM}
      COMMAND ${CMAKE_BINARY_DIR}/streams_gen ${CORE_BLDDIR}/Registry_processed.xml ${ST_GEN_ARG}
      DEPENDS streams_gen ${CORE_BLDDIR}/Registry_processed.xml
      WORKING_DIRECTORY ${CORE_INPUT_DIR}
    )
  endforeach()

  foreach(INPUT IN LISTS INPUTS)
    add_custom_command(
      OUTPUT ${CORE_BLDDIR}/${INPUT}
      COMMAND ${CMAKE_COMMAND} -E copy ${CORE_INPUT_DIR}/${INPUT} ${CORE_BLDDIR}/${INPUT}
      DEPENDS ${CORE_INPUT_DIR}/${INPUT}
      WORKING_DIRECTORY ${CORE_BLDDIR}
    )
  endforeach()
endmacro(handle_nl_st_gen)

include(${CMAKE_CURRENT_SOURCE_DIR}/core_${CORE}/${CORE}.cmake)

# Disable qsmp for some files
if (FFLAGS MATCHES ".*-qsmp.*")
  foreach(DISABLE_QSMP_FILE IN LISTS DISABLE_QSMP)
    get_filename_component(SOURCE_EXT ${DISABLE_QSMP_FILE} EXT)
    string(REPLACE "${SOURCE_EXT}" ".f90" SOURCE_F90 ${DISABLE_QSMP_FILE})
    set_property(SOURCE ${CMAKE_BINARY_DIR}/${SOURCE_F90} APPEND_STRING PROPERTY COMPILE_FLAGS " -nosmp")
  endforeach()
endif()

# Run all .F files through cpp to generate the f90 file
foreach(RAW_SOURCE_FILE IN LISTS RAW_SOURCES)
  get_filename_component(SOURCE_EXT ${RAW_SOURCE_FILE} EXT)
  if ( (SOURCE_EXT STREQUAL ".F" OR SOURCE_EXT STREQUAL ".F90") AND NOT RAW_SOURCE_FILE IN_LIST NO_PREPROCESS)
    string(REPLACE "${SOURCE_EXT}" ".f90" SOURCE_F90 ${RAW_SOURCE_FILE})
    get_filename_component(DIR_RELATIVE ${SOURCE_F90} DIRECTORY)
    set(DIR_ABSOLUTE ${CMAKE_BINARY_DIR}/${DIR_RELATIVE})
    if (NOT EXISTS ${DIR_ABSOLUTE})
      file(MAKE_DIRECTORY ${DIR_ABSOLUTE})
    endif()
    add_custom_command (
      OUTPUT ${CMAKE_BINARY_DIR}/${SOURCE_F90}
      COMMAND cpp -P -traditional ${CPPDEFS_LIST} ${INCLUDES_LIST} -Uvector
      ${CMAKE_CURRENT_SOURCE_DIR}/${RAW_SOURCE_FILE} > ${CMAKE_BINARY_DIR}/${SOURCE_F90}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${RAW_SOURCE_FILE} ${INC_DIR}/core_variables.inc)
    list(APPEND SOURCES ${CMAKE_BINARY_DIR}/${SOURCE_F90})
  else()
    list(APPEND SOURCES ${RAW_SOURCE_FILE})
  endif()
endforeach()

target_sources(${COMPONENT} PRIVATE ${SOURCES})
