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

# JGF TODO Check for build_options.mk
set(EXE_NAME ${CORE}_model)
set(NAMELIST_SUFFIX ${CORE})

# Map the ESM component corresponding to each MPAS core
if (CORE STREQUAL "ocean")
  set(COMPONENT ocn)
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

set(INCLUDES "-I${INSTALL_SHAREDPATH}/include -I${INSTALL_SHAREDPATH}/${COMP_INTERFACE}/${ESMFDIR}/${NINST_VALUE}/csm_share -I${NETCDF_PATH}/include -I${INSTALL_SHAREDPATH}/pio -I${PNETCDF_PATH}/include -I${CMAKE_CURRENT_SOURCE_DIR}/external/ezxml")

if (CORE STREQUAL "ocean")
  set(CPPDEFS "${CPPDEFS} -DCORE_OCEAN")
  set(INCLUDES "${INCLUDES} -I${CMAKE_BINARY_DIR}/framework -I${CMAKE_BINARY_DIR}/operators -I${CMAKE_BINARY_DIR}/core_ocean/BGC -I${CMAKE_BINARY_DIR}/core_ocean/shared -I${CMAKE_BINARY_DIR}/core_ocean/analysis_members -I${CMAKE_BINARY_DIR}/core_ocean/cvmix -I${CMAKE_BINARY_DIR}/core_ocean/mode_forward -I${CMAKE_BINARY_DIR}/core_ocean/mode_analysis -I${CMAKE_BINARY_DIR}/core_ocean/mode_init")
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

# driver (files live in E3SM)
list(APPEND RAW_SOURCES
  ../../mpas-ocean/driver/ocn_comp_mct.F
  ../../mpas-ocean/driver/mpaso_cpl_indices.F
  ../../mpas-ocean/driver/mpaso_mct_vars.F
)

# dycore
list(APPEND RAW_SOURCES
  core_ocean/mode_forward/mpas_ocn_forward_mode.F
  core_ocean/mode_forward/mpas_ocn_time_integration.F
  core_ocean/mode_forward/mpas_ocn_time_integration_rk4.F
  core_ocean/mode_forward/mpas_ocn_time_integration_split.F

  core_ocean/mode_analysis/mpas_ocn_analysis_mode.F

  core_ocean/mode_init/mpas_ocn_init_mode.F
  core_ocean/mode_init/mpas_ocn_init_spherical_utils.F
  core_ocean/mode_init/mpas_ocn_init_vertical_grids.F
  core_ocean/mode_init/mpas_ocn_init_cell_markers.F
  core_ocean/mode_init/mpas_ocn_init_interpolation.F
  core_ocean/mode_init/mpas_ocn_init_ssh_and_landIcePressure.F
  core_ocean/mode_init/mpas_ocn_init_baroclinic_channel.F
  core_ocean/mode_init/mpas_ocn_init_lock_exchange.F
  core_ocean/mode_init/mpas_ocn_init_dam_break.F
  core_ocean/mode_init/mpas_ocn_init_internal_waves.F
  core_ocean/mode_init/mpas_ocn_init_overflow.F
  core_ocean/mode_init/mpas_ocn_init_cvmix_WSwSBF.F
  core_ocean/mode_init/mpas_ocn_init_iso.F
  core_ocean/mode_init/mpas_ocn_init_soma.F
  core_ocean/mode_init/mpas_ocn_init_ziso.F
  core_ocean/mode_init/mpas_ocn_init_sub_ice_shelf_2D.F
  core_ocean/mode_init/mpas_ocn_init_periodic_planar.F
  core_ocean/mode_init/mpas_ocn_init_ecosys_column.F
  core_ocean/mode_init/mpas_ocn_init_sea_mount.F
  core_ocean/mode_init/mpas_ocn_init_global_ocean.F
  core_ocean/mode_init/mpas_ocn_init_isomip.F
  core_ocean/mode_init/mpas_ocn_init_isomip_plus.F
  core_ocean/mode_init/mpas_ocn_init_tidal_boundary.F

  core_ocean/driver/mpas_ocn_core.F
  core_ocean/driver/mpas_ocn_core_interface.F

  core_ocean/shared/mpas_ocn_init_routines.F
  core_ocean/shared/mpas_ocn_gm.F
  core_ocean/shared/mpas_ocn_diagnostics.F
  core_ocean/shared/mpas_ocn_diagnostics_routines.F
  core_ocean/shared/mpas_ocn_thick_ale.F
  core_ocean/shared/mpas_ocn_equation_of_state.F
  core_ocean/shared/mpas_ocn_equation_of_state_jm.F
  core_ocean/shared/mpas_ocn_equation_of_state_linear.F
  core_ocean/shared/mpas_ocn_thick_hadv.F
  core_ocean/shared/mpas_ocn_thick_vadv.F
  core_ocean/shared/mpas_ocn_thick_surface_flux.F
  core_ocean/shared/mpas_ocn_vel_coriolis.F
  core_ocean/shared/mpas_ocn_vel_vadv.F
  core_ocean/shared/mpas_ocn_vel_hmix.F
  core_ocean/shared/mpas_ocn_vel_hmix_del2.F
  core_ocean/shared/mpas_ocn_vel_hmix_leith.F
  core_ocean/shared/mpas_ocn_vel_hmix_del4.F
  core_ocean/shared/mpas_ocn_vel_forcing.F
  core_ocean/shared/mpas_ocn_vel_forcing_surface_stress.F
  core_ocean/shared/mpas_ocn_vel_forcing_explicit_bottom_drag.F
  core_ocean/shared/mpas_ocn_vel_forcing_rayleigh.F
  core_ocean/shared/mpas_ocn_vel_pressure_grad.F
  core_ocean/shared/mpas_ocn_vmix.F
  core_ocean/shared/mpas_ocn_vmix_coefs_const.F
  core_ocean/shared/mpas_ocn_vmix_coefs_rich.F
  core_ocean/shared/mpas_ocn_vmix_coefs_tanh.F
  core_ocean/shared/mpas_ocn_vmix_coefs_redi.F
  core_ocean/shared/mpas_ocn_vmix_cvmix.F
  core_ocean/shared/mpas_ocn_tendency.F
  core_ocean/shared/mpas_ocn_tracer_hmix.F
  core_ocean/shared/mpas_ocn_tracer_hmix_del2.F
  core_ocean/shared/mpas_ocn_tracer_hmix_del4.F
  core_ocean/shared/mpas_ocn_tracer_hmix_redi.F
  core_ocean/shared/mpas_ocn_tracer_advection.F
  core_ocean/shared/mpas_ocn_tracer_advection_mono.F
  core_ocean/shared/mpas_ocn_tracer_advection_std.F
  core_ocean/shared/mpas_ocn_tracer_nonlocalflux.F
  core_ocean/shared/mpas_ocn_tracer_short_wave_absorption.F
  core_ocean/shared/mpas_ocn_tracer_short_wave_absorption_jerlov.F
  core_ocean/shared/mpas_ocn_tracer_short_wave_absorption_variable.F
  core_ocean/shared/mpas_ocn_tracer_surface_restoring.F
  core_ocean/shared/mpas_ocn_tracer_interior_restoring.F
  core_ocean/shared/mpas_ocn_tracer_exponential_decay.F
  core_ocean/shared/mpas_ocn_tracer_ideal_age.F
  core_ocean/shared/mpas_ocn_tracer_TTD.F
  core_ocean/shared/mpas_ocn_tracer_ecosys.F
  core_ocean/shared/mpas_ocn_tracer_DMS.F
  core_ocean/shared/mpas_ocn_tracer_MacroMolecules.F
  core_ocean/shared/mpas_ocn_high_freq_thickness_hmix_del2.F
  core_ocean/shared/mpas_ocn_tracer_surface_flux_to_tend.F
  core_ocean/shared/mpas_ocn_test.F
  core_ocean/shared/mpas_ocn_constants.F
  core_ocean/shared/mpas_ocn_forcing.F
  core_ocean/shared/mpas_ocn_surface_bulk_forcing.F
  core_ocean/shared/mpas_ocn_surface_land_ice_fluxes.F
  core_ocean/shared/mpas_ocn_effective_density_in_land_ice.F
  core_ocean/shared/mpas_ocn_frazil_forcing.F
  core_ocean/shared/mpas_ocn_tidal_forcing.F
  core_ocean/shared/mpas_ocn_time_average_coupled.F
  core_ocean/shared/mpas_ocn_sea_ice.F
  core_ocean/shared/mpas_ocn_framework_forcing.F
  core_ocean/shared/mpas_ocn_time_varying_forcing.F
  core_ocean/shared/mpas_ocn_wetting_drying.F
)

set(CORE_OCN_BLDDIR ${CMAKE_BINARY_DIR}/core_ocean)
if (NOT EXISTS ${CORE_OCN_BLDDIR})
  file(MAKE_DIRECTORY ${CORE_OCN_BLDDIR})
endif()

# Get CVMix
execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/core_ocean/get_cvmix.sh
  WORKING_DIRECTORY ${CORE_OCN_BLDDIR})

# Get BGC
execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/core_ocean/get_BGC.sh
  WORKING_DIRECTORY ${CORE_OCN_BLDDIR})

# Add CVMix
list(APPEND RAW_SOURCES
  core_ocean/cvmix/cvmix_kinds_and_types.F90
  core_ocean/cvmix/cvmix_background.F90
  core_ocean/cvmix/cvmix_convection.F90
  core_ocean/cvmix/cvmix_ddiff.F90
  core_ocean/cvmix/cvmix_kpp.F90
  core_ocean/cvmix/cvmix_math.F90
  core_ocean/cvmix/cvmix_put_get.F90
  core_ocean/cvmix/cvmix_shear.F90
  core_ocean/cvmix/cvmix_tidal.F90
  core_ocean/cvmix/cvmix_utils.F90
)

# Add BGC
list(APPEND RAW_SOURCES
  core_ocean/BGC/BGC_mod.F90
  core_ocean/BGC/BGC_parms.F90
  core_ocean/BGC/DMS_mod.F90
  core_ocean/BGC/DMS_parms.F90
  core_ocean/BGC/MACROS_mod.F90
  core_ocean/BGC/MACROS_parms.F90
  core_ocean/BGC/co2calc.F90
)

# Add analysis members
list(APPEND RAW_SOURCES
  core_ocean/analysis_members/mpas_ocn_global_stats.F
  core_ocean/analysis_members/mpas_ocn_okubo_weiss.F
  core_ocean/analysis_members/mpas_ocn_okubo_weiss_eigenvalues.c
  core_ocean/analysis_members/mpas_ocn_layer_volume_weighted_averages.F
  core_ocean/analysis_members/mpas_ocn_surface_area_weighted_averages.F
  core_ocean/analysis_members/mpas_ocn_water_mass_census.F
  core_ocean/analysis_members/mpas_ocn_meridional_heat_transport.F
  core_ocean/analysis_members/mpas_ocn_test_compute_interval.F
  core_ocean/analysis_members/mpas_ocn_high_frequency_output.F
  core_ocean/analysis_members/mpas_ocn_zonal_mean.F
  core_ocean/analysis_members/mpas_ocn_lagrangian_particle_tracking_interpolations.F
  core_ocean/analysis_members/mpas_ocn_particle_list.F
  core_ocean/analysis_members/mpas_ocn_lagrangian_particle_tracking_reset.F
  core_ocean/analysis_members/mpas_ocn_lagrangian_particle_tracking.F
  core_ocean/analysis_members/mpas_ocn_eliassen_palm.F
  core_ocean/analysis_members/mpas_ocn_time_filters.F
  core_ocean/analysis_members/mpas_ocn_mixed_layer_depths.F
  core_ocean/analysis_members/mpas_ocn_pointwise_stats.F
  core_ocean/analysis_members/mpas_ocn_debug_diagnostics.F
  core_ocean/analysis_members/mpas_ocn_time_series_stats.F
  core_ocean/analysis_members/mpas_ocn_regional_stats.F
  core_ocean/analysis_members/mpas_ocn_rpn_calculator.F
  core_ocean/analysis_members/mpas_ocn_transect_transport.F
  core_ocean/analysis_members/mpas_ocn_eddy_product_variables.F
  core_ocean/analysis_members/mpas_ocn_moc_streamfunction.F
  core_ocean/analysis_members/mpas_ocn_analysis_driver.F
)

# Make build tools
add_executable(streams_gen tools/input_gen/streams_gen.c tools/input_gen/test_functions.c external/ezxml/ezxml.c)
add_executable(namelist_gen tools/input_gen/namelist_gen.c tools/input_gen/test_functions.c external/ezxml/ezxml.c)
add_executable(parse tools/registry/parse.c tools/registry/dictionary.c tools/registry/gen_inc.c tools/registry/fortprintf.c tools/registry/utility.c external/ezxml/ezxml.c)

# Make .inc files
add_custom_command (
  OUTPUT ${CORE_OCN_BLDDIR}/Registry_processed.xml
  COMMAND cpp -P -traditional ${CPPDEFS_LIST} -Uvector
  ${CMAKE_CURRENT_SOURCE_DIR}/core_ocean/Registry.xml > Registry_processed.xml
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/core_ocean/Registry.xml
  WORKING_DIRECTORY ${CORE_OCN_BLDDIR}
)

set(INC_DIR ${CORE_OCN_BLDDIR}/inc)
if (NOT EXISTS ${INC_DIR})
  file(MAKE_DIRECTORY ${INC_DIR})
endif()

add_custom_command(
  OUTPUT ${INC_DIR}/core_variables.inc
  COMMAND ${CMAKE_BINARY_DIR}/parse < ${CORE_OCN_BLDDIR}/Registry_processed.xml
  DEPENDS parse ${CORE_OCN_BLDDIR}/Registry_processed.xml
  WORKING_DIRECTORY ${INC_DIR}
)

# Generate core input
set(CORE_INPUT_DIR ${CORE_OCN_BLDDIR}/default_inputs)
if (NOT EXISTS ${CORE_INPUT_DIR})
  file(MAKE_DIRECTORY ${CORE_INPUT_DIR})
endif()

set(NL_GEN_ARGS
  "namelist.ocean"
  "namelist.ocean.forward mode=forward"
  "namelist.ocean.analysis mode=analysis"
  "namelist.ocean.init mode=init"
  "streams.ocean stream_list.ocean. mutable"
  "streams.ocean.forward stream_list.ocean.forward. mutable mode=forward"
  "streams.ocean.analysis stream_list.ocean.analysis. mutable mode=analysis"
  "streams.ocean.init stream_list.ocean.init. mutable mode=init"
)

foreach(NL_GEN_ARG IN LISTS NL_GEN_ARGS)
  separate_arguments(ITEMS UNIX_COMMAND "${NL_GEN_ARG}")
  list(GET ITEMS 0 ITEM)
  list(APPEND INPUTS ${ITEM})
  add_custom_command(
    OUTPUT ${CORE_INPUT_DIR}/${ITEM}
    COMMAND ${CMAKE_BINARY_DIR}/namelist_gen ${CORE_OCN_BLDDIR}/Registry_processed.xml ${NL_GEN_ARG}
    DEPENDS namelist_gen ${CORE_OCN_BLDDIR}/Registry_processed.xml
    WORKING_DIRECTORY ${CORE_INPUT_DIR}
  )
endforeach()

foreach(INPUT IN LISTS INPUTS)
  add_custom_command(
    OUTPUT ${CORE_OCN_BLDDIR}/${INPUT}
    COMMAND ${CMAKE_COMMAND} -E copy ${CORE_INPUT_DIR}/${INPUT} ${CORE_OCN_BLDDIR}/${INPUT}
    DEPENDS ${CORE_INPUT_DIR}/${INPUT}
    WORKING_DIRECTORY ${CORE_OCN_BLDDIR}
  )
endforeach()

# Run all .F files through cpp to generate the f90 file
foreach(RAW_SOURCE_FILE IN LISTS RAW_SOURCES)
  get_filename_component(SOURCE_EXT ${RAW_SOURCE_FILE} EXT)
  if (SOURCE_EXT STREQUAL ".F")
    string(REPLACE ".F" ".f90" SOURCE_F90 ${RAW_SOURCE_FILE})
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

