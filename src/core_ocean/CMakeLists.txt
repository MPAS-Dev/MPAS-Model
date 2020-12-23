# core_ocean
set(OCEAN_SOURCES
  mode_forward/mpas_ocn_forward_mode.F
  mode_forward/mpas_ocn_time_integration.F
  mode_forward/mpas_ocn_time_integration_rk4.F
  mode_forward/mpas_ocn_time_integration_split.F

  mode_analysis/mpas_ocn_analysis_mode.F

  mode_init/mpas_ocn_init_mode.F
  mode_init/mpas_ocn_init_spherical_utils.F
  mode_init/mpas_ocn_init_vertical_grids.F
  mode_init/mpas_ocn_init_cell_markers.F
  mode_init/mpas_ocn_init_interpolation.F
  mode_init/mpas_ocn_init_ssh_and_landIcePressure.F
  mode_init/mpas_ocn_init_baroclinic_channel.F
  mode_init/mpas_ocn_init_lock_exchange.F
  mode_init/mpas_ocn_init_dam_break.F
  mode_init/mpas_ocn_init_internal_waves.F
  mode_init/mpas_ocn_init_overflow.F
  mode_init/mpas_ocn_init_cvmix_WSwSBF.F
  mode_init/mpas_ocn_init_iso.F
  mode_init/mpas_ocn_init_soma.F
  mode_init/mpas_ocn_init_ziso.F
  mode_init/mpas_ocn_init_sub_ice_shelf_2D.F
  mode_init/mpas_ocn_init_periodic_planar.F
  mode_init/mpas_ocn_init_ecosys_column.F
  mode_init/mpas_ocn_init_sea_mount.F
  mode_init/mpas_ocn_init_global_ocean.F
  mode_init/mpas_ocn_init_isomip.F
  mode_init/mpas_ocn_init_hurricane.F
  mode_init/mpas_ocn_init_isomip_plus.F
  mode_init/mpas_ocn_init_tidal_boundary.F
  mode_init/mpas_ocn_init_smoothing.F

  shared/mpas_ocn_init_routines.F
  shared/mpas_ocn_gm.F
  shared/mpas_ocn_diagnostics.F
  shared/mpas_ocn_diagnostics_variables.F
  shared/mpas_ocn_mesh.F
  shared/mpas_ocn_thick_ale.F
  shared/mpas_ocn_equation_of_state.F
  shared/mpas_ocn_equation_of_state_jm.F
  shared/mpas_ocn_equation_of_state_linear.F
  shared/mpas_ocn_thick_hadv.F
  shared/mpas_ocn_thick_vadv.F
  shared/mpas_ocn_thick_surface_flux.F
  shared/mpas_ocn_vel_hadv_coriolis.F
  shared/mpas_ocn_vel_vadv.F
  shared/mpas_ocn_vel_hmix.F
  shared/mpas_ocn_vel_hmix_del2.F
  shared/mpas_ocn_vel_hmix_leith.F
  shared/mpas_ocn_vel_hmix_del4.F
  shared/mpas_ocn_vel_forcing.F
  shared/mpas_ocn_vel_forcing_surface_stress.F
  shared/mpas_ocn_vel_forcing_explicit_bottom_drag.F
  shared/mpas_ocn_vel_pressure_grad.F
  shared/mpas_ocn_vmix.F
  shared/mpas_ocn_vmix_coefs_redi.F
  shared/mpas_ocn_vmix_cvmix.F
  shared/mpas_ocn_tendency.F
  shared/mpas_ocn_tracer_hmix.F
  shared/mpas_ocn_tracer_hmix_del2.F
  shared/mpas_ocn_tracer_hmix_del4.F
  shared/mpas_ocn_tracer_hmix_redi.F
  shared/mpas_ocn_tracer_advection.F
  shared/mpas_ocn_tracer_advection_mono.F
  shared/mpas_ocn_tracer_advection_std.F
  shared/mpas_ocn_tracer_nonlocalflux.F
  shared/mpas_ocn_tracer_short_wave_absorption.F
  shared/mpas_ocn_tracer_short_wave_absorption_jerlov.F
  shared/mpas_ocn_tracer_short_wave_absorption_variable.F
  shared/mpas_ocn_tracer_surface_restoring.F
  shared/mpas_ocn_tracer_interior_restoring.F
  shared/mpas_ocn_tracer_exponential_decay.F
  shared/mpas_ocn_tracer_ideal_age.F
  shared/mpas_ocn_tracer_TTD.F
  shared/mpas_ocn_tracer_ecosys.F
  shared/mpas_ocn_tracer_DMS.F
  shared/mpas_ocn_tracer_MacroMolecules.F
  shared/mpas_ocn_high_freq_thickness_hmix_del2.F
  shared/mpas_ocn_tracer_surface_flux_to_tend.F
  shared/mpas_ocn_test.F
  shared/mpas_ocn_constants.F
  shared/mpas_ocn_config.F
  shared/mpas_ocn_forcing.F
  shared/mpas_ocn_surface_bulk_forcing.F
  shared/mpas_ocn_surface_land_ice_fluxes.F
  shared/mpas_ocn_effective_density_in_land_ice.F
  shared/mpas_ocn_frazil_forcing.F
  shared/mpas_ocn_tidal_forcing.F
  shared/mpas_ocn_time_average_coupled.F
  shared/mpas_ocn_sea_ice.F
  shared/mpas_ocn_framework_forcing.F
  shared/mpas_ocn_time_varying_forcing.F
  shared/mpas_ocn_wetting_drying.F
  shared/mpas_ocn_vel_tidal_potential.F
)

set(OCEAN_DRIVER
  driver/mpas_ocn_core.F
  driver/mpas_ocn_core_interface.F
)
#list(APPEND RAW_SOURCES ${OCEAN_DRIVER})
#list(APPEND DISABLE_QSMP ${OCEAN_DRIVER})

# Add CVMix
if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/cvmix/.git)
  message(FATAL "Missing core_ocean/cvmix/.git, did you forget to 'git submodule update --init --recursive' ?")
endif()
set(CVMIX_FILES
  cvmix/src/shared/cvmix_kinds_and_types.F90
  cvmix/src/shared/cvmix_background.F90
  cvmix/src/shared/cvmix_convection.F90
  cvmix/src/shared/cvmix_ddiff.F90
  cvmix/src/shared/cvmix_kpp.F90
  cvmix/src/shared/cvmix_math.F90
  cvmix/src/shared/cvmix_put_get.F90
  cvmix/src/shared/cvmix_shear.F90
  cvmix/src/shared/cvmix_tidal.F90
  cvmix/src/shared/cvmix_utils.F90
)

# Add BGC
if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/BGC/.git)
  message(FATAL "Missing core_ocean/BGC/.git, did you forget to 'git submodule update --init --recursive' ?")
endif()
set(BGC_FILES
  BGC/BGC_mod.F90
  BGC/BGC_parms.F90
  BGC/DMS_mod.F90
  BGC/DMS_parms.F90
  BGC/MACROS_mod.F90
  BGC/MACROS_parms.F90
  BGC/co2calc.F90
)

#list(APPEND RAW_SOURCES ${CVMIX_FILES} ${BGC_FILES})
#list(APPEND NO_PREPROCESS ${CVMIX_FILES} ${BGC_FILES})

# Add analysis members
set(OCEAN_ANALYSIS
  analysis_members/mpas_ocn_global_stats.F
  analysis_members/mpas_ocn_okubo_weiss.F
  analysis_members/mpas_ocn_okubo_weiss_eigenvalues.c
  analysis_members/mpas_ocn_layer_volume_weighted_averages.F
  analysis_members/mpas_ocn_surface_area_weighted_averages.F
  analysis_members/mpas_ocn_water_mass_census.F
  analysis_members/mpas_ocn_meridional_heat_transport.F
  analysis_members/mpas_ocn_test_compute_interval.F
  analysis_members/mpas_ocn_high_frequency_output.F
  analysis_members/mpas_ocn_zonal_mean.F
  analysis_members/mpas_ocn_lagrangian_particle_tracking_interpolations.F
  analysis_members/mpas_ocn_particle_list.F
  analysis_members/mpas_ocn_lagrangian_particle_tracking_reset.F
  analysis_members/mpas_ocn_lagrangian_particle_tracking.F
  analysis_members/mpas_ocn_eliassen_palm.F
  analysis_members/mpas_ocn_time_filters.F
  analysis_members/mpas_ocn_mixed_layer_depths.F
  analysis_members/mpas_ocn_pointwise_stats.F
  analysis_members/mpas_ocn_debug_diagnostics.F
  analysis_members/mpas_ocn_time_series_stats.F
  analysis_members/mpas_ocn_regional_stats.F
  analysis_members/mpas_ocn_rpn_calculator.F
  analysis_members/mpas_ocn_transect_transport.F
  analysis_members/mpas_ocn_eddy_product_variables.F
  analysis_members/mpas_ocn_moc_streamfunction.F
  analysis_members/mpas_ocn_ocean_heat_content.F
  analysis_members/mpas_ocn_mixed_layer_heat_budget.F
  analysis_members/mpas_ocn_sediment_flux_index.F
  analysis_members/mpas_ocn_sediment_transport.F
  analysis_members/mpas_ocn_harmonic_analysis.F
  analysis_members/mpas_ocn_analysis_driver.F
)

# Generate core input
#handle_st_nl_gen(
#  "namelist.ocean;namelist.ocean.forward mode=forward;namelist.ocean.analysis mode=analysis;namelist.ocean.init mode=init"
#  "streams.ocean stream_list.ocean. mutable;streams.ocean.forward stream_list.ocean.forward. mutable mode=forward;streams.ocean.analysis stream_list.ocean.analysis. mutable mode=analysis;streams.ocean.init stream_list.ocean.init. mutable mode=init"
#  ${CORE_INPUT_DIR} ${CORE_BLDDIR}
#)

set(CORE_OCEAN_SOURCES ${OCEAN_SOURCES} ${OCEAN_DRIVER} ${OCEAN_ANALYSIS} ${CVMIX_FILES} ${BGC_FILES})
add_library(dycore ${CORE_OCEAN_SOURCES})
target_compile_definitions(dycore PRIVATE ${Defines})
target_include_directories(dycore PRIVATE ${CORE_OCEAN_INCLUDE_DIRS}/inc ${FRAMEWORK_INCLUDE_DIRS} ${OPERATORS_INCLUDE_DIRS} ${PIO_INCLUDE_DIRS} ${ESMF_INCLUDE_DIRS} ${EZXML_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH})
add_dependencies(dycore framework ops esmf_time gen_include)

set(NL_GEN ${CMAKE_BINARY_DIR}/src/tools/input_gen/namelist_gen)
set(ST_GEN ${CMAKE_BINARY_DIR}/src/tools/input_gen/streams_gen)

add_custom_command(OUTPUT namelist.ocean
    COMMAND ${NL_GEN} ../Registry_processed.xml namelist.ocean
    COMMAND cp namelist.ocean ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${NL_GEN})

add_custom_command(OUTPUT namelist.ocean.forward
    COMMAND ${NL_GEN} ../Registry_processed.xml namelist.ocean.forward mode=forward
    COMMAND cp namelist.ocean.forward ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${NL_GEN})

add_custom_command(OUTPUT namelist.ocean.analysis
    COMMAND ${NL_GEN} ../Registry_processed.xml namelist.ocean.analysis mode=analysis
    COMMAND cp namelist.ocean.analysis ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${NL_GEN})

add_custom_command(OUTPUT namelist.ocean.init
    COMMAND ${NL_GEN} ../Registry_processed.xml namelist.ocean.init mode=init
    COMMAND cp namelist.ocean.init ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${NL_GEN})

add_custom_target(gen_namelists DEPENDS namelist.ocean namelist.ocean.forward
    namelist.ocean.analysis namelist.ocean.init)

add_custom_command(OUTPUT streams.ocean
    COMMAND ${ST_GEN} ../Registry_processed.xml streams.ocean stream_list.ocean. mutable
    COMMAND cp streams.ocean ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${ST_GEN})

add_custom_command(OUTPUT streams.ocean.forward
    COMMAND ${ST_GEN} ../Registry_processed.xml streams.ocean.forward stream_list.ocean.forward. mutable mode=forward
    COMMAND cp streams.ocean.forward ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${ST_GEN})

add_custom_command(OUTPUT streams.ocean.analysis
    COMMAND ${ST_GEN} ../Registry_processed.xml streams.ocean.analysis stream_list.ocean.analysis. mutable mode=analysis
    COMMAND cp streams.ocean.analysis ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${ST_GEN})

add_custom_command(OUTPUT streams.ocean.init
    COMMAND ${ST_GEN} ../Registry_processed.xml streams.ocean.init stream_list.ocean.init. mutable mode=init
    COMMAND cp streams.ocean.init ${CMAKE_BINARY_DIR}/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/src/core_ocean/default_inputs
    DEPENDS mkdir_default_inputs Registry_processed.xml ${ST_GEN})

add_custom_target(gen_streams DEPENDS streams.ocean streams.ocean.forward
    streams.ocean.analysis streams.ocean.init)

add_custom_command(OUTPUT default_inputs
    COMMAND mkdir ARGS default_inputs src/core_ocean/default_inputs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR})

add_custom_target(mkdir_default_inputs DEPENDS default_inputs)

add_custom_command(OUTPUT Registry_processed.xml
    COMMAND cpp ARGS -P ARGS -traditional ARGS ${CMAKE_CURRENT_SOURCE_DIR}/Registry.xml > Registry_processed.xml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_custom_target(gen_registry DEPENDS Registry_processed.xml)

add_custom_command(OUTPUT inc
    COMMAND mkdir ARGS inc
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_custom_target(mkdir_inc DEPENDS inc)

add_custom_command(OUTPUT config_declare.inc
    COMMAND ${CMAKE_BINARY_DIR}/src/tools/registry/parse < ${CMAKE_CURRENT_BINARY_DIR}/Registry_processed.xml
    WORKING_DIRECTORY inc
    DEPENDS gen_registry mkdir_inc parse)

add_custom_target(gen_include DEPENDS config_declare.inc)
