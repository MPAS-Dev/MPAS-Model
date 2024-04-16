# MPAS/src/core_init_atmosphere
#
# Targets
#   MPAS::core::init_atmosphere

## Generated includes
set(init_atm_core_inc
        block_dimension_routines.inc
        core_variables.inc
        define_packages.inc
        domain_variables.inc
        namelist_call.inc
        namelist_defines.inc
        setup_immutable_streams.inc
        structs_and_variables.inc)

## core_init_atosphere
set(init_atm_core_srcs
        mpas_atm_advection.F
        mpas_atmphys_constants.F
        mpas_atmphys_date_time.F
        mpas_atmphys_functions.F
        mpas_atmphys_initialize_real.F
        mpas_atmphys_utilities.F
        mpas_geotile_manager.F
        mpas_init_atm_bitarray.F
        mpas_init_atm_cases.F
        mpas_init_atm_core.F
        mpas_init_atm_core_interface.F
        mpas_init_atm_gwd.F
        mpas_init_atm_hinterp.F
        mpas_init_atm_llxy.F
        mpas_init_atm_queue.F
        mpas_init_atm_read_met.F
        mpas_init_atm_static.F
        mpas_init_atm_surface.F
        mpas_init_atm_vinterp.F
        mpas_kd_tree.F
        mpas_parse_geoindex.F
        mpas_stack.F
        read_geogrid.c)

add_library(core_init_atmosphere ${init_atm_core_srcs})
mpas_core_target(CORE init_atmosphere TARGET core_init_atmosphere INCLUDES ${init_atm_core_inc})
