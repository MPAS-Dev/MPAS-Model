# MPAS/src/core_sw
#
# Targets
#   MPAS::core::sw

## Generated includes
set(sw_core_inc
        block_dimension_routines.inc
        core_variables.inc
        define_packages.inc
        domain_variables.inc
        namelist_call.inc
        namelist_defines.inc
        setup_immutable_streams.inc
        structs_and_variables.inc)

## core_init_atosphere
set(sw_core_srcs
    mpas_sw_advection.F
    mpas_sw_constants.F
    mpas_sw_core.F
    mpas_sw_core_interface.F
    mpas_sw_global_diagnostics.F
    mpas_sw_test_cases.F
    mpas_sw_time_integration.F
    )

add_library(core_sw ${sw_core_srcs})
mpas_core_target(CORE sw TARGET core_sw INCLUDES ${sw_core_inc})
