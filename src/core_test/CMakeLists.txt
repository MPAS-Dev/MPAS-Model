include(${CMAKE_SOURCE_DIR}/cmake/Functions/setup_mpas_test_core.cmake)

set(test_core_inc
        block_dimension_routines.inc
        core_variables.inc
        define_packages.inc
        domain_variables.inc
        namelist_call.inc
        namelist_defines.inc
        setup_immutable_streams.inc
        structs_and_variables.inc)

## core_init_atosphere
set(test_core_srcs
        mpas_test_core.F
        mpas_test_core_field_tests.F
        mpas_test_core_halo_exch.F
        mpas_halo_testing.F
        mpas_test_core_interface.F
        mpas_test_core_sorting.F
        mpas_test_openacc.F
        mpas_test_core_string_utils.F
        mpas_test_core_dmpar.F
)

add_library(core_test ${test_core_srcs})
mpas_core_target(CORE test TARGET core_test INCLUDES ${test_core_inc})
add_subdirectory(fortest)