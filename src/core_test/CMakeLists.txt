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
        mpas_test_core_streams.F
        mpas_test_core_timekeeping_tests.F
        mpas_test_openacc.F
        mpas_test_core_string_utils.F
        mpas_test_core_stream_list.F
        mpas_test_core_stream_inquiry.F
        mpas_test_core_dmpar.F
)

add_library(core_test ${test_core_srcs})
mpas_core_target(CORE test TARGET core_test INCLUDES ${test_core_inc})
set_target_properties(mpas_test
        PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/test"
)
if (DEFINED MPAS_TEST_DATA_DIR AND EXISTS "${MPAS_TEST_DATA_DIR}")
    message(STATUS "MPAS_TEST_DATA_DIR is set and exists: ${MPAS_TEST_DATA_DIR}")
    setup_mpas_test_core(${CMAKE_BINARY_DIR}/test)
    add_test(NAME mpas_test
            COMMAND mpas_test
            WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/test")
else ()
    message(WARNING "MPAS_TEST_DATA_DIR is not set or does not exist; "
            "skipping test_mpas registration.")
endif ()

add_executable(test_xml_stream_parser test_xml_stream_parser.c)
set_target_properties(test_xml_stream_parser PROPERTIES
        LINKER_LANGUAGE C
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test/unity
)
target_link_libraries(test_xml_stream_parser PUBLIC
        unity::framework ezxml framework
)
add_test(NAME test_xml_stream_parser
        COMMAND test_xml_stream_parser)
