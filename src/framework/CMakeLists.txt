
set(MPAS_FRAMEWORK_SOURCES
        mpas_block_creator.F
        mpas_block_decomp.F
        mpas_bootstrapping.F
        mpas_c_interfacing.F
        mpas_constants.F
        mpas_decomp.F
        mpas_domain_routines.F
        mpas_field_routines.F
        mpas_forcing.F
        mpas_hash.F
        mpas_io_units.F
        mpas_kind_types.F
        mpas_pool_routines.F
        mpas_sort.F
        mpas_stream_list.F
        mpas_threading.F
        mpas_timer.F
        mpas_abort.F
        mpas_attlist.F
        mpas_derived_types.F
        mpas_dmpar.F
        mpas_framework.F
        mpas_halo.F
        mpas_io.F
        mpas_io_streams.F
        mpas_log.F
        mpas_stream_inquiry.F
        mpas_stream_manager.F
        mpas_string_utils.F
        mpas_timekeeping.F
        pool_hash.c
        random_id.c
        regex_matching.c
        xml_stream_parser.c
        stream_inquiry.c)

add_library(framework ${MPAS_FRAMEWORK_SOURCES})
set_MPAS_DEBUG_flag(framework)
set(FRAMEWORK_COMPILE_DEFINITIONS
    mpas=1
    MPAS_NATIVE_TIMERS)
if (MPAS_USE_PIO)
    list(APPEND FRAMEWORK_COMPILE_DEFINITIONS USE_PIO2 MPAS_PIO_SUPPORT)
    set(IO_LIBS
        PIO::PIO_Fortran
        PIO::PIO_C)
else()
    list(APPEND FRAMEWORK_COMPILE_DEFINITIONS MPAS_SMIOL_SUPPORT)
    set(IO_LIBS
        ${PROJECT_NAME}::external::smiolf)
endif()
target_compile_definitions(framework PRIVATE ${FRAMEWORK_COMPILE_DEFINITIONS})

mpas_fortran_target(framework)
add_library(${PROJECT_NAME}::framework ALIAS framework)

set_target_properties(framework PROPERTIES OUTPUT_NAME mpas_framework)

set(FRAMEWORK_LINK_LIBRARIES
        ${PROJECT_NAME}::external::esmf
        ${PROJECT_NAME}::external::ezxml
        ${IO_LIBS}
        PnetCDF::PnetCDF_Fortran
        MPI::MPI_Fortran)

if (MPAS_PROFILE)
    list(APPEND FRAMEWORK_LINK_LIBRARIES GPTL::GPTL)
endif ()
target_link_libraries(framework PUBLIC ${FRAMEWORK_LINK_LIBRARIES})

install(TARGETS framework EXPORT ${PROJECT_NAME}Exports
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
