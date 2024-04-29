
set(_mpas_framework_src
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

add_library(framework ${_mpas_framework_src})
if (${DO_PHYSICS})
    target_compile_definitions(framework PRIVATE DO_PHYSICS)
endif ()
if (MPAS_DOUBLE_PRECISION)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8")
else ()
    target_compile_definitions(framework PRIVATE SINGLE_PRECISION)
endif ()
if (${CMAKE_BUILD_TYPE} MATCHES "Debug")
    target_compile_definitions(framework PRIVATE MPAS_DEBUG)
endif ()
if (${PIO_FOUND})
    FILE(STRINGS ${PIO_PREFIX}/lib/libpio.settings PIO_SETTINGS)
    foreach (setting ${PIO_SETTINGS})
        string(FIND ${setting} "PIO Version" found)
        if (${found} GREATER -1)
            string(FIND ${setting} "2." pos)
            if (${pos} GREATER -1)
                set(PIO_VERSION 2)
            else ()
                set(PIO_VERSION 1)
            endif ()
            break()
        endif ()
    endforeach ()
    if (${PIO_VERSION} EQUAL 1)
        target_compile_definitions(framework PRIVATE USE_PIO1)
    else ()
        target_compile_definitions(framework PRIVATE USE_PIO2)
    endif ()
    target_compile_definitions(framework PRIVATE MPAS_PIO_SUPPORT)
endif ()
target_compile_definitions(framework PRIVATE mpas=1)
target_compile_definitions(framework PRIVATE MPAS_NATIVE_TIMERS)

mpas_fortran_target(framework)
add_library(${PROJECT_NAME}::framework ALIAS framework)

set_target_properties(framework PROPERTIES OUTPUT_NAME mpas_framework)

target_link_libraries(framework PUBLIC ${PROJECT_NAME}::external::esmf)
target_link_libraries(framework PUBLIC ${PROJECT_NAME}::external::ezxml)
target_link_libraries(framework PUBLIC PIO::PIO_Fortran PIO::PIO_C)
target_link_libraries(framework PUBLIC PnetCDF::PnetCDF_Fortran)
target_link_libraries(framework PUBLIC NetCDF::NetCDF_Fortran NetCDF::NetCDF_C)
target_link_libraries(framework PUBLIC MPI::MPI_Fortran)

if (MPAS_PROFILE)
    target_link_libraries(framework PUBLIC GPTL::GPTL)
endif ()

install(TARGETS framework EXPORT ${PROJECT_NAME}Exports
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
