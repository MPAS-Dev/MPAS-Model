list(APPEND _mpas_operators_src
    mpas_geometry_utils.F
    mpas_matrix_operations.F
    mpas_rbf_interpolation.F
    mpas_spline_interpolation.F
    mpas_tensor_operations.F
    mpas_tracer_advection_helpers.F
    mpas_tracer_advection_mono.F
    mpas_tracer_advection_std.F
    mpas_vector_operations.F
    mpas_vector_reconstruction.F)

add_library(operators ${_mpas_operators_src})

mpas_fortran_target(operators)

add_library(${PROJECT_NAME}::operators ALIAS operators)

set_target_properties(operators PROPERTIES OUTPUT_NAME mpas_operators)
target_link_libraries(operators PUBLIC ${PROJECT_NAME}::framework)

install(TARGETS operators EXPORT ${PROJECT_NAME}Exports
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
