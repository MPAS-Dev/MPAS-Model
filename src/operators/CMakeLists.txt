# operators
set(OPERATORS_SOURCES
  mpas_vector_operations.F
  mpas_matrix_operations.F
  mpas_tensor_operations.F
  mpas_rbf_interpolation.F
  mpas_vector_reconstruction.F
  mpas_spline_interpolation.F
  mpas_tracer_advection_helpers.F
  mpas_tracer_advection_mono.F
  mpas_tracer_advection_std.F
  mpas_geometry_utils.F
)

add_library(ops ${OPERATORS_SOURCES})
set_target_properties(ops PROPERTIES COMPILE_DEFINITIONS "${Defines}")
target_include_directories(ops PRIVATE ${FRAMEWORK_INCLUDE_DIRS} ${PIO_INCLUDE_DIRS} ${ESMF_INCLUDE_DIRS})
add_dependencies(ops framework esmf_time)
