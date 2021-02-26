# framework
set(FRAMEWORK_SOURCES
  mpas_kind_types.F
  mpas_framework.F
  mpas_timer.F
  mpas_timekeeping.F
  mpas_constants.F
  mpas_attlist.F
  mpas_hash.F
  mpas_sort.F
  mpas_block_decomp.F
  mpas_block_creator.F
  mpas_dmpar.F
  mpas_abort.F
  mpas_decomp.F
  mpas_threading.F
  mpas_io.F
  mpas_io_streams.F
  mpas_bootstrapping.F
  mpas_io_units.F
  mpas_stream_manager.F
  mpas_stream_list.F
  mpas_forcing.F
  mpas_c_interfacing.F
  random_id.c
  pool_hash.c
  mpas_derived_types.F
  mpas_domain_routines.F
  mpas_field_routines.F
  mpas_pool_routines.F
  xml_stream_parser.c
  regex_matching.c
  mpas_field_accessor.F
  mpas_log.F
)

add_library(framework ${FRAMEWORK_SOURCES})
set_target_properties(framework PROPERTIES COMPILE_DEFINITIONS "${Defines}")
target_include_directories(framework PRIVATE ${PIO_INCLUDE_DIRS} ${ESMF_INCLUDE_DIRS} ${EZXML_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH})
if (DEFINED ENV{PIO})
   add_dependencies(framework esmf_time)
else ()
   add_dependencies(framework piof esmf_time)
endif ()
