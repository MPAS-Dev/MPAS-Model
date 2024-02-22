add_custom_command(OUTPUT
                      Registry_processed.xml
                   COMMAND
                      cpp -P -traditional ${CMAKE_SOURCE_DIR}/src/core_init_atmosphere/Registry.xml > Registry_processed.xml
                   DEPENDS
                      Registry.xml)

add_custom_command(OUTPUT
                      block_dimension_routines.inc
                      core_variables.inc
                      define_packages.inc
                      domain_variables.inc
                      namelist_call.inc
                      namelist_defines.inc
                      setup_immutable_streams.inc
                      structs_and_variables.inc
                   COMMAND
                      ${CMAKE_BINARY_DIR}/src/tools/registry/parse Registry_processed.xml
                   DEPENDS
                      parse Registry_processed.xml)

add_custom_target(init_atm_core
                  DEPENDS
                     block_dimension_routines.inc
                     core_variables.inc
                     define_packages.inc
                     domain_variables.inc
                     namelist_call.inc
                     namelist_defines.inc
                     setup_immutable_streams.inc
                     structs_and_variables.inc)

add_custom_command(OUTPUT
                      namelist.init_atmosphere
                   COMMAND
                      ${CMAKE_BINARY_DIR}/src/tools/input_gen/namelist_gen Registry_processed.xml ${CMAKE_BINARY_DIR}/namelist.init_atmosphere in_defaults=true
                   DEPENDS
                      namelist_gen Registry_processed.xml)

add_custom_target(init_atm_namelist
                  DEPENDS
                     namelist.init_atmosphere)

add_custom_command(OUTPUT
                      streams.init_atmosphere
                   COMMAND
                      ${CMAKE_BINARY_DIR}/src/tools/input_gen/streams_gen Registry_processed.xml ${CMAKE_BINARY_DIR}/streams.init_atmosphere ${CMAKE_BINARY_DIR}/stream_list.init_atmosphere. listed
                   DEPENDS
                      streams_gen Registry_processed.xml)

add_custom_target(init_atm_streams
                  DEPENDS
                     streams.init_atmosphere)

add_dependencies(${MPAS_EXE_NAME} init_atm_core)
add_dependencies(${MPAS_EXE_NAME} init_atm_namelist)
add_dependencies(${MPAS_EXE_NAME} init_atm_streams)

set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR})

target_sources(${MPAS_EXE_NAME}
               PUBLIC
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
