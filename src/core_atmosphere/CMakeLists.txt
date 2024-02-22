target_compile_definitions(${MPAS_EXE_NAME} PUBLIC DO_PHYSICS)
target_compile_definitions(${MPAS_EXE_NAME} PUBLIC mpas=MPAS)

add_subdirectory(physics)
add_subdirectory(diagnostics)
add_subdirectory(dynamics)

add_custom_command(OUTPUT
                      Registry_processed.xml
                   COMMAND
                      cpp -P -traditional ${CMAKE_SOURCE_DIR}/src/core_atmosphere/Registry.xml > Registry_processed.xml
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

add_custom_target(atm_core
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
                      namelist.atmosphere
                   COMMAND
                      ${CMAKE_BINARY_DIR}/src/tools/input_gen/namelist_gen Registry_processed.xml ${CMAKE_BINARY_DIR}/namelist.atmosphere in_defaults=true
                   DEPENDS
                      namelist_gen Registry_processed.xml)

add_custom_target(atm_namelist
                  DEPENDS
                     namelist.atmosphere)

add_custom_command(OUTPUT
                      streams.atmosphere
                   COMMAND
                      ${CMAKE_BINARY_DIR}/src/tools/input_gen/streams_gen Registry_processed.xml ${CMAKE_BINARY_DIR}/streams.atmosphere ${CMAKE_BINARY_DIR}/stream_list.atmosphere. listed
                   DEPENDS
                      streams_gen Registry_processed.xml)

add_custom_target(atm_streams
                  DEPENDS
                     streams.atmosphere)

add_dependencies(${MPAS_EXE_NAME} atm_core)
add_dependencies(${MPAS_EXE_NAME} atm_namelist)
add_dependencies(${MPAS_EXE_NAME} atm_streams)

set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR})

target_sources(${MPAS_EXE_NAME}
               PUBLIC
                  mpas_atm_core.F
                  mpas_atm_core_interface.F
                  mpas_atm_dimensions.F
                  mpas_atm_halos.F
                  mpas_atm_threading.F)
