add_executable(namelist_gen)
add_executable(streams_gen)

target_sources(namelist_gen
               PUBLIC
                  namelist_gen.c
                  test_functions.c
              )

target_sources(streams_gen
               PUBLIC
                  streams_gen.c
                  test_functions.c
              )


#
# Add include paths and libraries from packages that are internally built by MPAS
# In particular, the ezxml library is needed by the namelist_gen and streams_gen tools
#
get_property(MPAS_INTERNAL_INCLUDE_DIRS GLOBAL PROPERTY MPAS_INTERNAL_INCLUDE_DIRS)
get_property(MPAS_INTERNAL_LIBRARIES GLOBAL PROPERTY MPAS_INTERNAL_LIBRARIES)
target_include_directories(namelist_gen PRIVATE ${MPAS_INTERNAL_INCLUDE_DIRS})
target_include_directories(streams_gen PRIVATE ${MPAS_INTERNAL_INCLUDE_DIRS})
target_link_libraries(namelist_gen PRIVATE ${MPAS_INTERNAL_LIBRARIES})
target_link_libraries(streams_gen PRIVATE ${MPAS_INTERNAL_LIBRARIES})
