add_library(smiol)

target_compile_definitions(smiol PUBLIC SMIOL_PNETCDF)

target_sources(smiol
               PRIVATE
                  smiolf.F90
                  smiol.c
                  smiol_utils.c)

set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR})
set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/src/external/SMIOL)
set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_LIBRARIES smiol)
