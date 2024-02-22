add_library(ezxml)

target_sources(ezxml
               PRIVATE
                  ezxml.c)

set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR})
set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_LIBRARIES ezxml)
