
#Parsing library core-independent code
add_library(parselib dictionary.c fortprintf.c utility.c)
target_link_libraries(parselib PUBLIC ${PROJECT_NAME}::external::ezxml)
target_link_libraries(parselib PUBLIC ${PROJECT_NAME}::external::esmf)

# Generate parser for each core
#
# Note: One parser is required per-core because the gen_inc.c depends on
# a pre-processor define MPAS_NAMELIST_SUFFIX which is core specific
foreach(_core IN LISTS MPAS_CORES)
    add_executable(mpas_parse_${_core} parse.c gen_inc.c)
    target_link_libraries(mpas_parse_${_core} PUBLIC parselib)
    target_compile_definitions(mpas_parse_${_core} PRIVATE MPAS_NAMELIST_SUFFIX=${_core}
                                                           MPAS_GIT_VERSION=${MPAS_GIT_VERSION}
                                                           MPAS_EXE_NAME=${_core}_model)
endforeach()
