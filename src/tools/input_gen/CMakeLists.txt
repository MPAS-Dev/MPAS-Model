
add_executable(mpas_namelist_gen namelist_gen.c test_functions.c)
target_link_libraries(mpas_namelist_gen PUBLIC ${PROJECT_NAME}::external::ezxml)

add_executable(mpas_streams_gen streams_gen.c test_functions.c)
target_link_libraries(mpas_streams_gen PUBLIC ${PROJECT_NAME}::external::ezxml)
