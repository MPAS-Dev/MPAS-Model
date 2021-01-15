#set(CMAKE_C_FLAGS "${CFLAGS} ${TOOL_TARGET_ARCH}")
set(NL_SRCS namelist_gen.c test_functions.c)

add_executable(namelist_gen ${NL_SRCS})
set_target_properties(namelist_gen PROPERTIES COMPILE_DEFINITIONS "${Defines}")
target_compile_options(namelist_gen PRIVATE "-Uvector")
target_include_directories(namelist_gen PRIVATE ${EZXML_INCLUDE_DIRS})
target_link_libraries(namelist_gen PRIVATE ezxml)
add_dependencies(namelist_gen ezxml)

set(ST_SRCS streams_gen.c test_functions.c)

add_executable(streams_gen ${ST_SRCS})
set_target_properties(streams_gen PROPERTIES COMPILE_DEFINITIONS "${Defines}")
target_compile_options(streams_gen PRIVATE "-Uvector")
target_include_directories(streams_gen PRIVATE ${EZXML_INCLUDE_DIRS})
target_link_libraries(streams_gen PRIVATE ezxml)
add_dependencies(streams_gen ezxml)
