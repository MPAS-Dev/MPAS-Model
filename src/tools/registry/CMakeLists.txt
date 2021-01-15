#set(CMAKE_C_FLAGS "${CFLAGS} ${TOOL_TARGET_ARCH}")
set(C_SRCS parse.c dictionary.c gen_inc.c fortprintf.c utility.c)

add_executable(parse ${C_SRCS})
set_target_properties(parse PROPERTIES COMPILE_DEFINITIONS "${Defines}")
target_compile_options(parse PRIVATE "-Uvector")
target_include_directories(parse PRIVATE ${EZXML_INCLUDE_DIRS})
target_link_libraries(parse PRIVATE ${EZXML_LIBRARIES})
add_dependencies(parse ezxml)
