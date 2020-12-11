set(C_SRCS ezxml.c)

add_library(ezxml ${C_SRCS})
set_target_properties(ezxml PROPERTIES COMPILE_DEFINITIONS "${Defines}")
