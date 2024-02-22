find_package(Git)
if(GIT_FOUND)
    execute_process(COMMAND ${GIT_EXECUTABLE} describe --dirty
                    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
                    OUTPUT_VARIABLE MPAS_GIT_VERSION
                    ERROR_QUIET
                    OUTPUT_STRIP_TRAILING_WHITESPACE
                   )
else()
    set(MPAS_GIT_VERSION "Unknown")
endif()

add_executable(parse)

target_compile_definitions(parse PUBLIC MPAS_NAMELIST_SUFFIX=${MPAS_CORE})
target_compile_definitions(parse PUBLIC MPAS_GIT_VERSION=${MPAS_GIT_VERSION})
target_compile_definitions(parse PUBLIC MPAS_EXE_NAME=${MPAS_EXE_NAME})
target_compile_definitions(parse PUBLIC MPAS_BUILD_TARGET=${MPAS_COMPILER})

target_sources(parse
               PUBLIC
                  dictionary.c
                  fortprintf.c
                  gen_inc.c
                  parse.c
                  utility.c)

#
# Add include paths and libraries from packages that are internally built by MPAS
# In particular, the ezxml library is needed by the parse tool
#
get_property(MPAS_INTERNAL_INCLUDE_DIRS GLOBAL PROPERTY MPAS_INTERNAL_INCLUDE_DIRS)
get_property(MPAS_INTERNAL_LIBRARIES GLOBAL PROPERTY MPAS_INTERNAL_LIBRARIES)
target_include_directories(parse PRIVATE ${MPAS_INTERNAL_INCLUDE_DIRS})
target_link_libraries(parse PRIVATE ${MPAS_INTERNAL_LIBRARIES})
