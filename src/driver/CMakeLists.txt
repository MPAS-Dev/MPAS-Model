# dycore
set(DYCORE_SOURCES
  mpas.F
  mpas_subdriver.F
)

add_library(dycore ${DYCORE_SOURCES})
set_target_properties(dycore PROPERTIES COMPILE_DEFINITIONS "${Defines}")
target_include_directories(dycore PRIVATE ${FRAMEWORK_INCLUDE_DIRS} ${PIO_INCLUDE_DIRS})
add_dependencies(dycore framework piof)
