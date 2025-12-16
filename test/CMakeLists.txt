add_library(mpas_test_utils STATIC mpas_test_utils_mod.f90)
set_target_properties(mpas_test_utils PROPERTIES LINKER_LANGUAGE Fortran)
target_include_directories(mpas_test_utils INTERFACE
        $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>
        $<INSTALL_INTERFACE:include>
)
target_link_libraries(mpas_test_utils PUBLIC PIO::PIO_Fortran )

add_pfunit_ctest (test_seaice_driver_mod
        TEST_SOURCES test_seaice_driver_mod.pf
        LINK_LIBRARIES framework core_atmosphere mpas_test_utils
)
