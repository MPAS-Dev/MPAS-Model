add_library(mpas_test_utils STATIC mpas_test_utils_mod.f90)
set_target_properties(mpas_test_utils PROPERTIES LINKER_LANGUAGE Fortran)
target_include_directories(mpas_test_utils INTERFACE
        $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod>
        $<INSTALL_INTERFACE:include>
)
target_link_libraries(mpas_test_utils PUBLIC PIO::PIO_Fortran )

add_pfunit_ctest (test_mpas_pio_put_att
        TEST_SOURCES test_mpas_pio_put_att_mod.pf
        LINK_LIBRARIES framework mpas_test_utils PIO::PIO_Fortran
)
