
add_library(pool_fixture SHARED pool.fixture.f90)
set_target_properties(pool_fixture PROPERTIES
        LINKER_LANGUAGE Fortran)
target_link_libraries(pool_fixture PRIVATE session_fixture core_test Fortest::fortest core_test)
target_include_directories( pool_fixture INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<INSTALL_INTERFACE:include/fortest/fortest>
)
add_executable(test_pool pool.test.f90)
set_target_properties(test_pool PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(test_pool PRIVATE session_fixture pool_fixture core_test Fortest::fortest )

