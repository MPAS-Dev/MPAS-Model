
add_library(stream_manager_fixture SHARED stream_manager.fixture.f90)
set_target_properties(stream_manager_fixture PROPERTIES
        LINKER_LANGUAGE Fortran)
target_link_libraries(stream_manager_fixture PRIVATE session_fixture core_test Fortest::fortest core_test)
target_include_directories( stream_manager_fixture INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<INSTALL_INTERFACE:include/fortest/fortest>
)
add_executable(test_stream_manager stream_manager.test.f90)
set_target_properties(test_stream_manager PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(test_stream_manager PRIVATE session_fixture stream_manager_fixture core_test Fortest::fortest )



