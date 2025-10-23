
add_executable(test_stream_manager stream_manager.test.f90 stream_manager.fixture.f90)
set_target_properties(test_stream_manager PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(test_stream_manager PRIVATE session_fixture core_test Fortest::fortest )



