add_library(session_fixture SHARED session.fixture.f90)
set_target_properties(session_fixture PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(session_fixture PRIVATE core_test Fortest::fortest)
target_include_directories(session_fixture INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:include/fortest/fortest>
)


add_subdirectory(alarm)
add_subdirectory(stream_manager)