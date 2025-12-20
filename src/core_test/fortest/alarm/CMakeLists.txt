add_executable(test_backward_alarm backward_alarm.test.f90 backward_alarm.fixture.f90)
set_target_properties(test_backward_alarm PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(test_backward_alarm PRIVATE  Fortest::fortest session_fixture core_test MPI::MPI_Fortran)




