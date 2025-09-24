add_library(session_fixture SHARED session.fixture.f90)
set_target_properties(session_fixture PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(session_fixture PRIVATE core_test Fortest::fortest)

add_executable(test_alarm_variable_time_interval alarm_variable_time_interval.fixture.f90
        alarm_variable_time_interval.test.f90
)
set_target_properties(test_alarm_variable_time_interval PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(test_alarm_variable_time_interval PRIVATE core_test Fortest::fortest session_fixture)

add_executable(test_alarm alarm.fixture.f90
        alarm.test.f90
)
set_target_properties(test_alarm PROPERTIES
        LINKER_LANGUAGE Fortran
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test
)
target_link_libraries(test_alarm PRIVATE core_test Fortest::fortest session_fixture)



