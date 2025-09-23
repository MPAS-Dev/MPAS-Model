module alarm_fixture_mod
    use mpi
    use test_suite_mod
    use mpas_subdriver
    use mpas_derived_types, only : core_type, domain_type
    implicit none

    type alarm_fixture_t
        type(MPAS_Time_type) :: clock_start_time
        type(MPAS_Time_type) :: clock_stop_time
        type(MPAS_Time_type) :: alarm_time
        type(MPAS_Time_type) :: alarm_start_time
        type(MPAS_Time_type) :: alarm_stop_time
        type(MPAS_Time_type) :: current_time
        type(MPAS_TimeInterval_type) :: clock_time_step
        type(MPAS_TimeInterval_type) :: alarm_interval
        type(MPAS_Clock_type) :: clock
        type(MPAS_Alarm_type), pointer :: alarm, window_alarm
        character(len = :), allocatable :: alarm_id, window_alarm_id
        integer :: num_hours
        integer :: window
    end type

contains

    subroutine setup_alarm(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(f_ptr, fixture)

        fixture%alarm_id = 'alarm'

        call mpas_set_time(fixture%clock_start_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_time(fixture%clock_stop_time, YYYY = 2000, MM = 01, DD = 11, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_timeInterval(fixture%clock_time_step, dt = 3600.0_RKIND, ierr = ierr)
        call mpas_create_clock(fixture%clock, fixture%clock_start_time, fixture%clock_time_step, fixture%clock_stop_time, ierr = ierr)

        call mpas_set_time(fixture%alarm_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)

        fixture%window_alarm_id = 'window_alarm'
        call mpas_set_timeInterval(fixture%alarm_interval, dt = 3600.0_RKIND, ierr = ierr)
        call mpas_add_clock_alarm(fixture%clock, fixture%alarm_id, fixture%alarm_time, &
                alarmTimeInterval = fixture%alarm_interval, ierr = ierr)
        fixture%alarm_start_time = fixture%alarm_time + mul_ti_n(fixture%alarm_interval, 2)
        fixture%alarm_stop_time = fixture%alarm_time + mul_ti_n(fixture%alarm_interval, 5)
        call mpas_add_clock_alarm(fixture%clock, fixture%window_alarm_id, fixture%alarm_time, &
                alarmStartTime = fixture%alarm_start_time, &
                alarmStopTime = fixture%alarm_stop_time, alarmTimeInterval = fixture%alarm_interval, ierr = ierr)


        ! Point fixture alarm pointer at the alarm
        fixture%alarm => fixture%clock%alarmListHead
        fixture%num_hours = 240
        fixture%window = 3
    end subroutine

    subroutine teardown_alarm(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(f_ptr, fixture)
        call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_remove_clock_alarm(fixture%clock, fixture%window_alarm_id, ierr = ierr)
        call mpas_destroy_clock(fixture%clock, ierr = ierr)
        nullify(fixture%alarm)
        nullify(fixture%window_alarm)
    end subroutine

end module
