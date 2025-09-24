module alarm_variable_time_interval_fixture_mod
    use mpi
    use test_suite_mod
    use mpas_subdriver
    use mpas_derived_types, only : core_type, domain_type
    implicit none

    type alarm_variable_time_interval_fixture_t
        type(MPAS_Time_type) :: clock_start_time
        type(MPAS_Time_type) :: clock_stop_time
        type(MPAS_Time_type) :: alarm_time
        type(MPAS_Time_type) :: alarm_start_time
        type(MPAS_Time_type) :: alarm_stop_time
        type(MPAS_Time_type) :: current_time
        type(MPAS_TimeInterval_type) :: clock_time_step
        type(MPAS_TimeInterval_type) :: alarm_start_time_interval
        type(MPAS_Clock_type) :: clock
        type(MPAS_Alarm_type), pointer :: alarm
        character(len = :), allocatable :: alarm_id
        integer :: window_size

        ! New fields for second alarm
        type(MPAS_Alarm_type), pointer :: alarm2
        character(len = :), allocatable :: alarm_id2
        type(MPAS_Time_type) :: alarm2_start_time
        type(MPAS_Time_type) :: alarm2_stop_time
    end type


contains

    subroutine setup_alarm_variable_time_interval(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(f_ptr, fixture)

        fixture%alarm_id = 'alarm_start_stop'

        call mpas_set_time(fixture%clock_start_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_time(fixture%clock_stop_time, YYYY = 2100, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_timeInterval(fixture%clock_time_step, dt = 3600.0_RKIND, ierr = ierr)
        call mpas_create_clock(fixture%clock, fixture%clock_start_time, fixture%clock_time_step, fixture%clock_stop_time, ierr = ierr)

        call mpas_set_time(fixture%alarm_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_time(fixture%alarm_start_time, YYYY = 2000, MM = 01, DD = 11, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_time(fixture%alarm_stop_time, YYYY = 2000, MM = 01, DD = 21, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)

        call mpas_set_timeInterval(fixture%alarm_start_time_interval, dt = 3600.0_RKIND, ierr = ierr)
        call mpas_add_clock_alarm(fixture%clock, fixture%alarm_id, fixture%alarm_time, &
                alarmTimeInterval = fixture%alarm_start_time_interval, &
                alarmStartTime = fixture%alarm_start_time, &
                alarmStopTime = fixture%alarm_stop_time, ierr = ierr)

        ! Point fixture alarm pointer at the first alarm
        fixture%alarm => fixture%clock%alarmListHead
        fixture%window_size = 240

        ! -----------------------------
        ! Setup second alarm (staggered window)
        fixture%alarm_id2 = 'alarm_overlap'

        call mpas_set_time(fixture%alarm2_start_time, YYYY = 2000, MM = 01, DD = 16, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_time(fixture%alarm2_stop_time, YYYY = 2000, MM = 01, DD = 26, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)

        call mpas_add_clock_alarm(fixture%clock, fixture%alarm_id2, fixture%alarm_time, &
                alarmTimeInterval = fixture%alarm_start_time_interval, &
                alarmStartTime = fixture%alarm2_start_time, &
                alarmStopTime = fixture%alarm2_stop_time, ierr = ierr)

        ! Point at second alarm (will be head of list now)
        fixture%alarm2 => fixture%clock%alarmListHead
    end subroutine

    subroutine teardown_alarm_variable_time_interval(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(f_ptr, fixture)

        if (allocated(fixture%alarm_id2)) then
            call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)
        end if
        call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_destroy_clock(fixture%clock, ierr = ierr)
        nullify(fixture%alarm)
        nullify(fixture%alarm2)
    end subroutine

end module
