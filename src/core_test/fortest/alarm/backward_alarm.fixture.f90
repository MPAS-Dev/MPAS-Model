module alarm_fixture
    use mpi
    use fortest_test_suite
    use mpas_subdriver
    use mpas_derived_types, only : core_type, domain_type, MPAS_Clock_type, &
            MPAS_Alarm_type, MPAS_Time_type, MPAS_TimeInterval_type
    implicit none

    type alarm_fixture_t
        type(MPAS_Time_type) :: clock_start_time
        type(MPAS_Time_type) :: clock_stop_time
        type(MPAS_Time_type) :: alarm_time
        type(MPAS_Time_type) :: current_time
        type(MPAS_TimeInterval_type) :: clock_time_step
        type(MPAS_TimeInterval_type) :: alarm_interval
        type(MPAS_Clock_type) :: forward_clock
        type(MPAS_Clock_type) :: backward_clock

        ! Original alarms
        type(MPAS_Alarm_type), pointer :: forward_alarm
        character(len = :), allocatable :: forward_alarm_id
        type(MPAS_Alarm_type), pointer :: backward_alarm
        character(len = :), allocatable :: backward_alarm_id
        type(MPAS_Alarm_type), pointer :: non_recurring_alarm
        character(len = :), allocatable :: non_recurring_alarm_id

        integer :: num_clock_steps = 120
        integer :: num_steps_per_interval = 6
        integer :: num_steps_before_start = 12
    end type


contains

    ! Utility: advance clock n steps, optionally resetting an alarm each step
    subroutine advance_clock_n_times(clock, n, alarm_id, reset)
        use mpas_derived_types, only : MPAS_Clock_type
        implicit none
        type(MPAS_Clock_type), intent(inout) :: clock
        integer, intent(in) :: n
        character(len = *), optional, intent(in) :: alarm_id
        logical, intent(in), optional :: reset
        integer :: i
        do i = 1, n
            if (present(alarm_id) .and. present(reset)) then
                if (reset) call mpas_reset_clock_alarm(clock, alarm_id)
            end if
            if (present(reset)) then
                if (reset) call mpas_reset_clock_alarm(clock, alarm_id)
            end if
            call mpas_reset_clock_alarm(clock, alarm_id)
            call mpas_advance_clock(clock)
        end do
    end subroutine advance_clock_n_times

    subroutine setup_backward_alarm(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(f_ptr, fixture)

        fixture%forward_alarm_id = 'forward_alarm'
        fixture%non_recurring_alarm_id = 'non_recurring_alarm'
        fixture%backward_alarm_id = 'backward_alarm'

        ! Clock setup
        call mpas_set_time(fixture%clock_start_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_time(fixture%clock_stop_time, YYYY = 2000, MM = 01, DD = 02, H = 16, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_timeInterval(fixture%clock_time_step, dt = 600.0_RKIND, ierr = ierr)
        call mpas_create_clock(fixture%forward_clock, fixture%clock_start_time, fixture%clock_time_step, &
                fixture%clock_stop_time, ierr = ierr)
        call mpas_create_clock(fixture%backward_clock, fixture%clock_start_time, fixture%clock_time_step, &
                fixture%clock_stop_time, ierr = ierr)
        call advance_clock_n_times(fixture%forward_clock, fixture%num_clock_steps - 2*fixture%num_steps_per_interval)
        call advance_clock_n_times(fixture%backward_clock, fixture%num_clock_steps + 2*fixture%num_steps_per_interval)
        call mpas_set_clock_direction(fixture%backward_clock, MPAS_BACKWARD, ierr = ierr)

        ! Forward alarm
        call mpas_set_time(fixture%alarm_time, YYYY = 2000, MM = 01, DD = 01, H = 20, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_timeInterval(fixture%alarm_interval, dt = 3600.0_RKIND, ierr = ierr)

        call mpas_add_clock_alarm(fixture%forward_clock, fixture%forward_alarm_id, fixture%alarm_time, &
                alarmTimeInterval = fixture%alarm_interval)
        fixture%forward_alarm => fixture%forward_clock%alarmListHead

        call mpas_add_clock_alarm(fixture%backward_clock, fixture%backward_alarm_id, fixture%alarm_time, &
                alarmTimeInterval = fixture%alarm_interval)
        fixture%backward_alarm => fixture%backward_clock%alarmListHead


        ! Non-recurring alarm
        call mpas_add_clock_alarm(fixture%forward_clock, fixture%non_recurring_alarm_id, fixture%alarm_time)
        fixture%non_recurring_alarm => fixture%forward_alarm%next
    end subroutine


    subroutine teardown_backward_alarm(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(f_ptr, fixture)

        call mpas_remove_clock_alarm(fixture%forward_clock, fixture%forward_alarm_id, ierr = ierr)
        call mpas_remove_clock_alarm(fixture%forward_clock, fixture%non_recurring_alarm_id, ierr = ierr)
        call mpas_remove_clock_alarm(fixture%backward_clock, fixture%backward_alarm_id, ierr = ierr)


        call mpas_destroy_clock(fixture%forward_clock, ierr = ierr)
        call mpas_destroy_clock(fixture%backward_clock, ierr = ierr)

        nullify(fixture%forward_alarm)
        nullify(fixture%non_recurring_alarm)
    end subroutine

end module
