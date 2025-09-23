module mpas_test_core_clock_alarms
    use mpas_derived_types, only : core_type, domain_type
    use mpas_timekeeping

    type clock_alarm_fixture_t
        type(MPAS_Time_type) :: clock_start_time
        type(MPAS_Time_type) :: clock_stop_time
        type(MPAS_Time_type) :: alarm_time
        type(MPAS_Time_type) :: alarm_start_time
        type(MPAS_Time_type) :: alarm_stop_time
        type(MPAS_Time_type) :: current_time
        type(MPAS_TimeInterval_type) :: clock_time_step
        type(MPAS_TimeInterval_type) :: alarm_start_time_interval
        type(MPAS_Clock_type) :: clock
        type(MPAS_Alarm_type) :: alarm
        character(len = :), allocatable :: alarm_id
        integer :: window_size
    end type
contains

    subroutine setup_clock_alarm_fixture(fixture)
        implicit none
        type(clock_alarm_fixture_t), intent(inout) :: fixture
        integer :: ierr
        fixture%alarm_id = 'variable_time_interval_alarm'
        call mpas_set_time(fixture%clock_start_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_time(fixture%clock_stop_time, YYYY = 2100, MM = 01, DD = 01, H = 0, &
                M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
        call mpas_set_timeInterval(fixture%clock_time_step, dt = 3600.0_RKIND, ierr = ierr)
        call mpas_create_clock(fixture%clock, fixture%clock_start_time, fixture%clock_time_step, &
                fixture%clock_stop_time, ierr = ierr)
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
        fixture%alarm = fixture%clock%alarmListHead
        fixture%window_size = 240
    end subroutine

    subroutine teardown_clock_alarm_fixture(fixture)
        implicit none
        type(clock_alarm_fixture_t), intent(inout) :: fixture
        integer :: ierr

        call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_destroy_clock(fixture%clock, ierr = ierr)
    end subroutine

    subroutine test_variable_time_interval_alarm_has_start_and_stop_time(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        ierr = 0
        call setup_clock_alarm_fixture(fixture)
        if (fixture%alarm%hasStartTime .eqv. .false.) then
            ierr = ierr + 1
        end if
        if (fixture%alarm%hasStopTime .eqv. .false.) then
            ierr = ierr + 1
        end if
        call teardown_clock_alarm_fixture(fixture)
    end subroutine

    subroutine test_variable_time_interval_alarm_inactive_before_start(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        integer :: i

        ierr = 0
        call setup_clock_alarm_fixture(fixture)

        ! Step just before start
        do i = 1, fixture%window_size - 1
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        if (mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            ierr = ierr + 1
        end if

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_variable_time_interval_alarm_inactive_before_start


    subroutine test_variable_time_interval_alarm_triggers_at_start_time(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t), intent(out) :: fixture
        integer, intent(out) :: ierr
        integer :: i

        ierr = 0
        call setup_clock_alarm_fixture(fixture)

        ! Advance to start boundary
        do i = 1, fixture%window_size
            !            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id)
            call mpas_advance_clock(fixture%clock)
        end do

        if (.not. mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id)) then
            ierr = ierr + 1
        end if

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_variable_time_interval_alarm_triggers_at_start_time


    subroutine test_variable_time_interval_alarm_rings_within_window(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        integer :: i

        ierr = 0
        call setup_clock_alarm_fixture(fixture)

        ! Step into window
        do i = 1, fixture%window_size
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        if (.not. mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            ierr = ierr + 1
        end if

        ! Stay inside window
        do i = 1, fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
            if (.not. mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
                ierr = ierr + 1
            end if
        end do

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_variable_time_interval_alarm_rings_within_window


    subroutine test_variable_time_interval_alarm_active_at_stop_time(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        integer :: i

        ierr = 0
        call setup_clock_alarm_fixture(fixture)

        ! Advance to stop
        do i = 1, fixture%window_size
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)
        if (fixture%current_time /= fixture%alarm_start_time) then
            ierr = ierr + 1
        end if
        do i = 1, fixture%window_size
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)
        if (fixture%current_time /= fixture%alarm_stop_time) then
            ierr = ierr + 1
        end if

        if (.not. mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            ierr = ierr + 1
        end if

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_variable_time_interval_alarm_active_at_stop_time

    !> Alarm should reactivate when stepping backward across stop boundary
    subroutine test_alarm_reactivates_crossing_stop_backward(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        integer :: i
        ierr = 0

        call setup_clock_alarm_fixture(fixture)

        ! Move beyond stop → should be inactive
        do i = 1, 2 * fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        if (mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            write(*, *) 'FAILED: alarm should be inactive after stop'
            ierr = ierr + 1
        end if

        ! Flip direction and reset
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)

        ! Step back into stop → should be active
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        if (.not. mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            write(*, *) 'FAILED: alarm should reactivate crossing stop backward'
            ierr = ierr + 1
        end if

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_alarm_reactivates_crossing_stop_backward


    !> Alarm should remain active while stepping backward into window,
    !! but deactivate once crossing start time
    subroutine test_alarm_active_crossing_start_backward(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        integer :: i
        ierr = 0

        call setup_clock_alarm_fixture(fixture)

        ! Move beyond stop
        do i = 1, 2 * fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)

        ! Step back into window until start → alarm should ring
        do i = 1, fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        if (.not. mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            write(*, *) 'FAILED: alarm should be active at start when stepping back'
            ierr = ierr + 1
        end if

        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)

        ! Step back once more → before start → should be inactive
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        if (mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            write(*, *) 'FAILED: alarm should be inactive before start when stepping back'
            ierr = ierr + 1
        end if

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_alarm_active_crossing_start_backward


    !> Adjust forward so alarm aligns with reference start time
    subroutine test_adjust_forward_aligns_to_start(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        integer :: i
        ierr = 0

        call setup_clock_alarm_fixture(fixture)

        ! Advance beyond start
        do i = 1, fixture%window_size
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW)
        if (fixture%current_time /= fixture%alarm_start_time) then
            write(*, *) 'FAILED: current time should be at start time'
            ierr = ierr + 1
        end if

        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_adjust_alarm_to_reference_time(fixture%clock, fixture%alarm_id, fixture%alarm_start_time, ierr = ierr)

        if (.not. eq_t_t(fixture%alarm%prevRingTime, fixture%current_time - fixture%clock_time_step)) then
            write(*, *) 'FAILED: prevRingTime did not align with adjusted start time'
            ierr = ierr + 1
        end if

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_adjust_forward_aligns_to_start


    subroutine test_variable_time_interval_alarm_inactive_after_stop(fixture, ierr)
        implicit none
        type(clock_alarm_fixture_t) :: fixture
        integer, intent(out) :: ierr
        integer :: i

        ierr = 0
        call setup_clock_alarm_fixture(fixture)

        ! Advance to stop
        do i = 1, fixture%window_size
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)
        if (fixture%current_time /= fixture%alarm_start_time) then
            ierr = ierr + 1
        end if
        do i = 1, fixture%window_size
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)
        if (fixture%current_time /= fixture%alarm_stop_time) then
            ierr = ierr + 1
        end if

        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)

        if (mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr)) then
            ierr = ierr + 1
        end if

        call teardown_clock_alarm_fixture(fixture)
    end subroutine test_variable_time_interval_alarm_inactive_after_stop


    subroutine test_core_clock_alarms_test(domain, ierr)
        implicit none
        type (domain_type), intent(inout) :: domain
        integer, intent(out) :: ierr
        type(clock_alarm_fixture_t), target :: fixture
        integer :: err

        call mpas_log_write('Clock alarm tests:')
        ierr = 0

        err = 0
        call test_variable_time_interval_alarm_has_start_and_stop_time(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm has start and stop time - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm has start and stop time - PASSED')
        end if

        err = 0
        call test_variable_time_interval_alarm_inactive_before_start(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm inactive before start - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm inactive before start - PASSED')
        end if

        err = 0
        call test_variable_time_interval_alarm_triggers_at_start_time(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm triggers at start time - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm triggers at start time - PASSED')
        end if

        err = 0
        call test_variable_time_interval_alarm_rings_within_window(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm rings within window - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm rings within window - PASSED')
        end if

        err = 0
        call test_variable_time_interval_alarm_active_at_stop_time(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm active at stop time - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm active at stop time - PASSED')
        end if

        err = 0
        call test_variable_time_interval_alarm_inactive_after_stop(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm inactive after stop - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm inactive after stop - PASSED')
        end if

        ! New tests
        err = 0
        call test_alarm_reactivates_crossing_stop_backward(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm reactivates crossing stop backward - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm reactivates crossing stop backward - PASSED')
        end if

        err = 0
        call test_alarm_active_crossing_start_backward(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm active crossing start backward - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm active crossing start backward - PASSED')
        end if

        err = 0
        call test_adjust_forward_aligns_to_start(fixture, err)
        if (err /= 0) then
            write(*, *) '    variable_time_interval_alarm adjust forward aligns to start - FAILED'
            ierr = ierr + err
        else
            call mpas_log_write('    variable_time_interval_alarm adjust forward aligns to start - PASSED')
        end if

    end subroutine test_core_clock_alarms_test
end module mpas_test_core_clock_alarms
