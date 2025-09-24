module test_clock_alarm_mod
    use mpi
    use fortest_test_session, only : test_session_t
    use fortest_assert, only : assert_true, assert_false, assert_equal
    use iso_c_binding, only : c_ptr, c_f_pointer
    use mpas_subdriver
    use mpas_timekeeping
    use mpas_derived_types, only : core_type, domain_type
    use alarm_variable_time_interval_fixture_mod
    implicit none
contains

    !---------------------------------------------------------------
    ! Basic properties
    !---------------------------------------------------------------
    subroutine test_alarm_has_start_and_stop_time(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        call c_f_pointer(test_ptr, fixture)
        call assert_true(fixture%alarm%hasStartTime)
        call assert_true(fixture%alarm%hasStopTime)
    end subroutine test_alarm_has_start_and_stop_time

    subroutine test_set_clock_direction(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)
        call assert_equal(mpas_get_clock_direction(fixture%clock, ierr = ierr), MPAS_FORWARD)
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call assert_equal(mpas_get_clock_direction(fixture%clock, ierr = ierr), MPAS_BACKWARD)
    end subroutine test_set_clock_direction

    !---------------------------------------------------------------
    ! Forward direction behavior
    !---------------------------------------------------------------
    subroutine test_alarm_inactive_before_start(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: i, ierr

        call c_f_pointer(test_ptr, fixture)

        ! Step just before start
        do i = 1, fixture%window_size - 1
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_inactive_before_start

    subroutine test_alarm_triggers_at_start_time(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: i, ierr

        call c_f_pointer(test_ptr, fixture)

        ! Advance to start boundary
        do i = 1, fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_triggers_at_start_time

    subroutine test_alarm_rings_within_window(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: i, ierr

        call c_f_pointer(test_ptr, fixture)

        ! Step into window
        do i = 1, fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))

        ! Stay inside window
        do i = 1, fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
            call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        end do
    end subroutine test_alarm_rings_within_window

    subroutine test_alarm_active_at_stop_time(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: i, ierr

        call c_f_pointer(test_ptr, fixture)

        ! Advance to stop
        do i = 1, fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_active_at_stop_time

    subroutine test_alarm_inactive_after_stop(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: i, ierr

        call c_f_pointer(test_ptr, fixture)

        ! Move past stop
        do i = 1, 2 * fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_inactive_after_stop

    !---------------------------------------------------------------
    ! Backward direction behavior
    !---------------------------------------------------------------
    subroutine test_alarm_reactivates_crossing_stop_backward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: i, ierr

        call c_f_pointer(test_ptr, fixture)

        ! Move beyond stop → inactive
        do i = 1, 2 * fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))

        ! Flip direction and reset
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)

        ! Step back into stop → active
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_reactivates_crossing_stop_backward

    subroutine test_alarm_active_crossing_start_backward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: i, ierr

        call c_f_pointer(test_ptr, fixture)

        ! Go beyond stop
        do i = 1, 2 * fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)

        ! Step back into window until start
        do i = 1, fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)

        ! Step back once more → before start → inactive
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_active_crossing_start_backward

    subroutine test_adjust_forward_aligns_to_start(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        do i = 1, fixture%window_size
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW)
        call assert_true(eq_t_t(fixture%alarm_start_time, fixture%current_time))
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_adjust_alarm_to_reference_time(fixture%clock, fixture%alarm_id, fixture%alarm_start_time, ierr = ierr)
        call assert_true(eq_t_t(fixture%alarm%prevRingTime, fixture%current_time - fixture%clock_time_step))
    end subroutine test_adjust_forward_aligns_to_start

    subroutine test_alarm_stop_before_start(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Invert start/stop manually
        fixture%alarm%startTime = fixture%alarm_stop_time
        fixture%alarm%stopTime = fixture%alarm_start_time
        fixture%alarm%hasStartTime = .true.
        fixture%alarm%hasStopTime = .true.

        do i = 1, 3 * fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
            call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        end do
    end subroutine test_alarm_stop_before_start

    subroutine test_alarm_start_equals_stop(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        fixture%alarm%startTime = fixture%alarm_start_time
        fixture%alarm%stopTime = fixture%alarm_start_time
        fixture%alarm%hasStartTime = .true.
        fixture%alarm%hasStopTime = .true.

        ! Advance up to the start time
        do i = 1, fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))

        ! Next step should already be inactive
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_start_equals_stop

    subroutine test_alarm_reset_inside_window(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Move clock into ringing window
        do i = 1, fixture%window_size + 1
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))

        ! Reset while still inside window
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_reset_inside_window

    subroutine test_alarm_inactive_after_window(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Step beyond stop time
        do i = 1, 2 * fixture%window_size + 2
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_inactive_after_window

    subroutine test_multiple_alarms_overlap(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Advance clock through overlapping period
        do i = 1, 2 * fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        ! During overlap, both alarms should ring
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id2, ierr = ierr))
    end subroutine test_multiple_alarms_overlap
    subroutine test_multiple_alarms_before_overlap(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Advance clock into window of first alarm but before second alarm
        do i = 1, fixture%window_size + 10
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id2, ierr = ierr))
    end subroutine test_multiple_alarms_before_overlap


    subroutine test_multiple_alarms_during_overlap(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Advance clock into overlapping period
        do i = 1, 2 * fixture%window_size
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id2, ierr = ierr))
    end subroutine test_multiple_alarms_during_overlap


    subroutine test_multiple_alarms_after_overlap(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Move clock well beyond stop of first alarm
        do i = 1, int(2.5 * fixture%window_size)
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW)
        call assert_true(eq_t_t(fixture%alarm2_stop_time, fixture%current_time))

        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id2, ierr = ierr))
    end subroutine test_multiple_alarms_after_overlap

    subroutine test_alarm_reset_after_backward_past_stop(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Advance clock forward past the stop time of alarm2
        do i = 1, int(2.5 * fixture%window_size)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        ! Switch to backward direction
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)

        ! Reset alarm2 after passing its stop time
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)

        ! Verify that alarm2 is marked as set again
        call assert_true(fixture%alarm2%isSet)
        ! Advance the clock one time step
        call mpas_advance_clock(fixture%clock, ierr = ierr)
    end subroutine test_alarm_reset_after_backward_past_stop

    !-----------------------------------------------------------------
    ! FAILING
    !-----------------------------------------------------------------
    ! Subroutine: test_multiple_alarms_backward_overlap
    ! Purpose:
    !   This subroutine tests the behavior of multiple alarms when the
    !   clock is moved backward after advancing beyond their stop times.
    !   It ensures that alarms are properly reset and their states are
    !   correctly evaluated during backward traversal.
    !
    ! Arguments:
    !   test_ptr - C pointer to the test fixture.
    !   ts_ptr   - C pointer to the test session.
    !   s_ptr    - C pointer to additional session data.
    !
    ! Local Variables:
    !   fixture - Pointer to the alarm fixture containing test data.
    !   ierr    - Integer error code for subroutine calls.
    !   i       - Loop counter for advancing the clock.
    !-----------------------------------------------------------------
    subroutine test_multiple_alarms_backward_overlap(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        integer :: ierr, i

        ! Associate the C pointer with the Fortran fixture pointer
        call c_f_pointer(test_ptr, fixture)

        ! Move the clock beyond the stop times of both alarms
        do i = 1, int(3.5 * fixture%window_size)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        ! Switch the clock direction to backward
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)

        ! Reset both alarms to prepare for backward traversal
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)

        ! Step backward into the second alarm’s window and verify states
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))

        ! Retrieve the current clock time and verify it is beyond the stop time
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW)
        call assert_true(fixture%current_time > fixture%alarm_stop_time)
    end subroutine test_multiple_alarms_backward_overlap


    !-----------------------------------------------------------------
    ! FAILING
    !-----------------------------------------------------------------
    ! Subroutine: test_alarm_threshold_greater_than_now_after_reset
    ! Purpose:
    !   This subroutine verifies that the alarm threshold time is greater
    !   than the current clock time after the alarm is reset and the clock
    !   is moved backward past the alarm's stop time.
    !
    ! Arguments:
    !   test_ptr - C pointer to the test fixture.
    !   ts_ptr   - C pointer to the test session.
    !   s_ptr    - C pointer to additional session data.
    !
    ! Local Variables:
    !   fixture         - Pointer to the alarm fixture containing test data.
    !   alarm_threshold - Computed threshold time for the alarm.
    !   alarm_now       - Current clock time.
    !   ierr            - Integer error code for subroutine calls.
    !   i               - Loop counter for advancing the clock.
    !-----------------------------------------------------------------
    subroutine test_alarm_threshold_greater_than_now_after_reset(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_variable_time_interval_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: alarm_threshold, alarm_now
        integer :: ierr, i

        ! Associate the C pointer with the Fortran fixture pointer
        call c_f_pointer(test_ptr, fixture)

        ! Advance the clock forward past the stop time of alarm2
        do i = 1, int(2.5 * fixture%window_size)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        ! Switch the clock direction to backward
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)

        ! Reset alarm2 so internal state is updated after direction change
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id2, ierr = ierr)

        ! If an alarm is ringing, the condition alarm_now >= alarm_threshold. Since alarm2
        ! is not ringing after the reset, we can check that alarm_threshold > alarm_now.

        ! Compute the alarm threshold and the current clock time
        alarm_threshold = fixture%alarm2%prevRingTime - fixture%alarm2%ringTimeInterval
        alarm_now = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr)

        ! Verify that the alarm threshold is greater than the current time
        call assert_true(alarm_threshold > alarm_now)
    end subroutine test_alarm_threshold_greater_than_now_after_reset


end module test_clock_alarm_mod
program test_clock_alarm
    use fortest_test_session, only : test_session_t
    use test_clock_alarm_mod, only : &
            test_alarm_has_start_and_stop_time, &
            test_set_clock_direction, &
            test_alarm_inactive_before_start, &
            test_alarm_triggers_at_start_time, &
            test_alarm_rings_within_window, &
            test_alarm_active_at_stop_time, &
            test_alarm_inactive_after_stop, &
            test_alarm_reactivates_crossing_stop_backward, &
            test_alarm_active_crossing_start_backward, &
            test_adjust_forward_aligns_to_start, &
            test_alarm_stop_before_start, &
            test_alarm_start_equals_stop, &
            test_alarm_reset_inside_window, &
            test_alarm_inactive_after_window, &
            test_multiple_alarms_overlap, &
            test_multiple_alarms_before_overlap, &
            test_multiple_alarms_during_overlap, &
            test_multiple_alarms_after_overlap, &
            test_multiple_alarms_backward_overlap, &
            test_alarm_reset_after_backward_past_stop, &
            test_alarm_threshold_greater_than_now_after_reset
    use alarm_variable_time_interval_fixture_mod, only : &
            alarm_variable_time_interval_fixture_t, &
            setup_alarm_variable_time_interval, &
            teardown_alarm_variable_time_interval
    use session_fixture_mod, only : &
            session_fixture_t, &
            setup_session, &
            teardown_session
    use iso_c_binding, only : c_loc, c_ptr
    implicit none

    type(test_session_t) :: session
    type(session_fixture_t), target :: session_fixture
    type(alarm_variable_time_interval_fixture_t), target :: test_fixture
    type(c_ptr) :: session_fixture_ptr, test_fixture_ptr

    ! Point C interoperable pointers at fixtures
    session_fixture_ptr = c_loc(session_fixture)
    test_fixture_ptr = c_loc(test_fixture)

    ! Register the suite and fixtures
    call session%register_test_suite("clock_alarm_test")

    call session%register_fixture(&
            setup = setup_session, &
            teardown = teardown_session, &
            args = session_fixture_ptr, &
            scope = "session")

    call session%register_fixture(&
            test_suite_name = "clock_alarm_test", &
            setup = setup_alarm_variable_time_interval, &
            teardown = teardown_alarm_variable_time_interval, &
            args = test_fixture_ptr, &
            scope = "test")

    ! Register all tests
    call session%register_test("clock_alarm_test", "alarm_has_start_and_stop_time", test_alarm_has_start_and_stop_time)
    call session%register_test("clock_alarm_test", "set_clock_direction", test_set_clock_direction)
    call session%register_test("clock_alarm_test", "alarm_inactive_before_start", test_alarm_inactive_before_start)
    call session%register_test("clock_alarm_test", "alarm_triggers_at_start_time", test_alarm_triggers_at_start_time)
    call session%register_test("clock_alarm_test", "alarm_rings_within_window", test_alarm_rings_within_window)
    call session%register_test("clock_alarm_test", "alarm_active_at_stop_time", test_alarm_active_at_stop_time)
    call session%register_test("clock_alarm_test", "alarm_inactive_after_stop", test_alarm_inactive_after_stop)
    call session%register_test("clock_alarm_test", "alarm_reactivates_crossing_stop_backward", test_alarm_reactivates_crossing_stop_backward)
    call session%register_test("clock_alarm_test", "alarm_active_crossing_start_backward", test_alarm_active_crossing_start_backward)
    call session%register_test("clock_alarm_test", "adjust_forward_aligns_to_start", test_adjust_forward_aligns_to_start)
    call session%register_test("clock_alarm_test", "alarm_stop_before_start", test_alarm_stop_before_start)
    call session%register_test("clock_alarm_test", "alarm_start_equals_stop", test_alarm_start_equals_stop)
    call session%register_test("clock_alarm_test", "alarm_reset_inside_window", test_alarm_reset_inside_window)
    call session%register_test("clock_alarm_test", "alarm_inactive_after_window", test_alarm_inactive_after_window)
    call session%register_test("clock_alarm_test", "multiple_alarms_overlap", test_multiple_alarms_overlap)
    call session%register_test("clock_alarm_test", "multiple_alarms_before_overlap", test_multiple_alarms_before_overlap)
    call session%register_test("clock_alarm_test", "multiple_alarms_during_overlap", test_multiple_alarms_during_overlap)
    call session%register_test("clock_alarm_test", "multiple_alarms_after_overlap", test_multiple_alarms_after_overlap)
    call session%register_test("clock_alarm_test", "multiple_alarms_backward_overlap", test_multiple_alarms_backward_overlap)
    call session%register_test("clock_alarm_test", "alarm_reset_after_backward_past_stop", test_alarm_reset_after_backward_past_stop)
    call session%register_test("clock_alarm_test", "alarm_threshold_greater_than_now_after_reset", test_alarm_threshold_greater_than_now_after_reset)

    ! Run the test suite
    call session%run()
    call session%finalize()
end program test_clock_alarm
