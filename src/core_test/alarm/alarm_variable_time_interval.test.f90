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
            test_adjust_forward_aligns_to_start
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

    ! Run the test suite
    call session%run()
    call session%finalize()
end program test_clock_alarm
