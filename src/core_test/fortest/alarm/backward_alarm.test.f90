module test_backward_alarm_mod
    use mpi
    use fortest_test_session, only : test_session_t
    use fortest_assert, only : assert_true, assert_false, assert_equal
    use iso_c_binding, only : c_ptr, c_f_pointer
    use mpas_subdriver
    use mpas_timekeeping
    use mpas_derived_types, only : core_type, domain_type
    use alarm_fixture
    implicit none
contains
    ! Utility: locate alarm in clock by ID
    subroutine get_alarm(clock, alarm_id, alarm_ptr)
        use mpas_derived_types, only : MPAS_Clock_type, MPAS_Alarm_type
        implicit none
        type(MPAS_Clock_type), intent(in) :: clock
        character(len = *), intent(in) :: alarm_id
        type(MPAS_Alarm_type), pointer :: alarm_ptr

        alarm_ptr => clock%alarmListHead
        do while (associated(alarm_ptr))
            if (trim(alarm_ptr%alarmID) == trim(alarm_id)) return
            alarm_ptr => alarm_ptr%next
        end do

        nullify(alarm_ptr)
    end subroutine get_alarm

    ! Alarm ringing tests to cover:
    !   * Check it is not ringing prior to start time
    !   * Check is is not ringing prior to start time after reset
    !   * Check if it is ringing at the start time prior to reset
    !   * Check it is not ringing after reset
    !   * Check it is ringing in the middle of it's active window prior to reset at start
    !   * Check it is ringing in the middle of it's active window after reset at start
    !   * Check it is not ringing after reset in the middle of it's active window
    !   * Check it is ringing at the stop time prior to reset
    !   * Check it is not ringing after reset at the stop time
    !> Validate mpas_is_alarm_ringing for non-recurring and recurring alarms
    subroutine test_forward_clock_alarm_is_ringing(test_ptr, ts_ptr, s_ptr, param_idx)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        integer(c_int), value :: param_idx
        type(alarm_fixture_t), pointer :: f
        logical :: got, expected
        integer :: ierr

        call c_f_pointer(test_ptr, f)
        select case (param_idx)
        case(1) ! Check it is not ringing prior to start time
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(2) ! Check is is not ringing prior to start time after reset
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(3) ! Check if it is ringing at the start time prior to reset
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(4) ! Check it is not ringing after reset
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(5) ! Check it is ringing in the middle of it's active window prior to reset at start
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start + f%num_clock_steps / 2)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(6) ! Check it is ringing in the middle of it's active window after reset at start
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call advance_clock_n_times(f%forward_clock, f%num_clock_steps / 2)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(7) ! Check it is not ringing after reset in the middle of it's active window
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start + f%num_clock_steps / 2)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(8) ! Check it is ringing at the stop time prior to reset
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start + f%num_clock_steps)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(9) ! Check it is not ringing after reset at the stop time
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start + f%num_clock_steps)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        end select
    end subroutine test_forward_clock_alarm_is_ringing

    ! Alarm ringing tests to cover:
    !   * Check it is not ringing prior to start time
    !   * Check is is not ringing prior to start time after reset
    !   * Check if it is ringing at the start time prior to reset
    !   * Check it is not ringing after reset
    !   * Check it is ringing in the middle of it's active window prior to reset at start
    !   * Check it is ringing in the middle of it's active window after reset at start
    !   * Check it is not ringing after reset in the middle of it's active window
    !> Validate mpas_is_alarm_ringing for non-recurring and recurring alarms
    subroutine test_backward_clock_alarm_is_ringing(test_ptr, ts_ptr, s_ptr, param_idx)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        integer(c_int), value :: param_idx
        type(alarm_fixture_t), pointer :: f
        logical :: got, expected
        integer :: ierr

        call c_f_pointer(test_ptr, f)
        select case (param_idx)
        case(1) ! Check it is not ringing prior to start time
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(2) ! Check is is not ringing prior to start time after reset
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(3) ! Check if it is ringing at the start time prior to reset
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(4) ! Check it is not ringing after reset
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(5) ! Check it is ringing in the middle of it's active window prior to reset at start
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start + f%num_clock_steps / 2)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(6) ! Check it is ringing in the middle of it's active window after reset at start
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            call advance_clock_n_times(f%backward_clock, f%num_clock_steps / 2)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(7) ! Check it is not ringing after reset in the middle of it's active window
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start + f%num_clock_steps / 2)
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        end select
    end subroutine test_backward_clock_alarm_is_ringing

    ! Alarm ringing tests to cover:
    !   * Check it rings before start time when clock direction is changed at start
    !   * Check it does not ring before start time when clock direction is changed and then reset
    !> Validate mpas_is_alarm_ringing for non-recurring and recurring alarms
    subroutine test_clock_alarm_dir_change_is_ringing(test_ptr, ts_ptr, s_ptr, param_idx)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        integer(c_int), value :: param_idx
        type(alarm_fixture_t), pointer :: f
        logical :: got, expected
        integer :: ierr

        call c_f_pointer(test_ptr, f)
        select case (param_idx)
        case(1)
            call mpas_set_clock_direction(f%forward_clock, MPAS_BACKWARD)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(2)
            call mpas_set_clock_direction(f%forward_clock, MPAS_BACKWARD)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(3)
            call mpas_set_clock_direction(f%backward_clock, MPAS_FORWARD)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(4)
            call mpas_set_clock_direction(f%backward_clock, MPAS_FORWARD)
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(5)
            call advance_clock_n_times(f%backward_clock, 2 * f%num_steps_before_start)
            call mpas_set_clock_direction(f%backward_clock, MPAS_FORWARD)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(6)
            call advance_clock_n_times(f%forward_clock, 2 * f%num_steps_before_start)
            call mpas_set_clock_direction(f%forward_clock, MPAS_BACKWARD)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(7)
            call advance_clock_n_times(f%backward_clock, 2 * f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            call mpas_set_clock_direction(f%backward_clock, MPAS_FORWARD)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(8)
            call advance_clock_n_times(f%forward_clock, 2 * f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call mpas_set_clock_direction(f%forward_clock, MPAS_BACKWARD)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(9)
            call advance_clock_n_times(f%backward_clock, 2 * f%num_steps_before_start)
            call mpas_set_clock_direction(f%backward_clock, MPAS_FORWARD)
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(10)
            call advance_clock_n_times(f%forward_clock, 2 * f%num_steps_before_start)
            call mpas_set_clock_direction(f%forward_clock, MPAS_BACKWARD)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        end select
    end subroutine test_clock_alarm_dir_change_is_ringing

    subroutine test_set_clock_time(test_ptr, ts_ptr, s_ptr, param_idx)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        integer(c_int), value :: param_idx
        type(alarm_fixture_t), pointer :: f
        logical :: got, expected
        integer :: ierr

        call c_f_pointer(test_ptr, f)
        select case (param_idx)
        case(1)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
            call mpas_set_clock_time(f%forward_clock, f%alarm_time, MPAS_NOW, ierr = ierr)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(2)
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
            call mpas_set_clock_time(f%backward_clock, f%alarm_time, MPAS_NOW, ierr = ierr)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(3)
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
            call mpas_set_clock_time(f%forward_clock, f%alarm_time + mul_ti_n(f%alarm_interval, 2), &
                    MPAS_NOW, ierr = ierr)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(4)
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
            call mpas_set_clock_time(f%backward_clock, f%alarm_time - mul_ti_n(f%alarm_interval, 2), &
                    MPAS_NOW, ierr = ierr)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        case(5)
            call assert_false(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
            call mpas_set_clock_time(f%forward_clock, mpas_get_clock_time(f%forward_clock, MPAS_NOW), MPAS_NOW, ierr = ierr)
            call assert_true(mpas_is_alarm_ringing(f%forward_clock, f%forward_alarm_id), verbosity = 2)
        case(6)
            call assert_false(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
            call mpas_set_clock_time(f%backward_clock, mpas_get_clock_time(f%backward_clock, MPAS_NOW), MPAS_NOW, ierr = ierr)
            call assert_true(mpas_is_alarm_ringing(f%backward_clock, f%backward_alarm_id), verbosity = 2)
        end select
    end subroutine test_set_clock_time

    subroutine test_get_next_ring_time(test_ptr, ts_ptr, s_ptr, param_idx)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        integer(c_int), value :: param_idx
        type(alarm_fixture_t), pointer :: f
        type(MPAS_Time_type) :: next_ring_time
        logical :: got, expected
        integer :: ierr

        call c_f_pointer(test_ptr, f)
        select case (param_idx)
        case(1)
            next_ring_time = mpas_alarm_get_next_ring_time(f%forward_clock, f%forward_alarm_id)
            call assert_true(eq_t_t(next_ring_time, f%alarm_time), verbosity = 2)
        case(2)
            next_ring_time = mpas_alarm_get_next_ring_time(f%backward_clock, f%backward_alarm_id)
            call assert_true(eq_t_t(next_ring_time, f%alarm_time), verbosity = 2)
        case(3)
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start)
            next_ring_time = mpas_alarm_get_next_ring_time(f%forward_clock, f%forward_alarm_id)
            call assert_true(eq_t_t(next_ring_time, f%alarm_time), verbosity = 2)
        case(4)
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start)
            next_ring_time = mpas_alarm_get_next_ring_time(f%backward_clock, f%backward_alarm_id)
            call assert_true(eq_t_t(next_ring_time, f%alarm_time), verbosity = 2)
        case(5)
            call advance_clock_n_times(f%forward_clock, f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%forward_clock, f%forward_alarm_id)
            next_ring_time = mpas_alarm_get_next_ring_time(f%forward_clock, f%forward_alarm_id)
            call assert_true(eq_t_t(next_ring_time, add_t_ti(f%alarm_time, f%alarm_interval)), verbosity = 2)
        case(6)
            call advance_clock_n_times(f%backward_clock, f%num_steps_before_start)
            call mpas_reset_clock_alarm(f%backward_clock, f%backward_alarm_id)
            next_ring_time = mpas_alarm_get_next_ring_time(f%backward_clock, f%backward_alarm_id)
            call assert_true(eq_t_t(next_ring_time, sub_t_ti(f%alarm_time, f%alarm_interval)), verbosity = 2)
        end select
    end subroutine test_get_next_ring_time

end module

program test_backward_alarm
    use fortest_test_session, only : test_session_t
    use test_backward_alarm_mod, only : &
            test_forward_clock_alarm_is_ringing, &
            test_backward_clock_alarm_is_ringing, &
            test_clock_alarm_dir_change_is_ringing, &
            test_set_clock_time, &
            test_get_next_ring_time

    use alarm_fixture, only : &
            alarm_fixture_t, &
            setup_backward_alarm, &
            teardown_backward_alarm
    use session_fixture_mod, only : &
            session_fixture_t, &
            setup_session, &
            teardown_session
    use iso_c_binding, only : c_loc, c_ptr
    implicit none

    type(test_session_t) :: session
    type(session_fixture_t), target :: session_fixture
    type(alarm_fixture_t), target :: test_fixture
    type(c_ptr) :: session_fixture_ptr, test_fixture_ptr

    ! Point C interoperable pointers at fixtures.
    session_fixture_ptr = c_loc(session_fixture)
    test_fixture_ptr = c_loc(test_fixture)

    ! Register the test suite and fixtures
    call session%register_test_suite("backward_alarm_test")

    call session%register_fixture(&
            setup = setup_session, &
            teardown = teardown_session, &
            args = session_fixture_ptr, &
            scope = "session")

    call session%register_fixture(&
            test_suite_name = "backward_alarm_test", &
            setup = setup_backward_alarm, &
            teardown = teardown_backward_alarm, &
            args = test_fixture_ptr, &
            scope = "test")
    ! Register tests
    call session%register_parameterized_test(&
            test_suite_name = "backward_alarm_test", &
            test_name = "test_forward_clock_alarm_is_ringing", &
            test = test_forward_clock_alarm_is_ringing, &
            num_params = 9)
    call session%register_parameterized_test(&
            test_suite_name = "backward_alarm_test", &
            test_name = "test_backward_clock_alarm_is_ringing", &
            test = test_backward_clock_alarm_is_ringing, &
            num_params = 7)
    call session%register_parameterized_test(&
            test_suite_name = "backward_alarm_test", &
            test_name = "test_clock_alarm_dir_change_is_ringing", &
            test = test_clock_alarm_dir_change_is_ringing, &
            num_params = 10)
    call session%register_parameterized_test(&
            test_suite_name = "backward_alarm_test", &
            test_name = "test_set_clock_time", &
            test = test_set_clock_time, &
            num_params = 6)
    call session%register_parameterized_test(&
            test_suite_name = "backward_alarm_test", &
            test_name = "test_get_next_ring_time", &
            test = test_get_next_ring_time, &
            num_params = 6)
    call session%run()
    call session%finalize()
end program

