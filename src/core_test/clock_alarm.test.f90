module test_clock_alarm_mod
    use mpi
    use fortest_test_session, only : test_session_t
    use fortest_assert, only : assert_true, assert_false, assert_equal
    use iso_c_binding, only : c_ptr, c_f_pointer
    use mpas_subdriver
    use mpas_timekeeping
    use mpas_derived_types, only : core_type, domain_type
    use test_clock_alarm_fixture_mod
    implicit none
contains

    subroutine start_stop_time_test1(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture

        call c_f_pointer(test_ptr, fixture)
        call assert_true(fixture%alarm%hasStartTime)
        call assert_true(fixture%alarm%hasStopTime)
    end subroutine start_stop_time_test1

    subroutine start_stop_time_test2(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: hour, ierr

        call c_f_pointer(test_ptr, fixture)
        do hour = 1, fixture%num_hours
            call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
    end subroutine start_stop_time_test2

    subroutine start_stop_time_test3(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: hour, ierr

        call c_f_pointer(test_ptr, fixture)
        do hour = 1, fixture%num_hours
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)
        call assert_true(eq_t_t(fixture%current_time, fixture%alarm_start_time))
    end subroutine start_stop_time_test3

    subroutine start_stop_time_test4(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: hour, ierr

        call c_f_pointer(test_ptr, fixture)
        do hour = 1, fixture%num_hours
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        do hour = 1, fixture%num_hours
            call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
    end subroutine start_stop_time_test4

    subroutine start_stop_time_test5(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: hour, ierr

        call c_f_pointer(test_ptr, fixture)
        do hour = 1, fixture%num_hours
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        do hour = 1, fixture%num_hours
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        do hour = 1, fixture%num_hours
            call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
    end subroutine start_stop_time_test5

    subroutine test_set_clock_direction(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)
        call assert_equal(mpas_get_clock_direction(fixture%clock, ierr = ierr), MPAS_FORWARD)
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call assert_equal(mpas_get_clock_direction(fixture%clock, ierr = ierr), MPAS_BACKWARD)
    end subroutine test_set_clock_direction

    subroutine test_prev_ring_time_forward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: hour, ierr

        call c_f_pointer(test_ptr, fixture)

        do hour = 1, 2 * fixture%num_hours
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do

        call assert_true(eq_t_t(fixture%alarm%prevRingTime, &
                mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr) - &
                        fixture%alarm%ringTimeInterval))
    end subroutine test_prev_ring_time_forward

    subroutine test_prev_ring_time_backward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)

        call assert_true(eq_t_t(fixture%alarm%prevRingTime, &
                mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr) + &
                        fixture%alarm%ringTimeInterval))
    end subroutine test_prev_ring_time_backward

    subroutine test_prev_ring_time_backward_step(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)

        call assert_true(eq_t_t(fixture%alarm%prevRingTime, &
                mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr) - &
                        mpas_get_clock_timestep(fixture%clock) + &
                        fixture%alarm%ringTimeInterval))
    end subroutine test_prev_ring_time_backward_step

    subroutine test_prev_ring_time_reset(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        call assert_true(eq_t_t(fixture%alarm%prevRingTime, fixture%alarm_time - fixture%alarm%ringTimeInterval))
        do i = 1, 2 * fixture%num_hours
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call assert_true(eq_t_t(fixture%alarm%prevRingTime, fixture%alarm_time - fixture%alarm%ringTimeInterval))
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call assert_true(eq_t_t(fixture%alarm%prevRingTime, mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)))
    end subroutine test_prev_ring_time_reset

    subroutine test_alarm_reactivates_when_reversed(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(clock_alarm_test_fixture_t), pointer :: fixture
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)
        do i = 1, 2 * fixture%num_hours
            call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
            call mpas_advance_clock(fixture%clock, ierr = ierr)
        end do
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call mpas_advance_clock(fixture%clock, ierr = ierr)
        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr = ierr))
    end subroutine test_alarm_reactivates_when_reversed

end module test_clock_alarm_mod
program test_clock_alarm
    use fortest_test_session, only : test_session_t
    use test_clock_alarm_mod, only : &
            start_stop_time_test1, start_stop_time_test2, start_stop_time_test3, &
            start_stop_time_test4, start_stop_time_test5, test_set_clock_direction, &
            test_prev_ring_time_forward, test_prev_ring_time_backward, &
            test_prev_ring_time_backward_step, test_prev_ring_time_reset, &
            test_alarm_reactivates_when_reversed
    use test_clock_alarm_fixture_mod, only : &
            clock_alarm_suite_fixture_t, &
            clock_alarm_test_fixture_t, &
            setup_clock_alarm_suite, teardown_clock_alarm_suite, &
            setup_clock_alarm_test, teardown_clock_alarm_test
    use iso_c_binding, only : c_loc, c_ptr
    implicit none

    type(test_session_t) :: session
    type(clock_alarm_suite_fixture_t), target :: suite_fixture
    type(clock_alarm_test_fixture_t),  target :: test_fixture
    type(c_ptr) :: suite_fixture_ptr, test_fixture_ptr

    ! Create C pointers to fixtures
    suite_fixture_ptr = c_loc(suite_fixture)
    test_fixture_ptr  = c_loc(test_fixture)

    ! Register suite
    call session%register_test_suite("clock_alarm_test")

    ! Register fixtures
    call session%register_fixture(&
            setup    = setup_clock_alarm_suite, &
            teardown = teardown_clock_alarm_suite, &
            args     = suite_fixture_ptr, &
            scope    = "session")

    call session%register_fixture(&
            test_suite_name = "clock_alarm_test", &
            setup           = setup_clock_alarm_test, &
            teardown        = teardown_clock_alarm_test, &
            args            = test_fixture_ptr, &
            scope           = "test")

    ! Register tests
    call session%register_test("clock_alarm_test", "start_stop_time_test1", start_stop_time_test1)
    call session%register_test("clock_alarm_test", "start_stop_time_test2", start_stop_time_test2)
    call session%register_test("clock_alarm_test", "start_stop_time_test3", start_stop_time_test3)
    call session%register_test("clock_alarm_test", "start_stop_time_test4", start_stop_time_test4)
    call session%register_test("clock_alarm_test", "start_stop_time_test5", start_stop_time_test5)
    call session%register_test("clock_alarm_test", "test_set_clock_direction", test_set_clock_direction)
    call session%register_test("clock_alarm_test", "test_prev_ring_time_forward", test_prev_ring_time_forward)
    call session%register_test("clock_alarm_test", "test_prev_ring_time_backward", test_prev_ring_time_backward)
    call session%register_test("clock_alarm_test", "test_prev_ring_time_backward_step", test_prev_ring_time_backward_step)
    call session%register_test("clock_alarm_test", "test_prev_ring_time_reset", test_prev_ring_time_reset)
    call session%register_test("clock_alarm_test", "test_alarm_reactivates_when_reversed", test_alarm_reactivates_when_reversed)

    ! Run session
    call session%run()
    call session%finalize()
end program test_clock_alarm
