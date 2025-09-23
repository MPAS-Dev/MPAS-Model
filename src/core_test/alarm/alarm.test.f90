module test_alarm_mod
    use mpi
    use fortest_test_session, only : test_session_t
    use fortest_assert, only : assert_true, assert_false, assert_equal
    use iso_c_binding, only : c_ptr, c_f_pointer
    use mpas_subdriver
    use mpas_timekeeping
    use mpas_derived_types, only : core_type, domain_type
    use alarm_fixture_mod
    use mpas_timekeeping
    implicit none
contains
    !> Walk the clock's alarm linked list and return a pointer to
    !! the alarm matching the given alarm ID. Returns NULL() if not found.
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

        ! Not found → nullify
        nullify(alarm_ptr)
    end subroutine get_alarm


    !> Test subroutine for verifying if the clock is at its start time.
    !! This subroutine checks if the `mpas_is_clock_start_time` function correctly identifies
    !! the start time of the clock in the provided alarm fixture.
    !!
    !! @param[in] test_ptr  C pointer to the test fixture (alarm_fixture_t).
    !! @param[in] ts_ptr    Unused parameter (reserved for future use).
    !! @param[in] s_ptr     Unused parameter (reserved for future use).
    subroutine test_mpas_is_clock_start_time(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr  ! C pointers for fixture and unused arguments.
        type(alarm_fixture_t), pointer :: fixture      ! Pointer to the alarm fixture.

        ! Convert the C pointer to a Fortran pointer for the alarm fixture.
        call c_f_pointer(test_ptr, fixture)

        ! Assert that the clock in the fixture is at its start time.
        call assert_true(mpas_is_clock_start_time(fixture%clock))
    end subroutine

    !> Test subroutine for verifying if the clock is at its stop time.
    !! This subroutine checks if the `mpas_is_clock_stop_time` function correctly identifies
    !! the stop time of the clock in the provided alarm fixture after advancing the clock.
    !!
    !! @param[in] test_ptr  C pointer to the test fixture (alarm_fixture_t).
    !! @param[in] ts_ptr    Unused parameter (reserved for future use).
    !! @param[in] s_ptr     Unused parameter (reserved for future use).
    subroutine test_mpas_is_clock_stop_time(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr  ! C pointers for fixture and unused arguments.
        type(alarm_fixture_t), pointer :: fixture      ! Pointer to the alarm fixture.
        integer :: i                                   ! Loop variable for advancing the clock.

        ! Convert the C pointer to a Fortran pointer for the alarm fixture.
        call c_f_pointer(test_ptr, fixture)

        ! Advance the clock in the fixture by the number of hours specified.
        do i = 1, fixture%num_hours
            call mpas_advance_clock(fixture%clock)
        end do

        ! Assert that the clock in the fixture is at its stop time.
        call assert_true(mpas_is_clock_stop_time(fixture%clock))
    end subroutine

    subroutine test_mpas_set_clock_direction_backward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr  ! C pointers for fixture and unused arguments.
        type(alarm_fixture_t), pointer :: fixture      ! Pointer to the alarm fixture.
        type(MPAS_TimeInterval_type) :: time_step
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)
        time_step = fixture%clock_time_step
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call assert_equal(ierr, 0)
        call assert_true(eq_ti_ti(mpas_get_clock_timestep(fixture%clock, ierr = ierr), -time_step))
    end subroutine

    subroutine test_mpas_set_clock_direction_forward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr  ! C pointers for fixture and unused arguments.
        type(alarm_fixture_t), pointer :: fixture      ! Pointer to the alarm fixture.
        type(MPAS_TimeInterval_type) :: time_step
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)
        time_step = fixture%clock_time_step
        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr = ierr)
        call mpas_set_clock_direction(fixture%clock, MPAS_FORWARD, ierr = ierr)
        call assert_equal(ierr, 0)
        call assert_true(eq_ti_ti(mpas_get_clock_timestep(fixture%clock, ierr = ierr), time_step))
    end subroutine

    subroutine test_mpas_set_clock_time_step(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr  ! C pointers for fixture and unused arguments.
        type(alarm_fixture_t), pointer :: fixture      ! Pointer to the alarm fixture.
        type(MPAS_TimeInterval_type) :: new_time_step
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)
        new_time_step = fixture%clock_time_step / 2
        call mpas_set_clock_timestep(fixture%clock, new_time_step, ierr = ierr)
        call assert_equal(ierr, 0)
        call assert_true(eq_ti_ti(mpas_get_clock_timestep(fixture%clock, ierr = ierr), new_time_step))
    end subroutine

    subroutine test_mpas_advance_clock_default(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: before, after
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Record current time
        before = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)

        ! Advance with default clock timestep
        call mpas_advance_clock(fixture%clock, ierr = ierr)

        ! Get time after advancing
        after = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)

        ! Assert the difference matches the clock's timestep
        call assert_true(eq_ti_ti(after - before, fixture%clock_time_step))
    end subroutine test_mpas_advance_clock_default


    subroutine test_mpas_advance_clock_with_custom_step(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: before, after
        type(MPAS_TimeInterval_type) :: custom_step
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Define a custom step (double the fixture step)
        custom_step = mul_ti_n(fixture%clock_time_step, 2)

        ! Record current time
        before = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)

        ! Advance with custom step
        call mpas_advance_clock(fixture%clock, timeStep = custom_step, ierr = ierr)

        ! Get time after advancing
        after = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)

        ! Assert the difference matches the custom step
        call assert_true(eq_ti_ti(after - before, custom_step))
    end subroutine test_mpas_advance_clock_with_custom_step

    subroutine test_mpas_set_clock_time_now(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: new_time, current_time
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Define a new current time
        call mpas_set_time(new_time, YYYY = 2001, MM = 1, DD = 1, H = 12, M = 0, S = 0, ierr = ierr)

        ! Set the clock's NOW time
        call mpas_set_clock_time(fixture%clock, new_time, MPAS_NOW, ierr = ierr)

        ! Query the current time
        current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr = ierr)

        ! Assert that the new time matches
        call assert_true(eq_t_t(current_time, new_time))
    end subroutine test_mpas_set_clock_time_now


    subroutine test_mpas_set_clock_time_start(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: new_time, start_time
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Define a new start time
        call mpas_set_time(new_time, YYYY = 1999, MM = 12, DD = 31, H = 23, M = 0, S = 0, ierr = ierr)

        ! Set the clock's START time
        call mpas_set_clock_time(fixture%clock, new_time, MPAS_START_TIME, ierr = ierr)

        ! Query the start time
        start_time = mpas_get_clock_time(fixture%clock, MPAS_START_TIME, ierr = ierr)

        ! Assert that the new time matches
        call assert_true(eq_t_t(start_time, new_time))
    end subroutine test_mpas_set_clock_time_start


    !> Test subroutine for setting the clock's STOP time.
    !! This subroutine verifies that the `mpas_set_clock_time` function correctly sets
    !! the STOP time of the clock in the provided alarm fixture.
    !!
    !! @param[in] test_ptr  C pointer to the test fixture (alarm_fixture_t).
    !! @param[in] ts_ptr    Unused parameter (reserved for future use).
    !! @param[in] s_ptr     Unused parameter (reserved for future use).
    subroutine test_mpas_set_clock_time_stop(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr  ! C pointers for fixture and unused arguments.
        type(alarm_fixture_t), pointer :: fixture      ! Pointer to the alarm fixture.
        type(MPAS_Time_type) :: new_time, stop_time    ! Variables for the new and queried stop times.
        integer :: ierr                                ! Error code variable.

        ! Convert the C pointer to a Fortran pointer for the alarm fixture.
        call c_f_pointer(test_ptr, fixture)

        ! Define a new stop time.
        call mpas_set_time(new_time, YYYY = 2101, MM = 1, DD = 1, H = 0, M = 0, S = 0, ierr = ierr)

        ! Set the clock's STOP time.
        call mpas_set_clock_time(fixture%clock, new_time, MPAS_STOP_TIME, ierr = ierr)

        ! Query the stop time.
        stop_time = mpas_get_clock_time(fixture%clock, MPAS_STOP_TIME, ierr = ierr)

        ! Assert that the new time matches the queried stop time.
        call assert_true(eq_t_t(stop_time, new_time))
    end subroutine test_mpas_set_clock_time_stop

    !> Test subroutine for adding a non-recurring alarm.
    !! This subroutine verifies that a non-recurring alarm can be added to the clock
    !! and that its properties are correctly set.
    !!
    !! @param[in] test_ptr  C pointer to the test fixture (alarm_fixture_t).
    !! @param[in] ts_ptr    Unused parameter (reserved for future use).
    !! @param[in] s_ptr     Unused parameter (reserved for future use).
    subroutine test_add_non_recurring_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr  ! C pointers for fixture and unused arguments.
        type(alarm_fixture_t), pointer :: fixture      ! Pointer to the alarm fixture.
        type(MPAS_Alarm_type), pointer :: nonrecurring_alarm  ! Pointer to the non-recurring alarm.
        type(MPAS_Time_type) :: custom_time            ! Custom time for the alarm.
        integer :: ierr                                ! Error code variable.

        ! Convert the C pointer to a Fortran pointer for the alarm fixture.
        call c_f_pointer(test_ptr, fixture)

        ! Define a custom time for the alarm.
        call mpas_set_time(custom_time, YYYY = 2001, MM = 1, DD = 1, H = 6, M = 0, S = 0, ierr = ierr)

        ! Add a non-recurring alarm to the clock.
        call mpas_add_clock_alarm(fixture%clock, "nonrecurring", custom_time, ierr = ierr)

        ! Assert that the alarm was added successfully.
        call assert_equal(ierr, 0)

        ! Retrieve the alarm from the clock's alarm list.
        call get_alarm(fixture%clock, "nonrecurring", nonrecurring_alarm)

        ! Assert that the alarm is non-recurring and its times are correctly set.
        call assert_false(nonrecurring_alarm%isRecurring)
        call assert_true(eq_t_t(nonrecurring_alarm%ringTime, custom_time))
        call assert_true(eq_t_t(nonrecurring_alarm%prevRingTime, custom_time))
    end subroutine

    subroutine test_add_recurring_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: custom_time
        type(MPAS_TimeInterval_type) :: interval
        type(MPAS_Alarm_type), pointer :: new_alarm
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_time(custom_time, YYYY = 2002, MM = 2, DD = 2, H = 12, M = 0, S = 0, ierr = ierr)
        call mpas_set_timeInterval(interval, dt = 7200.0_RKIND, ierr = ierr)

        call mpas_add_clock_alarm(fixture%clock, "recurring", custom_time, alarmTimeInterval = interval, ierr = ierr)
        call assert_equal(ierr, 0)

        call get_alarm(fixture%clock, "recurring", new_alarm)

        call assert_true(new_alarm%isRecurring)
        call assert_true(eq_t_t(new_alarm%ringTime, custom_time))
        call assert_true(eq_ti_ti(new_alarm%ringTimeInterval, interval))
    end subroutine

    subroutine test_add_alarm_with_start_stop(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: start_time, stop_time, custom_time
        type(MPAS_Alarm_type), pointer :: new_alarm
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_time(custom_time, YYYY = 2003, MM = 3, DD = 3, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_set_time(start_time, YYYY = 2003, MM = 3, DD = 4, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_set_time(stop_time, YYYY = 2003, MM = 3, DD = 5, H = 0, M = 0, S = 0, ierr = ierr)

        call mpas_add_clock_alarm(fixture%clock, "with_start_stop", custom_time, &
                alarmStartTime = start_time, alarmStopTime = stop_time, ierr = ierr)

        call get_alarm(fixture%clock, "with_start_stop", new_alarm)

        call assert_equal(ierr, 0)
        call assert_true(new_alarm%hasStartTime)
        call assert_true(new_alarm%hasStopTime)
        call assert_true(eq_t_t(new_alarm%startTime, start_time))
        call assert_true(eq_t_t(new_alarm%stopTime, stop_time))
    end subroutine

    !> Test that adding a duplicate alarm ID adjacent to head is rejected.
    subroutine test_add_duplicate_adjacent_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr
        type(MPAS_Alarm_type), pointer :: found

        call c_f_pointer(test_ptr, fixture)

        ! Try adding another alarm with the same ID as the fixture head
        call mpas_add_clock_alarm(fixture%clock, fixture%window_alarm_id, fixture%alarm_time, ierr = ierr)

        ! Should fail and not insert anything
        call assert_equal(ierr, 1)
        call get_alarm(fixture%clock, fixture%window_alarm_id, found)
        call assert_true(associated(found))  ! original still exists
        call assert_equal(found%alarmID, fixture%window_alarm_id)
        call assert_false(associated(found%next))  ! no duplicate added
    end subroutine


    !> Test that adding a non-adjacent duplicate alarm ID is rejected.
    subroutine test_add_duplicate_nonadjacent_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: other_time
        integer :: ierr
        type(MPAS_Alarm_type), pointer :: found, other_alarm

        call c_f_pointer(test_ptr, fixture)

        ! First add a different alarm
        call mpas_set_time(other_time, YYYY = 2001, MM = 1, DD = 2, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_add_clock_alarm(fixture%clock, "unique_alarm", other_time, ierr = ierr)
        call get_alarm(fixture%clock, "unique_alarm", other_alarm)
        call assert_true(associated(other_alarm))

        ! Now try adding another alarm with the same ID as the original head
        call mpas_add_clock_alarm(fixture%clock, fixture%alarm_id, fixture%alarm_time, ierr = ierr)

        ! Should fail
        call assert_equal(ierr, 1)
        call get_alarm(fixture%clock, fixture%alarm_id, found)
        call assert_true(associated(found))  ! original still exists
        call assert_false(trim(other_alarm%alarmID) == trim(fixture%alarm_id)) ! not overwritten
    end subroutine


    !> Test removing the head alarm from the list.
    subroutine test_remove_head_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Alarm_type), pointer :: found
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Remove the fixture's head alarm
        call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)

        call get_alarm(fixture%clock, fixture%alarm_id, found)
        call assert_equal(ierr, 0)
        call assert_false(associated(found))  ! should be gone
    end subroutine


    !> Test removing a non-head alarm (in the middle of the list).
    subroutine test_remove_middle_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: later_time
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Add a second alarm
        call mpas_set_time(later_time, YYYY = 2001, MM = 1, DD = 2, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_add_clock_alarm(fixture%clock, "last", later_time, ierr = ierr)

        ! Remove it
        call mpas_remove_clock_alarm(fixture%clock, fixture%window_alarm_id, ierr = ierr)

        call assert_equal(ierr, 0)
        call assert_false(associated(fixture%clock%alarmListHead%next%next))
    end subroutine


    !> Test removing a nonexistent alarm does nothing.
    subroutine test_remove_nonexistent_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Alarm_type), pointer :: still_there
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Try removing an ID that doesn't exist
        call mpas_remove_clock_alarm(fixture%clock, "does_not_exist", ierr = ierr)

        ! Original alarm should still be present
        call get_alarm(fixture%clock, fixture%alarm_id, still_there)
        call assert_equal(ierr, 0)
        call assert_true(associated(still_there))
    end subroutine


    !> Test removing the last alarm leaves an empty list.
    subroutine test_remove_last_alarm(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! First, remove the head (fixture alarm)
        call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
        call mpas_remove_clock_alarm(fixture%clock, fixture%window_alarm_id, ierr = ierr)

        call assert_equal(ierr, 0)
        call assert_false(associated(fixture%clock%alarmListHead))  ! empty
    end subroutine

    !> Test that mpas_alarm_interval returns the correct interval for a recurring alarm.
    subroutine test_alarm_interval_recurring(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_TimeInterval_type) :: interval
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Retrieve the interval for the fixture's recurring alarm
        interval = mpas_alarm_interval(fixture%clock, fixture%alarm_id, ierr)

        call assert_equal(ierr, 0)
        call assert_true(eq_ti_ti(interval, fixture%alarm_interval))
    end subroutine


    !> Test that mpas_alarm_interval returns zero interval and error for nonexistent alarm.
    subroutine test_alarm_interval_nonexistent(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_TimeInterval_type) :: interval, zero
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_timeInterval(zero, S = 0)

        ! Ask for an ID that doesn't exist
        interval = mpas_alarm_interval(fixture%clock, "not_real", ierr)

        call assert_equal(ierr, 1)
        call assert_true(eq_ti_ti(interval, zero))
    end subroutine


    !> Test that mpas_alarm_interval works when the alarm has start/stop times set.
    subroutine test_alarm_interval_with_start_stop(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: start_time, stop_time, custom_time
        type(MPAS_TimeInterval_type) :: interval
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Define a new alarm with start and stop times
        call mpas_set_time(custom_time, YYYY = 2005, MM = 5, DD = 5, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_set_time(start_time, YYYY = 2005, MM = 5, DD = 6, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_set_time(stop_time, YYYY = 2005, MM = 5, DD = 7, H = 0, M = 0, S = 0, ierr = ierr)

        call mpas_add_clock_alarm(fixture%clock, "start_stop_alarm", custom_time, &
                alarmTimeInterval = fixture%alarm_interval, &
                alarmStartTime = start_time, alarmStopTime = stop_time, ierr = ierr)

        ! Retrieve the interval for this alarm
        interval = mpas_alarm_interval(fixture%clock, "start_stop_alarm", ierr)

        call assert_equal(ierr, 0)
        call assert_true(eq_ti_ti(interval, fixture%alarm_interval))
    end subroutine

    !> Test: minimum interval is zero when no alarms are present
    subroutine test_minimum_alarm_interval_empty(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_TimeInterval_type) :: interval, zero
        type(mpas_stream_list_type), pointer :: empty_list

        call c_f_pointer(test_ptr, fixture)

        ! Create a zero interval for comparison
        call mpas_set_timeInterval(zero, dt = 0.0_RKIND)

        ! Empty list
        nullify(empty_list)
        call mpas_minimum_alarm_interval(fixture%clock, empty_list, interval)

        call assert_true(eq_ti_ti(interval, zero))
    end subroutine


    !> Test: minimum interval picks the smallest among multiple recurring alarms
    subroutine test_minimum_alarm_interval_multiple(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: t1, t2
        type(MPAS_TimeInterval_type) :: short_dt, long_dt, interval
        type(mpas_stream_list_type), pointer :: alarm_list
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Define two different intervals
        call mpas_set_timeInterval(short_dt, dt = 1800.0_RKIND, ierr = ierr)  ! 30 min
        call mpas_set_timeInterval(long_dt, dt = 7200.0_RKIND, ierr = ierr)   ! 2 hr

        ! Create two new alarms with different intervals
        call mpas_set_time(t1, YYYY = 2001, MM = 1, DD = 1, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_set_time(t2, YYYY = 2001, MM = 1, DD = 1, H = 6, M = 0, S = 0, ierr = ierr)
        call mpas_add_clock_alarm(fixture%clock, "short_alarm", t1, alarmTimeInterval = short_dt, ierr = ierr)
        call mpas_add_clock_alarm(fixture%clock, "long_alarm", t2, alarmTimeInterval = long_dt, ierr = ierr)

        ! Build a simple stream list pointing to both alarms
        allocate(alarm_list)
        alarm_list%name = "short_alarm"
        allocate(alarm_list%next)
        alarm_list%next%name = "long_alarm"
        nullify(alarm_list%next%next)

        call mpas_minimum_alarm_interval(fixture%clock, alarm_list, interval)

        call assert_true(eq_ti_ti(interval, short_dt))
    end subroutine


    !> Test: minimum interval works when alarms have start/stop times
    subroutine test_minimum_alarm_interval_with_start_stop(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: t_start, t_stop, t_alarm
        type(MPAS_TimeInterval_type) :: interval, dt
        type(mpas_stream_list_type), pointer :: alarm_list
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Define start/stop times
        call mpas_set_time(t_alarm, YYYY = 2002, MM = 2, DD = 2, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_set_time(t_start, YYYY = 2002, MM = 2, DD = 3, H = 0, M = 0, S = 0, ierr = ierr)
        call mpas_set_time(t_stop, YYYY = 2002, MM = 2, DD = 4, H = 0, M = 0, S = 0, ierr = ierr)

        ! Interval: 1 hour
        call mpas_set_timeInterval(dt, dt = 1800.0_RKIND, ierr = ierr)

        call mpas_add_clock_alarm(fixture%clock, "startstop_alarm", t_alarm, &
                alarmTimeInterval = dt, alarmStartTime = t_start, alarmStopTime = t_stop, ierr = ierr)

        ! Build a stream list with that alarm
        allocate(alarm_list)
        alarm_list%name = "startstop_alarm"
        nullify(alarm_list%next)

        call mpas_minimum_alarm_interval(fixture%clock, alarm_list, interval)

        call assert_true(eq_ti_ti(interval, dt))
    end subroutine

    !> Test: next ring time is prevRingTime + interval in forward direction
    subroutine test_alarm_next_ring_time_forward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: expected, result
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_clock_direction(fixture%clock, MPAS_FORWARD, ierr=ierr)

        expected = fixture%alarm%prevRingTime + fixture%alarm%ringTimeInterval
        result   = mpas_alarm_get_next_ring_time(fixture%clock, fixture%alarm_id)

        call assert_true(eq_t_t(result, expected))
    end subroutine


    !> Test: next ring time is prevRingTime - interval in backward direction
    subroutine test_alarm_next_ring_time_backward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: expected, result
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr=ierr)

        expected = fixture%alarm%prevRingTime - fixture%alarm%ringTimeInterval
        result   = mpas_alarm_get_next_ring_time(fixture%clock, fixture%alarm_id)

        call assert_true(eq_t_t(result, expected))
    end subroutine


    !> Test: at an exact ring time before reset, returns current time
    subroutine test_alarm_next_ring_time_at_ring_before_reset(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: result
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        ! Advance clock until alarm should ring
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr=ierr)
        do i = 1, 1
            call mpas_advance_clock(fixture%clock, ierr=ierr)
        end do

        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr)
        result = mpas_alarm_get_next_ring_time(fixture%clock, fixture%alarm_id)

        ! At ring time, next ring time should equal current clock time
        call assert_true(eq_t_t(result, fixture%current_time))
    end subroutine


    !> Test: at an exact ring time after reset, returns the following ring time
    subroutine test_alarm_next_ring_time_at_ring_after_reset(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: expected, result
        integer :: ierr, i

        call c_f_pointer(test_ptr, fixture)

        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr=ierr)
        ! Advance clock until alarm should ring
        do i = 1, 1
            call mpas_advance_clock(fixture%clock, ierr=ierr)
        end do

        ! Reset alarm at the ring time
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr=ierr)

        ! Now, next ring time should be pushed one interval forward
        expected = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr) + fixture%alarm%ringTimeInterval
        result   = mpas_alarm_get_next_ring_time(fixture%clock, fixture%alarm_id)

        call assert_true(eq_t_t(result, expected))
    end subroutine

    !> Test: mpas_is_alarm_defined returns true for an existing alarm
    subroutine test_alarm_defined_true(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        logical :: result
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        result = mpas_is_alarm_defined(fixture%clock, fixture%alarm_id, ierr)

        call assert_true(result)
        call assert_equal(ierr, 0)
    end subroutine


    !> Test: mpas_is_alarm_defined returns false for a nonexistent alarm
    subroutine test_alarm_defined_false(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        logical :: result
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        result = mpas_is_alarm_defined(fixture%clock, "not_real", ierr)

        call assert_false(result)
        call assert_equal(ierr, 0)
    end subroutine


    !> Test: mpas_alarm_interval returns zero interval for a non-recurring alarm
    subroutine test_alarm_interval_nonrecurring(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: custom_time
        type(MPAS_TimeInterval_type) :: interval, zero
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Add a non-recurring alarm
        call mpas_set_time(custom_time, YYYY=2001, MM=2, DD=1, H=0, M=0, S=0, ierr=ierr)
        call mpas_add_clock_alarm(fixture%clock, "nonrecurring", custom_time, ierr=ierr)

        ! Expected zero interval
        call mpas_set_timeInterval(zero, S=0)
        interval = mpas_alarm_interval(fixture%clock, "nonrecurring", ierr)

        call assert_equal(ierr, 1)
        call assert_true(eq_ti_ti(interval, zero))
    end subroutine

    !> Test: alarm is ringing at its scheduled time
    subroutine test_alarm_ringing_at_ring_time(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Advance one step to ring
        call mpas_advance_clock(fixture%clock, ierr=ierr)

        call assert_true(mpas_is_alarm_ringing(fixture%clock, fixture%alarm_id, ierr=ierr))
    end subroutine


    !> Test: alarm is not ringing before its ring time
    subroutine test_alarm_not_ringing_before_time(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        call assert_false(mpas_is_alarm_ringing(fixture%clock, fixture%window_alarm_id, ierr=ierr))
    end subroutine

    !> Test: ringing alarms list contains the correct alarm
    subroutine test_get_clock_ringing_alarms_basic(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr, n
        character(len=ShortStrKIND) :: list(MPAS_MAX_ALARMS)

        call c_f_pointer(test_ptr, fixture)

        ! Advance into ring
        call mpas_advance_clock(fixture%clock, ierr=ierr)

        call mpas_get_clock_ringing_alarms(fixture%clock, n, list, ierr=ierr)
        call assert_equal(n, 1)
        call assert_true(trim(list(1)) == fixture%alarm_id)
    end subroutine


    !> Test: ringing alarms list empty when no alarm active
    subroutine test_get_clock_ringing_alarms_none(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr, n
        character(len=ShortStrKIND) :: list(MPAS_MAX_ALARMS)

        call c_f_pointer(test_ptr, fixture)

        call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr=ierr)
        call mpas_get_clock_ringing_alarms(fixture%clock, n, list, ierr=ierr)
        call assert_equal(n, 0)
    end subroutine

    !> Test: forward clock disables alarm at stop time
    subroutine test_update_alarm_active_state_forward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr
        type(MPAS_Time_type) :: now

        call c_f_pointer(test_ptr, fixture)

        fixture%current_time = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr=ierr)
        fixture%alarm%stopTime = fixture%current_time  ! 1 hour later
        fixture%alarm%hasStopTime = .true.
        fixture%alarm%isSet = .true.
        call mpas_advance_clock(fixture%clock, ierr=ierr)  ! move to stop time
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr=ierr)
        now = mpas_get_clock_time(fixture%clock, MPAS_NOW, ierr=ierr)

        call mpas_update_alarm_active_state(fixture%clock, fixture%alarm, now)
        call assert_false(fixture%alarm%isSet)
    end subroutine


    !> Test: backward clock re-enables alarm at stop time
    subroutine test_update_alarm_active_state_backward(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        integer :: ierr
        type(MPAS_Time_type) :: now

        call c_f_pointer(test_ptr, fixture)

        call mpas_set_clock_direction(fixture%clock, MPAS_BACKWARD, ierr=ierr)
        fixture%alarm%hasStopTime = .true.
        now = fixture%alarm%stopTime

        fixture%alarm%isSet = .false.
        call mpas_update_alarm_active_state(fixture%clock, fixture%alarm, now)
        call assert_true(fixture%alarm%isSet)
    end subroutine

    !> Test: reset disables non-recurring alarm after first ring
    subroutine test_reset_clock_alarm_nonrecurring(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: custom_time
        type(MPAS_Alarm_type), pointer :: single_alarm
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        ! Add a non-recurring alarm
        call mpas_set_time(custom_time, YYYY=2001, MM=1, DD=1, H=0, M=0, S=0, ierr=ierr)
        call mpas_add_clock_alarm(fixture%clock, "single", custom_time, ierr=ierr)

        ! Advance to the ring and reset
        call mpas_set_clock_time(fixture%clock, custom_time, MPAS_NOW, ierr=ierr)
        call mpas_reset_clock_alarm(fixture%clock, "single", ierr=ierr)
        call get_alarm(fixture%clock, "single", single_alarm)
        call assert_false(single_alarm%isSet)  ! should be disabled after ringing
    end subroutine


    !> Test: reset recurring alarm updates prevRingTime forward
    subroutine test_reset_clock_alarm_recurring(test_ptr, ts_ptr, s_ptr)
        use iso_c_binding, only: c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: test_ptr, ts_ptr, s_ptr
        type(alarm_fixture_t), pointer :: fixture
        type(MPAS_Time_type) :: before, after
        integer :: ierr

        call c_f_pointer(test_ptr, fixture)

        before = fixture%alarm%prevRingTime

        call mpas_advance_clock(fixture%clock, ierr=ierr)
        call mpas_reset_clock_alarm(fixture%clock, fixture%alarm_id, ierr=ierr)

        after = fixture%alarm%prevRingTime
        call assert_true(after > before)
    end subroutine





end module test_alarm_mod

!> Main program to test the alarm functionality.
!! This program sets up and runs a suite of tests for verifying the behavior of the alarm module.
!! It registers the necessary fixtures and test cases, and executes the test suite.
program test_alarm
    use fortest_test_session, only : test_session_t
    use test_alarm_mod, only : test_mpas_is_clock_start_time, test_mpas_is_clock_stop_time, &
            test_mpas_set_clock_direction_backward, test_mpas_set_clock_direction_forward, &
            test_mpas_set_clock_time_step, &
            test_mpas_advance_clock_default, test_mpas_advance_clock_with_custom_step, &
            test_mpas_set_clock_time_now, test_mpas_set_clock_time_start, test_mpas_set_clock_time_stop, &
            test_add_non_recurring_alarm, test_add_recurring_alarm, test_add_alarm_with_start_stop, &
            test_add_duplicate_adjacent_alarm, test_add_duplicate_nonadjacent_alarm, &
            test_remove_head_alarm, test_remove_middle_alarm, test_remove_nonexistent_alarm, &
            test_remove_last_alarm, &
            test_alarm_interval_recurring, test_alarm_interval_nonexistent, &
            test_alarm_interval_with_start_stop, &
            test_minimum_alarm_interval_empty, test_minimum_alarm_interval_multiple, &
            test_minimum_alarm_interval_with_start_stop, &
            test_alarm_next_ring_time_forward, test_alarm_next_ring_time_backward, &
            test_alarm_next_ring_time_at_ring_before_reset, test_alarm_next_ring_time_at_ring_after_reset, &
            test_alarm_defined_true, test_alarm_defined_false, &
            test_alarm_interval_nonrecurring, test_alarm_interval_recurring, test_alarm_interval_nonexistent, &
            test_alarm_ringing_at_ring_time, test_alarm_not_ringing_before_time, &
            test_get_clock_ringing_alarms_basic, test_get_clock_ringing_alarms_none, &
            test_update_alarm_active_state_forward, test_update_alarm_active_state_backward, &
            test_reset_clock_alarm_nonrecurring, test_reset_clock_alarm_recurring
    use alarm_fixture_mod, only : &
            alarm_fixture_t, &
            setup_alarm, &
            teardown_alarm
    use session_fixture_mod, only : &
            session_fixture_t, &
            setup_session, &
            teardown_session
    use iso_c_binding, only : c_loc, c_ptr
    implicit none

    ! Declare variables for the test session and fixtures.
    type(test_session_t) :: session                     ! Test session object.
    type(session_fixture_t), target :: session_fixture  ! Session-level fixture.
    type(alarm_fixture_t), target :: test_fixture       ! Test-level fixture.
    type(c_ptr) :: session_fixture_ptr, test_fixture_ptr ! C pointers to the fixtures.

    ! Point C interoperable pointers at fixtures.
    session_fixture_ptr = c_loc(session_fixture)
    test_fixture_ptr = c_loc(test_fixture)

    ! Register the test suite and its associated fixtures.
    call session%register_test_suite("alarm_test")  ! Register the test suite named "alarm_test".
    !
    call session%register_fixture(&
            setup = setup_session, &                ! Setup routine for the session fixture.
            teardown = teardown_session, &          ! Teardown routine for the session fixture.
            args = session_fixture_ptr, &           ! Pointer to the session fixture.
            scope = "session")                      ! Scope of the fixture (session-level).
    !
    call session%register_fixture(&
            test_suite_name = "alarm_test", &       ! Associate the fixture with the "alarm_test" suite.
            setup = setup_alarm, &                 ! Setup routine for the test fixture.
            teardown = teardown_alarm, &           ! Teardown routine for the test fixture.
            args = test_fixture_ptr, &             ! Pointer to the test fixture.
            scope = "test")                        ! Scope of the fixture (test-level).

    ! Register all test cases in the suite.
    call session%register_test("alarm_test", "test_mpas_is_clock_start_time", test_mpas_is_clock_start_time)
    call session%register_test("alarm_test", "test_mpas_is_clock_stop_time", test_mpas_is_clock_stop_time)
    call session%register_test("alarm_test", "test_mpas_set_clock_direction", test_mpas_set_clock_direction_forward)
    call session%register_test("alarm_test", "test_mpas_set_clock_direction_backward", test_mpas_set_clock_direction_backward)
    call session%register_test("alarm_test", "test_mpas_set_clock_time_step", test_mpas_set_clock_time_step)
    call session%register_test("alarm_test", "test_mpas_advance_clock_default", test_mpas_advance_clock_default)
    call session%register_test("alarm_test", "test_mpas_advance_clock_with_custom_step", test_mpas_advance_clock_with_custom_step)
    call session%register_test("alarm_test", "test_mpas_set_clock_time_now", test_mpas_set_clock_time_now)
    call session%register_test("alarm_test", "test_mpas_set_clock_time_start", &
            test_mpas_set_clock_time_start)
    call session%register_test("alarm_test", "test_mpas_set_clock_time_stop", &
            test_mpas_set_clock_time_stop)
    call session%register_test("alarm_test", "test_add_non_recurring_alarm", &
            test_add_non_recurring_alarm)
    call session%register_test("alarm_test", "test_add_recurring_alarm", &
            test_add_recurring_alarm)
    call session%register_test("alarm_test", "test_add_alarm_with_start_stop", &
            test_add_alarm_with_start_stop)
    call session%register_test("alarm_test", "test_remove_head_alarm", &
            test_remove_head_alarm)
    call session%register_test("alarm_test", "test_remove_middle_alarm", &
            test_remove_middle_alarm)
    call session%register_test("alarm_test", "test_remove_nonexistent_alarm", &
            test_remove_nonexistent_alarm)
    call session%register_test("alarm_test", "test_remove_last_alarm", &
            test_remove_last_alarm)
    call session%register_test("alarm_test", "test_add_duplicate_adjacent_alarm", &
            test_add_duplicate_adjacent_alarm)
    call session%register_test("alarm_test", "test_add_duplicate_nonadjacent_alarm", &
            test_add_duplicate_nonadjacent_alarm)
    call session%register_test("alarm_test", "test_alarm_interval_recurring", &
            test_alarm_interval_recurring)
    call session%register_test("alarm_test", "test_alarm_interval_nonexistent", &
            test_alarm_interval_nonexistent)
    call session%register_test("alarm_test", "test_alarm_interval_with_start_stop", &
            test_alarm_interval_with_start_stop)
    call session%register_test("alarm_test", "test_minimum_alarm_interval_empty", &
            test_minimum_alarm_interval_empty)
    call session%register_test("alarm_test", "test_minimum_alarm_interval_multiple", &
            test_minimum_alarm_interval_multiple)
    call session%register_test("alarm_test", "test_minimum_alarm_interval_with_start_stop", &
            test_minimum_alarm_interval_with_start_stop)
    call session%register_test("alarm_test", "test_alarm_next_ring_time_forward", &
            test_alarm_next_ring_time_forward)
    call session%register_test("alarm_test", "test_alarm_next_ring_time_backward", &
            test_alarm_next_ring_time_backward)
    call session%register_test("alarm_test", "test_alarm_next_ring_time_at_ring_before_reset", &
            test_alarm_next_ring_time_at_ring_before_reset)
    call session%register_test("alarm_test", "test_alarm_next_ring_time_at_ring_after_reset", &
            test_alarm_next_ring_time_at_ring_after_reset)
    call session%register_test("alarm_test", "test_alarm_defined_true", &
            test_alarm_defined_true)
    call session%register_test("alarm_test", "test_alarm_defined_false", &
            test_alarm_defined_false)
    call session%register_test("alarm_test", "test_alarm_interval_recurring", &
            test_alarm_interval_recurring)
    call session%register_test("alarm_test", "test_alarm_interval_nonexistent", &
            test_alarm_interval_nonexistent)
    call session%register_test("alarm_test", "test_alarm_interval_nonrecurring", &
            test_alarm_interval_nonrecurring)
    call session%register_test("alarm_test", "test_alarm_ringing_at_ring_time", &
            test_alarm_ringing_at_ring_time)
    call session%register_test("alarm_test", "test_alarm_not_ringing_before_time", &
            test_alarm_not_ringing_before_time)
    call session%register_test("alarm_test", "test_get_clock_ringing_alarms_basic", &
            test_get_clock_ringing_alarms_basic)
    call session%register_test("alarm_test", "test_get_clock_ringing_alarms_none", &
            test_get_clock_ringing_alarms_none)
    call session%register_test("alarm_test", "test_update_alarm_active_state_forward", &
            test_update_alarm_active_state_forward)
    call session%register_test("alarm_test", "test_update_alarm_active_state_backward", &
            test_update_alarm_active_state_backward)
    call session%register_test("alarm_test", "test_reset_clock_alarm_nonrecurring", &
            test_reset_clock_alarm_nonrecurring)
    call session%register_test("alarm_test", "test_reset_clock_alarm_recurring", &
            test_reset_clock_alarm_recurring)

    ! Run the test suite and finalize the session.
    call session%run()
    call session%finalize()
end program test_alarm