module test_clock_alarm_mod
   use mpi
   use test_suite_mod
   use assert_mod
   use iso_c_binding, only: c_ptr, c_f_pointer
   use mpas_subdriver
   use mpas_timekeeping
   use mpas_derived_types, only: core_type, domain_type
   use test_clock_alarm_fixture_mod
   implicit none
contains

   subroutine start_stop_time_test1(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t

      call c_f_pointer(t_ptr, t)
      call assert_true(t%alarm % hasStartTime)
      call assert_true(t%alarm % hasStopTime)
   end subroutine start_stop_time_test1

   subroutine start_stop_time_test2(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: hour, ierr

      call c_f_pointer(t_ptr, t)
      do hour = 1, t%numHours
         call assert_false(mpas_is_alarm_ringing(t%clock, t%alarmStartStopID, ierr = ierr))
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
   end subroutine start_stop_time_test2

   subroutine start_stop_time_test3(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: hour, ierr

      call c_f_pointer(t_ptr, t)
      do hour = 1, t%numHours
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
      t%currentTime = mpas_get_clock_time(t%clock, MPAS_NOW, ierr = ierr)
      call assert_true(eq_t_t(t%currentTime, t%alarmStartTime))
   end subroutine start_stop_time_test3

   subroutine start_stop_time_test4(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: hour, ierr

      call c_f_pointer(t_ptr, t)
      do hour = 1, t%numHours
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
      do hour = 1, t%numHours
         call assert_true(mpas_is_alarm_ringing(t%clock, t%alarmStartStopID, ierr = ierr))
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
   end subroutine start_stop_time_test4

   subroutine start_stop_time_test5(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: hour, ierr

      call c_f_pointer(t_ptr, t)
      do hour = 1, t%numHours
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
      do hour = 1, t%numHours
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
      call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
      call mpas_advance_clock(t%clock, ierr = ierr)
      do hour = 1, t%numHours
         call assert_false(mpas_is_alarm_ringing(t%clock, t%alarmStartStopID, ierr = ierr))
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
   end subroutine start_stop_time_test5

   subroutine test_set_clock_direction(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: hour, ierr

      call c_f_pointer(t_ptr, t)
      call assert_equal(mpas_get_clock_direction(t%clock, ierr = ierr), MPAS_FORWARD)
      call mpas_set_clock_direction(t%clock, MPAS_BACKWARD, ierr = ierr)
      call assert_equal(mpas_get_clock_direction(t%clock, ierr = ierr), MPAS_BACKWARD)
   end subroutine


   !=============================================================
   ! Test: prevRingTime updates correctly in forward direction
   !=============================================================
   subroutine test_prev_ring_time_forward(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: hour, ierr

      call c_f_pointer(t_ptr, t)

      ! Advance forward through numHours twice
      do hour = 1, 2 * t%numHours
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do

      ! Verify prevRingTime is one interval behind current time
      call assert_true(eq_t_t(t%alarm%prevRingTime, &
            mpas_get_clock_time(t%clock, MPAS_NOW, ierr = ierr) - &
                  t%alarm%ringTimeInterval))
   end subroutine


   !=============================================================
   ! Test: prevRingTime flips correctly when clock goes backward
   !=============================================================
   subroutine test_prev_ring_time_backward(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: ierr

      call c_f_pointer(t_ptr, t)

      call mpas_set_clock_direction(t%clock, MPAS_BACKWARD, ierr = ierr)

      ! In backward mode, prevRingTime should be current + interval
      call assert_true(eq_t_t(t%alarm%prevRingTime, &
            mpas_get_clock_time(t%clock, MPAS_NOW, ierr = ierr) + &
                  t%alarm%ringTimeInterval))
   end subroutine


   !=============================================================
   ! Test: prevRingTime after stepping one tick backwards
   !=============================================================
   subroutine test_prev_ring_time_backward_step(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: ierr

      call c_f_pointer(t_ptr, t)

      call mpas_set_clock_direction(t%clock, MPAS_BACKWARD, ierr = ierr)
      call mpas_advance_clock(t%clock, ierr = ierr)

      call assert_true(eq_t_t(t%alarm%prevRingTime, &
            mpas_get_clock_time(t%clock, MPAS_NOW, ierr = ierr) - &
                  mpas_get_clock_timestep(t%clock) + &
                  t%alarm%ringTimeInterval))
   end subroutine


   !=============================================================
   ! Test: prevRingTime resets to now and is idempotent
   !=============================================================
   subroutine test_prev_ring_time_reset(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: ierr, i

      call c_f_pointer(t_ptr, t)

      call assert_true(eq_t_t(t%alarm%prevRingTime, t%alarmTime - t%alarm%ringTimeInterval))
      do i = 1, 2 * t%numHours
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
      call assert_true(eq_t_t(t%alarm%prevRingTime, t%alarmTime - t%alarm%ringTimeInterval))
      call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
      call assert_true(eq_t_t(t%alarm%prevRingTime, mpas_get_clock_time(t%clock, MPAS_NOW, ierr = ierr)))

   end subroutine

   subroutine test_alarm_reactivates_when_reversed(t_ptr, ts_ptr, s_ptr)
      implicit none
      type(c_ptr), value :: t_ptr, ts_ptr, s_ptr
      type(clock_alarm_test_fixture_t), pointer :: t
      integer :: ierr, i

      call c_f_pointer(t_ptr, t)
      do i = 1, 2 * t%numHours
         call mpas_reset_clock_alarm(t%clock, t%alarmStartStopID, ierr = ierr)
         call mpas_advance_clock(t%clock, ierr = ierr)
      end do
      call assert_true(mpas_is_alarm_ringing(t%clock, t%alarmStartStopID, ierr = ierr))
   end subroutine

end module test_clock_alarm_mod


program test_clock_alarm
   use test_clock_alarm_mod
   use test_clock_alarm_fixture_mod
   use iso_c_binding, only: c_loc, c_ptr
   implicit none
   type(test_suite_t) :: clock_alarm_suite
   type(clock_alarm_suite_fixture_t), target :: clock_alarm_suite_fixture
   type(clock_alarm_test_fixture_t), target :: clock_alarm_test_fixture
   type(c_ptr) :: clock_alarm_suite_fixture_ptr, clock_alarm_test_fixture_ptr

   clock_alarm_suite_fixture_ptr = c_loc(clock_alarm_suite_fixture)
   clock_alarm_test_fixture_ptr = c_loc(clock_alarm_test_fixture)

   clock_alarm_suite = test_suite_t(name = "test_clock_alarm")
   call clock_alarm_suite%register_fixture(&
         name = "clock_alarm_suite_fixture", &
         setup = setup_clock_alarm_suite, &
         teardown = teardown_clock_alarm_suite, &
         args = clock_alarm_suite_fixture_ptr, &
         scope = "suite")
   call clock_alarm_suite%register_fixture(&
         name = "clock_alarm_test_fixture", &
         setup = setup_clock_alarm_test, &
         teardown = teardown_clock_alarm_test, &
         args = clock_alarm_test_fixture_ptr, &
         scope = "test")
   call clock_alarm_suite%register_test(&
         name = "start_stop_time_test1", &
         test = start_stop_time_test1)
   call clock_alarm_suite%register_test(&
         name = "start_stop_time_test2", &
         test = start_stop_time_test2)
   call clock_alarm_suite%register_test(&
         name = "start_stop_time_test3", &
         test = start_stop_time_test3)
   call clock_alarm_suite%register_test(&
         name = "start_stop_time_test4", &
         test = start_stop_time_test4)
   call clock_alarm_suite%register_test(&
         name = "start_stop_time_test5", &
         test = start_stop_time_test5)
   call clock_alarm_suite%register_test(&
         name = "test_set_clock_direction", &
         test = test_set_clock_direction)
   call clock_alarm_suite%register_test(&
         name = "test_prev_ring_time_forward", &
         test = test_prev_ring_time_forward)
   call clock_alarm_suite%register_test(&
         name = "test_prev_ring_time_backward", &
         test = test_prev_ring_time_backward)
   call clock_alarm_suite%register_test(&
         name = "test_prev_ring_time_backward_step", &
         test = test_prev_ring_time_backward_step)
   call clock_alarm_suite%register_test(&
         name = "test_prev_ring_time_reset", &
         test = test_prev_ring_time_reset)
   call clock_alarm_suite%register_test(&
         name = "test_alarm_reactivates_when_reversed", &
         test = test_alarm_reactivates_when_reversed)

   call clock_alarm_suite%run()
end program test_clock_alarm
