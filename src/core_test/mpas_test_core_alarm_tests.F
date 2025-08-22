! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!

module test_core_alarm_tests_mod

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_field_routines
   use mpas_dmpar
   use mpas_threading
   use mpas_log
   use mpas_timer
   use mpas_timekeeping
   use mpas_derived_types, only: domain_type, MPAS_Clock_type, MPAS_Time_type, MPAS_TimeInterval_type
   use mpas_kind_types, only: StrKIND
   use mpas_log, only: mpas_log_write
   use mpas_timekeeping, only: mpas_set_time, mpas_set_timeInterval, mpas_create_clock, &
         mpas_add_clock_alarm, mpas_is_alarm_ringing, mpas_reset_clock_alarm
   use mpas_stream_manager
   use mpas_stream_list

   implicit none
   public

contains

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine setup(domain, clock, ierr)
      implicit none

      type (domain_type), intent(inout) :: domain
      type (MPAS_Clock_type), intent(out) :: clock
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Time_type) :: test_startTime
      type (MPAS_Time_type) :: test_stopTime
      type (MPAS_Time_type) :: test_alarmTime
      type (MPAS_Time_type) :: test_alarmStopTime
      type (MPAS_TimeInterval_type) :: test_timeStep
      type (MPAS_TimeInterval_type) :: test_alarmTimeInterval

      ierr = 0

      call mpas_set_time(test_startTime, YYYY = 2000, MM = 01, DD = 01, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_time(test_stopTime, YYYY = 2100, MM = 01, DD = 01, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_timeInterval(test_timeStep, dt = 1800.0_RKIND, ierr = ierr_local)
      call mpas_create_clock(clock, test_startTime, test_timeStep, test_stopTime, ierr = ierr_local)

      call mpas_set_time(test_alarmTime, YYYY = 2000, MM = 01, DD = 01, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_timeInterval(test_alarmTimeInterval, dt = 3600.0_RKIND, ierr = ierr_local)
      call mpas_add_clock_alarm(clock, 'test', test_alarmTime, &
            alarmTimeInterval = test_alarmTimeInterval, ierr = ierr_local)
      call mpas_reset_clock_alarm(clock, 'test', ierr = ierr_local)
   end subroutine setup

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine teardown(clock, ierr)
      implicit none

      type (MPAS_Clock_type), intent(inout) :: clock
      integer, intent(out) :: ierr

      integer :: ierr_local

      ierr = 0
      call mpas_remove_clock_alarm(clock, 'test', ierr = ierr_local)
      call mpas_remove_clock_alarm(clock, 'test_stop_time', ierr = ierr_local)
   end subroutine teardown

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_alarm_rings_at_interval(domain, ierr)
      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Clock_type) :: test_clock
      integer :: i

      ierr = 0
      call setup(domain, test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      if (.not. mpas_is_alarm_ringing(test_clock, 'test', ierr = ierr_local)) then
         ierr = ierr + 1
      end if
      call mpas_reset_clock_alarm(test_clock, 'test', ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      if (mpas_is_alarm_ringing(test_clock, 'test', ierr = ierr_local)) then
         ierr = ierr + 1
      end if
      call teardown(test_clock, ierr = ierr_local)
   end subroutine mpas_test_alarm_rings_at_interval

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_alarm_rings_constantly_when_not_reset(domain, ierr)
      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Clock_type) :: test_clock
      integer :: i

      ierr = 0
      call setup(domain, test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      if (.not. mpas_is_alarm_ringing(test_clock, 'test', ierr = ierr_local)) then
         ierr = ierr + 1
      end if
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      if (.not. mpas_is_alarm_ringing(test_clock, 'test', ierr = ierr_local)) then
         ierr = ierr + 1
      end if
      call teardown(test_clock, ierr = ierr_local)
   end subroutine mpas_test_alarm_rings_constantly_when_not_reset

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_get_next_ring_time_for_ringing_alarm(domain, ierr)
      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Clock_type) :: test_clock
      type (MPAS_Time_type) :: next_ring_time, next_ring_time_ref
      integer :: i

      ierr = 0
      call mpas_set_time(next_ring_time_ref, YYYY = 2000, MM = 01, DD = 01, H = 1, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call setup(domain, test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      next_ring_time = mpas_alarm_get_next_ring_time(test_clock, 'test')
      if (.not. eq_t_t(next_ring_time_ref, next_ring_time)) then
         ierr = ierr + 1
      end if
      call teardown(test_clock, ierr = ierr_local)
   end subroutine mpas_test_get_next_ring_time_for_ringing_alarm

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_get_next_ring_time_for_alarm(domain, ierr)
      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Clock_type) :: test_clock
      type (MPAS_Time_type) :: next_ring_time, next_ring_time_ref
      integer :: i

      ierr = 0
      call mpas_set_time(next_ring_time_ref, YYYY = 2000, MM = 01, DD = 01, H = 2, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call setup(domain, test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_reset_clock_alarm(test_clock, 'test')
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      next_ring_time = mpas_alarm_get_next_ring_time(test_clock, 'test')
      if (.not. eq_t_t(next_ring_time_ref, next_ring_time)) then
         ierr = ierr + 1
      end if
      call teardown(test_clock, ierr = ierr_local)
   end subroutine mpas_test_get_next_ring_time_for_alarm

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_minimum_alarm_interval(domain, ierr)
      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Clock_type) :: test_clock
      type (MPAS_TimeInterval_type) :: minimum_alarm_interval_ref, minimum_alarm_interval
      character(len = :), allocatable :: stream_id, filename
      type (mpas_stream_list_type), pointer :: stream_cursor, alarm_cursor
      logical :: stream_exists
      integer :: i
      ierr = 0

      stream_id = 'test'
      filename = 'test.nc'
      call mpas_set_timeInterval(minimum_alarm_interval_ref, YY = 0, MM = 0, DD = 0, H = 1, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call setup(domain, domain%streamManager%streamClock, ierr = ierr_local)
      call MPAS_stream_mgr_create_stream(domain%streamManager, &
            stream_id, MPAS_STREAM_OUTPUT, filename, ierr = ierr_local)
      call MPAS_stream_mgr_add_alarm(domain%streamManager, stream_id, 'test', &
            MPAS_STREAM_OUTPUT, ierr = ierr_local)
      stream_exists = mpas_stream_list_query(domain%streamManager % streams, stream_id, stream_cursor)
      alarm_cursor => stream_cursor % alarmList_out % head
      call mpas_minimum_alarm_interval(domain%streamManager%streamClock, alarm_cursor, minimum_alarm_interval)
      if (.not. eq_ti_ti(minimum_alarm_interval_ref, minimum_alarm_interval)) then
         ierr = ierr + 1
      end if
      call MPAS_stream_mgr_destroy_stream(domain%streamManager, &
            stream_id, ierr = ierr_local)
      call teardown(test_clock, ierr = ierr_local)
   end subroutine

   !**************************************************************************************
   !--------------------------------------------------------------------------------------
   subroutine mpas_alarm_tests(domain, ierr)
      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr
      integer :: err

      ierr = 0

      call mpas_log_write('Testing MPAS alarm routines:')

      call mpas_test_alarm_rings_at_interval(domain, ierr = err)
      if (err == 0) then
         call mpas_log_write('   mpas_test_alarm_rings_at_interval: SUCCESS')
      else
         ierr = ierr + 1
         call mpas_log_write('   mpas_test_alarm_rings_at_interval: FAILURE')
      end if

      call mpas_test_alarm_rings_constantly_when_not_reset(domain, ierr = err)
      if (err == 0) then
         call mpas_log_write('   mpas_test_alarm_rings_constantly_when_not_reset: SUCCESS')
      else
         ierr = ierr + 1
         call mpas_log_write('   mpas_test_alarm_rings_constantly_when_not_reset: FAILURE')
      end if

      call mpas_test_get_next_ring_time_for_ringing_alarm(domain, ierr = err)
      if (err == 0) then
         call mpas_log_write('   mpas_test_get_next_ring_time_for_ringing_alarm: SUCCESS')
      else
         ierr = ierr + 1
         call mpas_log_write('   mpas_test_get_next_ring_time_for_ringing_alarm: FAILURE')
      end if

      call mpas_test_get_next_ring_time_for_alarm(domain, ierr = err)
      if (err == 0) then
         call mpas_log_write('   mpas_test_get_next_ring_time_for_alarm: SUCCESS')
      else
         ierr = ierr + 1
         call mpas_log_write('   mpas_test_get_next_ring_time_for_alarm: FAILURE')
      end if

      call mpas_test_minimum_alarm_interval(domain, ierr = err)
      if (err == 0) then
         call mpas_log_write('   mpas_test_minimum_alarm_interval: SUCCESS')
      else
         ierr = ierr + 1
         call mpas_log_write('   mpas_test_minimum_alarm_interval: FAILURE')
      end if

   end subroutine mpas_alarm_tests

end module test_core_alarm_tests_mod

