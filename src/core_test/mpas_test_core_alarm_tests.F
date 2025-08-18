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

   implicit none
   public

contains

   subroutine setup(domain, clock, ierr)
      use mpas_derived_types, only: domain_type, MPAS_Clock_type, MPAS_Time_type, MPAS_TimeInterval_type
      use mpas_kind_types, only: StrKIND
      use mpas_log, only: mpas_log_write
      use mpas_timekeeping, only: mpas_set_time, mpas_set_timeInterval, mpas_create_clock, &
            mpas_add_clock_alarm, mpas_is_alarm_ringing, mpas_reset_clock_alarm

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

      call mpas_set_time(test_startTime, YYYY = 2000, MM = 01, DD = 01, H = 0, M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_time(test_stopTime, YYYY = 2100, MM = 01, DD = 01, H = 0, M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_timeInterval(test_timeStep, dt = 1800.0_RKIND, ierr = ierr_local)
      call mpas_create_clock(clock, test_startTime, test_timeStep, test_stopTime, ierr = ierr_local)

      call mpas_set_time(test_alarmTime, YYYY = 2000, MM = 01, DD = 01, H = 0, M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_time(test_alarmStopTime, YYYY = 2000, MM = 01, DD = 01, H = 2, M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_timeInterval(test_alarmTimeInterval, dt = 3600.0_RKIND, ierr = ierr_local)
      call mpas_add_clock_alarm(clock, 'test', test_alarmTime, &
            alarmTimeInterval = test_alarmTimeInterval, ierr = ierr_local)
      call mpas_add_clock_alarm(clock, 'test_stop_time', test_alarmTime, &
            alarmTimeInterval = test_alarmTimeInterval, alarmStopTime = test_alarmStopTime, ierr = ierr_local)
      call mpas_reset_clock_alarm(clock, 'test', ierr = ierr_local)
      call mpas_reset_clock_alarm(clock, 'test_stop_time', ierr = ierr_local)
   end subroutine setup

   subroutine teardown(clock, ierr)
      use mpas_derived_types, only: MPAS_Clock_type
      use mpas_timekeeping, only: mpas_remove_clock_alarm

      implicit none

      type (MPAS_Clock_type), intent(inout) :: clock
      integer, intent(out) :: ierr

      integer :: ierr_local

      ierr = 0
      call mpas_remove_clock_alarm(clock, 'test', ierr = ierr_local)
      call mpas_remove_clock_alarm(clock, 'test_stop_time', ierr = ierr_local)
   end subroutine teardown

   subroutine mpas_test_alarm_rings_at_interval(domain, ierr)

      use mpas_derived_types, only: domain_type, MPAS_Clock_type
      use mpas_timekeeping, only: mpas_reset_clock_alarm, mpas_advance_clock, mpas_is_alarm_ringing

      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Clock_type) :: test_clock
      integer :: i

      ierr = 0
      call setup(domain, test_clock, ierr = ierr_local)
      do i = 1, 12
         call mpas_advance_clock(test_clock, ierr = ierr_local)
         if (mod(i, 2) == 0) then
            if (.not. mpas_is_alarm_ringing(test_clock, 'test', ierr = ierr_local)) then
               ierr = ierr + 1
            end if
         else
            if (mpas_is_alarm_ringing(test_clock, 'test', ierr = ierr_local)) then
               ierr = ierr + 1
            end if
         end if
         call mpas_reset_clock_alarm(test_clock, 'test', ierr = ierr_local)
      end do
      call teardown(test_clock, ierr = ierr_local)
   end subroutine mpas_test_alarm_rings_at_interval

   subroutine mpas_test_alarm_rings_constantly_when_not_reset(domain, ierr)

      use mpas_derived_types, only: domain_type, MPAS_Clock_type
      use mpas_timekeeping, only: mpas_reset_clock_alarm, mpas_advance_clock, mpas_is_alarm_ringing

      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: ierr_local
      type (MPAS_Clock_type) :: test_clock
      integer :: i

      ierr = 0
      call setup(domain, test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      do i = 1, 12
         call mpas_advance_clock(test_clock, ierr = ierr_local)
         if (.not. mpas_is_alarm_ringing(test_clock, 'test', ierr = ierr_local)) then
            ierr = ierr + 1
         end if
         call mpas_advance_clock(test_clock, ierr = ierr_local)
      end do
      call teardown(test_clock, ierr = ierr_local)
   end subroutine mpas_test_alarm_rings_constantly_when_not_reset

   subroutine mpas_test_alarm_rings_before_stop_time(domain, ierr)

      use mpas_derived_types, only: domain_type, MPAS_Clock_type
      use mpas_timekeeping, only: mpas_reset_clock_alarm, mpas_advance_clock, mpas_is_alarm_ringing

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
      if (.not. mpas_is_alarm_ringing(test_clock, 'test_stop_time', ierr = ierr_local)) then
         ierr = ierr + 1
      end if
      call mpas_reset_clock_alarm(test_clock, 'test_stop_time', ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      call mpas_advance_clock(test_clock, ierr = ierr_local)
      if (.not. mpas_is_alarm_ringing(test_clock, 'test_stop_time', ierr = ierr_local)) then
         ierr = ierr + 1
      end if
      call mpas_reset_clock_alarm(test_clock, 'test_stop_time', ierr = ierr_local)
      call teardown(test_clock, ierr = ierr_local)
   end subroutine mpas_test_alarm_rings_before_stop_time

   subroutine mpas_alarm_tests(domain, ierr)

      use mpas_derived_types, only: domain_type
      use mpas_log, only: mpas_log_write

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

      call mpas_test_alarm_rings_before_stop_time(domain, ierr = err)
      if (err == 0) then
         call mpas_log_write('   mpas_test_alarm_rings_before_stop_time: SUCCESS')
      else
         ierr = ierr + 1
         call mpas_log_write('   mpas_test_alarm_rings_before_stop_time: FAILURE')
      end if

   end subroutine mpas_alarm_tests

end module test_core_alarm_tests_mod

