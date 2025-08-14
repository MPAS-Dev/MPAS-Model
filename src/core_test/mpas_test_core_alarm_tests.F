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

   subroutine mpas_alarm_tests(domain, ierr)

      use mpas_derived_types, only : domain_type, MPAS_Clock_type, MPAS_Time_type, MPAS_TimeInterval_type
      use mpas_kind_types, only : StrKIND
      use mpas_log, only : mpas_log_write
      use mpas_timekeeping, only : mpas_set_time, mpas_set_timeInterval, mpas_create_clock, &
              mpas_add_clock_alarm, mpas_is_alarm_ringing, mpas_reset_clock_alarm

      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: istep
      integer :: ierr_local
      character(len=StrKIND) :: test_mesg
      type (MPAS_Clock_type) :: test_clock
      type (MPAS_Time_type) :: test_startTime
      type (MPAS_Time_type) :: test_stopTime
      type (MPAS_Time_type) :: test_currTime
      type (MPAS_Time_type) :: test_alarmTime
      type (MPAS_Time_type) :: test_refTime
      type (MPAS_TimeInterval_type) :: test_timeStep
      type (MPAS_TimeInterval_type) :: test_alarmTimeInterval

      ierr = 0

      call mpas_set_time(test_startTime, YYYY=2000, MM=01, DD=01, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_set_time(test_stopTime, YYYY=2100, MM=01, DD=01, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_set_timeInterval(test_timeStep, dt=3600.0_RKIND, ierr=ierr_local)
      call mpas_create_clock(test_clock, test_startTime, test_timeStep, test_stopTime, ierr=ierr_local)

      call mpas_set_time(test_alarmTime, YYYY=2000, MM=01, DD=01, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_set_timeInterval(test_alarmTimeInterval, dt=86400.0_RKIND, ierr=ierr_local)
      call mpas_add_clock_alarm(test_clock, 'test', test_alarmTime, test_alarmTimeInterval, ierr_local)

      call mpas_advance_clock(test_clock, ierr=ierr_local)
      if (mpas_is_alarm_ringing(test_clock, 'test', ierr=ierr_local)) then
         test_mesg = 'Alarm should not be ringing after first advance'
         call mpas_log_write(test_mesg, err=ierr_local)
         ierr = ierr + ierr_local
      end if



   end subroutine mpas_alarm_tests

end module test_core_alarm_tests_mod
