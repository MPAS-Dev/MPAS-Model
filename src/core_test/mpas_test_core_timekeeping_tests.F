! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!

#define MPAS_ADJUST_ALARM_VERBOSE( M ) ! M

module test_core_timekeeping_tests

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_field_routines
   use mpas_dmpar
   use mpas_threading
   use mpas_log
   use mpas_timer
   use mpas_timekeeping

   implicit none
   private

   public :: test_core_test_intervals, &
             mpas_adjust_alarm_tests

   contains

   !***********************************************************************
   !
   !  routine test_core_test_intervals
   !
   !> \brief   MPAS Test Core timekeeping interval tests
   !> \author  Doug Jacobsen
   !> \date    08/23/2016
   !> \details 
   !>  This routine performs tests related to timekeeping intervals
   !
   !-----------------------------------------------------------------------
   subroutine test_core_test_intervals(domain, threadErrs, err)!{{{

      type (domain_type), intent(inout) :: domain
      integer, dimension(:), intent(out) :: threadErrs
      integer, intent(out) :: err

      character (len=StrKIND) :: int1_str, int2_str
      character (len=StrKIND) :: ref_str

      integer :: threadNum
      integer :: iErr, err_tmp
      integer :: divs

      call mpas_timer_start('timekeeping tests')

      threadNum = mpas_threading_get_thread_num()
      err = 0

      if ( threadNum == 0 ) then
         call mpas_log_write(' Performing time interval tests')

         call mpas_log_write('  Test 1:')
         call test_core_interval_test('0001-01-01_00:00:00', '0000-01-00_10:00:00', '0001_00:00:00', 31_I8KIND, '0000_10:00:00', err_tmp)
         if ( err_tmp == 0 ) then
            call mpas_log_write('   Result: PASSED')
         else
            call mpas_log_write(' * Result: FAILED', MPAS_LOG_ERR)
         end if

         call mpas_log_write('  Test 2:')
         call test_core_interval_test('0001-01-01_00:00:00', '0000-01-00_00:00:00', '0001_00:00:00', 31_I8KIND, '0000_00:00:00', err_tmp)
         if ( err_tmp == 0 ) then
            call mpas_log_write('   Result: PASSED')
         else
            call mpas_log_write(' * Result: FAILED', MPAS_LOG_ERR)
         end if

         call mpas_log_write('  Test 3:')
         call test_core_interval_test('0001-02-01_00:00:00', '0000-01-00_10:00:00', '0001_00:00:00', 28_I8KIND, '0000_10:00:00', err_tmp)
         if ( err_tmp == 0 ) then
            call mpas_log_write('   Result: PASSED')
         else
            call mpas_log_write(' * Result: FAILED', MPAS_LOG_ERR)
         end if

         call mpas_log_write('  Test 4:')
         call test_core_interval_test('0001-02-01_00:00:00', '0000-01-00_00:00:00', '0001_00:00:00', 28_I8KIND, '0000_00:00:00', err_tmp)
         if ( err_tmp == 0 ) then
            call mpas_log_write('   Result: PASSED')
         else
            call mpas_log_write(' * Result: FAILED', MPAS_LOG_ERR)
         end if

         call mpas_log_write('  Test 5:')
         call test_core_interval_test('0001-01-01_00:00:00', '0000-00-00_01:00:00', '0000_00:30:00', 2_I8KIND, '0000_00:00:00', err_tmp)
         if ( err_tmp == 0 ) then
            call mpas_log_write('   Result: PASSED')
         else
            call mpas_log_write(' * Result: FAILED', MPAS_LOG_ERR)
         end if

         call mpas_log_write('  Test 6:')
         call test_core_interval_test('0001-01-01_00:00:00', '0001-01-00_00:00:00', '0001-00-00_00:00:00', 1_I8KIND, '0000-00-31_00:00:00', err_tmp)
         if ( err_tmp == 0 ) then
            call mpas_log_write('   Result: PASSED')
         else
            call mpas_log_write(' * Result: FAILED', MPAS_LOG_ERR)
         end if

         call mpas_log_write('  Test 7:')
         call test_core_interval_test('0000-01-01_00:00:00', '1850-00-00_00:00:00', '00:00:01', 58341600000_I8KIND, '0000-00-00_00:00:00', err_tmp)
         if ( err_tmp == 0 ) then
            call mpas_log_write('   Result: PASSED')
         else
            call mpas_log_write(' * Result: FAILED', MPAS_LOG_ERR)
         end if

         call mpas_log_write(' Completed time interval tests')

      end if

      call mpas_timer_stop('timekeeping tests')

   end subroutine test_core_test_intervals!}}}

   subroutine test_core_interval_test(ref_str, int1_str, int2_str, expected_divs, expected_remainder_str, ierr)!{{{
      character (len=*), intent(in) :: ref_str, int1_str, int2_str
      integer (kind=I8KIND), intent(in) :: expected_divs
      character (len=*), intent(in) :: expected_remainder_str
      integer, intent(out) :: ierr

      integer (kind=I8KIND) :: divs

      character (len=StrKIND) :: remainder_str
      character (len=StrKIND) :: temp_str

      type (mpas_time_type) :: ref_time
      type (mpas_timeinterval_type) :: int1, int2, remainder
      type (mpas_timeinterval_type) :: expected_remainder

      integer :: err_tmp

      ierr = 0

      call mpas_log_write('')
      call mpas_log_write('  Testing time intervals:')
      call mpas_log_write('      Reference time: ' // trim(ref_str))
      call mpas_log_write('      Interval 1: ' // trim(int1_str))
      call mpas_log_write('      Interval 2: ' // trim(int2_str))

      call mpas_set_time(ref_time, dateTimeString=ref_str, ierr=err_tmp)
      call mpas_set_timeinterval(int1, timeString=int1_str, ierr=err_tmp)
      call mpas_set_timeinterval(int2, timeString=int2_str, ierr=err_tmp)
      call mpas_set_timeinterval(expected_remainder, timeString=expected_remainder_str, ierr=err_tmp)

      call mpas_log_write('      -- Calling interval division')

      call mpas_interval_division(ref_time, int1, int2, divs, remainder)

      call mpas_get_timeinterval(remainder, startTimeIn=ref_time, timeString=remainder_str)

      call mpas_log_write('      Interval Division summary')
      write(temp_str,*) '         Divisions: ', divs
      call mpas_log_write(trim(temp_str))
      call mpas_log_write('          Remainder: ' // trim(remainder_str))
      call mpas_log_write('')

      if ( divs == expected_divs ) then
         call mpas_log_write('          Div Test: PASSED')
      else
         call mpas_log_write(' **       Div Test: FAILED', MPAS_LOG_ERR)
         ierr = 1
      end if

      if ( remainder == expected_remainder ) then
         call mpas_log_write('          Remainder Test: PASSED')
      else
         call mpas_log_write(' **       Remainder Test: FAILED', MPAS_LOG_ERR)
         ierr = 1
      end if


   end subroutine test_core_interval_test!}}}

   !***********************************************************************
   !
   !  routine mpas_adjust_alarm_tests
   !
   !> \brief   Tests functionality of mpas_adjust_alarm_to_reference_time
   !> \author  Michael Duda
   !> \date    25 Feb 2025
   !> \details
   !>  This routine tests the functionality of the
   !>  mpas_adjust_alarm_to_reference_time routine for combinations of the
   !>  following possibilities:
   !>
   !>  - The current time is aligned with the new alarm time grid
   !>  - The current time is not aligned with the new alarm time grid
   !>
   !>  - The reference time is before the current time on the clock
   !>  - The reference time is the same as the current time on the clock
   !>  - The reference time is after the current time on the clock
   !>
   !>  - The clock is running forwards
   !>  - The clock is running backwards
   !>
   !>  Upon return, the ierr arugment is set to the number of failed tests.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_adjust_alarm_tests(domain, ierr)

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
      MPAS_ADJUST_ALARM_VERBOSE( character(len=StrKIND) :: timeStamp )

      ierr = 0

      !
      ! Create a clock with an initial time of 2000-01-01_00 and with a 1-hour 'tick' length
      ! (The stopping time is set to 2100-01-01_00.)
      !
      call mpas_set_time(test_startTime, YYYY=2000, MM=01, DD=01, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_set_time(test_stopTime, YYYY=2100, MM=01, DD=01, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_set_timeInterval(test_timeStep, dt=3600.0_RKIND, ierr=ierr_local)

      call mpas_create_clock(test_clock, test_startTime, test_timeStep, test_stopTime, ierr=ierr_local)

      !
      ! Add a recurring alarm to the clock with an initial reference time of 2000-01-01_00 and
      ! a ringing interval of 1 day.
      !
      call mpas_set_time(test_alarmTime, YYYY=2000, MM=01, DD=01, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_set_timeInterval(test_alarmTimeInterval, dt=86400.0_RKIND, ierr=ierr_local)

      call mpas_add_clock_alarm(test_clock, 'foobar', test_alarmTime, test_alarmTimeInterval, ierr_local)

#ifdef MPAS_ADVANCE_TEST_CLOCK
      do istep = 1, 24*365
          if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
              call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)
              test_currTime = mpas_get_clock_time(test_clock, MPAS_NOW, iErr)
              call mpas_get_time(test_currTime, dateTimeString=timeStamp)
              call mpas_log_write('**ALARM** '//trim(timeStamp))
          end if
          call mpas_advance_clock(test_clock, ierr=ierr_local)
      end do
#endif

      MPAS_ADJUST_ALARM_VERBOSE( test_currTime = mpas_get_clock_time(test_clock, MPAS_NOW, iErr) )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_get_time(test_currTime, dateTimeString=timeStamp) )

      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Now it is '//trim(timeStamp)) )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('') )


      write(test_mesg, '(a)') '  forward clock, ref_time < now, now is on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 1999-06-15_00') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=1999, MM=6, DD=15, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  forward clock, ref_time > now, now is on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 2010-02-01_00') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=2010, MM=2, DD=1, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  forward clock, ref_time = now, now is on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 2000-01-01_00') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=2000, MM=1, DD=1, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  forward clock, ref_time < now, now is NOT on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 1999-06-15_08') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=1999, MM=6, DD=15, H=8, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  forward clock, ref_time > now, now is NOT on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 2010-02-01_18') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=2010, MM=2, DD=1, H=18, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      !
      ! Set clock to run backwards in time
      !
      call mpas_set_clock_direction(test_clock, MPAS_BACKWARD, ierr_local)

      write(test_mesg, '(a)') '  backward clock, ref_time < now, now is on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 1999-06-15_00') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=1999, MM=6, DD=15, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  backward clock, ref_time > now, now is on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 2010-02-01_00') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=2010, MM=2, DD=1, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  backward clock, ref_time = now, now is on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 2000-01-01_00') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=2000, MM=1, DD=1, H=0, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  backward clock, ref_time < now, now is NOT on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 1999-06-15_08') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=1999, MM=6, DD=15, H=8, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

      write(test_mesg, '(a)') '  backward clock, ref_time > now, now is NOT on new alarm time grid: '
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('=================================') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('Setting ref time to 2010-02-01_18') )
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('---------------------------------') )
      call mpas_set_time(test_refTime, YYYY=2010, MM=2, DD=1, H=18, M=0, S=0, S_n=0, S_d=0, ierr=ierr_local)
      call mpas_adjust_alarm_to_reference_time(test_clock, 'foobar', test_refTime, ierr_local)
      MPAS_ADJUST_ALARM_VERBOSE( call mpas_print_alarm(test_clock, 'foobar', ierr_local) )
      if (mpas_is_alarm_ringing(test_clock, 'foobar', ierr=ierr_local)) then
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is RINGING') )
          test_mesg = trim(test_mesg)//' SUCCESS'
      else
          MPAS_ADJUST_ALARM_VERBOSE( call mpas_log_write('-> Alarm is NOT ringing') )
          test_mesg = trim(test_mesg)//' FAILURE'
          ierr = ierr + 1
      end if
      call mpas_log_write(trim(test_mesg))
      call mpas_reset_clock_alarm(test_clock, 'foobar', ierr=ierr_local)

   end subroutine mpas_adjust_alarm_tests

end module test_core_timekeeping_tests
