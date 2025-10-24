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
             mpas_adjust_alarm_tests, &
             mpas_window_alarm_tests

   !-----------------------------------------------------------------------
   !  type alarm_fixture_t
   !
   !> \brief Fixture type for testing alarm ringing behavior with windows.
   !> \author Andy Stokely
   !> \date   10/02/2025
   !> \details
   !>   This type provides all clocks, alarms, and precomputed iteration
   !>   steps needed to exercise windowed alarm test cases.
   !>
   !>   The test timeline is organized as:
   !>
   !>       A                B          C            D                E
   !>       |----------------|----------|------------|----------------|
   !>       ^                ^          ^            ^                ^
   !>   Clock start      Alarm anchor   Window    Window stop     Clock stop
   !>                                    start
   !>
   !>   Fields map directly to these points and derived step counts.
   !-----------------------------------------------------------------------
   type alarm_fixture_t
      !> Clock start time (A on the timeline)
      type(MPAS_Time_type)         :: clock_start_time

      !> Clock stop time (E on the timeline)
      type(MPAS_Time_type)         :: clock_stop_time

      !> Alarm anchor time (B on the timeline)
      type(MPAS_Time_type)         :: alarm_time

      !> Alarm window start time (C on the timeline)
      type(MPAS_Time_type)         :: window_start_time

      !> Alarm window stop time (D on the timeline)
      type(MPAS_Time_type)         :: window_stop_time

      !> Clock step interval (time advanced per tick)
      type(MPAS_TimeInterval_type) :: clock_time_step

      !> Alarm recurrence interval (ring spacing)
      type(MPAS_TimeInterval_type) :: alarm_interval

      !> Clock under test
      type(MPAS_Clock_type)        :: clock

      !> Alarm with a user-defined active window
      type(MPAS_Alarm_type), pointer :: alarm

      !> Identifier string for the alarm
      character(len=:), allocatable  :: alarm_id

      !> Total number of steps the clock will run
      integer :: num_clock_steps        = 120

      !> Number of substeps per interval (clock resolution)
      integer :: num_steps_per_interval = 6

      !> Steps from clock start to alarm anchor (A→B)
      integer :: num_steps_before_anchor = 12

      !> Steps from anchor to window start (B→C)
      integer :: num_steps_before_window = 36

      !> Steps covering full window duration (C→D)
      integer :: num_steps_window        = 36

      !> Steps from window end to clock end (D→E)
      integer :: num_steps_post_window   = 36

      !> Absolute step index at anchor (B)
      integer :: steps_to_anchor

      !> Absolute step index at window start (C)
      integer :: steps_to_window_start

      !> Absolute step index at window midpoint
      integer :: steps_to_window_midpoint

      !> Absolute step index at window end (D)
      integer :: steps_to_window_end

      !> Absolute step index in post-window region
      integer :: steps_to_post_window
   end type alarm_fixture_t

contains


   !-----------------------------------------------------------------------
   !  subroutine advance_clock_n_times
   !
   !> \brief Advance a clock forward by a specified number of steps.
   !> \author Andy Stokely
   !> \date   10/02/2025
   !> \details This routine repeatedly advances the provided clock
   !>   forward by calling mpas_advance_clock `n` times.
   !-----------------------------------------------------------------------
   subroutine advance_clock_n_times(clock, n)
      use mpas_derived_types, only : MPAS_Clock_type
      implicit none
      type(MPAS_Clock_type), intent(inout) :: clock
      integer, intent(in)                  :: n
      integer                              :: i

      do i = 1, n
         call mpas_advance_clock(clock)
      end do
   end subroutine advance_clock_n_times


   !-----------------------------------------------------------------------
   !  subroutine setup_alarm_fixture
   !
   !> \brief Initialize a test fixture with a clock and alarms.
   !> \author Andy Stokely
   !> \date   10/02/2025
   !> \details This routine allocates and initializes an alarm_fixture_t
   !>   structure.
   !-----------------------------------------------------------------------
   subroutine setup_alarm_fixture(fixture)
      implicit none
      type(alarm_fixture_t), pointer :: fixture
      integer                        :: ierr

      allocate(fixture)

      ! Alarm ID
      fixture%alarm_id        = 'alarm'

      ! Clock setup
      call mpas_set_time(fixture%clock_start_time, YYYY=2000, MM=01, DD=01, H=0,  &
            M=0, S=0, S_n=0, S_d=0, ierr=ierr)
      call mpas_set_time(fixture%clock_stop_time,  YYYY=2000, MM=01, DD=01, H=20, &
            M=0, S=0, S_n=0, S_d=0, ierr=ierr)

      call mpas_set_timeInterval(fixture%clock_time_step, dt=600.0_RKIND, ierr=ierr)

      call mpas_create_clock(fixture%clock, fixture%clock_start_time, &
            fixture%clock_time_step, fixture%clock_stop_time, ierr=ierr)

      ! Alarm anchor and interval
      call mpas_set_time(fixture%alarm_time, YYYY=2000, MM=01, DD=01, H=2, M=0, S=0, S_n=0, S_d=0, ierr=ierr)
      call mpas_set_timeInterval(fixture%alarm_interval, dt=3600.0_RKIND, ierr=ierr)

      ! Alarm window
      call mpas_set_time(fixture%window_start_time, YYYY=2000, MM=01, DD=01, H=8, M=0, S=0, S_n=0, S_d=0, ierr=ierr)
      call mpas_set_time(fixture%window_stop_time,  YYYY=2000, MM=01, DD=01, H=14, M=0, S=0, S_n=0, S_d=0, ierr=ierr)

      ! Add alarms to the clock
      call mpas_add_clock_alarm(fixture%clock, fixture%alarm_id, fixture%alarm_time, &
            alarmTimeInterval = fixture%alarm_interval, &
            alarmStartTime    = fixture%window_start_time, &
            alarmStopTime     = fixture%window_stop_time)
      fixture%alarm => fixture%clock%alarmListHead

      ! Derived iteration steps
      fixture%steps_to_anchor          = fixture%num_steps_before_anchor
      fixture%steps_to_window_start    = fixture%num_steps_before_anchor + fixture%num_steps_before_window
      fixture%steps_to_window_midpoint = fixture%steps_to_window_start + fixture%num_steps_window/2
      fixture%steps_to_window_end      = fixture%steps_to_window_start + fixture%num_steps_window
      fixture%steps_to_post_window     = fixture%steps_to_window_end + fixture%num_steps_post_window/2
   end subroutine setup_alarm_fixture


   !-----------------------------------------------------------------------
   !  subroutine teardown_alarm_fixture
   !
   !> \brief Finalize and deallocate a test alarm fixture.
   !> \author Andy Stokely
   !> \date   10/02/2025
   !> \details This routine removes alarms from the test clock, destroys
   !>   the clock, nullifies alarm pointers, and deallocates the fixture.
   !>   It ensures a clean state for subsequent tests.
   !-----------------------------------------------------------------------
   subroutine teardown_alarm_fixture(fixture)
      implicit none
      type(alarm_fixture_t), pointer :: fixture
      integer                        :: ierr

      call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr=ierr)

      call mpas_destroy_clock(fixture%clock, ierr=ierr)

      nullify(fixture%alarm)
      deallocate(fixture)
   end subroutine teardown_alarm_fixture


   !-----------------------------------------------------------------------
   !  subroutine test_window_alarm
   !
   !> \brief Unit test for alarm ringing behavior with user-defined windows.
   !> \author Andy Stokely
   !> \date   10/02/2025
   !> \details This test suite verifies that an alarm rings only when expected
   !>   relative to the defined timeline:
   !>
   !>   A = Clock start time
   !>   B = Alarm anchor time
   !>   C = Window start time
   !>   D = Window end time
   !>   E = Clock end time
   !>
   !>   Timeline:
   !>      A ---------------- B ----- C ---------------- D --------- E
   !>
   !>   The test covers 16 cases, including behavior:
   !>   - Before anchor (A → B)
   !>   - Between anchor and window start (B → C)
   !>   - At window boundaries (C, D)
   !>   - Inside the window (C → D)
   !>   - After leaving the window (D → E)
   !>   - After reset operations at various points
   !>   - With anchor times shifted into the window
   !>   - When the clock direction changes (forward/backward)
   !>
   !>   Each case checks whether the alarm rings at the correct times and
   !>   uses mpas_log_write to log PASS/FAIL outcomes.
   !-----------------------------------------------------------------------
   subroutine test_window_alarm(case_idx, ierr)
      implicit none
      integer, intent(in) :: case_idx
      integer, intent(inout) :: ierr
      type(alarm_fixture_t), pointer :: f
      logical :: ringing

      call setup_alarm_fixture(f)
      ierr = 0

      select case (case_idx)

         !-----------------------------------------------------------------------
         ! Case 1: Before anchor (A → B)
         ! Alarm should not ring before the anchor time.
         !-----------------------------------------------------------------------
      case(1)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing before anchor time')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing before anchor time')
         end if

         !-----------------------------------------------------------------------
         ! Case 2: Before anchor after reset (A → B)
         ! Reset should not cause false ringing before anchor.
         !-----------------------------------------------------------------------
      case(2)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing before anchor time after reset')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing before anchor time after reset')
         end if

         !-----------------------------------------------------------------------
         ! Case 3: After anchor but before window (B → C)
         ! Alarm should not ring after anchor until the window begins.
         !-----------------------------------------------------------------------
      case(3)
         call advance_clock_n_times(f%clock, f%steps_to_anchor + 2*f%num_steps_per_interval)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing after anchor, before window')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing after anchor, before window')
         end if

         !-----------------------------------------------------------------------
         ! Case 4: At window start (C)
         ! Alarm should ring exactly at the start of the window.
         !-----------------------------------------------------------------------
      case(4)
         call advance_clock_n_times(f%clock, f%steps_to_window_start)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing at start of window')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing at start of window')
         end if

         !-----------------------------------------------------------------------
         ! Case 5: At window start (C) after reset
         ! Reset should prevent ringing immediately at window start.
         !-----------------------------------------------------------------------
      case(5)
         call advance_clock_n_times(f%clock, f%steps_to_window_start)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing at start of window after reset')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing at start of window after reset')
         end if

         !-----------------------------------------------------------------------
         ! Case 6: Middle of window (C → D)
         ! Alarm should ring inside the window.
         !-----------------------------------------------------------------------
      case(6)
         call advance_clock_n_times(f%clock, f%steps_to_window_midpoint)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing in middle of window')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing in middle of window')
         end if

         !-----------------------------------------------------------------------
         ! Case 7: Middle of window after reset at window start
         ! Alarm should ring again after being reset at C.
         !-----------------------------------------------------------------------
      case(7)
         call advance_clock_n_times(f%clock, f%steps_to_window_start)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         call advance_clock_n_times(f%clock, f%num_steps_window/2)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing in middle of window after reset')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing in middle of window after reset')
         end if

         !-----------------------------------------------------------------------
         ! Case 8: Middle of window after reset inside window
         ! Reset prevents alarm from ringing again immediately inside window.
         !-----------------------------------------------------------------------
      case(8)
         call advance_clock_n_times(f%clock, f%steps_to_window_midpoint)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing in middle of window after reset')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing in middle of window after reset')
         end if

         !-----------------------------------------------------------------------
         ! Case 9: At window end (D)
         ! Alarm should ring at the end of the window.
         !-----------------------------------------------------------------------
      case(9)
         call advance_clock_n_times(f%clock, f%steps_to_window_end)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing at end of window')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing at end of window')
         end if

         !-----------------------------------------------------------------------
         ! Case 10: At window end (D) after reset
         ! Reset should suppress ringing at window end.
         !-----------------------------------------------------------------------
      case(10)
         call advance_clock_n_times(f%clock, f%steps_to_window_end)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing at end of window after reset')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing at end of window after reset')
         end if

         !-----------------------------------------------------------------------
         ! Case 11: After leaving window (D → E) before reset outside of window
         ! Alarm should ring once more just after window ends.
         !-----------------------------------------------------------------------
      case(11)
         call advance_clock_n_times(f%clock, f%steps_to_window_midpoint)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         call advance_clock_n_times(f%clock, f%num_steps_window/2 + f%num_steps_post_window/2)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing after window')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing after window')
         end if

         !-----------------------------------------------------------------------
         ! Case 12: After leaving window (D → E) after reset outside of window
         ! Alarm should not ring after reset outside window.
         !-----------------------------------------------------------------------
      case(12)
         call advance_clock_n_times(f%clock, f%steps_to_window_end + f%num_steps_post_window/2)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing after window after reset')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing after window after reset')
         end if

         !-----------------------------------------------------------------------
         ! Case 13: Anchor after window start (B > C)
         ! Ensures alarm does not ring before a delayed anchor, even inside window.
         !-----------------------------------------------------------------------
      case(13)
         call mpas_remove_clock_alarm(f%clock, f%alarm_id)
         call mpas_set_time(f%alarm_time, YYYY=2000, MM=01, DD=01, H=9, M=0, S=0, S_n=0, S_d=0, ierr=ierr)
         call mpas_add_clock_alarm(f%clock, f%alarm_id, f%alarm_time, &
               alarmTimeInterval = f%alarm_interval, &
               alarmStartTime    = f%window_start_time, &
               alarmStopTime     = f%window_stop_time)
         call advance_clock_n_times(f%clock, f%steps_to_window_start)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing before anchor (after window start)')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing before anchor (after window start)')
         end if
         call advance_clock_n_times(f%clock, 2*f%num_steps_per_interval)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing after anchor (after window start)')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing after anchor (after window start)')
         end if

         !-----------------------------------------------------------------------
         ! Case 14: Clock direction change inside window (forward → backward)
         ! Alarm should still ring in window after direction change.
         !-----------------------------------------------------------------------
      case(14)
         call advance_clock_n_times(f%clock, f%steps_to_window_midpoint)
         call mpas_set_clock_direction(f%clock, MPAS_BACKWARD)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing in window after direction change')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing in window after direction change')
         end if

         !-----------------------------------------------------------------------
         ! Case 15: Re-entering window boundary after direction change
         ! Alarm should ring again at boundary after leaving and re-entering.
         !-----------------------------------------------------------------------
      case(15)
         call advance_clock_n_times(f%clock, f%steps_to_window_end)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         call advance_clock_n_times(f%clock, f%num_steps_post_window/2)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing after leaving window before re-entering')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing after leaving window before re-entering')
         end if
         call mpas_set_clock_direction(f%clock, MPAS_BACKWARD)
         call advance_clock_n_times(f%clock, f%num_steps_post_window/2)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (.not. ringing) then
            call mpas_log_write('FAIL: Alarm is not ringing at boundary after re-entering')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is ringing at boundary after re-entering')
         end if

         !-----------------------------------------------------------------------
         ! Case 16: Reset at window start and reverse direction to exit window
         !> Alarm should not ring when moving backward out of the window after
         !> being reset at the window start.
         !-----------------------------------------------------------------------
      case(16)
         call advance_clock_n_times(f%clock, f%steps_to_window_midpoint)
         call mpas_set_clock_direction(f%clock, MPAS_BACKWARD)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         call advance_clock_n_times(f%clock, f%num_steps_window / 2)
         call mpas_reset_clock_alarm(f%clock, f%alarm_id)
         call advance_clock_n_times(f%clock, f%steps_to_window_start / 2)

         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm rang after reset when moving backward out of window')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm did not ring after reset when moving backward out of window')
         end if

         !-----------------------------------------------------------------------
         ! Case 17: Outside window after direction change (D → E → backward)
         ! Alarm should not ring outside window when clock direction flips.
         !-----------------------------------------------------------------------
      case(17)
         call advance_clock_n_times(f%clock, f%steps_to_window_end + f%num_steps_post_window/2)
         call mpas_set_clock_direction(f%clock, MPAS_BACKWARD)
         ringing = mpas_is_alarm_ringing(f%clock, f%alarm_id)
         if (ringing) then
            call mpas_log_write('FAIL: Alarm is ringing outside of window after direction change')
            ierr = ierr + 1
         else
            call mpas_log_write('PASS: Alarm is not ringing outside of window after direction change')
         end if

      end select

      call teardown_alarm_fixture(f)
   end subroutine test_window_alarm


   !-----------------------------------------------------------------------
   !  function mpas_window_alarm_tests
   !
   !> \brief Run all unit tests for windowed alarm ringing behavior.
   !> \author Andy Stokely
   !> \date   10/02/2025
   !> \details This driver function executes the suite of test cases
   !>   implemented in test_window_alarm, aggregating any errors encountered.
   !-----------------------------------------------------------------------
   integer function mpas_window_alarm_tests(domain) result(ierr)
      type(domain_type), intent(inout) :: domain

      integer :: i, ierr_local

      ierr = 0
      do i = 1, 17
         ierr_local = 0
         call test_window_alarm(i, ierr_local)
         ierr = ierr + ierr_local
      end do
   end function mpas_window_alarm_tests


   !-----------------------------------------------------------------------
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

      call mpas_add_clock_alarm(test_clock, 'foobar', test_alarmTime, test_alarmTimeInterval, ierr=ierr_local)

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
