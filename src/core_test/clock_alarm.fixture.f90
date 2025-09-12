module test_clock_alarm_fixture_mod
   use mpi
   use test_suite_mod
   use mpas_subdriver
   use mpas_derived_types, only: core_type, domain_type
   implicit none

   type clock_alarm_suite_fixture_t
      type (core_type), pointer :: corelist => null()
      type (domain_type), pointer :: domain => null()
      integer :: external_comm
   end type clock_alarm_suite_fixture_t

   type clock_alarm_test_fixture_t
      type (MPAS_Time_type) :: clockStartTime
      type (MPAS_Time_type) :: clockStopTime
      type (MPAS_Time_type) :: alarmTime
      type (MPAS_Time_type) :: alarmStartTime
      type (MPAS_Time_type) :: alarmStopTime
      type (MPAS_Time_type) :: currentTime
      type (MPAS_TimeInterval_type) :: clockTimeStep
      type (MPAS_TimeInterval_type) :: alarmStartTimeInterval
      type (MPAS_Clock_type) :: clock
      type (MPAS_Alarm_type), pointer :: alarm
      character(len = :), allocatable :: alarmStartStopID
      integer :: numHours
   end type clock_alarm_test_fixture_t

contains
   subroutine setup_clock_alarm_suite(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_suite_fixture_t), pointer :: f
      integer :: ierr
      character(len = StrKIND) :: startTimeStamp
      type (MPAS_Time_Type) :: startTime
      type (mpas_pool_type), pointer :: modelPool
      character (len = StrKIND), pointer :: xtime

      ierr = 0
      call c_f_pointer(f_ptr, f)
      call MPI_Init(ierr)
      f%external_comm = MPI_COMM_WORLD
      call mpas_init(f%corelist, f%domain, external_comm = f%external_comm, &
            namelistFileParam = 'namelist.test', &
            streamsFileParam = 'streams.test')
      clock => f%domain % clock

      startTime = mpas_get_clock_time(clock, MPAS_START_TIME, ierr)
      call mpas_get_time(startTime, dateTimeString = startTimeStamp)
      call mpas_pool_get_subpool(f%domain % blocklist % structs, 'model', modelPool)
      call mpas_pool_get_array(modelPool, 'xtime', xtime)
      xtime = startTimeStamp
      call mpas_stream_mgr_read(f%domain % streamManager, ierr = ierr)
      call mpas_stream_mgr_reset_alarms(f%domain % streamManager, direction = MPAS_STREAM_INPUT, ierr = ierr)
   end subroutine setup_clock_alarm_suite

   subroutine teardown_clock_alarm_suite(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_suite_fixture_t), pointer :: f
      integer :: ierr

      call c_f_pointer(f_ptr, f)
      call MPI_Finalize(ierr)
   end subroutine teardown_clock_alarm_suite

   subroutine setup_clock_alarm_test(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_test_fixture_t), pointer :: f
      integer :: ierr_local

      call c_f_pointer(f_ptr, f)

      f%alarmStartStopID = 'alarm_start_stop'

      call mpas_set_time(f%clockStartTime, YYYY = 2000, MM = 01, DD = 01, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_time(f%clockStopTime, YYYY = 2100, MM = 01, DD = 01, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_timeInterval(f%clockTimeStep, dt = 3600.0_RKIND, ierr = ierr_local)
      call mpas_create_clock(f%clock, f%clockStartTime, f%clockTimeStep, f%clockStopTime, ierr = ierr_local)
      call mpas_set_time(f%alarmTime, YYYY = 2000, MM = 01, DD = 01, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_time(f%alarmStartTime, YYYY = 2000, MM = 01, DD = 11, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_time(f%alarmStopTime, YYYY = 2000, MM = 01, DD = 21, H = 0, &
            M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr_local)
      call mpas_set_timeInterval(f%alarmStartTimeInterval, dt = 3600.0_RKIND, ierr = ierr_local)
      call mpas_add_clock_alarm(f%clock, f%alarmStartStopID, f%alarmTime, &
            alarmTimeInterval = f%alarmStartTimeInterval, &
            alarmStartTime = f%alarmStartTime, &
            alarmStopTime = f%alarmStopTime, ierr = ierr_local)
      ! Point fixture alarm pointer at the alarm
      f%alarm => f%clock%alarmListHead
      f%numHours = 240
   end subroutine setup_clock_alarm_test

   subroutine teardown_clock_alarm_test(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_test_fixture_t), pointer :: f
      integer :: ierr_local

      call c_f_pointer(f_ptr, f)
      call mpas_remove_clock_alarm(f%clock, f%alarmStartStopID, ierr = ierr_local)
      call mpas_destroy_clock(f%clock, ierr = ierr_local)
      nullify(f%alarm)
   end subroutine teardown_clock_alarm_test

end module test_clock_alarm_fixture_mod

