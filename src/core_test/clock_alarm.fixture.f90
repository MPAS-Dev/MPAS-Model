module test_clock_alarm_fixture_mod
   use mpi
   use test_suite_mod
   use mpas_subdriver
   use mpas_derived_types, only: core_type, domain_type
   implicit none

   type clock_alarm_suite_fixture_t
      type(core_type),   pointer :: core_list   => null()
      type(domain_type), pointer :: domain      => null()
      integer                      :: external_comm
   end type clock_alarm_suite_fixture_t

   type clock_alarm_test_fixture_t
      type(MPAS_Time_type)          :: clock_start_time
      type(MPAS_Time_type)          :: clock_stop_time
      type(MPAS_Time_type)          :: alarm_time
      type(MPAS_Time_type)          :: alarm_start_time
      type(MPAS_Time_type)          :: alarm_stop_time
      type(MPAS_Time_type)          :: current_time
      type(MPAS_TimeInterval_type)  :: clock_time_step
      type(MPAS_TimeInterval_type)  :: alarm_start_time_interval
      type(MPAS_Clock_type)         :: clock
      type(MPAS_Alarm_type), pointer :: alarm
      character(len = :), allocatable :: alarm_id
      integer                         :: num_hours
   end type clock_alarm_test_fixture_t

contains

   subroutine setup_clock_alarm_suite(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_suite_fixture_t), pointer :: fixture
      integer :: ierr
      character(len = StrKIND) :: start_time_str
      type(MPAS_Time_Type) :: start_time
      type(mpas_pool_type), pointer :: model_pool
      character(len = StrKIND), pointer :: xtime

      ierr = 0
      call c_f_pointer(f_ptr, fixture)
      call MPI_Init(ierr)
      fixture%external_comm = MPI_COMM_WORLD

      call mpas_init(fixture%core_list, fixture%domain, external_comm = fixture%external_comm, &
              namelistFileParam = 'namelist.test', &
              streamsFileParam = 'streams.test')

      clock => fixture%domain%clock

      start_time = mpas_get_clock_time(clock, MPAS_START_TIME, ierr)
      call mpas_get_time(start_time, dateTimeString = start_time_str)
      call mpas_pool_get_subpool(fixture%domain%blocklist%structs, 'model', model_pool)
      call mpas_pool_get_array(model_pool, 'xtime', xtime)
      xtime = start_time_str

      call mpas_stream_mgr_read(fixture%domain%streamManager, ierr = ierr)
      call mpas_stream_mgr_reset_alarms(fixture%domain%streamManager, direction = MPAS_STREAM_INPUT, ierr = ierr)
   end subroutine setup_clock_alarm_suite

   subroutine teardown_clock_alarm_suite(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_suite_fixture_t), pointer :: fixture
      integer :: ierr

      call c_f_pointer(f_ptr, fixture)
      call MPI_Finalize(ierr)
   end subroutine teardown_clock_alarm_suite

   subroutine setup_clock_alarm_test(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_test_fixture_t), pointer :: fixture
      integer :: ierr

      call c_f_pointer(f_ptr, fixture)

      fixture%alarm_id = 'alarm_start_stop'

      call mpas_set_time(fixture%clock_start_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
              M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
      call mpas_set_time(fixture%clock_stop_time, YYYY = 2100, MM = 01, DD = 01, H = 0, &
              M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
      call mpas_set_timeInterval(fixture%clock_time_step, dt = 3600.0_RKIND, ierr = ierr)
      call mpas_create_clock(fixture%clock, fixture%clock_start_time, fixture%clock_time_step, fixture%clock_stop_time, ierr = ierr)

      call mpas_set_time(fixture%alarm_time, YYYY = 2000, MM = 01, DD = 01, H = 0, &
              M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
      call mpas_set_time(fixture%alarm_start_time, YYYY = 2000, MM = 01, DD = 11, H = 0, &
              M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)
      call mpas_set_time(fixture%alarm_stop_time, YYYY = 2000, MM = 01, DD = 21, H = 0, &
              M = 0, S = 0, S_n = 0, S_d = 0, ierr = ierr)

      call mpas_set_timeInterval(fixture%alarm_start_time_interval, dt = 3600.0_RKIND, ierr = ierr)
      call mpas_add_clock_alarm(fixture%clock, fixture%alarm_id, fixture%alarm_time, &
              alarmTimeInterval = fixture%alarm_start_time_interval, &
              alarmStartTime    = fixture%alarm_start_time, &
              alarmStopTime     = fixture%alarm_stop_time, ierr = ierr)

      ! Point fixture alarm pointer at the alarm
      fixture%alarm => fixture%clock%alarmListHead
      fixture%num_hours = 240
   end subroutine setup_clock_alarm_test

   subroutine teardown_clock_alarm_test(f_ptr)
      use iso_c_binding, only: c_ptr, c_f_pointer
      implicit none
      type(c_ptr), value :: f_ptr
      type(clock_alarm_test_fixture_t), pointer :: fixture
      integer :: ierr

      call c_f_pointer(f_ptr, fixture)
      call mpas_remove_clock_alarm(fixture%clock, fixture%alarm_id, ierr = ierr)
      call mpas_destroy_clock(fixture%clock, ierr = ierr)
      nullify(fixture%alarm)
   end subroutine teardown_clock_alarm_test

end module test_clock_alarm_fixture_mod
