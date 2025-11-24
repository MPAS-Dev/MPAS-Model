module bdd_logging
   implicit none
contains

   subroutine when(statement)
      implicit none
      character(len = *), intent(in) :: statement
      print *, "When ", trim(statement)
   end subroutine when

   subroutine and_then(statement)
      implicit none
      character(len = *), intent(in) :: statement
      print *, "and ", trim(statement)
   end subroutine and_then

   subroutine should(statement)
      implicit none
      character(len = *), intent(in) :: statement
      print *, "it should ", trim(statement)
   end subroutine should
end module bdd_logging


module pool_fixture
   use mpi
   use mpas_subdriver
   use mpas_derived_types
   use mpas_timekeeping
   use mpas_stream_manager
   use iso_c_binding, only: c_ptr, c_f_pointer

   implicit none

   type pool_fixture_t
      type(core_type), pointer :: core_list => null()
      type(domain_type), pointer :: domain => null()
      integer, pointer :: external_comm
      type(MPAS_Clock_type), pointer :: clock
      type(mpas_pool_type), pointer :: pool_size1
      type(mpas_pool_type), pointer :: pool_size4
      integer :: ierr
      integer :: val1, val2, val3
      integer, pointer :: val1_ptr, val2_ptr, val3_ptr
      character(len = :), allocatable :: key1, key2, key3
   end type pool_fixture_t

contains

   subroutine setup_pool(f_ptr)
      implicit none
      type(c_ptr), value :: f_ptr
      type(pool_fixture_t), pointer :: f
      integer :: ierr

      call c_f_pointer(f_ptr, f)
      call setup_mpas(f)
      allocate(f%pool_size1)
      allocate(f%pool_size4)
      call mpas_pool_create_pool(f%pool_size1, 1)
      call mpas_pool_create_pool(f%pool_size4, 4)
      f%key1 = 'a'
      f%key2 = 'b'
      f%key3 = 'c'
      f%val1 = 1
      f%val2 = 2
      f%val3 = 3
      nullify(f%val1_ptr, f%val2_ptr, f%val3_ptr)
      f%ierr = 0
   end subroutine setup_pool


   subroutine teardown_pool(f_ptr)
      implicit none
      type(c_ptr), value :: f_ptr
      type(pool_fixture_t), pointer :: f

      call c_f_pointer(f_ptr, f)
      call mpas_finalize(f%core_list, f%domain)
      nullify(f%core_list, f%domain, f%clock)
      if (associated(f%pool_size1)) then
         deallocate(f%pool_size1)
      end if
      if (associated(f%pool_size4)) then
         deallocate(f%pool_size4)
      end if
      if(associated(f%val1_ptr)) nullify(f%val1_ptr)
      if(associated(f%val2_ptr)) nullify(f%val2_ptr)
      if(associated(f%val3_ptr)) nullify(f%val3_ptr)
      f%ierr = 0
   end subroutine teardown_pool

   subroutine setup_mpas(f)
      implicit none
      type(pool_fixture_t), intent(inout), pointer :: f
      integer :: ierr
      character(len = StrKIND) :: start_time_str
      type(MPAS_Time_Type) :: start_time
      type(mpas_pool_type), pointer :: model_pool
      character(len = StrKIND), pointer :: xtime

      ierr = 0
      call mpas_init(f%core_list, f%domain, external_comm = MPI_COMM_WORLD, &
            namelistFileParam = 'namelist.test', streamsFileParam = 'streams.test')

      f%clock => f%domain%clock

      start_time = mpas_get_clock_time(f%clock, MPAS_START_TIME)
      call mpas_get_time(start_time, dateTimeString = start_time_str)

      call mpas_pool_get_subpool(f%domain%blocklist%structs, 'model', model_pool)
      call mpas_pool_get_array(model_pool, 'xtime', xtime)
      xtime = start_time_str

      call mpas_stream_mgr_read(f%domain%streamManager)
      call mpas_stream_mgr_reset_alarms(f%domain%streamManager, direction = MPAS_STREAM_INPUT)
   end subroutine setup_mpas

end module pool_fixture
