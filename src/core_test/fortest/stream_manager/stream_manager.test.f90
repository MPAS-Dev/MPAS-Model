module test_stream_manager_mod
   use mpi
   use fortest_test_session, only: test_session_t
   use fortest_assert, only: assert_true, assert_false, assert_equal
   use iso_c_binding, only: c_ptr, c_f_pointer
   use mpas_subdriver
   use mpas_timekeeping
   use mpas_derived_types, only: core_type, domain_type
   use stream_manager_fixture, only: stream_manager_fixture_t
   use mpas_stream_list
   implicit none
contains

   subroutine test_create_stream(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use mpas_stream_manager
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use stream_manager_fixture, only: stream_manager_fixture_t
      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr, num_streams
      type(MPAS_stream_list_Type), pointer :: stream
      logical :: stream_exists
      integer :: int_property_value
      character(len = StrKIND) :: char_property_value
      type(MPAS_pool_Field_info_type) :: info
      type(MPAS_pool_Type), pointer :: pool
      integer, pointer :: field1

      call c_f_pointer(f_ptr, f)
      select case (param_idx)
      case (1)
         num_streams = f%manager%numStreams
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test_output.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_equal(f%manager%numStreams, num_streams + 1, verbosity = 2)
      case (2)
         nullify(stream)
         num_streams = f%manager%numStreams
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test_output.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Default properties

         call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_FILENAME, &
               char_property_value, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_equal(trim(char_property_value), 'test_output.nc', verbosity = 2)

         call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_PRECISION, &
               int_property_value, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_equal(int_property_value, MPAS_IO_NATIVE_PRECISION, verbosity = 2)

         call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_FILENAME_INTV, &
               char_property_value, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_equal(trim(char_property_value), 'none', verbosity = 2)

         call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_IOTYPE, &
               int_property_value, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_equal(int_property_value, MPAS_IO_PNETCDF, verbosity = 2)

         call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_CLOBBER, &
               int_property_value, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_equal(int_property_value, MPAS_STREAM_CLOBBER_NEVER)
      end select
   end subroutine

   subroutine test_destroy_stream(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use fortest_assert, only: assert_true, assert_false, assert_equal
      use mpas_stream_list
      use mpas_stream_manager
      use stream_manager_fixture, only: stream_manager_fixture_t
      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      type(MPAS_stream_list_type), pointer :: stream
      integer :: ierr, num_streams
      logical :: exists
      type(MPAS_pool_Type), pointer :: pool

      call c_f_pointer(f_ptr, f)

      select case (param_idx)

         !-----------------------------------------------------------------------
         !> **Case 1**
         !> \brief Destroy a valid stream and confirm that it is removed from
         !>        the stream manager and that numStreams decreases by one.
         !-----------------------------------------------------------------------
      case (1)
         ! Create a test stream
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test_output.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR)
         num_streams = f%manager%numStreams

         ! Destroy the stream
         call MPAS_stream_mgr_destroy_stream(f%manager, 'stream', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify stream count decremented
         call assert_equal(f%manager%numStreams, num_streams - 1, verbosity = 2)

         ! Verify the stream no longer exists
         nullify(stream)
         exists = MPAS_stream_list_query(f%manager%streams, 'stream', stream, ierr)
         call assert_false(exists, verbosity = 2)

         !-----------------------------------------------------------------------
         !> **Case 2**
         !> \brief Attempt to destroy a non-existent stream and confirm that
         !>        the routine reports an error without modifying numStreams.
         !-----------------------------------------------------------------------
      case (2)
         num_streams = f%manager%numStreams
         call MPAS_stream_mgr_destroy_stream(f%manager, 'nonexistent', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)
         call assert_equal(f%manager%numStreams, num_streams, verbosity = 2)

         !-----------------------------------------------------------------------
         !> **Case 3**
         !> \brief Destroy multiple streams in succession to ensure that
         !>        sequential removals are consistent and leave no residuals.
         !-----------------------------------------------------------------------
      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 's1', MPAS_STREAM_INPUT, 'f1.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 's2', MPAS_STREAM_INPUT, 'f2.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 's3', MPAS_STREAM_INPUT, 'f3.nc', ierr = ierr)

         num_streams = f%manager%numStreams
         call assert_true(num_streams >= 3)

         call MPAS_stream_mgr_destroy_stream(f%manager, 's2', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR)
         call assert_equal(f%manager%numStreams, num_streams - 1)

         ! Confirm s2 is gone, but s1 and s3 remain
         nullify(stream)
         call assert_false(MPAS_stream_list_query(f%manager%streams, 's2', stream, ierr))
         call assert_true(MPAS_stream_list_query(f%manager%streams, 's1', stream, ierr))
         call assert_true(MPAS_stream_list_query(f%manager%streams, 's3', stream, ierr))
      end select
   end subroutine test_destroy_stream

   subroutine test_add_pool(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use fortest_assert, only: assert_true, assert_false, assert_equal
      use mpas_stream_list
      use mpas_stream_manager
      use stream_manager_fixture, only: stream_manager_fixture_t
      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      type(MPAS_Pool_Type), pointer :: pool
      character(len = StrKIND) :: pool_name
      logical :: exists
      type(MPAS_stream_list_type), pointer :: stream
      integer :: int_logical

      call c_f_pointer(f_ptr, f)

      select case (param_idx)

      case (1)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
      case (2)
         call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)
      case (3)
         ! As of now, this test fails due to a known bug in MPAS_stream_mgr_add_pool
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'pool_not_in_allStructs', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)
      case (4)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct4', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)
      case (5)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct5', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)
      case (6)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct6', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select
   end subroutine test_add_pool

   subroutine test_add_field(f_ptr, ts_ptr, s_ptr, param_idx)
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use fortest_assert, only: assert_true, assert_false, assert_equal
      use mpas_stream_manager
      use mpas_stream_list
      use stream_manager_fixture, only: stream_manager_fixture_t
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      type(MPAS_stream_list_type), pointer :: stream
      integer, pointer :: test_ptr_int
      character(len = StrKIND), pointer :: test_ptr_char
      logical :: stream_exists

      call c_f_pointer(f_ptr, f)
      nullify(stream)

      select case (param_idx)

      case (1)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify that the field exists in the stream’s field pool
         stream_exists = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
         call mpas_pool_get_config(stream%field_pool, 'field1', value = test_ptr_int)
         call assert_true(associated(test_ptr_int), verbosity = 2)

      case (2)
         call MPAS_stream_mgr_add_field(f%manager, 'nonexistent', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'unknown_field', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      case (4)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         ! The field should not be duplicated
         ! (Optionally check no change in number of items if your pool mock supports it)

      case (5)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         stream_exists = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
         stream%immutable = .true.
         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)
      case (6)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', packages = 'package1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         stream_exists = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
         call mpas_pool_get_config(stream%field_pkg_pool, 'field1:packages', value = test_ptr_char)
         call assert_true(associated(test_ptr_char), verbosity = 2)
         if (associated(test_ptr_char)) then
            call assert_equal(trim(test_ptr_char), 'package1', verbosity = 2)
         end if
      end select
   end subroutine test_add_field

   subroutine test_add_stream_fields(f_ptr, ts_ptr, s_ptr, param_idx)
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use fortest_assert, only: assert_true, assert_false, assert_equal
      use mpas_stream_manager
      use mpas_stream_list
      use stream_manager_fixture, only: stream_manager_fixture_t
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      type(MPAS_stream_list_type), pointer :: stream, refStream
      integer, pointer :: test_ptr_int
      character(len = StrKIND), pointer :: test_ptr_char
      logical :: ok

      call c_f_pointer(f_ptr, f)
      nullify(stream)
      nullify(refStream)

      select case (param_idx)

         !------------------------------------------------------------
         ! Case 1: Normal operation — fields from refStream copied to target stream
         !------------------------------------------------------------
      case (1)
         call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_OUTPUT, 'target.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Add some fields to the reference stream
         call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field2', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Copy all fields from ref → target
         call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify that both fields now exist in the target stream
         ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
         call assert_true(ok, verbosity = 2)
         call mpas_pool_get_config(stream%field_pool, 'field1', value = test_ptr_int)
         call assert_true(associated(test_ptr_int), verbosity = 2)
         call mpas_pool_get_config(stream%field_pool, 'field2', value = test_ptr_int)
         call assert_true(associated(test_ptr_int), verbosity = 2)

         !------------------------------------------------------------
         ! Case 2: Reference stream does not exist
         !------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_INPUT, 'target.nc', ierr = ierr)
         call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'no_ref', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !------------------------------------------------------------
         ! Case 3: Target stream does not exist
         !------------------------------------------------------------
      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr = ierr)
         call MPAS_stream_mgr_add_stream_fields(f%manager, 'no_target', 'ref', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !------------------------------------------------------------
         ! Case 4: Target stream is immutable
         !------------------------------------------------------------
      case (4)
         call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_INPUT, 'target.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr = ierr)
         ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
         stream%immutable = .true.
         call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !------------------------------------------------------------
         ! Case 5: Fields already exist in target stream
         !------------------------------------------------------------
      case (5)
         call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_INPUT, 'target.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'target', 'field1', ierr = ierr)
         call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         ! Ensure the existing field is not duplicated
         ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
         call mpas_pool_get_config(stream%field_pool, 'field1', value = test_ptr_int)
         call assert_true(associated(test_ptr_int), verbosity = 2)

         !------------------------------------------------------------
         ! Case 6: Attach packages when copying fields
         !------------------------------------------------------------
      case (6)
         call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_OUTPUT, 'target.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr = ierr)
         call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', packages = 'pkg1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
         call mpas_pool_get_config(stream%field_pkg_pool, 'field1:packages', value = test_ptr_char)
         call assert_true(associated(test_ptr_char), verbosity = 2)
         if (associated(test_ptr_char)) then
            call assert_equal(trim(test_ptr_char), 'pkg1', verbosity = 2)
         end if

      end select
   end subroutine test_add_stream_fields

   subroutine test_remove_field(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use fortest_assert, only: assert_true, assert_false, assert_equal
      use mpas_stream_manager
      use mpas_stream_list
      use stream_manager_fixture, only: stream_manager_fixture_t
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      type(MPAS_stream_list_type), pointer :: stream
      integer, pointer :: test_ptr
      logical :: ok

      call c_f_pointer(f_ptr, f)
      nullify(stream)
      nullify(test_ptr)

      select case (param_idx)

         !------------------------------------------------------------
         ! Case 1: Normal operation — remove existing field
         !------------------------------------------------------------
      case (1)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify field1 exists
         ok = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
         call mpas_pool_get_config(stream%field_pool, 'field1', value = test_ptr)
         call assert_true(associated(test_ptr), verbosity = 2)

         ! Remove field1
         call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify field1 no longer exists
         call mpas_pool_get_config(stream%field_pool, 'field1', value = test_ptr)
         call assert_false(associated(test_ptr), verbosity = 2)

         !------------------------------------------------------------
         ! Case 2: Stream does not exist
         !------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_remove_field(f%manager, 'nonexistent', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !------------------------------------------------------------
         ! Case 3: Field does not exist in valid stream
         !------------------------------------------------------------
      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Attempt to remove a non-existent field
         call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field_missing', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !------------------------------------------------------------
         ! Case 4: Stream is immutable
         !------------------------------------------------------------
      case (4)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr = ierr)

         ok = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
         stream%immutable = .true.

         call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         ! Verify field still exists
         call mpas_pool_get_config(stream%field_pool, 'field1', value = test_ptr)
         call assert_true(associated(test_ptr), verbosity = 2)

         !------------------------------------------------------------
         ! Case 5: Attempting to remove a field twice
         !------------------------------------------------------------
      case (5)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Second removal should error (field already gone)
         call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select
   end subroutine test_remove_field

   subroutine test_add_pkg(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use fortest_assert, only: assert_true, assert_false, assert_equal
      use mpas_stream_manager
      use mpas_stream_list
      use stream_manager_fixture, only: stream_manager_fixture_t
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr, err_local
      type(MPAS_stream_list_type), pointer :: stream
      logical, pointer :: pkg_ptr
      logical :: stream_exists

      call c_f_pointer(f_ptr, f)
      nullify(stream)
      nullify(pkg_ptr)

      select case (param_idx)

         !-------------------------------------------------------------------
         ! Case 1: Successful package attachment
         !-------------------------------------------------------------------
      case (1)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_add_pkg(f%manager, 'stream', 'package1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify package now exists in the stream’s pkg_pool
         stream_exists = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
         call assert_true(stream_exists, verbosity = 2)

         call mpas_pool_get_package(stream%pkg_pool, 'package1', pkg_ptr)
         call assert_true(associated(pkg_ptr), verbosity = 2)
         if (associated(pkg_ptr)) then
            call assert_true(pkg_ptr, verbosity = 2)
         end if

         !-------------------------------------------------------------------
         ! Case 2: Stream does not exist
         !-------------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_add_pkg(f%manager, 'nonexistent_stream', 'package1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !-------------------------------------------------------------------
         ! Case 3: Package not found in global registry
         !-------------------------------------------------------------------
      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_add_pkg(f%manager, 'stream', 'missing_pkg', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select
   end subroutine test_add_pkg

   subroutine test_add_alarm(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use mpas_stream_manager
      use mpas_timekeeping
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use stream_manager_fixture, only: stream_manager_fixture_t
      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f

      integer :: ierr
      type(MPAS_stream_list_type), pointer :: stream, alarm_node
      logical :: success

      call c_f_pointer(f_ptr, f)

      select case (param_idx)

         !--------------------------------------------------------------
         ! Case 1: Add a valid alarm to an existing input stream
         !--------------------------------------------------------------
      case (1)
         ! Create a stream
         call MPAS_stream_mgr_create_stream(f%manager, 'input_stream', MPAS_STREAM_INPUT, 'infile.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Add a clock alarm that should exist
         call mpas_add_clock_alarm(f%manager%streamClock, 'test_alarm', &
               mpas_get_clock_time(f%manager%streamClock, MPAS_START_TIME, ierr), &
               mpas_get_clock_timestep(f%manager%streamClock, ierr), ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         ! Add the alarm to the stream manager
         call MPAS_stream_mgr_add_alarm(f%manager, 'input_stream', 'test_alarm', MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify that the alarm exists in the stream's input alarm list
         nullify(stream)
         success = MPAS_stream_list_query(f%manager%streams, 'input_stream', stream, ierr = ierr)
         call assert_true(success, verbosity = 2)
         call assert_true(associated(stream), verbosity = 2)

         nullify(alarm_node)
         success = MPAS_stream_list_query(stream%alarmList_in, 'test_alarm', alarm_node, ierr = ierr)
         call assert_true(success, verbosity = 2)
         call assert_true(associated(alarm_node%xref), verbosity = 2)

         ! Verify that the manager’s alarm list also contains the new alarm
         nullify(alarm_node)
         success = MPAS_stream_list_query(f%manager%alarms_in, 'test_alarm', alarm_node, ierr = ierr)
         call assert_true(success, verbosity = 2)
         call assert_true(associated(alarm_node%streamList), verbosity = 2)

         !--------------------------------------------------------------
         ! Case 2: Attempt to add an alarm for a non-existent stream
         !--------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_add_alarm(f%manager, 'missing_stream', 'test_alarm', MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !--------------------------------------------------------------
         ! Case 3: Attempt to add an alarm that is not defined on the clock
         !--------------------------------------------------------------
      case (3)
         ! Create a stream but don’t add a clock alarm
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_no_alarm', MPAS_STREAM_OUTPUT, 'file.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_no_alarm', 'missing_alarm', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !--------------------------------------------------------------
         ! Case 4: Attempt to add a duplicate alarm
         !--------------------------------------------------------------
      case (4)
         ! Create and add stream
         call MPAS_stream_mgr_create_stream(f%manager, 'dup_stream', MPAS_STREAM_OUTPUT, 'dup.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Define the alarm on the clock
         call mpas_add_clock_alarm(f%manager%streamClock, 'dup_alarm', &
               mpas_get_clock_time(f%manager%streamClock, MPAS_START_TIME, ierr), &
               mpas_get_clock_timestep(f%manager%streamClock, ierr), ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         ! Add the alarm once
         call MPAS_stream_mgr_add_alarm(f%manager, 'dup_stream', 'dup_alarm', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Attempt to add the same alarm again (should fail)
         call MPAS_stream_mgr_add_alarm(f%manager, 'dup_stream', 'dup_alarm', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select

   end subroutine test_add_alarm

   subroutine test_remove_pkg(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use fortest_assert, only: assert_true, assert_false, assert_equal
      use mpas_stream_manager
      use mpas_stream_list
      use stream_manager_fixture, only: stream_manager_fixture_t
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      type(MPAS_stream_list_type), pointer :: stream
      logical, pointer :: pkg_ptr
      logical :: stream_exists

      call c_f_pointer(f_ptr, f)
      nullify(stream)
      nullify(pkg_ptr)

      select case (param_idx)

         !-------------------------------------------------------------------
         ! Case 1: Successful package removal
         !-------------------------------------------------------------------
      case (1)
         ! Create a new stream
         call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Attach an existing package first (fixture provides package1)
         call MPAS_stream_mgr_add_pkg(f%manager, 'stream', 'package1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify the package is present
         stream_exists = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
         call assert_true(stream_exists, verbosity = 2)
         call mpas_pool_get_package(stream%pkg_pool, 'package1', pkg_ptr)
         call assert_true(associated(pkg_ptr), verbosity = 2)

         ! Remove the package
         call MPAS_stream_mgr_remove_pkg(f%manager, 'stream', 'package1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify package is no longer present
         call mpas_pool_get_package(stream%pkg_pool, 'package1', pkg_ptr)
         call assert_false(associated(pkg_ptr), verbosity = 2)

         !-------------------------------------------------------------------
         ! Case 2: Stream does not exist
         !-------------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_remove_pkg(f%manager, 'nonexistent_stream', 'package1', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select
   end subroutine test_remove_pkg

   subroutine test_remove_alarm(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use mpas_stream_manager
      use mpas_timekeeping
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use stream_manager_fixture, only: stream_manager_fixture_t
      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f

      integer :: ierr
      type(MPAS_stream_list_type), pointer :: stream, alarm_node
      logical :: success

      call c_f_pointer(f_ptr, f)

      select case (param_idx)

         !--------------------------------------------------------------
         ! Case 1: Successfully remove an input alarm from a valid stream
         !--------------------------------------------------------------
      case (1)
         ! Create a stream and alarm
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_in', MPAS_STREAM_INPUT, 'file.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call mpas_add_clock_alarm(f%manager%streamClock, 'alarm_in', &
               mpas_get_clock_time(f%manager%streamClock, MPAS_START_TIME, ierr), &
               mpas_get_clock_timestep(f%manager%streamClock, ierr), ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_in', 'alarm_in', MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Sanity check: alarm exists
         success = MPAS_stream_list_query(f%manager%alarms_in, 'alarm_in', alarm_node, ierr = ierr)
         call assert_true(success, verbosity = 2)
         call assert_true(associated(alarm_node%streamList), verbosity = 2)

         ! Now remove the alarm
         call MPAS_stream_mgr_remove_alarm(f%manager, 'stream_in', 'alarm_in', MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Verify that the alarm was removed from the stream
         nullify(stream)
         success = MPAS_stream_list_query(f%manager%streams, 'stream_in', stream, ierr = ierr)
         call assert_true(success, verbosity = 2)
         success = MPAS_stream_list_query(stream%alarmList_in, 'alarm_in', alarm_node, ierr = ierr)
         call assert_false(success, verbosity = 2)

         ! Verify that the manager’s alarm list was cleared
         success = MPAS_stream_list_query(f%manager%alarms_in, 'alarm_in', alarm_node, ierr = ierr)
         call assert_false(success, verbosity = 2)

         !--------------------------------------------------------------
         ! Case 2: Attempt to remove an alarm from a non-existent stream
         !--------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_remove_alarm(f%manager, 'missing_stream', 'alarm_in', MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !--------------------------------------------------------------
         ! Case 3: Attempt to remove an alarm that does not exist on the stream
         !--------------------------------------------------------------
      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_no_alarm', MPAS_STREAM_OUTPUT, 'file.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call mpas_add_clock_alarm(f%manager%streamClock, 'alarm_missing', &
               mpas_get_clock_time(f%manager%streamClock, MPAS_START_TIME, ierr), &
               mpas_get_clock_timestep(f%manager%streamClock, ierr), ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         ! No call to MPAS_stream_mgr_add_alarm, so alarm isn’t linked
         call MPAS_stream_mgr_remove_alarm(f%manager, 'stream_no_alarm', 'alarm_missing', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !--------------------------------------------------------------
         ! Case 4: Remove an output alarm that has multiple streams, ensuring
         !         it remains until last stream is removed
         !--------------------------------------------------------------
      case (4)
         ! Create two output streams
         call MPAS_stream_mgr_create_stream(f%manager, 'stream1', MPAS_STREAM_OUTPUT, 'out1.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream2', MPAS_STREAM_OUTPUT, 'out2.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Add the clock alarm
         call mpas_add_clock_alarm(f%manager%streamClock, 'shared_alarm', &
               mpas_get_clock_time(f%manager%streamClock, MPAS_START_TIME, ierr), &
               mpas_get_clock_timestep(f%manager%streamClock, ierr), ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         ! Add the alarm to both streams
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream1', 'shared_alarm', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream2', 'shared_alarm', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Remove alarm from stream1 only
         call MPAS_stream_mgr_remove_alarm(f%manager, 'stream1', 'shared_alarm', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Alarm should still exist in manager’s alarm list
         success = MPAS_stream_list_query(f%manager%alarms_out, 'shared_alarm', alarm_node, ierr = ierr)
         call assert_true(success, verbosity = 2)
         call assert_true(MPAS_stream_list_length(alarm_node%streamList) == 1, verbosity = 2)

         ! Remove alarm from stream2 (last one)
         call MPAS_stream_mgr_remove_alarm(f%manager, 'stream2', 'shared_alarm', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Now alarm should be deleted entirely
         success = MPAS_stream_list_query(f%manager%alarms_out, 'shared_alarm', alarm_node, ierr = ierr)
         call assert_false(success, verbosity = 2)

         !--------------------------------------------------------------
         ! Case 5: Attempt to remove using invalid direction
         !--------------------------------------------------------------
      case (5)
         call MPAS_stream_mgr_create_stream(f%manager, 'bad_dir_stream', MPAS_STREAM_OUTPUT, 'file.nc', ierr = ierr)
         call mpas_add_clock_alarm(f%manager%streamClock, 'alarm_bad_dir', &
               mpas_get_clock_time(f%manager%streamClock, MPAS_START_TIME, ierr), &
               mpas_get_clock_timestep(f%manager%streamClock, ierr), ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, 'bad_dir_stream', 'alarm_bad_dir', MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Direction not INPUT or OUTPUT → should fail
         call MPAS_stream_mgr_remove_alarm(f%manager, 'bad_dir_stream', 'alarm_bad_dir', MPAS_STREAM_INPUT_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select

   end subroutine test_remove_alarm

   subroutine test_reset_alarms(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use mpas_stream_manager
      use mpas_timekeeping
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use stream_manager_fixture, only: stream_manager_fixture_t
      use fortest_assert, only: assert_equal, assert_true, assert_false
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      logical :: is_ringing
      character(len = StrKIND) :: alarm_id1, alarm_id2

      call c_f_pointer(f_ptr, f)
      alarm_id1 = 'alarm1'
      alarm_id2 = 'alarm2'

      select case (param_idx)

         !-----------------------------------------------------------------------
         ! Case 1: Reset all ringing alarms (no streamID or direction)
         !-----------------------------------------------------------------------
      case (1)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_input', MPAS_STREAM_INPUT, 'file_input.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_output.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_input', alarm_id1, MPAS_STREAM_INPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_output', alarm_id2, MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_true(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         call MPAS_stream_mgr_reset_alarms(f%manager, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_false(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_false(is_ringing, verbosity = 2)

         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2, ierr = ierr)


         !-----------------------------------------------------------------------
         ! Case 2: Reset INPUT direction alarms only
         !-----------------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_input', MPAS_STREAM_INPUT, 'file_input.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_output.nc', ierr = ierr)

         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2, f%clock_start_time, f%clock_time_step, ierr = ierr)

         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_input', alarm_id1, MPAS_STREAM_INPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_output', alarm_id2, MPAS_STREAM_OUTPUT, ierr = ierr)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_true(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         call MPAS_stream_mgr_reset_alarms(f%manager, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_false(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2, ierr = ierr)


         !-----------------------------------------------------------------------
         ! Case 3: Reset OUTPUT direction alarms only
         !-----------------------------------------------------------------------
      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_input', MPAS_STREAM_INPUT, 'file_input.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_output.nc', ierr = ierr)

         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2, f%clock_start_time, f%clock_time_step, ierr = ierr)

         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_input', alarm_id1, MPAS_STREAM_INPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_output', alarm_id2, MPAS_STREAM_OUTPUT, ierr = ierr)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_true(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         call MPAS_stream_mgr_reset_alarms(f%manager, direction = MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_true(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_false(is_ringing, verbosity = 2)

         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2, ierr = ierr)


         !-----------------------------------------------------------------------
         ! Case 4: Invalid stream name should return error
         !-----------------------------------------------------------------------
      case (4)
         call MPAS_stream_mgr_reset_alarms(f%manager, streamID = 'nonexistent', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)


         !-----------------------------------------------------------------------
         ! Case 5: Reset alarms for a specific stream ID
         !-----------------------------------------------------------------------
      case (5)
         call MPAS_stream_mgr_create_stream(f%manager, 'streamA', MPAS_STREAM_OUTPUT, 'fileA.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'streamB', MPAS_STREAM_OUTPUT, 'fileB.nc', ierr = ierr)

         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2, f%clock_start_time, f%clock_time_step, ierr = ierr)

         call MPAS_stream_mgr_add_alarm(f%manager, 'streamA', alarm_id1, MPAS_STREAM_OUTPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, 'streamB', alarm_id2, MPAS_STREAM_OUTPUT, ierr = ierr)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_true(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         call MPAS_stream_mgr_reset_alarms(f%manager, streamID = 'streamA', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_false(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2, ierr = ierr)


         !-----------------------------------------------------------------------
         ! Case 6: Reset INPUT direction for OUTPUT stream ID (should not affect any alarms)
         !-----------------------------------------------------------------------
      case (6)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_input', MPAS_STREAM_INPUT, 'file_input.nc', ierr = ierr)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_output.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2, f%clock_start_time, f%clock_time_step, ierr = ierr)

         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_input', alarm_id1, MPAS_STREAM_INPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_output', alarm_id2, MPAS_STREAM_OUTPUT, ierr = ierr)

         ! Verify both are ringing
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_true(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         ! Try to reset INPUT alarms but filter on OUTPUT stream
         call MPAS_stream_mgr_reset_alarms(f%manager, streamID = 'stream_output', direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Neither alarm should have changed
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id1)
         call assert_true(is_ringing, verbosity = 2)
         is_ringing = mpas_is_alarm_ringing(f%manager%streamClock, alarm_id2)
         call assert_true(is_ringing, verbosity = 2)

         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2, ierr = ierr)

      end select
   end subroutine test_reset_alarms

   subroutine test_ringing_alarms(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use mpas_stream_manager
      use mpas_timekeeping
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use stream_manager_fixture, only: stream_manager_fixture_t
      use fortest_assert, only: assert_equal, assert_true, assert_false
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      logical :: result
      character(len = StrKIND) :: alarm_id1, alarm_id2

      call c_f_pointer(f_ptr, f)
      alarm_id1 = 'alarm1'
      alarm_id2 = 'alarm2'

      !-----------------------------------------------------------------------
      ! Common setup for all valid cases
      !-----------------------------------------------------------------------
      if (param_idx >= 1 .and. param_idx <= 10) then
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_input', MPAS_STREAM_INPUT, 'file_input.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_output.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Add two distinct alarms to the clock
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         ! Attach alarms to both streams
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_input', alarm_id1, MPAS_STREAM_INPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, 'stream_output', alarm_id2, MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
      end if

      !-----------------------------------------------------------------------
      ! Case-specific behavior
      !-----------------------------------------------------------------------
      select case (param_idx)

         !-----------------------------------------------------------------------
         ! Case 1: Any alarms ringing (no streamID, no direction)
         !-----------------------------------------------------------------------
      case (1)
         result = MPAS_stream_mgr_ringing_alarms(f%manager, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_true(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 2: INPUT direction only — should detect alarm1
         !-----------------------------------------------------------------------
      case (2)
         result = MPAS_stream_mgr_ringing_alarms(f%manager, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_true(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 3: OUTPUT direction only — should detect alarm2
         !-----------------------------------------------------------------------
      case (3)
         result = MPAS_stream_mgr_ringing_alarms(f%manager, direction = MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_true(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 4: Specific streamID (stream_input)
         !-----------------------------------------------------------------------
      case (4)
         result = MPAS_stream_mgr_ringing_alarms(f%manager, streamID = 'stream_input', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_true(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 5: Specific streamID (stream_output)
         !-----------------------------------------------------------------------
      case (5)
         result = MPAS_stream_mgr_ringing_alarms(f%manager, streamID = 'stream_output', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_true(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 6: Nonexistent streamID should return false and set error
         !-----------------------------------------------------------------------
      case (6)
         result = MPAS_stream_mgr_ringing_alarms(f%manager, streamID = 'nonexistent', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)
         call assert_false(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 7: After reset_alarms — all alarms cleared
         !-----------------------------------------------------------------------
      case (7)
         call MPAS_stream_mgr_reset_alarms(f%manager, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         result = MPAS_stream_mgr_ringing_alarms(f%manager, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_false(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 8: After resetting only INPUT direction
         !-----------------------------------------------------------------------
      case (8)
         call MPAS_stream_mgr_reset_alarms(f%manager, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! INPUT alarm should no longer ring
         result = MPAS_stream_mgr_ringing_alarms(f%manager, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_false(result, verbosity = 2)

         ! OUTPUT alarm should still ring
         result = MPAS_stream_mgr_ringing_alarms(f%manager, direction = MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_true(result, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 9: After resetting only OUTPUT direction
         !-----------------------------------------------------------------------
      case (9)
         call MPAS_stream_mgr_reset_alarms(f%manager, direction = MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! OUTPUT alarm cleared
         result = MPAS_stream_mgr_ringing_alarms(f%manager, direction = MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_false(result, verbosity = 2)

         ! INPUT alarm still active
         result = MPAS_stream_mgr_ringing_alarms(f%manager, direction = MPAS_STREAM_INPUT, ierr = ierr)
         call assert_true(result, verbosity = 2)

      case (10)
         call MPAS_stream_mgr_reset_alarms(f%manager, streamID = 'stream_input', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         result = MPAS_stream_mgr_ringing_alarms(f%manager, streamID = 'stream_input', ierr = ierr)
         call assert_false(result, verbosity = 2)

         result = MPAS_stream_mgr_ringing_alarms(f%manager, streamID = 'stream_output', ierr = ierr)
         call assert_true(result, verbosity = 2)

      end select

      !-----------------------------------------------------------------------
      ! Common teardown
      !-----------------------------------------------------------------------
      if (param_idx >= 1 .and. param_idx <= 10) then
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2, ierr = ierr)
      end if

   end subroutine test_ringing_alarms

   subroutine test_get_stream_interval(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use mpas_stream_manager
      use mpas_timekeeping
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use stream_manager_fixture, only: stream_manager_fixture_t
      use fortest_assert, only: assert_equal, assert_true
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      type(MPAS_TimeInterval_type) :: interval
      integer :: ierr
      real(RKIND) :: seconds
      character(len = StrKIND) :: alarm_id1_min, alarm_id1_max
      character(len = StrKIND) :: alarm_id2_min, alarm_id2_max
      character(len = StrKIND) :: stream_in, stream_out
      Type(MPAS_TimeInterval_type) :: min_input_interval, min_output_interval

      call c_f_pointer(f_ptr, f)
      stream_in = 'stream_input'
      stream_out = 'stream_output'
      alarm_id1_min = 'alarm1_min'
      alarm_id1_max = 'alarm1_max'
      alarm_id2_min = 'alarm2_min'
      alarm_id2_max = 'alarm2_max'
      min_input_interval = f%clock_time_step / 4
      min_output_interval = f%clock_time_step / 2

      !-----------------------------------------------------------------------
      ! Common setup for all valid cases (1–6)
      !-----------------------------------------------------------------------
      if (param_idx >= 1 .and. param_idx <= 6) then
         call MPAS_stream_mgr_create_stream(f%manager, stream_in, MPAS_STREAM_INPUT, 'file_in.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call MPAS_stream_mgr_create_stream(f%manager, stream_out, MPAS_STREAM_OUTPUT, 'file_out.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1_min, f%clock_start_time, min_input_interval, ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id1_max, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2_min, f%clock_start_time, min_output_interval, ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)
         call mpas_add_clock_alarm(f%manager%streamClock, alarm_id2_max, f%clock_start_time, f%clock_time_step, ierr = ierr)
         call assert_equal(ierr, 0, verbosity = 2)

         ! Attach the alarm to both directions for test consistency
         call MPAS_stream_mgr_add_alarm(f%manager, stream_in, alarm_id1_min, MPAS_STREAM_INPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, stream_in, alarm_id1_max, MPAS_STREAM_INPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, stream_out, alarm_id2_min, MPAS_STREAM_OUTPUT, ierr = ierr)
         call MPAS_stream_mgr_add_alarm(f%manager, stream_out, alarm_id2_max, MPAS_STREAM_OUTPUT, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
      end if

      !-----------------------------------------------------------------------
      ! Case-specific tests
      !-----------------------------------------------------------------------
      select case (param_idx)

         !-----------------------------------------------------------------------
         ! Case 1: Valid INPUT stream interval
         !-----------------------------------------------------------------------
      case (1)
         interval = MPAS_stream_mgr_get_stream_interval(f%manager, stream_in, MPAS_STREAM_INPUT, ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_true(interval .EQ. min_input_interval, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 2: Valid OUTPUT stream interval
         !-----------------------------------------------------------------------
      case (2)
         interval = MPAS_stream_mgr_get_stream_interval(f%manager, stream_out, MPAS_STREAM_OUTPUT, ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
         call assert_true(interval .EQ. min_output_interval, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 3: Invalid request — INPUT interval for OUTPUT-only stream
         !-----------------------------------------------------------------------
      case (3)
         interval = MPAS_stream_mgr_get_stream_interval(f%manager, stream_out, MPAS_STREAM_INPUT, ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 4: Invalid request — OUTPUT interval for INPUT-only stream
         !-----------------------------------------------------------------------
      case (4)
         interval = MPAS_stream_mgr_get_stream_interval(f%manager, stream_in, MPAS_STREAM_OUTPUT, ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 5: Invalid direction code (not input or output)
         !-----------------------------------------------------------------------
      case (5)
         interval = MPAS_stream_mgr_get_stream_interval(f%manager, stream_in, -999, ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 6: Nonexistent stream name
         !-----------------------------------------------------------------------
      case (6)
         interval = MPAS_stream_mgr_get_stream_interval(f%manager, 'nonexistent', MPAS_STREAM_INPUT, ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select

      !-----------------------------------------------------------------------
      ! Common teardown
      !-----------------------------------------------------------------------
      if (param_idx >= 1 .and. param_idx <= 6) then
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1_min, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id1_max, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2_min, ierr = ierr)
         call mpas_remove_clock_alarm(f%manager%streamClock, alarm_id2_max, ierr = ierr)
      end if

   end subroutine test_get_stream_interval

   subroutine test_add_att(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
      use mpas_stream_manager
      use mpas_pool_routines
      use iso_c_binding, only: c_ptr, c_f_pointer, c_int
      use stream_manager_fixture, only: stream_manager_fixture_t
      use fortest_assert, only: assert_equal, assert_true, assert_false
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      integer(c_int), value :: param_idx
      type(stream_manager_fixture_t), pointer :: f
      integer :: ierr
      integer, pointer :: queryVal
      character(len = StrKIND) :: attName
      integer :: attVal, retrievedVal

      call c_f_pointer(f_ptr, f)
      attName = 'num_files'
      attVal = 42

      !-----------------------------------------------------------------------
      ! Case-specific behavior
      !-----------------------------------------------------------------------
      select case (param_idx)

         !-----------------------------------------------------------------------
         ! Case 1: Add attribute to an existing stream (streamID present)
         !-----------------------------------------------------------------------
      case (1)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_out.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_add_att(f%manager, attName, attVal, streamID = 'stream_output', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         nullify(queryVal)
         call mpas_pool_get_config(f%manager%streams%head%att_pool, attName, queryVal)
         call assert_true(associated(queryVal), verbosity = 2)
         call assert_equal(queryVal, attVal, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 2: Add attribute globally (applies to defaultAtts)
         !-----------------------------------------------------------------------
      case (2)
         call MPAS_stream_mgr_add_att(f%manager, attName, attVal, ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         nullify(queryVal)
         call mpas_pool_get_config(f%manager%defaultAtts, attName, queryVal)
         call assert_true(associated(queryVal), verbosity = 2)
         call assert_equal(queryVal, attVal, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 3: Update existing integer attribute (overwrite value)
         !-----------------------------------------------------------------------
      case (3)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_out.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         call MPAS_stream_mgr_add_att(f%manager, attName, attVal, streamID = 'stream_output', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Overwrite value
         call MPAS_stream_mgr_add_att(f%manager, attName, attVal + 10, streamID = 'stream_output', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         nullify(queryVal)
         call mpas_pool_get_config(f%manager%streams%head%att_pool, attName, queryVal)
         call assert_true(associated(queryVal), verbosity = 2)
         call assert_equal(queryVal, attVal + 10, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 4: Nonexistent streamID should return error
         !-----------------------------------------------------------------------
      case (4)
         call MPAS_stream_mgr_add_att(f%manager, attName, attVal, streamID = 'nonexistent', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

         !-----------------------------------------------------------------------
         ! Case 5: Attribute exists but is non-integer type (should trigger error)
         !-----------------------------------------------------------------------
      case (5)
         call MPAS_stream_mgr_create_stream(f%manager, 'stream_output', MPAS_STREAM_OUTPUT, 'file_out.nc', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)

         ! Manually add a character attribute with same name to trigger type conflict
         call mpas_pool_add_config(f%manager%streams%head%att_pool, attName, 'wrong_type')
         call MPAS_stream_mgr_add_att(f%manager, attName, attVal, streamID = 'stream_output', ierr = ierr)
         call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity = 2)

      end select

   end subroutine test_add_att
end module

program test_stream_manager
   use fortest_test_session, only: test_session_t

   use session_fixture_mod, only: session_fixture_t, &
         setup_session, &
         teardown_session
   use iso_c_binding, only: c_loc, c_ptr
   use test_stream_manager_mod, only: &
         test_create_stream, &
         test_destroy_stream, &
         test_add_pool, &
         test_add_field, &
         test_add_stream_fields, &
         test_remove_field, &
         test_add_pkg, &
         test_remove_pkg, &
         test_add_alarm, &
         test_remove_alarm, &
         test_reset_alarms, &
         test_ringing_alarms, &
         test_get_stream_interval, &
         test_add_att

   use stream_manager_fixture, only: stream_manager_fixture_t, &
         setup_stream_manager, teardown_stream_manager
   implicit none
   type(test_session_t) :: session
   type(c_ptr) :: test_fixture_ptr, session_fixture_ptr
   type(session_fixture_t), target :: session_fixture
   type(stream_manager_fixture_t), target :: test_fixture
   integer :: ierr

   call MPI_Init(ierr)
   session_fixture_ptr = c_loc(session_fixture)
   test_fixture_ptr = c_loc(test_fixture)

   ! Register the test suite and fixtures
   call session%register_test_suite("stream_manager_test")

   call session%register_fixture(&
         setup = setup_stream_manager, &
         teardown = teardown_stream_manager, &
         args = test_fixture_ptr, &
         scope = "test", &
         test_suite_name = "stream_manager_test")
   !    Register tests
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_create_stream", &
         test = test_create_stream, &
         num_params = 1)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_destroy_stream", &
         test = test_destroy_stream, &
         num_params = 3)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_add_pool", &
         test = test_add_pool, &
         num_params = 6)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_add_field", &
         test = test_add_field, &
         num_params = 6)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_add_stream_fields", &
         test = test_add_stream_fields, &
         num_params = 6)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_remove_field", &
         test = test_remove_field, &
         num_params = 5)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_add_pkg", &
         test = test_add_pkg, &
         num_params = 3)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_remove_pkg", &
         test = test_remove_pkg, &
         num_params = 2)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_add_alarm", &
         test = test_add_alarm, &
         num_params = 4)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_remove_alarm", &
         test = test_remove_alarm, &
         num_params = 5)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_reset_alarms", &
         test = test_reset_alarms, &
         num_params = 6)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_ringing_alarms", &
         test = test_ringing_alarms, &
         num_params = 10)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_get_stream_interval", &
         test = test_get_stream_interval, &
         num_params = 6)
   call session%register_parameterized_test(&
         test_suite_name = "stream_manager_test", &
         test_name = "test_add_att", &
         test = test_add_att, &
         num_params = 5)


   ! Run the tests
   call session%run()
   call session%finalize()
   call MPI_Finalize(ierr)

end program

