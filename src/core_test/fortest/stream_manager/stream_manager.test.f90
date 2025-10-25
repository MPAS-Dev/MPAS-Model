module test_stream_manager_mod
    use mpi
    use fortest_test_session, only : test_session_t
    use fortest_assert, only : assert_true, assert_false, assert_equal
    use iso_c_binding, only : c_ptr, c_f_pointer
    use mpas_subdriver
    use mpas_timekeeping
    use mpas_derived_types, only : core_type, domain_type
    use stream_manager_fixture, only : stream_manager_fixture_t
    use mpas_stream_list
    implicit none
contains

    subroutine test_create_stream(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
        use mpas_stream_manager
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        use stream_manager_fixture, only : stream_manager_fixture_t
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
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        use fortest_assert, only : assert_true, assert_false, assert_equal
        use mpas_stream_list
        use mpas_stream_manager
        use stream_manager_fixture, only : stream_manager_fixture_t
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
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        use fortest_assert, only : assert_true, assert_false, assert_equal
        use mpas_stream_list
        use mpas_stream_manager
        use stream_manager_fixture, only : stream_manager_fixture_t
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

    subroutine test_add_field(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        use fortest_assert, only : assert_true, assert_false, assert_equal
        use mpas_stream_manager
        use mpas_stream_list
        use stream_manager_fixture, only : stream_manager_fixture_t
        implicit none

        type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
        integer(c_int), value :: param_idx
        type(stream_manager_fixture_t), pointer :: f
        integer :: ierr
        type(MPAS_stream_list_type), pointer :: stream
        integer, pointer :: test_ptr_int
        character(len=StrKIND), pointer :: test_ptr_char
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
            call mpas_pool_get_config(stream%field_pool, 'field1', value=test_ptr_int)
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
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', packages='package1', ierr = ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity = 2)
            stream_exists = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
            call mpas_pool_get_config(stream%field_pkg_pool, 'field1:packages', value=test_ptr_char)
            call assert_true(associated(test_ptr_char), verbosity = 2)
            if (associated(test_ptr_char)) then
                call assert_equal(trim(test_ptr_char), 'package1', verbosity = 2)
            end if
        end select
    end subroutine test_add_field

    subroutine test_add_stream_fields(f_ptr, ts_ptr, s_ptr, param_idx)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        use fortest_assert, only : assert_true, assert_false, assert_equal
        use mpas_stream_manager
        use mpas_stream_list
        use stream_manager_fixture, only : stream_manager_fixture_t
        implicit none

        type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
        integer(c_int), value :: param_idx
        type(stream_manager_fixture_t), pointer :: f
        integer :: ierr
        type(MPAS_stream_list_type), pointer :: stream, refStream
        integer, pointer :: test_ptr_int
        character(len=StrKIND), pointer :: test_ptr_char
        logical :: ok

        call c_f_pointer(f_ptr, f)
        nullify(stream)
        nullify(refStream)

        select case (param_idx)

            !------------------------------------------------------------
            ! Case 1: Normal operation — fields from refStream copied to target stream
            !------------------------------------------------------------
        case (1)
            call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_OUTPUT, 'target.nc', ierr=ierr)
            call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ! Add some fields to the reference stream
            call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field2', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ! Copy all fields from ref → target
            call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ! Verify that both fields now exist in the target stream
            ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
            call assert_true(ok, verbosity=2)
            call mpas_pool_get_config(stream%field_pool, 'field1', value=test_ptr_int)
            call assert_true(associated(test_ptr_int), verbosity=2)
            call mpas_pool_get_config(stream%field_pool, 'field2', value=test_ptr_int)
            call assert_true(associated(test_ptr_int), verbosity=2)

            !------------------------------------------------------------
            ! Case 2: Reference stream does not exist
            !------------------------------------------------------------
        case (2)
            call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_INPUT, 'target.nc', ierr=ierr)
            call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'no_ref', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity=2)

            !------------------------------------------------------------
            ! Case 3: Target stream does not exist
            !------------------------------------------------------------
        case (3)
            call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr=ierr)
            call MPAS_stream_mgr_add_stream_fields(f%manager, 'no_target', 'ref', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity=2)

            !------------------------------------------------------------
            ! Case 4: Target stream is immutable
            !------------------------------------------------------------
        case (4)
            call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_INPUT, 'target.nc', ierr=ierr)
            call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr=ierr)
            ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
            stream%immutable = .true.
            call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity=2)

            !------------------------------------------------------------
            ! Case 5: Fields already exist in target stream
            !------------------------------------------------------------
        case (5)
            call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_INPUT, 'target.nc', ierr=ierr)
            call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'target', 'field1', ierr=ierr)
            call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)
            ! Ensure the existing field is not duplicated
            ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
            call mpas_pool_get_config(stream%field_pool, 'field1', value=test_ptr_int)
            call assert_true(associated(test_ptr_int), verbosity=2)

            !------------------------------------------------------------
            ! Case 6: Attach packages when copying fields
            !------------------------------------------------------------
        case (6)
            call MPAS_stream_mgr_create_stream(f%manager, 'target', MPAS_STREAM_OUTPUT, 'target.nc', ierr=ierr)
            call MPAS_stream_mgr_create_stream(f%manager, 'ref', MPAS_STREAM_INPUT, 'ref.nc', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'ref', 'field1', ierr=ierr)
            call MPAS_stream_mgr_add_stream_fields(f%manager, 'target', 'ref', packages='pkg1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ok = MPAS_stream_list_query(f%manager%streams, 'target', stream)
            call mpas_pool_get_config(stream%field_pkg_pool, 'field1:packages', value=test_ptr_char)
            call assert_true(associated(test_ptr_char), verbosity=2)
            if (associated(test_ptr_char)) then
                call assert_equal(trim(test_ptr_char), 'pkg1', verbosity=2)
            end if

        end select
    end subroutine test_add_stream_fields

    subroutine test_remove_field(f_ptr, ts_ptr, s_ptr, param_idx) bind(C)
        use iso_c_binding, only : c_ptr, c_f_pointer, c_int
        use fortest_assert, only : assert_true, assert_false, assert_equal
        use mpas_stream_manager
        use mpas_stream_list
        use stream_manager_fixture, only : stream_manager_fixture_t
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
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field2', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field3', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ! Verify field1 exists
            ok = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
            call mpas_pool_get_config(stream%field_pool, 'field1', value=test_ptr)
            call assert_true(associated(test_ptr), verbosity=2)
            ! Verify field2 exists
            call mpas_pool_get_config(stream%field_pool, 'field2', value=test_ptr)
            call assert_true(associated(test_ptr), verbosity=2)
            ! Verify field3 exists
            call mpas_pool_get_config(stream%field_pool, 'field3', value=test_ptr)
            call assert_true(associated(test_ptr), verbosity=2)

            ! Remove field1
            call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)
            ! Remove field3
            call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field3', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)
            ! Remove field2
            call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field2', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ! Verify field1 no longer exists
            call mpas_pool_get_config(stream%field_pool, 'field1', value=test_ptr)
            call assert_false(associated(test_ptr), verbosity=2)
            ! Verify field2 no longer exists
            call mpas_pool_get_config(stream%field_pool, 'field2', value=test_ptr)
            call assert_false(associated(test_ptr), verbosity=2)
            ! Verify field3 no longer exists
            call mpas_pool_get_config(stream%field_pool, 'field3', value=test_ptr)
            call assert_false(associated(test_ptr), verbosity=2)

            !------------------------------------------------------------
            ! Case 2: Stream does not exist
            !------------------------------------------------------------
        case (2)
            call MPAS_stream_mgr_remove_field(f%manager, 'nonexistent', 'field1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity=2)

            !------------------------------------------------------------
            ! Case 3: Field does not exist in valid stream
            !------------------------------------------------------------
        case (3)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ! Attempt to remove a non-existent field
            call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field_missing', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity=2)

            !------------------------------------------------------------
            ! Case 4: Stream is immutable
            !------------------------------------------------------------
        case (4)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr=ierr)

            ok = MPAS_stream_list_query(f%manager%streams, 'stream', stream)
            stream%immutable = .true.

            call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity=2)

            ! Verify field still exists
            call mpas_pool_get_config(stream%field_pool, 'field1', value=test_ptr)
            call assert_true(associated(test_ptr), verbosity=2)

            !------------------------------------------------------------
            ! Case 5: Attempting to remove a field twice
            !------------------------------------------------------------
        case (5)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test.nc', ierr=ierr)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_NOERR, verbosity=2)

            ! Second removal should error (field already gone)
            call MPAS_stream_mgr_remove_field(f%manager, 'stream', 'field1', ierr=ierr)
            call assert_equal(ierr, MPAS_STREAM_MGR_ERROR, verbosity=2)

        end select
    end subroutine test_remove_field




end module

program test_stream_manager
    use fortest_test_session, only : test_session_t

    use session_fixture_mod, only : session_fixture_t, &
            setup_session, &
            teardown_session
    use iso_c_binding, only : c_loc, c_ptr
    use test_stream_manager_mod, only : &
            test_create_stream, &
            test_destroy_stream, &
            test_add_pool, &
            test_add_field, &
            test_add_stream_fields, &
            test_remove_field
    use stream_manager_fixture, only : stream_manager_fixture_t, &
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
    ! Register tests
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

    ! Run the tests
    call session%run()
    call session%finalize()
    call MPI_Finalize(ierr)

end program

