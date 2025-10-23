!=======================================================================
!  Module: stream_manager_tests_mod
!
!> \brief Regression tests for MPAS stream manager routines.
!> \author Andy Stokely
!> \date   10/20/2025
!> \details
!>   This module provides unit-style regression tests for key MPAS
!>   stream manager routines, including stream creation, destruction,
!>   pool association, and field addition. All tests use MPAS-native
!>   logging instead of an external framework and are structured in
!>   the same style as the MPAS alarm tests.
!=======================================================================
module mpas_stream_manager_tests_mod
    use mpi
    use mpas_log,            only : mpas_log_write
    use mpas_subdriver
    use mpas_derived_types
    use mpas_timekeeping
    use mpas_stream_manager
    use mpas_stream_list
    implicit none

    !-----------------------------------------------------------------------
    !> \brief Fixture that provides a valid MPAS stream manager context.
    !-----------------------------------------------------------------------
    type stream_manager_fixture_t
        type(core_type), pointer   :: core_list => null()
        type(domain_type), pointer :: domain    => null()
        type(MPAS_streamManager_type), pointer :: manager
        type(MPAS_Clock_type), pointer :: clock
    end type stream_manager_fixture_t

contains

    !-----------------------------------------------------------------------
    !  subroutine setup_stream_manager_fixture
    !
    !> \brief Initialize a stream manager fixture with clock and pools.
    !-----------------------------------------------------------------------
    subroutine setup_stream_manager_fixture(f)
        implicit none
        type(stream_manager_fixture_t), intent(inout) :: f
        integer :: ierr
        type(MPAS_Pool_type), pointer :: allFields, allStructs, allPackages
        type(MPAS_IO_context_type), pointer :: ioContext

        call mpas_init(f%core_list, f%domain, external_comm=MPI_COMM_WORLD, &
                namelistFileParam='namelist.test', streamsFileParam='streams.test')
        f%clock => f%domain%clock

        allocate(allFields)
        call mpas_pool_create_pool(allFields)
        call mpas_pool_add_field(allFields, 'field1')
        call mpas_pool_add_field(allFields, 'field2')

        allocate(allStructs)
        call mpas_pool_create_pool(allStructs)
        call mpas_pool_add_subpool(allStructs, 'struct1')
        call mpas_pool_add_subpool(allStructs, 'struct2')

        allocate(allPackages)
        call mpas_pool_create_pool(allPackages)
        call mpas_pool_add_package(allPackages, 'package1')
        call mpas_pool_add_package(allPackages, 'package2')

        allocate(ioContext)
        allocate(f%manager)

        call MPAS_stream_mgr_init(f%manager, ioContext, f%clock, &
                allFields, allPackages, allStructs, ierr=ierr)
    end subroutine setup_stream_manager_fixture


    !-----------------------------------------------------------------------
    !  subroutine teardown_stream_manager_fixture
    !
    !> \brief Finalize and deallocate stream manager fixture.
    !-----------------------------------------------------------------------
    subroutine teardown_stream_manager_fixture(f)
        implicit none
        type(stream_manager_fixture_t), intent(inout) :: f
        call mpas_finalize(f%core_list, f%domain)
        nullify(f%manager, f%clock, f%domain, f%core_list)
    end subroutine teardown_stream_manager_fixture


    !=======================================================================
    !  STREAM CREATION TESTS
    !=======================================================================
    subroutine test_create_stream(f, case_id, ierr)
        implicit none
        type(stream_manager_fixture_t), intent(inout) :: f
        integer, intent(in) :: case_id
        integer, intent(inout) :: ierr
        integer :: local_err, rc, int_val
        character(len=StrKIND) :: char_val
        local_err = 0

        call setup_stream_manager_fixture(f)

        select case (case_id)
        case (1)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test_output.nc', ierr=rc)
            if (rc == MPAS_STREAM_MGR_NOERR) then
                call mpas_log_write('PASS: stream created successfully')
            else
                call mpas_log_write('FAIL: stream creation failed')
                local_err = local_err + 1
            end if

        case (2)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'test_output.nc', ierr=rc)
            call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_FILENAME, char_val, &
                    direction=MPAS_STREAM_INPUT, ierr=rc)
            if (trim(char_val) /= 'test_output.nc') then
                call mpas_log_write('FAIL: filename property incorrect')
                local_err = local_err + 1
            end if

            call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_PRECISION, int_val, &
                    direction=MPAS_STREAM_INPUT, ierr=rc)
            if (int_val /= MPAS_IO_NATIVE_PRECISION) then
                call mpas_log_write('FAIL: precision property incorrect')
                local_err = local_err + 1
            end if

            call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_FILENAME_INTV, char_val, &
                    direction=MPAS_STREAM_INPUT, ierr=rc)
            if (trim(char_val) /= 'none') then
                call mpas_log_write('FAIL: filename interval property incorrect')
                local_err = local_err + 1
            end if

            call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_IOTYPE, int_val, &
                    direction=MPAS_STREAM_INPUT, ierr=rc)
            if (int_val /= MPAS_IO_PNETCDF) then
                call mpas_log_write('FAIL: IOTYPE property incorrect')
                local_err = local_err + 1
            end if

            call MPAS_stream_mgr_get_property(f%manager, 'stream', MPAS_STREAM_PROPERTY_CLOBBER, int_val, &
                    direction=MPAS_STREAM_INPUT, ierr=rc)
            if (int_val /= MPAS_STREAM_CLOBBER_NEVER) then
                call mpas_log_write('FAIL: CLOBBER property incorrect')
                local_err = local_err + 1
            end if

            if (local_err == 0) call mpas_log_write('PASS: default properties correct')
        end select

        ierr = ierr + local_err
        call teardown_stream_manager_fixture(f)
    end subroutine test_create_stream


    !=======================================================================
    !  STREAM DESTRUCTION TESTS
    !=======================================================================
    subroutine test_destroy_stream(f, case_id, ierr)
        implicit none
        type(stream_manager_fixture_t), intent(inout) :: f
        integer, intent(in) :: case_id
        integer, intent(inout) :: ierr
        integer :: rc, local_err, n_before
        type(MPAS_stream_list_type), pointer :: s
        logical :: exists
        local_err = 0

        call setup_stream_manager_fixture(f)

        select case (case_id)
        case (1)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            n_before = f%manager%numStreams
            call MPAS_stream_mgr_destroy_stream(f%manager, 'stream', ierr=rc)
            if (rc == MPAS_STREAM_MGR_NOERR .and. f%manager%numStreams == n_before - 1) then
                call mpas_log_write('PASS: stream destroyed successfully')
            else
                call mpas_log_write('FAIL: destroy_stream failed')
                local_err = local_err + 1
            end if
            nullify(s)
            exists = MPAS_stream_list_query(f%manager%streams, 'stream', s, rc)
            if (exists) then
                call mpas_log_write('FAIL: stream still exists after destroy')
                local_err = local_err + 1
            end if

        case (2)
            n_before = f%manager%numStreams
            call MPAS_stream_mgr_destroy_stream(f%manager, 'nonexistent', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR .and. f%manager%numStreams == n_before) then
                call mpas_log_write('PASS: nonexistent stream handled correctly')
            else
                call mpas_log_write('FAIL: nonexistent stream mishandled')
                local_err = local_err + 1
            end if

        case (3)
            call MPAS_stream_mgr_create_stream(f%manager, 's1', MPAS_STREAM_INPUT, 'f1.nc', ierr=rc)
            call MPAS_stream_mgr_create_stream(f%manager, 's2', MPAS_STREAM_INPUT, 'f2.nc', ierr=rc)
            call MPAS_stream_mgr_create_stream(f%manager, 's3', MPAS_STREAM_INPUT, 'f3.nc', ierr=rc)
            n_before = f%manager%numStreams
            call MPAS_stream_mgr_destroy_stream(f%manager, 's2', ierr=rc)
            if (rc /= MPAS_STREAM_MGR_NOERR .or. f%manager%numStreams /= n_before - 1) then
                call mpas_log_write('FAIL: destroy_stream sequence inconsistent')
                local_err = local_err + 1
            else
                call mpas_log_write('PASS: sequential destroy consistent')
            end if
        end select

        ierr = ierr + local_err
        call teardown_stream_manager_fixture(f)
    end subroutine test_destroy_stream


    !=======================================================================
    !  ADD POOL TESTS
    !
    !> \brief Validate behavior of MPAS_stream_mgr_add_pool across valid and
    !>        invalid subpool additions.
    !> \author Andy Stokely
    !> \date   10/20/2025
    !> \details
    !>   Exercises six core cases:
    !>     (1) Add valid pool to an existing stream.
    !>     (2) Attempt to add pool to non-existent stream.
    !>     (3) Known bug case (omitted).
    !>     (4–6) Add invalid subpools (struct4, struct5, struct6) to confirm
    !>           consistent rejection behavior.
    !=======================================================================
    subroutine test_add_pool(f, case_id, ierr)
        implicit none
        type(stream_manager_fixture_t), intent(inout) :: f
        integer, intent(in) :: case_id
        integer, intent(inout) :: ierr
        integer :: rc, local_err
        local_err = 0

        call setup_stream_manager_fixture(f)

        select case (case_id)

            !-----------------------------------------------------------------------
            ! Case 1: Add valid pool to a valid stream
            !-----------------------------------------------------------------------
        case (1)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct1', ierr=rc)
            if (rc == MPAS_STREAM_MGR_NOERR) then
                call mpas_log_write('PASS: pool struct1 added successfully')
            else
                call mpas_log_write('FAIL: failed to add valid pool struct1')
                local_err = local_err + 1
            end if

            !-----------------------------------------------------------------------
            ! Case 2: Attempt to add pool to non-existent stream
            !-----------------------------------------------------------------------
        case (2)
            call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct1', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR) then
                call mpas_log_write('PASS: correctly rejected pool addition to non-existent stream')
            else
                call mpas_log_write('FAIL: accepted pool addition to non-existent stream')
                local_err = local_err + 1
            end if

            !-----------------------------------------------------------------------
            ! Case 4: Add invalid pool struct4
            !-----------------------------------------------------------------------
        case (4)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct4', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR) then
                call mpas_log_write('PASS: invalid pool struct4 correctly rejected')
            else
                call mpas_log_write('FAIL: invalid pool struct4 accepted')
                local_err = local_err + 1
            end if

            !-----------------------------------------------------------------------
            ! Case 5: Add invalid pool struct5
            !-----------------------------------------------------------------------
        case (5)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct5', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR) then
                call mpas_log_write('PASS: invalid pool struct5 correctly rejected')
            else
                call mpas_log_write('FAIL: invalid pool struct5 accepted')
                local_err = local_err + 1
            end if

            !-----------------------------------------------------------------------
            ! Case 6: Add invalid pool struct6
            !-----------------------------------------------------------------------
        case (6)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_pool(f%manager, 'stream', 'struct6', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR) then
                call mpas_log_write('PASS: invalid pool struct6 correctly rejected')
            else
                call mpas_log_write('FAIL: invalid pool struct6 accepted')
                local_err = local_err + 1
            end if

        end select

        ierr = ierr + local_err
        call teardown_stream_manager_fixture(f)
    end subroutine test_add_pool



    !=======================================================================
    !  ADD FIELD TESTS
    !=======================================================================
    subroutine test_add_field(f, case_id, ierr)
        implicit none
        type(stream_manager_fixture_t), intent(inout) :: f
        integer, intent(in) :: case_id
        integer, intent(inout) :: ierr
        integer :: rc, local_err
        type(MPAS_stream_list_type), pointer :: s
        integer, pointer :: ip
        character(len=StrKIND), pointer :: cp
        logical :: exists
        local_err = 0

        call setup_stream_manager_fixture(f)

        select case (case_id)
        case (1)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr=rc)
            exists = MPAS_stream_list_query(f%manager%streams, 'stream', s)
            if (exists) then
                call mpas_pool_get_config(s%field_pool, 'field1', value=ip)
                if (associated(ip)) then
                    call mpas_log_write('PASS: field added successfully')
                else
                    call mpas_log_write('FAIL: field not linked')
                    local_err = local_err + 1
                end if
            end if

        case (2)
            call MPAS_stream_mgr_add_field(f%manager, 'nonexistent', 'field1', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR) then
                call mpas_log_write('PASS: nonexistent stream correctly rejected')
            else
                call mpas_log_write('FAIL: nonexistent stream accepted')
                local_err = local_err + 1
            end if

        case (3)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'unknown_field', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR) then
                call mpas_log_write('PASS: unknown field correctly rejected')
            else
                call mpas_log_write('FAIL: unknown field accepted')
                local_err = local_err + 1
            end if

        case (4)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr=rc)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr=rc)
            if (rc == MPAS_STREAM_MGR_NOERR) then
                call mpas_log_write('PASS: duplicate field accepted safely')
            else
                call mpas_log_write('FAIL: duplicate field rejected incorrectly')
                local_err = local_err + 1
            end if

        case (5)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            exists = MPAS_stream_list_query(f%manager%streams, 'stream', s)
            s%immutable = .true.
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', ierr=rc)
            if (rc == MPAS_STREAM_MGR_ERROR) then
                call mpas_log_write('PASS: immutable stream correctly rejected field addition')
            else
                call mpas_log_write('FAIL: immutable stream accepted field addition')
                local_err = local_err + 1
            end if

        case (6)
            call MPAS_stream_mgr_create_stream(f%manager, 'stream', MPAS_STREAM_INPUT, 'file.nc', ierr=rc)
            call MPAS_stream_mgr_add_field(f%manager, 'stream', 'field1', packages='package1', ierr=rc)
            exists = MPAS_stream_list_query(f%manager%streams, 'stream', s)
            call mpas_pool_get_config(s%field_pkg_pool, 'field1:packages', value=cp)
            if (associated(cp) .and. trim(cp) == 'package1') then
                call mpas_log_write('PASS: field-package link correct')
            else
                call mpas_log_write('FAIL: field-package link incorrect')
                local_err = local_err + 1
            end if
        end select

        ierr = ierr + local_err
        call teardown_stream_manager_fixture(f)
    end subroutine test_add_field

end module mpas_stream_manager_tests_mod
