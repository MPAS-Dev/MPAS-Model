! Copyright (c) 2023 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
module mpas_test_core_stream_inquiry

    use mpas_derived_types, only : dm_info, MPAS_streamInfo_type
    use mpas_log, only : mpas_log_write

    private

    public :: mpas_test_stream_inquiry


    contains


    !-----------------------------------------------------------------------
    !  routine mpas_test_stream_inquiry
    !
    !> \brief Main driver for tests of the mpas_stream_inquiry module
    !> \author Michael Duda
    !> \date   17 November 2023
    !> \details
    !>  This routine invokes tests for individual routines in the
    !>  mpas_stream_inquiry module, and reports PASSED/FAILED for each of
    !>  those tests.
    !>
    !>  Return value: The total number of test that failed on any MPI rank.
    !
    !-----------------------------------------------------------------------
    function mpas_test_stream_inquiry(dminfo) result(ierr_count)

        use mpas_kind_types, only : StrKIND
        use mpas_dmpar, only : mpas_dmpar_max_int
        use mpas_stream_inquiry, only : MPAS_stream_inquiry_new_streaminfo

        implicit none

        ! Arguments
        type (dm_info), intent(inout) :: dminfo

        ! Return value
        integer :: ierr_count

        ! Local variables
        integer :: ierr, ierr_global
        character(len=StrKIND) :: routine_name
        type (MPAS_streamInfo_type), pointer :: streamInfo

        ierr_count = 0

        call mpas_log_write('--- Begin stream_inquiry tests')

        !
        ! Create a new instance of the MPAS_streamInfo_type derived type
        !
        nullify(streamInfo)
        streamInfo => MPAS_stream_inquiry_new_streaminfo()

        !
        ! Initialize the instance with the streams.test file
        ! A failure here on any task causes this routine to return early
        !
        routine_name = 'streamInfo % init'
        ierr = streamInfo % init(dminfo % comm, 'streams.test')
        call mpas_dmpar_max_int(dminfo, ierr, ierr_global)
        if (ierr_global == 0) then
            call mpas_log_write('    '//trim(routine_name)//' - PASSED')
        else
            ierr_count = ierr_count + 1
            call mpas_log_write('    '//trim(routine_name)//' - FAILED')
            deallocate(streamInfo)
            return
        end if

        !
        ! Test streamInfo % query routine
        !
        routine_name = 'streamInfo % query'
        ierr = test_streaminfo_query(streamInfo)
        call mpas_dmpar_max_int(dminfo, ierr, ierr_global)
        if (ierr_global == 0) then
            call mpas_log_write('    '//trim(routine_name)//' - PASSED')
        else
            ierr_count = ierr_count + 1
            call mpas_log_write('    '//trim(routine_name)//' - FAILED')
        end if

        !
        ! Finalize the MPAS_streamInfo_type instance
        !
        routine_name = 'streamInfo % finalize'
        ierr = streamInfo % finalize()
        call mpas_dmpar_max_int(dminfo, ierr, ierr_global)
        if (ierr_global == 0) then
            call mpas_log_write('    '//trim(routine_name)//' - PASSED')
        else
            ierr_count = ierr_count + 1
            call mpas_log_write('    '//trim(routine_name)//' - FAILED')
        end if

        deallocate(streamInfo)

    end function mpas_test_stream_inquiry


    !-----------------------------------------------------------------------
    !  routine test_streaminfo_query
    !
    !> \brief Tests the streaminfo_query / streamInfo % query routine
    !> \author Michael Duda
    !> \date   17 November 2023
    !> \details
    !>  This routine tests the streaminfo_query routine.
    !>
    !>  Return value: The total number of test that failed on the calling rank.
    !
    !-----------------------------------------------------------------------
    function test_streaminfo_query(streamInfo) result(ierr_count)

        use mpas_kind_types, only : StrKIND

        implicit none

        ! Arguments
        type (MPAS_streamInfo_type), intent(inout) :: streamInfo

        ! Return value
        integer :: ierr_count

        ! Local variables
        logical :: success
        character(len=StrKIND) :: attvalue

        ierr_count = 0


        !
        ! Query about the existence of an immutable stream that exists
        !
        if (streamInfo % query('input')) then
            call mpas_log_write('        query existence of an immutable stream that exists - PASSED')
        else
            call mpas_log_write('        query existence of an immutable stream that exists - FAILED')
            ierr_count = ierr_count + 1
        end if

        !
        ! Query about the existence of a mutable stream that exists
        !
        if (streamInfo % query('mutable_test')) then
            call mpas_log_write('        query existence of a mutable stream that exists - PASSED')
        else
            call mpas_log_write('        query existence of a mutable stream that exists - FAILED')
            ierr_count = ierr_count + 1
        end if

        !
        ! Query about the existence of a stream that does not exist
        !
        if (.not. streamInfo % query('foobar')) then
            call mpas_log_write('        query existence of a stream that does not exist - PASSED')
        else
            call mpas_log_write('        query existence of a stream that does not exist - FAILED')
            ierr_count = ierr_count + 1
        end if

        !
        ! Query about the existence of an attribute that exists (immutable stream)
        !
        if (streamInfo % query('input', attname='filename_template')) then
            call mpas_log_write('        query existence of an attribute that exists (immutable stream) - PASSED')
        else
            call mpas_log_write('        query existence of an attribute that exists (immutable stream) - FAILED')
            ierr_count = ierr_count + 1
        end if

        !
        ! Query about the existence of an attribute that exists (mutable stream)
        !
        if (streamInfo % query('mutable_test', attname='type')) then
            call mpas_log_write('        query existence of an attribute that exists (mutable stream) - PASSED')
        else
            call mpas_log_write('        query existence of an attribute that exists (mutable stream) - FAILED')
            ierr_count = ierr_count + 1
        end if

        !
        ! Query about the existence of an attribute that does not exist
        !
        if (.not. streamInfo % query('input', attname='input_start_time')) then
            call mpas_log_write('        query existence of an attribute that does not exist - PASSED')
        else
            call mpas_log_write('        query existence of an attribute that does not exist - FAILED')
            ierr_count = ierr_count + 1
        end if

        !
        ! Query the value of an attribute (immutable stream)
        !
        success = streamInfo % query('input', attname='input_interval', attvalue=attvalue)
        if (success .and. trim(attvalue) == 'initial_only') then
            call mpas_log_write('        query value of an attribute (immutable stream) - PASSED')
        else
            call mpas_log_write('        query value of an attribute (immutable stream) - FAILED')
            ierr_count = ierr_count + 1
        end if

        !
        ! Query the value of an attribute (mutable stream)
        !
        success = streamInfo % query('mutable_test', attname='filename_template', attvalue=attvalue)
        if (success .and. trim(attvalue) == 'mutable_test.nc') then
            call mpas_log_write('        query value of an attribute (mutable stream) - PASSED')
        else
            call mpas_log_write('        query value of an attribute (mutable stream) - FAILED')
            ierr_count = ierr_count + 1
        end if

    end function test_streaminfo_query

end module mpas_test_core_stream_inquiry
