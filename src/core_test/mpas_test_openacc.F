! Copyright (c) 2024 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
module mpas_test_core_openacc

    use mpas_derived_types, only : dm_info
    use mpas_log, only : mpas_log_write

    private

    public :: mpas_test_openacc

    contains

    !-----------------------------------------------------------------------
    !  routine mpas_test_openacc
    !
    !> \brief Main driver for tests of OpenACC functionality in MPAS
    !> \author G. Dylan Dickerson
    !> \date   14 May 2024
    !> \details
    !>  This routine invokes tests for expected OpenACC behavior and any
    !>  framework routines that are specific to OpenACC.
    !>
    !>  Return value: The total number of test that failed on any MPI rank.
    !
    !-----------------------------------------------------------------------
    function mpas_test_openacc(dminfo) result(ierr_count)

        use mpas_kind_types, only : StrKIND
        use mpas_dmpar, only : mpas_dmpar_max_int

        implicit none

        ! Arguments
        type (dm_info), intent(inout) :: dminfo

        ! Return value
        integer :: ierr_count

        ! Local variables
        integer :: ierr, ierr_global
        ! Use routine_name to track what is being tested next
        character(len=StrKIND) :: routine_name

        ierr_count = 0

        call mpas_log_write('--- Begin OpenACC tests')

        routine_name = 'openacc_test_stub'
        ierr = openacc_test_stub(dminfo)
        call mpas_dmpar_max_int(dminfo, ierr, ierr_global)
        if (ierr_global == 0) then
            call mpas_log_write('   '//trim(routine_name)//' - PASSED')
        else
            ierr_count = ierr_count + 1
            call mpas_log_write('   '//trim(routine_name)//' - FAILED')
        end if

        call mpas_log_write('--- End OpenACC tests')

        ! Deallocate any dynamic vars here

    end function mpas_test_openacc


    !-----------------------------------------------------------------------
    !  routine openacc_test_stub
    !
    !> \brief Stub function as a placeholder for later tests
    !> \author G. Dylan Dickerson
    !> \date   14 May 2024
    !> \details
    !>  This routine acts similarly to how the implemented routines
    !>  should.
    !>
    !>  Return value: The total number of test that failed on any MPI rank.
    !
    !-----------------------------------------------------------------------
    function openacc_test_stub(dminfo) result(ierr_count)

        use mpas_kind_types, only : StrKIND
        use mpas_dmpar, only : mpas_dmpar_max_int

        implicit none

        ! Arguments
        type (dm_info), intent(inout) :: dminfo

        ! Return value
        integer :: ierr_count


        ierr_count = 0

#ifdef MPAS_OPENACC
        call mpas_log_write('   in openacc_test_stub and MPAS_OPENACC is defined')
#else
        call mpas_log_write('ERROR: in openacc_test_stub and MPAS_OPENACC is not defined')
        ierr_count = ierr_count + 1
#endif

    end function openacc_test_stub

end module mpas_test_core_openacc
