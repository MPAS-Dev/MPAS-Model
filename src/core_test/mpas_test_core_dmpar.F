! Copyright (c) 2023 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
module mpas_test_core_dmpar

    use mpas_derived_types, only : dm_info
    use mpas_log, only : mpas_log_write

    private

    public :: mpas_test_dmpar


    contains


    !-----------------------------------------------------------------------
    !  routine mpas_test_dmpar
    !
    !> \brief Main driver for tests of the mpas_dmpar module
    !> \author Michael Duda
    !> \date   14 November 2023
    !> \details
    !>  This routine invokes tests for individual routines in the mpas_dmpar
    !>  module, and reports PASSED/FAILED for each of those tests.
    !>
    !>  Return value: The total number of test that failed on any MPI rank.
    !
    !-----------------------------------------------------------------------
    function mpas_test_dmpar(dminfo) result(ierr_count)

        use mpas_dmpar, only : mpas_dmpar_max_int
        use mpas_kind_types, only : StrKIND

        implicit none

        ! Arguments
        type (dm_info), intent(inout) :: dminfo

        ! Return value
        integer :: ierr_count

        ! Local variables
        integer :: ierr, ierr_global
        character(len=StrKIND) :: routine_name


        ierr_count = 0

        call mpas_log_write('--- Begin dmpar tests')

        !
        ! Test mpas_dmpar_sum_int8 routine
        !
        routine_name = 'mpas_dmpar_sum_int8'
        ierr = test_sum_int8(dminfo)
        call mpas_dmpar_max_int(dminfo, ierr, ierr_global)
        if (ierr_global == 0) then
            call mpas_log_write('    '//trim(routine_name)//' - PASSED')
        else
            ierr_count = ierr_count + 1
            call mpas_log_write('    '//trim(routine_name)//' - FAILED')
        end if

    end function mpas_test_dmpar


    !-----------------------------------------------------------------------
    !  routine test_sum_int8
    !
    !> \brief Tests the mpas_dmpar_sum_int8 routine
    !> \author Michael Duda
    !> \date   14 November 2023
    !> \details
    !>  This routine tests the mpas_dmpar_sum_int8 routine.
    !>
    !>  Return value: The total number of test that failed on the calling rank.
    !
    !-----------------------------------------------------------------------
    function test_sum_int8(dminfo) result(ierr_count)

        use mpas_dmpar, only : mpas_dmpar_sum_int8
        use mpas_kind_types, only : I8KIND

        implicit none

        ! Arguments
        type (dm_info), intent(inout) :: dminfo

        ! Return value
        integer :: ierr_count

        ! Local variables
        integer(kind=I8KIND) :: ival, ival_sum
        integer :: nranks, myrank

        ierr_count = 0

        myrank = dminfo % my_proc_id
        nranks = dminfo % nprocs

        !
        ! Compute sum(huge(ival) / nranks)
        ! Correct result should be at least (huge(ival) - nranks) when accounting
        ! for truncation in the integer division operation
        !
        ival = huge(ival) / nranks
        call mpas_dmpar_sum_int8(dminfo, ival, ival_sum)
        if (ival_sum >= huge(ival) - nranks) then
            call mpas_log_write('        int8 sum to HUGE() - PASSED')
        else
            call mpas_log_write('        int8 sum to HUGE() - FAILED')
            ierr_count = 1
        end if

        !
        ! Compute sum(-huge(ival) / nranks)
        ! Correct result should be at most (-huge(ival) + nranks) when accounting
        ! for truncation in the integer division operation
        !
        ival = -huge(ival) / nranks
        call mpas_dmpar_sum_int8(dminfo, ival, ival_sum)
        if (ival_sum <= -huge(ival) + nranks) then
            call mpas_log_write('        int8 sum to -HUGE() - PASSED')
        else
            call mpas_log_write('        int8 sum to -HUGE() - FAILED')
            ierr_count = 1
        end if

        !
        ! Compute sum of N alternating positive and negative values, where N is
        ! the largest even number not greater than the number of ranks.
        ! The magnitude of the values to be summed is (huge(ival) / nranks) to
        ! avoid overflow for any order of summation.
        !
        ival = huge(ival) / nranks
        if (mod(myrank, 2) == 1) then
            ival = -ival
        end if

        ! If we have an odd number of ranks, set value on rank 0 to zero
        if (mod(nranks, 2) /= 0) then
            if (myrank == 0) then
                ival = 0
            end if
        end if
        call mpas_dmpar_sum_int8(dminfo, ival, ival_sum)
        if (ival_sum == 0_I8KIND) then
            call mpas_log_write('        int8 sum to zero - PASSED')
        else
            call mpas_log_write('        int8 sum to zero - FAILED')
            ierr_count = 1
        end if

    end function test_sum_int8

end module mpas_test_core_dmpar
