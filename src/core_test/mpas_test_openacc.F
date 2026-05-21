! Copyright (c) 2024 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
module mpas_test_core_openacc

    use mpas_log, only : mpas_log_write

    private

    public :: mpas_test_openacc

    contains

    !-----------------------------------------------------------------------
    !  function mpas_test_openacc
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
    function mpas_test_openacc(domain) result(ierr_count)

        use mpas_derived_types, only : domain_type
        use mpas_kind_types, only : StrKIND
        use mpas_dmpar, only : mpas_dmpar_max_int

        implicit none

        ! Arguments
        type (domain_type), intent(inout) :: domain

        ! Return value
        integer :: ierr_count

        ! Local variables
        integer :: ierr, ierr_global
        ! Use test_log_str to track what is being tested next
        character(len=StrKIND) :: test_log_str

        ierr_count = 0

        call mpas_log_write('--- Begin OpenACC tests')

        test_log_str = 'Simple CPU-GPU reproducibility test'
        ierr = openacc_test_rep_arrs(domain)
        if (ierr == 0) then
            call mpas_log_write('   '//trim(test_log_str)//' - PASSED')
        else
            ierr_count = ierr_count + 1
            call mpas_log_write('   '//trim(test_log_str)//' - FAILED')
        end if

        ! Make sure all threads have the max number of tests failed in
        call mpas_dmpar_max_int(domain % dminfo, ierr_count, ierr_global)
        ierr_count = ierr_global

    end function mpas_test_openacc


    !-----------------------------------------------------------------------
    !  routine openacc_test_rep_arrs
    !
    !> \brief OpenACC test of representative of array usage
    !> \author G. Dylan Dickerson
    !> \date   29 May 2024
    !> \details
    !>  Replicates patterns from the core_atmosphere dynamics and
    !>  compares the results on the CPU to those on the GPU. These
    !>  patterns include a main routine that fetches arrays and
    !>  dimensions that are passed to work routines and loops
    !>  in the work routine that calculate some helper values before the
    !>  result.
    !>
    !>  Return value: 0 (success) if the CPU and GPU results match on
    !>  all ranks, 1 otherwise
    !-----------------------------------------------------------------------
    function openacc_test_rep_arrs(domain) result(ierr)

        use mpas_derived_types, only : domain_type, mpas_pool_type
        use mpas_kind_types, only : RKIND
        use mpas_pool_routines, only : mpas_pool_get_subpool,mpas_pool_get_dimension, &
                                       mpas_pool_get_array

        implicit none

        ! Arguments
        type (domain_type), intent(inout) :: domain

        ! Return value
        integer :: ierr

        ! Local variables
        real (kind=RKIND) :: diff

        type (mpas_pool_type), pointer :: mesh_pool
        integer, pointer :: nCells,nCellsSolve
        integer, pointer :: nEdges,nEdgesSolve
        real (kind=RKIND), dimension(:), pointer :: areaCell
        integer, dimension(:), pointer :: indexToCellID
        integer, dimension(:), pointer :: nEdgesOnCell
        integer, dimension(:,:), pointer :: cellsOnEdge

        type (mpas_pool_type), pointer :: openaccTest_pool
        real (kind=RKIND), dimension(:), pointer :: array_cpu
        real (kind=RKIND), dimension(:), pointer :: array_gpu

        ierr = 0
        diff = 0.0_RKIND

        !
        ! Fetch variables
        !
        nullify(mesh_pool)
        call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh_pool)

        nullify(nCells)
        call mpas_pool_get_dimension(mesh_pool, 'nCells', nCells)

        nullify(nEdges)
        call mpas_pool_get_dimension(mesh_pool, 'nEdges', nEdges)

        nullify(nCellsSolve)
        call mpas_pool_get_dimension(mesh_pool, 'nCellsSolve', nCellsSolve)

        nullify(nEdgesSolve)
        call mpas_pool_get_dimension(mesh_pool, 'nEdgesSolve', nEdgesSolve)

        nullify(areaCell)
        call mpas_pool_get_array(mesh_pool, 'areaCell', areaCell)

        nullify(indexToCellID)
        call mpas_pool_get_array(mesh_pool, 'indexToCellID', indexToCellID)

        nullify(nEdgesOnCell)
        call mpas_pool_get_array(mesh_pool, 'nEdgesOnCell', nEdgesOnCell)

        nullify(cellsOnEdge)
        call mpas_pool_get_array(mesh_pool, 'cellsOnEdge', cellsOnEdge)

        nullify(openaccTest_pool)
        call mpas_pool_get_subpool(domain % blocklist % structs, 'openaccTest', openaccTest_pool)

        nullify(array_cpu)
        call mpas_pool_get_array(openaccTest_pool, 'edge_cpu', array_cpu)

        nullify(array_gpu)
        call mpas_pool_get_array(openaccTest_pool, 'edge_gpu', array_gpu)

        call rep_arrs_work_cpu(nCells,nEdges,nCellsSolve,nEdgesSolve, &
                               areaCell,indexToCellID,nEdgesOnCell,cellsOnEdge, &
                               array_cpu)

        call rep_arrs_work_gpu(nCells,nEdges,nCellsSolve,nEdgesSolve, &
                               areaCell,indexToCellID,nEdgesOnCell,cellsOnEdge, &
                               array_gpu)

        diff = sum(abs(array_cpu(1:nEdges) - array_gpu(1:nEdges)))

        if (diff > 0.0_RKIND) then
            ierr = ierr + 1
        end if

    end function openacc_test_rep_arrs


    !-----------------------------------------------------------------------
    !  routine rep_arrs_work_cpu
    !
    !> \brief CPU work routine for OpenACC representative arrays test
    !> \author G. Dylan Dickerson
    !> \date   29 May 2024
    !> \details
    !> Performs some array work on the CPU, based on patterns in the
    !> MPAS-A dycore.
    !
    !-----------------------------------------------------------------------
    subroutine rep_arrs_work_cpu(nCells, nEdges, nCellsSolve, nEdgesSolve, &
                                 areaCell, indexToCellID, nEdgesOnCell, cellsOnEdge, &
                                 edge_arr_cpu)

        use mpas_kind_types, only : RKIND

        implicit none

        ! arguments
        integer, intent(in) :: nCells, nEdges, nCellsSolve, nEdgesSolve
        real (kind=RKIND), dimension(:), intent(in) :: areaCell
        integer, dimension(:), intent(in) :: indexToCellID
        integer, dimension(:), intent(in) :: nEdgesOnCell
        integer, dimension(:,:), intent(in) :: cellsOnEdge
        real (kind=RKIND), dimension(:), intent(inout) :: edge_arr_cpu

        ! locals
        integer :: iCell, iEdge, cell1, cell2
        real (kind=RKIND), dimension(nCells) :: invArea, help_arr

        ! Compute any helpers and initialize arrs
        do iCell=1,nCells
           invArea(iCell) = 1.0_RKIND / areaCell(iCell)
           help_arr(iCell) = 0.0_RKIND
        end do
        do iEdge=1,nEdges
           edge_arr_cpu(iEdge) = 0.0_RKIND
        end do

        ! Compute helper values (for all owned cells)
        do iCell=1,nCellsSolve
           help_arr(iCell) = (nEdgesOnCell(iCell)+indexToCellID(iCell)) * invArea(iCell)
        end do

        ! Compute final value (for all owned edges)
        do iEdge=1,nEdgesSolve
           cell1 = cellsOnEdge(1,iEdge)
           cell2 = cellsOnEdge(2,iEdge)

           edge_arr_cpu(iEdge) = 0.5_RKIND * (help_arr(cell1) + help_arr(cell2))
        end do
    end subroutine rep_arrs_work_cpu


    !-----------------------------------------------------------------------
    !  routine rep_arrs_work_gpu
    !
    !> \brief GPU work routine for OpenACC representative arrays test
    !> \author G. Dylan Dickerson
    !> \date   29 May 2024
    !> \details
    !> Performs some array work on the GPU, based on patterns in the
    !> MPAS-A dycore.
    !
    !-----------------------------------------------------------------------
    subroutine rep_arrs_work_gpu(nCells, nEdges, nCellsSolve, nEdgesSolve,  &
                                 areaCell, indexToCellID, nEdgesOnCell, cellsOnEdge, &
                                 edge_arr_gpu)

        use mpas_kind_types, only : RKIND

        implicit none

        ! arguments
        integer, intent(in) :: nCells, nEdges, nCellsSolve, nEdgesSolve
        real (kind=RKIND), dimension(:), intent(in) :: areaCell
        integer, dimension(:), intent(in) :: indexToCellID
        integer, dimension(:), intent(in) :: nEdgesOnCell
        integer, dimension(:,:), intent(in) :: cellsOnEdge
        real (kind=RKIND), dimension(:), intent(inout) :: edge_arr_gpu

        ! locals
        integer :: iCell, iEdge, cell1, cell2
        real (kind=RKIND), dimension(nCells) :: invArea
        real (kind=RKIND), dimension(nCells) :: help_arr

        !$acc enter data copyin(nCells,nEdges, &
        !$acc                   areaCell(:), indexToCellID(:), &
        !$acc                   nEdgesOnCell(:),cellsOnEdge(:,:))

        !$acc enter data create(edge_arr_gpu(:),iCell,iEdge,cell1,cell2, &
        !$acc                   invArea(:),help_arr(:))

        ! Compute any helpers and initialize arrs
        !$acc parallel default(present) async
        !$acc loop gang worker vector
        do iCell=1,nCells
           invArea(iCell) = 1.0_RKIND / areaCell(iCell)
           help_arr(iCell) = 0.0_RKIND
        end do

        !$acc loop gang worker vector
        do iEdge=1,nEdges
           edge_arr_gpu(iEdge) = 0.0_RKIND
        end do
        !$acc end parallel

        ! Compute helper values (for all owned cells)
        !$acc parallel default(present) wait
        !$acc loop gang worker vector
        do iCell=1,nCellsSolve
           help_arr(iCell) = (nEdgesOnCell(iCell)+indexToCellID(iCell)) * invArea(iCell)
        end do
        !$acc end parallel

        ! Compute final value (for all owned edges)
        !$acc parallel default(present) wait
        !$acc loop gang worker vector private(cell1, cell2)
        do iEdge=1,nEdgesSolve
           cell1 = cellsOnEdge(1,iEdge)
           cell2 = cellsOnEdge(2,iEdge)

           edge_arr_gpu(iEdge) = 0.5_RKIND * (help_arr(cell1) + help_arr(cell2))
        end do
        !$acc end parallel

        !$acc exit data delete(nCells,nEdges, &
        !$acc                  areaCell(:), indexToCellID(:), &
        !$acc                  nEdgesOnCell(:),cellsOnEdge(:,:), &
        !$acc                  iCell,iEdge,cell1,cell2,invArea(:),help_arr(:))

        !$acc exit data copyout(edge_arr_gpu(:))

    end subroutine rep_arrs_work_gpu


end module mpas_test_core_openacc
