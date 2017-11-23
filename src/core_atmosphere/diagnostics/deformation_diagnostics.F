!  deformation_diagnostics.F
!
!  Created by John Wong, October / 2017
!  Copyright (c) 2017 The Weather Company. All rights reserved.
!
!  This module computes wind deformation based on method used in the
!  implementation for 2D Smagorinsky eddy viscosity in atm_compute_dyn_tend_work.
!
!----------------------------------------------------------------------->

module deformation_diagnostics

    use mpas_derived_types, only : MPAS_pool_type
    use mpas_kind_types, only : RKIND

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag

    public :: deformation_diagnostics_setup, &
              deformation_diagnostics_update, &
              deformation_diagnostics_compute, &
              deformation_diagnostics_reset, &
              deformation_diagnostics_cleanup

    private

    contains


    !-----------------------------------------------------------------------
    !  routine deformation_diagnostics_setup
    !
    !> \brief   Initialize the deformation_diagnostics module
    !> \author  John Wong
    !> \date    20 Nov 2017
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine deformation_diagnostics_setup(all_pools)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_pool_routines, only : mpas_pool_get_subpool

        implicit none

        type (MPAS_pool_type), pointer :: all_pools

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)

    end subroutine deformation_diagnostics_setup


    subroutine deformation_diagnostics_update()
        implicit none
        ! Nothing to update (every timestep) for deformation
    end subroutine deformation_diagnostics_update


    !-----------------------------------------------------------------------
    !  routine deformation_diagnostics_compute
    !
    !> \brief  Compute deformation for model output
    !> \author John Wong
    !> \date   20 Nov 2017
    !> \details
    !>  Compute deformation diagnostic before model output is written. Implementation
    !>  is based on that done for Smagorinsky horizontal eddy viscosity.
    !
    !-----------------------------------------------------------------------
    subroutine deformation_diagnostics_compute()

        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer, pointer :: nCells, nVertLevels, nEdges
        integer, dimension(:), pointer :: nEdgesOnCell
        integer, dimension(:,:), pointer :: edgesOnCell

        real (kind=RKIND), dimension(:,:), pointer :: u
        real (kind=RKIND), dimension(:,:), pointer :: v
        real (kind=RKIND), dimension(:,:), pointer :: defc_a
        real (kind=RKIND), dimension(:,:), pointer :: defc_b

        real (kind=RKIND), dimension(:), allocatable :: d_diag, d_off_diag

        real (kind=RKIND), dimension(:,:), pointer :: deformation

        integer :: iEdge, iCell, k, e


        ! Mesh information
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
        call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)

        ! States and diagnostics, integration coefficients
        call mpas_pool_get_array(state, 'u', u, 2)
        call mpas_pool_get_array(diag, 'v', v)
        call mpas_pool_get_array(mesh, 'defc_a', defc_a)
        call mpas_pool_get_array(mesh, 'defc_b', defc_b)

        ! Target array
        call mpas_pool_get_array(diag, 'deformation', deformation)

        ! Allocate arrays for holding diagonal and off-diagonal deformation terms
        allocate(d_diag(nVertLevels))
        allocate(d_off_diag(nVertLevels))

        do iCell=1,nCells
            d_diag(:) = 0.0
            d_off_diag(:) = 0.0

            ! Integrate over edges around the cell
            do iEdge=1,nEdgesOnCell(iCell)
                e = edgesOnCell(iEdge,iCell)
                d_diag(:)     = d_diag(:)     + defc_a(iEdge,iCell)*u(:,e) - &
                                                defc_b(iEdge,iCell)*v(:,e)
                d_off_diag(:) = d_off_diag(:) + defc_b(iEdge,iCell)*u(:,e) + &
                                                defc_a(iEdge,iCell)*v(:,e)
            end do

            ! Combine the diagonal and off-diagonal terms
            deformation(:,iCell) = sqrt(d_diag(:)**2 + d_off_diag(:)**2)
        end do

        deallocate(d_diag)
        deallocate(d_off_diag)

    end subroutine deformation_diagnostics_compute



    subroutine deformation_diagnostics_reset()
        implicit none
        ! Nothing to reset for deformation
    end subroutine deformation_diagnostics_reset


    subroutine deformation_diagnostics_cleanup()
        implicit none
        ! Nothing to cleanup for deformation
    end subroutine deformation_diagnostics_cleanup


end module deformation_diagnostics

