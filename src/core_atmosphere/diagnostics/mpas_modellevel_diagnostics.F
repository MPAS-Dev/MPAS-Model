! Copyright (c) 2022, University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html
!
module mpas_modellevel_diagnostics

    use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
    use mpas_kind_types, only : RKIND
    use mpas_constants, only : rvord

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    !type (MPAS_pool_type), pointer :: diag_physics

    type (MPAS_clock_type), pointer :: clock

    public :: modellevel_diagnostics_setup, &
              modellevel_diagnostics_compute

    private


    contains


    !-----------------------------------------------------------------------
    !  routine modellevel_diagnostics_setup
    !
    !> \brief Initialize the modellevel diagnostic module
    !> \author Jihyeon Jang
    !> \date   30 January 2026
    !> \details
    !>  Initialize the diagnostic and save pointers to subpools for
    !>  reuse in this module
    !
    !-----------------------------------------------------------------------
    subroutine modellevel_diagnostics_setup(all_pools, simulation_clock)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_pool_routines, only : mpas_pool_get_subpool

        implicit none

        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        clock => simulation_clock

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)

        !call mpas_pool_get_array(diag, 'temperature', temperature)
        !call mpas_pool_get_array(diag, 'spechum', spechum)
        !
        ! Zero-out the initial field
        !
        !temperature(:,:) = 0.0_RKIND
        !spechum(:,:) = 0.0_RKIND

    end subroutine modellevel_diagnostics_setup


    !-----------------------------------------------------------------------
    !  routine modellevel_diagnostics_compute
    !
    !> \brief Compute diagnostic before model output is written
    !> \author Jihyeon Jang
    !> \date   30 January 2026
    !> \details
    !>  Compute diagnostic before model output is written
    !>  The following fields are computed by this routine:
    !>     temperature
    !>     spechum
    !
    !-----------------------------------------------------------------------
    subroutine modellevel_diagnostics_compute()

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer :: iCell, k
        integer :: time_lev
        integer, pointer :: nCellsSolve, nVertLevels
        integer, pointer :: index_qv

        real (kind=RKIND), dimension(:,:), pointer :: temperature
        real (kind=RKIND), dimension(:,:), pointer :: spechum
        real (kind=RKIND), dimension(:,:), pointer :: exner, theta_m
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars

        logical :: need_ml_diags, need_temperature, need_spechum

        time_lev = 1

        need_ml_diags = .false.
        need_temperature = MPAS_field_will_be_written('temperature')
        need_ml_diags = need_ml_diags .or. need_temperature
        need_spechum = MPAS_field_will_be_written('spechum')
        need_ml_diags = need_ml_diags .or. need_spechum


        if (need_ml_diags) then
           call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
           call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        end if

        if (need_temperature .or. need_spechum) then
           call mpas_pool_get_dimension(state, 'index_qv', index_qv)
           call mpas_pool_get_array(state, 'scalars', scalars, time_lev)
        end if

        if (need_temperature) then
           call mpas_pool_get_array(state, 'theta_m', theta_m, time_lev)
           call mpas_pool_get_array(diag, 'exner', exner)
           call mpas_pool_get_array(diag, 'temperature', temperature)
        end if

        if (need_spechum) then
           call mpas_pool_get_array(diag, 'spechum', spechum)
        end if

        if (need_temperature) then
           do iCell=1,nCellsSolve
           do k=1,nVertLevels
              temperature(k,iCell) = (theta_m(k,iCell)/(1._RKIND+rvord*scalars(index_qv,k,iCell)))*exner(k,iCell)
           end do
           end do
        end if

        if (need_spechum) then
           do iCell=1,nCellsSolve
           do k=1,nVertLevels
              spechum(k,iCell) = scalars(index_qv,k,iCell) / (1.0_RKIND+scalars(index_qv,k,iCell))
           end do
           end do
        end if

    end subroutine modellevel_diagnostics_compute

end module mpas_modellevel_diagnostics
