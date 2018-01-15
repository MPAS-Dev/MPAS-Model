!  unstagger_diagnostics.F
!
!  Created by John Wong, January / 2018
!  Copyright (c) 2018 The Weather Company. All rights reserved.
!
!  This module unstaggers certain variable from cell interface to cell
!  center for post-processing.
!
!----------------------------------------------------------------------->

module unstagger_diagnostics

    use mpas_derived_types, only : MPAS_pool_type
    use mpas_kind_types, only : RKIND

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag

    public :: unstagger_diagnostics_setup, &
              unstagger_diagnostics_update, &
              unstagger_diagnostics_compute, &
              unstagger_diagnostics_reset, &
              unstagger_diagnostics_cleanup

    private

    contains


    !-----------------------------------------------------------------------
    !  routine unstagger_diagnostics_setup
    !
    !> \brief   Initialize the unstagger_diagnostics module
    !> \author  John Wong
    !> \date    20 Nov 2017
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine unstagger_diagnostics_setup(all_pools)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_dimension, mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        type (MPAS_pool_type), pointer :: all_pools

        integer, pointer :: nVertLevels
        real (kind=RKIND), dimension(:,:), pointer :: z, z_unstag

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)

        ! Unstagger zgrid into zgrid_unstag
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_array(mesh, 'zgrid', z)
        call mpas_pool_get_array(diag, 'zgrid_unstag', z_unstag)
        z_unstag(:,:) = 0.5 * (z(2:nVertLevels+1,:) + z(1:nVertLevels,:))

    end subroutine unstagger_diagnostics_setup


    subroutine unstagger_diagnostics_update()
        implicit none
        ! Nothing to update (every timestep) for unstaggering
    end subroutine unstagger_diagnostics_update


    !-----------------------------------------------------------------------
    !  routine unstagger_diagnostics_compute
    !
    !> \brief  Unstagger selected variables.
    !> \author John Wong
    !> \date   20 Nov 2017
    !> \details
    !>  Unstagger selected variables from cell interface to center.
    !
    !-----------------------------------------------------------------------
    subroutine unstagger_diagnostics_compute()

        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        integer, pointer :: nVertLevels

        real (kind=RKIND), dimension(:,:), pointer :: w, z
        real (kind=RKIND), dimension(:,:), pointer :: w_unstag, z_unstag

        if ( MPAS_field_will_be_written('w_unstag') ) then
            call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

            ! Unstagger w into w_unstag
            call mpas_pool_get_array(state, 'w', w, 1)
            call mpas_pool_get_array(diag, 'w_unstag', w_unstag)
            w_unstag(:,:) = 0.5 * (w(2:nVertLevels+1,:) + w(1:nVertLevels,:))
        end if

    end subroutine unstagger_diagnostics_compute


    subroutine unstagger_diagnostics_reset()
        implicit none
        ! Nothing to reset for deformation
    end subroutine unstagger_diagnostics_reset


    subroutine unstagger_diagnostics_cleanup()
        implicit none
        ! Nothing to cleanup for deformation
    end subroutine unstagger_diagnostics_cleanup


end module unstagger_diagnostics

