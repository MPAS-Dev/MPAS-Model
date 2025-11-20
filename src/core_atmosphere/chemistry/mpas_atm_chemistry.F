! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!-----------------------------------------------------------------------
!  mpas_atm_chemistry
!
!> \brief Manages interactions with chemistry packages
!> \author CheMPAS-A Developers
!> \date   20 August 2025
!> \details
!>  This module manages the interactions with chemistry packages,
!>  including initialization, time-stepping, and finalization.
!>  It provides a framework for integrating various chemistry models
!>  into the CheMPAS-A system.
!
!-------------------------------------------------------------------------
module mpas_atm_chemistry

    implicit none

    private

    public :: chemistry_init, chemistry_step, chemistry_finalize

    contains

    !------------------------------------------------------------------------
    !  routine chemistry_init
    !
    !> \brief Initializes the chemistry packages
    !> \author CheMPAS-A Developers
    !> \date   20 August 2025
    !> \details
    !>  This routine initializes the chemistry packages, setting up
    !>  necessary parameters and data structures for the simulation.
    !------------------------------------------------------------------------
    subroutine chemistry_init(configs, dimensions)

#ifdef MPAS_USE_MUSICA
        use mpas_musica, only: musica_init
#endif
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only: mpas_pool_type
        use mpas_kind_types, only: StrKIND
        use mpas_pool_routines, only: mpas_pool_get_config, mpas_pool_get_dimension 

        type (mpas_pool_type), intent(in) :: configs
        type (mpas_pool_type), intent(in) :: dimensions

#ifdef MPAS_USE_MUSICA
        integer                       :: error_code
        character(len=:), allocatable :: error_message
        integer                       :: nVertLevels
        integer, pointer              :: nVertLevels_ptr
        ! MUSICA will get the MICM JSON config from a namelist
        ! hardcode filepath for now
        character(len=StrKIND) :: filepath = 'chapman.json'
#endif

        call mpas_log_write('Initializing chemistry packages...')

#ifdef MPAS_USE_MUSICA
        call mpas_pool_get_dimension(dimensions, 'nVertLevels', nVertLevels_ptr)
        nVertLevels = nVertLevels_ptr

        call musica_init(filepath, nVertLevels, error_code, error_message)

        ! TODO check error_code and generate MPAS error log message
#endif

    end subroutine chemistry_init


    !------------------------------------------------------------------------
    !  routine chemistry_step
    !
    !> \brief Steps the chemistry packages
    !> \author CheMPAS-A Developers
    !> \date   20 August 2025
    !> \details
    !>  This routine steps the chemistry packages, updating their state
    !>  based on the current simulation time and conditions.
    !------------------------------------------------------------------------
    subroutine chemistry_step()

#ifdef MPAS_USE_MUSICA
        use mpas_musica, only: musica_step
#endif
        use mpas_log, only : mpas_log_write

#ifdef MPAS_USE_MUSICA
        call mpas_log_write('Stepping chemistry packages...')
        ! call musica_step()
#endif

    end subroutine chemistry_step


    !------------------------------------------------------------------------
    !  routine chemistry_finalize
    !
    !> \brief Finalizes the chemistry packages
    !> \author CheMPAS-A Developers
    !> \date   20 August 2025
    !> \details
    !>  This routine finalizes the chemistry packages, cleaning up
    !>  any resources and data structures used during the simulation.
    !------------------------------------------------------------------------
    subroutine chemistry_finalize()

#ifdef MPAS_USE_MUSICA
        use mpas_musica, only: musica_finalize
#endif
        use mpas_log, only : mpas_log_write

#ifdef MPAS_USE_MUSICA
        call mpas_log_write('Finalizing chemistry packages...')
        call musica_finalize()
#endif

    end subroutine chemistry_finalize

end module mpas_atm_chemistry
