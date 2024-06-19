! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atm_diagnostics_packages

 
 use mpas_kind_types
 use mpas_derived_types, only : mpas_pool_type, mpas_io_context_type, MPAS_LOG_ERR, MPAS_LOG_WARN
 use mpas_pool_routines, only : mpas_pool_get_config, mpas_pool_get_package
 use mpas_log, only : mpas_log_write

 implicit none
 private
 public :: diagnostics_setup_packages


! Module mpas_diagnostics_packages contains the definitions for the diagnostics packages
! Script is modeled after mpas_atmphys_packages.F
!
! Manda Chasteen, 21 May 2024

 contains


!=================================================================================================================
 function diagnostics_setup_packages(configs, packages, iocontext) result(ierr)
!=================================================================================================================

 ! inout arguments:
 type (mpas_pool_type), intent(inout) :: configs
 type (mpas_pool_type), intent(inout) :: packages
 type (mpas_io_context_type), intent(inout) :: iocontext

 ! Isobaric diagnostics config:
 logical, pointer :: config_isobaric

 ! Isobaric diagnostics package:
 logical, pointer :: isobaricActive            

 integer :: ierr                    

!-----------------------------------------------------------------------------------------------------------------

! call mpas_log_write('')
! call mpas_log_write('--- enter subroutine diagnostics_setup_packages:')

 ierr = 0

!-----------------------------------------------------------------------------------------------------------------
!--- initialization of package of isobaric diagnostics
!-----------------------------------------------------------------------------------------------------------------

 call mpas_log_write('----- Setting up isobaric diagnostics variables -----')
 call mpas_log_write('')

 nullify(config_isobaric)
 call mpas_pool_get_config(configs, 'config_isobaric', config_isobaric)

 nullify(isobaricActive)
 call mpas_pool_get_package(packages, 'isobaricActive', isobaricActive)

 if (associated(config_isobaric) .and. associated(isobaricActive)) then
    isobaricActive = config_isobaric
    call mpas_log_write('    isobaricActive           = $l', logicArgs=(/isobaricActive/))
 else
    ierr = ierr + 1
    call mpas_log_write('Package setup failed for ''isobaric''. '// &
        'Either ''isobaric'' is not a package, or ''config_isobaric'' is not a namelist option.', &
        messageType=MPAS_LOG_ERR)
 end if


 end function diagnostics_setup_packages

!=================================================================================================================
 end module mpas_atm_diagnostics_packages
!=================================================================================================================



