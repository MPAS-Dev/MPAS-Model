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


! Module mpas_diagnostics_packages contains the definitions for the tendency and PV diagnostics packages
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

 ! microphysics config:
 character(len=StrKIND), pointer :: config_microp_scheme

 ! LBC config:
 logical, pointer :: config_apply_lbcs
 
 ! Tendencies diagnostics config:
 logical, pointer :: config_tend

 ! MC note: May's code in mpas_atm_core_interface is written in terms of tendenciesActive, but 
 ! physics code is written in terms of package names in Registry... why?
 
 ! Tendencies package:
 logical, pointer :: tendenciesActive

 ! PV diagnostics configs:
 logical, pointer :: config_pv_diag, config_pv_tend, config_pv_scalar, &
                     config_pv_microphys, config_pv_isobaric

 ! PV diagnostics packages:
 logical, pointer :: pv_diagnosticsActive, pv_tendenciesActive, pv_scalarActive, &
                     pv_microphysicsActive, pv_isobaricActive

 integer :: ierr                    

!-----------------------------------------------------------------------------------------------------------------

! call mpas_log_write('')
! call mpas_log_write('--- enter subroutine diagnostics_setup_packages:')

 ierr = 0

!-----------------------------------------------------------------------------------------------------------------
!--- initialization of package for model tendency diagnostics:
!-----------------------------------------------------------------------------------------------------------------

 call mpas_log_write('----- Setting up tendency diagnostics variables -----')
 call mpas_log_write('')
 
 nullify(config_tend)
 call mpas_pool_get_config(configs, 'config_tend', config_tend)

 nullify(tendenciesActive)
 call mpas_pool_get_package(packages, 'tendenciesActive', tendenciesActive)

 if (associated(config_tend) .and. associated(tendenciesActive)) then
    tendenciesActive = config_tend
    call mpas_log_write('    tendenciesActive           = $l', logicArgs=(/tendenciesActive/))
 else
    ierr = ierr + 1
    call mpas_log_write('Package setup failed for ''tendencies''. '// &
        'Either ''tendencies'' is not a package, or ''config_tend'' is not a namelist option.', &
        messageType=MPAS_LOG_ERR)
 end if


!-----------------------------------------------------------------------------------------------------------------
!--- initialization of packages for PV diagnostics:
!    This contains compatability checks for various config_pv options. 
!-----------------------------------------------------------------------------------------------------------------

 call mpas_log_write('----- Performing compatability checks for PV diagnostics configs -----')
 call mpas_log_write('')
 
 call mpas_pool_get_config(configs, 'config_apply_lbcs', config_apply_lbcs)
 call mpas_pool_get_config(configs, 'config_microp_scheme', config_microp_scheme)
 
 nullify(config_pv_diag)  
 call mpas_pool_get_config(configs, 'config_pv_diag', config_pv_diag) 

 nullify(config_pv_tend) 
 call mpas_pool_get_config(configs, 'config_pv_tend', config_pv_tend)

 nullify(config_pv_scalar)
 call mpas_pool_get_config(configs, 'config_pv_scalar', config_pv_scalar)  

 nullify(config_pv_microphys)
 call mpas_pool_get_config(configs, 'config_pv_microphys', config_pv_microphys)  

 nullify(config_pv_isobaric)
 call mpas_pool_get_config(configs, 'config_pv_isobaric', config_pv_isobaric)  

  
 ! Before setting packages, need to check compatability of config flags and then disable them as appropriate.

 ! if limited area simulation, disable all PV flags if activated
 if (config_apply_lbcs .and. (config_pv_diag .or. config_pv_tend .or. config_pv_scalar &
    .or. config_pv_microphys .or. config_pv_isobaric)) then
    call mpas_log_write('PV diagnostics are not supported for limited-area simulations. Disabling.', MPAS_LOG_WARN)
    config_pv_diag = .false.
    config_pv_tend = .false.
    config_pv_scalar = .false.
    config_pv_microphys = .false.
    config_pv_isobaric = .false.
 end if 

 ! if dependent config_pv flags are activated but parent config_pv_diag flag is not, deactivate them. 
 if ((.not. config_pv_diag) .and. (config_pv_tend .or. config_pv_scalar .or. config_pv_microphys .or. config_pv_isobaric)) then
    config_pv_tend = .false.
    config_pv_scalar = .false.
    config_pv_microphys = .false.
    config_pv_isobaric = .false.
    call mpas_log_write('config_pv_diag is not activated; deactivated all dependent PV configs.', MPAS_LOG_WARN)
 end if 

 ! if config_pv_tend is activated but config_tend is not, deactivate.
 if ((.not. config_tend) .and. (config_pv_tend .or. config_pv_microphys .or. config_pv_isobaric)) then
    config_pv_tend = .false.
    config_pv_microphys = .false.
    config_pv_isobaric = .false.
    call mpas_log_write('config_tend is not activated; deactivated all dependent PV configs.', MPAS_LOG_WARN)
 end if 
    
 ! if config_pv_microphys or config_pv_isobaric is activated but config_pv_tend is not, deactivate.
 if ((.not. config_pv_tend) .and. (config_pv_microphys .or. config_pv_isobaric)) then
    config_pv_microphys = .false.
    config_pv_isobaric = .false.
    call mpas_log_write('config_pv_tend is not activated; deactivated all dependent PV configs.', MPAS_LOG_WARN)
 end if

 ! Ensure either Thompson or Thompson-aerosol scheme is enabled for microphysics PV tendencies
 if ((config_pv_microphys) .and. ((trim(config_microp_scheme) /= 'mp_thompson') .and. (trim(config_microp_scheme) /= 'mp_thompson_aerosols'))) then
    call mpas_log_write('config_pv_microphys is not compatible with   = '''//trim(config_microp_scheme)//''' -- disabling', MPAS_LOG_WARN)
    config_pv_microphys = .false.
 end if 
 

 call mpas_log_write('----- Setting up PV diagnostics variables -----')
 call mpas_log_write('')

 nullify(pv_diagnosticsActive)
 nullify(pv_tendenciesActive)
 nullify(pv_scalarActive)
 nullify(pv_microphysicsActive)
 nullify(pv_isobaricActive)
 
 call mpas_pool_get_package(packages, 'pv_diagnosticsActive', pv_diagnosticsActive)
 call mpas_pool_get_package(packages, 'pv_tendenciesActive', pv_tendenciesActive)
 call mpas_pool_get_package(packages, 'pv_scalarActive', pv_scalarActive)
 call mpas_pool_get_package(packages, 'pv_microphysicsActive', pv_microphysicsActive)
 call mpas_pool_get_package(packages, 'pv_isobaricActive', pv_isobaricActive)


 ! pv_diagnostics:
 if (associated(config_pv_diag) .and. associated(pv_diagnosticsActive)) then
    pv_diagnosticsActive = config_pv_diag
    call mpas_log_write('    pv_diagnosticsActive          = $l', logicArgs=(/pv_diagnosticsActive/))
 else
    ierr = ierr + 1
    call mpas_log_write('Package setup failed for ''pv_diagnostics''. '// &
        'Either ''pv_diagnostics'' is not a package, ''config_pv_diag'' is not a namelist option, or '//&
        ' ''config_pv_diag'' has been disabled due to incompatability with other model configuration options.', &
        messageType=MPAS_LOG_ERR)
 end if
 
 ! pv_tendencies:
 if (associated(config_pv_tend) .and. associated(pv_tendenciesActive)) then
    pv_tendenciesActive = config_pv_tend
    call mpas_log_write('    pv_tendenciesActive           = $l', logicArgs=(/pv_tendenciesActive/))
 else
    ierr = ierr + 1
    call mpas_log_write('Package setup failed for ''pv_tendencies''. '// &
        'Either ''pv_tendencies'' is not a package, ''config_pv_tend'' is not a namelist option, or '//&
        ' ''config_pv_tend'' has been disabled due to incompatability with other model configuration options.', &
        messageType=MPAS_LOG_ERR)
 end if

 ! pv_scalar:
 if (associated(config_pv_scalar) .and. associated(pv_scalarActive)) then
    pv_scalarActive = config_pv_scalar
    call mpas_log_write('    pv_scalarActive               = $l', logicArgs=(/pv_scalarActive/))
 else
    ierr = ierr + 1
    call mpas_log_write('Package setup failed for ''pv_scalar''. '// &
        'Either ''pv_scalar'' is not a package, ''config_pv_scalar'' is not a namelist option, or '//&
        ' ''config_pv_scalar'' has been disabled due to incompatability with other model configuration options.', &
        messageType=MPAS_LOG_ERR)
 end if

 ! pv_microphys:
 if (associated(config_pv_microphys) .and. associated(pv_microphysicsActive)) then
    pv_microphysicsActive = config_pv_microphys
    call mpas_log_write('    pv_microphysicsActive         = $l', logicArgs=(/pv_microphysicsActive/))
 else
    ierr = ierr + 1
    call mpas_log_write('Package setup failed for ''pv_microphysics''. '// &
        'Either ''pv_microphysics'' is not a package, ''config_pv_microphys'' is not a namelist option, or '//&
        ' ''config_pv_microphys'' has been disabled due to incompatability with other model configuration options.', &
        messageType=MPAS_LOG_ERR)
 end if

 ! pv_isobaric:
 if (associated(config_pv_isobaric) .and. associated(pv_isobaricActive)) then
    pv_isobaricActive = config_pv_isobaric
    call mpas_log_write('    pv_isobaricActive             = $l', logicArgs=(/pv_isobaricActive/))
 else
    ierr = ierr + 1
    call mpas_log_write('Package setup failed for ''pv_isobaric''. '// &
        'Either ''pv_isobaric'' is not a package, ''config_pv_isobaric'' is not a namelist option, or '//&
        ' ''config_pv_isobaric'' has been disabled due to incompatability with other model configuration options.', &
        messageType=MPAS_LOG_ERR)
 end if
 

 end function diagnostics_setup_packages

!=================================================================================================================
 end module mpas_atm_diagnostics_packages
!=================================================================================================================



