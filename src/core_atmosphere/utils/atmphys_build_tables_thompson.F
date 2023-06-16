! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module atmphys_build_tables_thompson
 use module_mp_thompson

 implicit none
 private
 public:: build_tables_thompson

!builds the files containing the look-up tables for the Thompson cloud microphysics scheme.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2016-11-01.


 contains


!=================================================================================================================
 subroutine build_tables_thompson
!=================================================================================================================

 use mpas_io_units, only : mpas_new_unit, mpas_release_unit

!local variables:
 logical, parameter:: l_mp_tables = .false.
 integer:: istatus
 integer:: mp_unit

!-----------------------------------------------------------------------------------------------------------------

!--- partial initialization before building the look-up tables:
 call thompson_init(l_mp_tables)

 call mpas_new_unit(mp_unit, unformatted = .true.)

!--- building look-up table for rain collecting graupel:
 write(0,*)
 write(0,*) '--- building MP_THOMPSON_QRacrQG_DATA.DBL'
 open(unit=mp_unit,file='MP_THOMPSON_QRacrQG_DATA.DBL',form='unformatted',status='new',iostat=istatus)
 if (istatus /= 0) then
    call print_parallel_mesg('MP_THOMPSON_QRacrQG_DATA.DBL')
    return
 end if
 call qr_acr_qg
 write(mp_unit) tcg_racg
 write(mp_unit) tmr_racg
 write(mp_unit) tcr_gacr
 write(mp_unit) tmg_gacr
 write(mp_unit) tnr_racg
 write(mp_unit) tnr_gacr
 close(unit=mp_unit)

!--- building look-up table for rain collecting snow:
 write(0,*)
 write(0,*) '--- building MP_THOMPSON_QRacrQS_DATA.DBL'
 open(unit=mp_unit,file='MP_THOMPSON_QRacrQS_DATA.DBL',form='unformatted',status='new',iostat=istatus)
 if (istatus /= 0) then
    call print_parallel_mesg('MP_THOMPSON_QRacrQS_DATA.DBL')
    return
 end if
 call qr_acr_qs
 write(mp_unit)tcs_racs1
 write(mp_unit)tmr_racs1
 write(mp_unit)tcs_racs2
 write(mp_unit)tmr_racs2
 write(mp_unit)tcr_sacr1
 write(mp_unit)tms_sacr1
 write(mp_unit)tcr_sacr2
 write(mp_unit)tms_sacr2
 write(mp_unit)tnr_racs1
 write(mp_unit)tnr_racs2
 write(mp_unit)tnr_sacr1
 write(mp_unit)tnr_sacr2
 close(unit=mp_unit)

!--- building look-up table for freezing of cloud droplets:
 write(0,*)
 write(0,*) '--- building MP_THOMPSON_freezeH2O_DATA.DBL'
 open(unit=mp_unit,file='MP_THOMPSON_freezeH2O_DATA.DBL',form='unformatted',status='new',iostat=istatus)
 if (istatus /= 0) then
    call print_parallel_mesg('MP_THOMPSON_freezeH2O_DATA.DBL')
    return
 end if
 call freezeH2O
 write(mp_unit) tpi_qrfz
 write(mp_unit) tni_qrfz
 write(mp_unit) tpg_qrfz
 write(mp_unit) tnr_qrfz
 write(mp_unit) tpi_qcfz
 write(mp_unit) tni_qcfz
 close(unit=mp_unit)
 
!--- building look-up table for autoconversion of cloud ice to snow:
 write(0,*)
 write(0,*) '--- building MP_THOMPSON_QIautQS_DATA.DBL'
 open(unit=mp_unit,file='MP_THOMPSON_QIautQS_DATA.DBL',form='unformatted',status='new',iostat=istatus)
 if (istatus /= 0) then
    call print_parallel_mesg('MP_THOMPSON_QIautQS_DATA.DBL')
    return
 end if
 call qi_aut_qs
 write(mp_unit) tpi_ide
 write(mp_unit) tps_iaus
 write(mp_unit) tni_iaus
 close(unit=mp_unit)
 call mpas_release_unit(mp_unit)

 write(0,*)
 write(0,*) 'Finished building all tables.'
 write(0,*)
 write(0,*) '*******************************************************************************'
 write(0,*) 'To preserve these tables when running ''make clean'', please copy the following'
 write(0,*) 'files to the src/core_atmosphere/physics/physics_wrf/files/ directory:'
 write(0,*)
 write(0,*) '  MP_THOMPSON_QRacrQG_DATA.DBL'
 write(0,*) '  MP_THOMPSON_QRacrQS_DATA.DBL'
 write(0,*) '  MP_THOMPSON_freezeH2O_DATA.DBL'
 write(0,*) '  MP_THOMPSON_QIautQS_DATA.DBL'
 write(0,*)
 write(0,*) 'Tables in the src/core_atmosphere/physics/physics_wrf/files/ directory '
 write(0,*) 'will be automatically linked to the top-level MPAS directory when compiling'
 write(0,*) 'the ''atmosphere'' core.'
 write(0,*) '*******************************************************************************'

 end subroutine build_tables_thompson


!=================================================================================================================
 subroutine print_parallel_mesg(filename)
!=================================================================================================================

 character(len=*), intent(in) :: filename

 write(0,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
 write(0,*) '! Error encountered while trying to create new file '//trim(filename)
 write(0,*) '! '
 write(0,*) '! Please ensure that this file does not exist before running ''build_tables'','
 write(0,*) '! and ensure that ''build_tables'' is *NOT* run in parallel.'
 write(0,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

 end subroutine print_parallel_mesg

!=================================================================================================================
 end module atmphys_build_tables_thompson 
!=================================================================================================================
