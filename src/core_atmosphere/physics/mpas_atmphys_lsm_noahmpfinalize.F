! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_lsm_noahmpfinalize
 use mpas_log,only: mpas_log_write

 use mpas_atmphys_vars,only: mpas_noahmp
 use NoahmpIOVarFinalizeMod,only: NoahmpIOVarFinalizeDefault


 private
 public:: sf_noahmp_deallocate


 contains

!=================================================================================================================
 subroutine sf_noahmp_deallocate( )
!=================================================================================================================
!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine sf_noahmp_deallocate:')


!--- deallocate Noahmp arrays:
 call NoahmpIOVarFinalizeDefault(mpas_noahmp)


!call mpas_log_write('--- end subroutine sf_noahmp_deallocate:')

 end subroutine sf_noahmp_deallocate

!=================================================================================================================
 end module mpas_atmphys_lsm_noahmpfinalize
!=================================================================================================================
