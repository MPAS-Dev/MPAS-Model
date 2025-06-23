! Copyright (c) 2025,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
module mpas_atmchem_control


  implicit none
  private
  public:: chemistry_setup_packages

!MPAS chemistry package configuration routines.
!ACOM Software Engineering Team 2025
!
! subroutines in mpas_atmchem_init:
! ---------------------------------
! chemistry_setup_packages : identifies available chemistry packages

contains


!=================================================================================================================
 subroutine chemistry_setup_packages()
!=================================================================================================================
  use mpas_musica, only : log_musica_support

  call log_musica_support()

 end subroutine chemistry_setup_packages

!=================================================================================================================
end module mpas_atmchem_control
!=================================================================================================================