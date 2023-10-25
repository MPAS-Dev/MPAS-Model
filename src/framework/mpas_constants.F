! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!***********************************************************************
!
!  mpas_constants
!
!> \brief   MPAS Constant Module
!> \author  Michael Duda
!> \date    03/27/13
!> \details 
!> This module provides various constants that can be used in different parts of MPAS.
!> They may or may not be a physical quantity.
!
!-----------------------------------------------------------------------

module mpas_constants

   use mpas_kind_types

#ifdef MPAS_CAM_DYCORE
   use physconst, only : pii => pi
   use physconst, only : gravity => gravit
   use physconst, only : omega
   use physconst, only : a => rearth
   use physconst, only : cp => cpair
   use physconst, only : rgas => rair
   use physconst, only : rv => rh2o
   real (kind=RKIND) :: rvord = huge(1.0_RKIND)                       ! Derived in mpas_constants_compute_derived
   real (kind=RKIND) :: cv = huge(1.0_RKIND)                          ! Derived in mpas_constants_compute_derived
   real (kind=RKIND) :: cvpm = huge(1.0_RKIND)                        ! Derived in mpas_constants_compute_derived
#else
   real (kind=RKIND), parameter :: pii     = 3.141592653589793_RKIND  !< Constant: Pi
   real (kind=RKIND), parameter :: a       = 6371229.0_RKIND          !< Constant: Spherical Earth radius [m]
   real (kind=RKIND), parameter :: omega   = 7.29212e-5_RKIND         !< Constant: Angular rotation rate of the Earth [s-1]
   real (kind=RKIND), parameter :: gravity = 9.80616_RKIND            !< Constant: Acceleration due to gravity [m s-2]
   real (kind=RKIND), parameter :: rgas    = 287.0_RKIND              !< Constant: Gas constant for dry air [J kg-1 K-1]
   real (kind=RKIND), parameter :: rv      = 461.6_RKIND              !< Constant: Gas constant for water vapor [J kg-1 K-1]
!  real (kind=RKIND), parameter :: cp      = 1003.0_RKIND             !< Constant: Specific heat of dry air at constant pressure [J kg-1 K-1]
   real (kind=RKIND), parameter :: cp      = 7.0_RKIND*rgas/2.0_RKIND !< Constant: Specific heat of dry air at constant pressure [J kg-1 K-1]
   real (kind=RKIND), parameter :: rvord   = rv / rgas                !
   real (kind=RKIND), parameter :: cv      = cp - rgas                !< Constant: Specific heat of dry air at constant volume [J kg-1 K-1]
   real (kind=RKIND), parameter :: cvpm    = -cv / cp                 !
#endif
   real (kind=RKIND), parameter :: p0 = 1.0e5_RKIND                   !< Constant: 100000 Pa
   real (kind=RKIND), parameter :: prandtl = 1.0_RKIND                !< Constant: Prandtl number

   contains


!***********************************************************************
!
!  mpas_constants_compute_derived
!
!> \brief   Computes derived constants
!> \author  Michael Duda
!> \date    8 May 2020
!> \details 
!>  This routine provides a place where physical constants provided by
!>  the mpas_constants module may be computed at runtime. For example,
!>  if some constants depend on namelist options or other runtime
!>  settings, other constants that derive from them may be computed in
!>  this routine.
!>
!>  At present, the MPAS infrastructure does not call this routine, and
!>  it is the responsibility of any MPAS core that needs to compute
!>  derived constants at runtime to add calls to this routine, e.g., in
!>  its core_init routine.
!
!-----------------------------------------------------------------------
   subroutine mpas_constants_compute_derived()

       implicit none

#ifdef MPAS_CAM_DYCORE
       !
       ! In the case of CAM-MPAS, rgas may depend on a CAM namelist option,
       ! so physical constants that depend on rgas must be computed here after
       ! CAM has called the physconst_readnl routine.
       !

       rvord   = rv / rgas
       cv      = cp - rgas
       cvpm    = -cv / cp
#endif

   end subroutine mpas_constants_compute_derived

end module mpas_constants
