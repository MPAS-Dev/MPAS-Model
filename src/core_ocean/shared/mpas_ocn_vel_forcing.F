! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_forcing
!
!> \brief MPAS ocean velocity forcing driver
!> \author Doug Jacobsen, Mark Petersen, Todd Ringler, Phil Jones
!> \date   September 2011, updated May 2020
!> \details
!>  This module contains the main driver routine for computing momentum (vel)
!>  tendencies from forcing at top and bottom boundaries.
!
!-------------------------------------------------------------------------------

module ocn_vel_forcing

   use mpas_derived_types

   use ocn_constants
   use ocn_vel_forcing_surface_stress
   use ocn_vel_forcing_explicit_bottom_drag

   implicit none
   private
   save

   !----------------------------------------------------------------------------
   ! Public parameters
   !----------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! Public member functions
   !----------------------------------------------------------------------------

   public :: ocn_vel_forcing_tend, &
             ocn_vel_forcing_init

   !----------------------------------------------------------------------------
   ! Private module variables
   !----------------------------------------------------------------------------


!*******************************************************************************

contains

!*******************************************************************************
!
!  routine ocn_vel_forcing_tend
!
!> \brief   Computes tendency term from forcings
!> \author  Doug Jacobsen, Mark Petersen, Todd Ringler, Phil Jones
!> \date    15 September 2011, updated May 2020
!> \details
!>  This routine computes the forcing tendency for momentum
!>  based on current state and user choices of forcings.
!>  Multiple forcings may be chosen and added together.  These
!>  tendencies are generally computed by calling the specific routine
!>  for the chosen forcing, so this routine is primarily a
!>  driver for managing these choices.
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_forcing_tend(normalVelocity,                    &
                                   surfaceFluxAttenuationCoefficient, &
                                   surfaceStress, kineticEnergyCell,  & 
                                   layerThicknessEdge, tend, err)

      !-------------------------------------------------------------------------
      ! input variables
      !-------------------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         normalVelocity,    &!< [in] Normal velocity at edges
         kineticEnergyCell   !< [in] kinetic energy at cell

      real (kind=RKIND), dimension(:), intent(in) :: &
         surfaceFluxAttenuationCoefficient, & !< [in] attenuation coef for sfc flx
         surfaceStress       !< [in] surface stress at edges

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         layerThicknessEdge  !< [in] thickness at edge

      !-------------------------------------------------------------------------
      ! input/output variables
      !-------------------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(inout) :: &
         tend            !< [inout] velocity tendency

      !-------------------------------------------------------------------------
      ! output variables
      !-------------------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-------------------------------------------------------------------------
      ! local variables
      !-------------------------------------------------------------------------

      integer :: err1 ! local error flag

      ! End preamble
      !-------------
      ! Begin code

      ! call relevant routines for computing tendencies
      ! note that the user can choose multiple options and the
      !   tendencies will be added together

      err = 0

      call ocn_vel_forcing_surface_stress_tend( &
                                   surfaceFluxAttenuationCoefficient, &
                                   surfaceStress, layerThicknessEdge, &
                                   tend, err1)
      err = ior(err, err1)

      call ocn_vel_forcing_explicit_bottom_drag_tend(normalVelocity, &
                  kineticEnergyCell, layerThicknessEdge, tend, err1)

      err = ior(err, err1)

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_forcing_tend!}}}

!*******************************************************************************
!
!  routine ocn_vel_forcing_init
!
!> \brief   Initializes ocean forcings
!> \author  Doug Jacobsen, Mark Petersen, Todd Ringler, Phil Jones
!> \date    September 2011, May 2020
!> \details
!>  This routine initializes quantities related to forcing at the top and
!>  bottome of the ocean. Since a multiple forcings are available,
!>  this routine primarily calls the individual init routines for each.
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_forcing_init(err)

      !-------------------------------------------------------------------------
      ! output variables
      !-------------------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-------------------------------------------------------------------------
      ! local variables
      !-------------------------------------------------------------------------

      integer :: err1, err2  ! local error flags

      ! End preamble
      !-------------
      ! Begin code

      ! call individual init routines for each parameterization



      call ocn_vel_forcing_surface_stress_init(err1)
      call ocn_vel_forcing_explicit_bottom_drag_init(err2)

      err = ior(err1, err2)

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_forcing_init!}}}

!*******************************************************************************

end module ocn_vel_forcing

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
