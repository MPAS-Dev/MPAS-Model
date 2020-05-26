! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_hmix
!
!> \brief MPAS ocean horizontal momentum mixing driver
!> \author Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date   September 2011, update April 2020
!> \details
!>  This module contains the main driver routine for computing
!>  horizontal velocity mixing tendencies.
!>
!-----------------------------------------------------------------------

module ocn_vel_hmix

   use mpas_timer
   use ocn_vel_hmix_del2
   use ocn_vel_hmix_leith
   use ocn_vel_hmix_del4
   use ocn_constants
   use ocn_config

   implicit none
   private
   save

   !--------------------------------------------------------------------
   ! Public parameters
   !--------------------------------------------------------------------

   !--------------------------------------------------------------------
   ! Public member functions
   !--------------------------------------------------------------------

   public :: ocn_vel_hmix_tend, &
             ocn_vel_hmix_init

   !--------------------------------------------------------------------
   ! Private module variables
   !--------------------------------------------------------------------

   logical :: hmixOn

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_vel_hmix_tend
!
!> \brief   Computes tendency term for horizontal momentum mixing
!> \author  Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date    September 2011, updated April 2020
!> \details
!>  This routine computes the horizontal mixing tendency for momentum
!>  based on current state and user choices of mixing parameterization.
!>  Multiple parameterizations may be chosen and added together.  These
!>  tendencies are generally computed by calling the specific routine
!>  for the chosen parameterization, so this routine is primarily a
!>  driver for managing these choices.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_tend(divergence, relVorticity, tend, err)

      !-----------------------------------------------------------------
      ! input variables
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         divergence      !< [in] velocity divergence

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         relVorticity    !< [in] relative vorticity

      !-----------------------------------------------------------------
      ! input/output variables
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(inout) :: &
         tend            !< [inout] accumulated velocity tendency

      !-----------------------------------------------------------------
      ! output variables
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------

      integer :: err1  ! temp for managing multiple error codes

      ! end preamble
      !-------------
      ! begin code

      ! return if no mixing is turned on, otherwise start timer

      err = 0
      if (.not. hmixOn) return

      call mpas_timer_start("vel hmix")

      !-----------------------------------------------------------------
      ! call relevant routines for computing tendencies
      ! note that the user can choose multiple options and the
      !   tendencies will be added together
      ! if OpenACC is enabled, the subroutine arguments are assumed
      !   to have been copied to the device in the calling tendency
      !   routine and retained there along with any synchronization 
      !   with the host. That also means all the routines called below
      !   must also be GPU enabled so that the tendency is added on
      !   the device and not the host.
      !-----------------------------------------------------------------

      call ocn_vel_hmix_del2_tend(divergence, relVorticity, tend, err1)
      err = ior(err1, err)

      call ocn_vel_hmix_leith_tend(divergence, relVorticity, tend, err1)
      err = ior(err1, err)

      call ocn_vel_hmix_del4_tend(divergence, relVorticity, tend, err1)
      err = ior(err1, err)

      call mpas_timer_stop("vel hmix")

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_tend!}}}

!***********************************************************************
!
!  routine ocn_vel_hmix_init
!
!> \brief   Initializes ocean momentum horizontal mixing quantities
!> \author  Mark Petersen, Doug Jacobsen, Todd Ringler
!> \date    September 2011, updated April 2020
!> \details
!>  This routine initializes a variety of quantities related to
!>  horizontal velocity mixing in the ocean. Since a variety of
!>  parameterizations are available, this routine primarily calls the
!>  individual init routines for each parameterization.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_init(err)

      !-----------------------------------------------------------------
      ! output variables
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------

      integer :: err1, err2, err3 ! error codes from each init

      ! end preamble
      !-------------
      ! begin code

      err = 0

      !-----------------------------------------------------------------
      ! set overall mixing flag - on by default
      !-----------------------------------------------------------------

      hmixOn = .true.
      if (config_disable_vel_hmix) hmixOn = .false.

      !-----------------------------------------------------------------
      ! call individual init routines for each parameterization
      !-----------------------------------------------------------------

      if (hmixOn) then
         call ocn_vel_hmix_del2_init(err1)
         call ocn_vel_hmix_leith_init(err2)
         call ocn_vel_hmix_del4_init(err3)
         err = ior(ior(err1, err2),err3)
      endif

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_init!}}}

!***********************************************************************

end module ocn_vel_hmix

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
