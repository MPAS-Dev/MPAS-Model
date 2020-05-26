! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_forcing_explicit_bottom_drag
!
!> \brief MPAS ocean explicit bottom drag
!> \author Mark Petersen, Phil Jones
!> \date   August 2017, updated May 2020
!> \details
!>  This module contains the routine for computing
!>  tendencies from explicit bottom drag.
!
!-------------------------------------------------------------------------------

module ocn_vel_forcing_explicit_bottom_drag

   use mpas_timer

   use ocn_constants
   use ocn_config
   use ocn_mesh

   implicit none
   private
   save

   !----------------------------------------------------------------------------
   ! Public parameters
   !----------------------------------------------------------------------------

   !----------------------------------------------------------------------------
   ! Public member functions
   !----------------------------------------------------------------------------

   public :: ocn_vel_forcing_explicit_bottom_drag_tend, &
             ocn_vel_forcing_explicit_bottom_drag_init

   !----------------------------------------------------------------------------
   ! Private module variables
   !----------------------------------------------------------------------------

   logical :: explicitBottomDragOn  ! on/off switch for explicit bottom drag

   real (kind=RKIND) :: explicitBottomDragCoef ! drag coefficient

!*******************************************************************************

contains

!*******************************************************************************
!
!  routine ocn_vel_forcing_explicit_bottom_drag_tend
!
!> \brief   Computes tendency term from explicit bottom drag
!> \author  Mark Petersen, Phil Jones
!> \date    15 August 2017, updated May 2020
!> \details
!>  This routine computes the explicit bottom drag tendency for momentum
!>  based on current state.
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_forcing_explicit_bottom_drag_tend(normalVelocity,    &
                                    kineticEnergyCell, layerThicknessEdge, &
                                    tend, err)

      !-------------------------------------------------------------------------
      ! input variables
      !-------------------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         normalVelocity,       &!< [in] velocity
         kineticEnergyCell,    &!< [in] kinetic energy at cell
         layerThicknessEdge     !< [in] thickness at edge

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

      integer ::      &
         iEdge,       &! edge loop iterator
         kBot,        &! bottom level index for each edge
         cell1, cell2  ! neighbor cell indices across edge

      ! End preamble
      !-------------
      ! Begin code

      ! Exit if not selected. Otherwise, start timer

      err = 0
      if ( .not. explicitBottomDragOn ) return

      call mpas_timer_start('vel explicit bottom drag')


      ! Explicit bottom drag term:
      ! du/dt = ... - c |u| u / h
      ! appied to bottom layer only.
      ! This term comes from the bottom boundary condition in the 
      ! vertical momentum mixing, and is explicit if both |u| and u 
      ! are chosen to be at time level n. 

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(cellsOnEdge, maxLevelEdgeTop, tend, &
      !$acc            kineticEnergyCell, normalVelocity,  &
      !$acc            layerThicknessEdge)                 &
      !$acc    private(kBot, cell1, cell2)
#else
      !$omp parallel do schedule(runtime) &
#endif
      do iEdge = 1, nEdgesOwned
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         kBot  = maxLevelEdgeTop(iEdge)

         tend(kBot,iEdge) = tend(kBot,iEdge) - explicitBottomDragCoef * & 
                            sqrt(kineticEnergyCell(kBot,cell1) +  &
                                 kineticEnergyCell(kBot,cell2)) * &
                            normalVelocity(kBot,iEdge)/           &
                            layerThicknessEdge(kBot,iEdge)
      enddo
#ifdef MPAS_OPENACC
      !$omp end parallel do
#endif

      call mpas_timer_stop('vel explicit bottom drag')

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_forcing_explicit_bottom_drag_tend!}}}

!*******************************************************************************
!
!  routine ocn_vel_forcing_explicit_bottom_drag_init
!
!> \brief   Initializes ocean explicit bottom drag forcing
!> \author  Mark Petersen, Phil Jones
!> \date    August 2017, updated May 2020
!> \details
!>  This routine initializes quantities related to explicit bottom drag
!>  in the ocean.
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_forcing_explicit_bottom_drag_init(err)

      !-------------------------------------------------------------------------
      ! output variables
      !-------------------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !{{{
      !-------------------------------------------------------------------------
      ! local variables
      !-------------------------------------------------------------------------

      ! End preamble
      !-------------
      ! Begin code

      ! Set default values for module variables

      err = 0
      explicitBottomDragOn   = .false.
      explicitBottomDragCoef = 0.0_RKIND

      ! Revise values based on user input in ocn_config

      if (config_use_explicit_bottom_drag) then
         explicitBottomDragOn   = .true.
         explicitBottomDragCoef = config_explicit_bottom_drag_coeff
      endif

      if (config_disable_vel_explicit_bottom_drag) &
         explicitBottomDragOn = .false.

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_forcing_explicit_bottom_drag_init!}}}

!*******************************************************************************

end module ocn_vel_forcing_explicit_bottom_drag

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
