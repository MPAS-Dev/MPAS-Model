! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_vadv
!
!> \brief MPAS ocean vertical advection
!> \author Mark Petersen, Phil Jones, Rob Aulwes
!> \date   September 2011, updated May 2020
!> \details
!>  This module contains the routine for computing
!>  tendencies for momentum vertical advection.
!>
!-------------------------------------------------------------------------------

module ocn_vel_vadv

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

   public :: ocn_vel_vadv_tend, &
             ocn_vel_vadv_init

   !----------------------------------------------------------------------------
   ! Private module variables
   !----------------------------------------------------------------------------

   logical :: velVadvOn  ! on/off flag for this tendency

!*******************************************************************************

contains

!*******************************************************************************
!
!  routine ocn_vel_vadv_tend
!
!> \brief   Computes tendency term for vertical advection
!> \author  Mark Petersen
!> \date    September 2011
!> \details
!>  This routine computes the vertical advection tendency for momentum
!>  based on current state and adds to the sum of velocity tendencies.
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_vadv_tend(normalVelocity, layerThicknessEdge, &
                                vertAleTransportTop, tend, err)

      !-------------------------------------------------------------------------
      ! input variables
      !-------------------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         normalVelocity,     &!< [in] Horizontal velocity
         layerThicknessEdge, &!< [in] thickness at edge
         vertAleTransportTop  !< [in] Vertical velocity on top layer

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

      integer ::        &
         iEdge, k,      &! loop counters
         kmax,          &! max vertical level on edge
         cell1, cell2    ! neighbor cell indices across edge

      real (kind=RKIND) :: &
         vertAleTransportTopEdge  ! vertical transport at top of edge

      real (kind=RKIND), dimension(:,:), allocatable :: &
         w_dudzTopEdge   ! vertical advection tendency at top of edge

      ! End preamble
      !-------------
      ! Begin code

      ! Return if not turned on, otherwise start timer

      err = 0
      if (.not. velVadvOn) return

      call mpas_timer_start("vel vadv")

      ! allocate a temporary for vertical velocity
      allocate(w_dudzTopEdge(nVertLevels+1,nEdgesOwned))

      ! transfer data to the device
      !$acc enter data create(w_dudzTopEdge)

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(cellsOnEdge, maxLevelEdgeTop,       &
      !$acc            w_dudzTopEdge, vertAleTransportTop, &
      !$acc            normalVelocity, layerThicknessEdge) &
      !$acc    private(k, kmax, cell1, cell2, vertAleTransportTopEdge)
#else
      !$omp parallel
      !$omp do schedule(runtime) &
      !$omp    private(k, kmax, cell1, cell2, vertAleTransportTopEdge)
#endif
      do iEdge = 1, nEdgesOwned
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         kmax  = maxLevelEdgeTop(iEdge)
         w_dudzTopEdge(1,iEdge) = 0.0_RKIND

         do k = 2, kmax

            ! Average w from cell center to edge
            vertAleTransportTopEdge = 0.5_RKIND* &
                                      (vertAleTransportTop(k,cell1) + &
                                       vertAleTransportTop(k,cell2))

            ! compute dudz at vertical interface with first order derivative.
            w_dudzTopEdge(k,iEdge) = vertAleTransportTopEdge*         &
                               (normalVelocity(k-1,iEdge) -     &
                                normalVelocity(k  ,iEdge))/     &
                    (0.5_RKIND*(layerThicknessEdge(k-1,iEdge) + &
                                layerThicknessEdge(k  ,iEdge)))
 
         end do
         w_dudzTopEdge(kmax+1,iEdge) = 0.0_RKIND
      end do

      ! Tendency is the average w*du/dz from vertical interface to 
      ! vertical middle of cell
#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(maxLevelEdgeTop, w_dudzTopEdge) &
      !$acc    private(k)
#else
      !$omp do schedule(runtime) &
      !$omp    private(k)
#endif
      do iEdge = 1, nEdgesOwned
      do k = 1, maxLevelEdgeTop(iEdge)
         tend(k,iEdge) = tend(k,iEdge) -                 &
                         0.5*(w_dudzTopEdge(k  ,iEdge) + &
                              w_dudzTopEdge(k+1,iEdge))
      enddo
      enddo
#ifndef MPAS_OPENACC
      !$omp end do
      !$omp end parallel
#endif

      ! remove/transfer data from the device
      !$acc exit data delete(w_dudzTopEdge)

      deallocate(w_dudzTopEdge)

      call mpas_timer_stop("vel vadv")

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_vadv_tend!}}}

!*******************************************************************************
!
!  routine ocn_vel_vadv_init
!
!> \brief   Initializes ocean momentum vertical advection
!> \author  Mark Petersen, Phil Jones, Rob Aulwes
!> \date    September 2011, update May 2020
!> \details
!>  This routine initializes a variety of quantities related to
!>  vertical velocity advection in the ocean.
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_vadv_init(err)

      !-------------------------------------------------------------------------
      ! Output variables
      !-------------------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-------------------------------------------------------------------------
      ! Local variables
      !-------------------------------------------------------------------------

      ! End preamble
      !-------------
      ! Begin code

      err = 0

      ! Set on/off flag based on input configuration options

      ! Explicit switch
      velVadvOn = .true.
      if ( config_disable_vel_vadv ) velVadvOn = .false.

      ! Some vertical coordinate choices disable vertical advection
      if (trim(config_vert_coord_movement) == 'impermeable_interfaces') &
          velVadvOn = .false.

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_vadv_init!}}}

!*******************************************************************************

end module ocn_vel_vadv

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
