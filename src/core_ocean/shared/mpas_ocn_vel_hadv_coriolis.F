! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_hadv_coriolis
!
!> \brief MPAS ocean horizontal velocity advection coriolis tendencies
!> \author Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones, Rob Aulwes
!> \date   September 2011, updated May 2020
!> \details
!>  This module contains the routine for computing
!>  tendencies from the horizontal advection and coriolis force.
!>
!
!-------------------------------------------------------------------------------

module ocn_vel_hadv_coriolis

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

   public :: ocn_vel_hadv_coriolis_tend, &
             ocn_vel_hadv_coriolis_init

   !----------------------------------------------------------------------------
   ! Private module variables
   !----------------------------------------------------------------------------

   logical :: hadvAndCoriolisOn ! main on/off switch forr hadv

   real (kind=RKIND) :: RK4Mask ! some terms masked when RK4 is on/off

!*******************************************************************************

contains

!*******************************************************************************
!
!  routine ocn_vel_hadv_coriolis_tend
!
!> \brief   Computes tendency term for horizontal advection and coriolis force
!> \author  Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones, Rob Aulwes
!> \date    September 2011, updated May 2020
!> \details
!>  This routine computes the horizontal momentum advection and coriolis
!>  tendencies based on current state and adds them to the total vel tendency
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_hadv_coriolis_tend(normalizedRelativeVorticityEdge,    &
                                         normalizedPlanetaryVorticityEdge,   &
                                         layerThicknessEdge, normalVelocity, &
                                         kineticEnergyCell, tend, err)

      !-------------------------------------------------------------------------
      ! input variables
      !-------------------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         normalizedRelativeVorticityEdge, &!< [in] rel vorticity/thick on edge
         normalizedPlanetaryVorticityEdge,&!< [in] planetary vorticity/thick
         layerThicknessEdge,              &!< [in] Thickness on edge
         normalVelocity,                  &!< [in] Horizontal velocity
         kineticEnergyCell                 !< [in] Kinetic Energy

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

      integer ::       &
         j, k, iEdge,  &! loop counters
         cell1, cell2, &! neighboring cell indices
         kmax,         &! deepest vertical level
         eoe            ! edge on edge index

      real (kind=RKIND) :: &
         workVorticity,    &! temporary vorticity factor
         invLength,        &! inverse of dcEdge
         edgeWeight         ! weight for each edge contribution

      real (kind=RKIND), dimension(:,:), allocatable :: &
         qArr               ! temporary for vorticity term

      ! End preamble
      !-------------
      ! Begin code

      ! Set error and return if this tendency has been turned off
      ! Otherwise, start timer

      err = 0
      if ( .not. hadvAndCoriolisOn ) return

      call mpas_timer_start("coriolis")

      allocate( qArr(nVertLevels,nEdgesOwned) )
      !$acc enter data create(qArr)

      ! Loop over edges. Tendencies are only needed for owned edges

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(maxLevelEdgeTop, qArr, nEdgesOnEdge, & 
      !$acc            edgesOnEdge, weightsOnEdge,          &
      !$acc            normalizedRelativeVorticityEdge,     &
      !$acc            normalizedPlanetaryVorticityEdge,    &
      !$acc            normalVelocity, layerThicknessEdge)  &
      !$acc    private(j, k, kmax, eoe, edgeWeight, workVorticity)
#else
      !$omp parallel
      !$omp do schedule(runtime) &
      !$omp    private(j, k, kmax, eoe, edgeWeight, workVorticity)
#endif
      do iEdge = 1, nEdgesOwned
         kmax = maxLevelEdgeTop(iEdge)

         do k = 1, kmax
            qArr(k,iEdge) = 0.0_RKIND
         end do

         do j = 1, nEdgesOnEdge(iEdge)
            eoe = edgesOnEdge(j, iEdge)
            edgeWeight = weightsOnEdge(j, iEdge)

            do k = 1, kmax
               workVorticity = 0.5_RKIND* &
                  (normalizedRelativeVorticityEdge (k,iEdge) + &
                   normalizedPlanetaryVorticityEdge(k,iEdge)*RK4Mask + &
                   normalizedRelativeVorticityEdge (k,eoe)   + &
                   normalizedPlanetaryVorticityEdge(k,eoe)*RK4Mask)
               qArr(k,iEdge) = &
               qArr(k,iEdge) + edgeWeight*normalVelocity(k,eoe)* &
                        workVorticity*layerThicknessEdge(k,eoe)
            end do

         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end do
#endif

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(maxLevelEdgeTop, cellsOnEdge,          & 
      !$acc            dcEdge, tend, qArr, kineticEnergyCell) &
      !$acc    private(k, cell1, cell2, invLength)
#else
      !$omp do schedule(runtime) &
      !$omp    private(k, cell1, cell2, invLength)
#endif
      do iEdge = 1, nEdgesOwned
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         invLength = 1.0_RKIND / dcEdge(iEdge)

         do k = 1, maxLevelEdgeTop(iEdge)
            tend(k, iEdge) = tend(k, iEdge) + (qArr(k,iEdge) - &
                             (kineticEnergyCell(k, cell2) &
                            - kineticEnergyCell(k, cell1) ) * invLength)
         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end do
      !$omp end parallel
#endif

      !$acc exit data delete(qArr)
      deallocate( qArr )

      call mpas_timer_stop("coriolis")

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_hadv_coriolis_tend!}}}

!*******************************************************************************
!
!  routine ocn_vel_hadv_coriolis_init
!
!> \brief  Initializes momentum horizontal advection and coriolis tendencies
!> \author Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones, Rob Aulwes
!> \date   September 2011, updated May 2020
!
!-------------------------------------------------------------------------------

   subroutine ocn_vel_hadv_coriolis_init(err)

      !-------------------------------------------------------------------------
      ! Output variables
      !-------------------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !{{{
      !-------------------------------------------------------------------------
      ! Local variables
      !-------------------------------------------------------------------------

      ! End preamble
      !-------------
      ! Begin code

      err = 0

      ! Set main on/off flag based on input config flag

      hadvAndCoriolisOn = .true.
      if ( config_disable_vel_coriolis ) hadvAndCoriolisOn = .false.

      ! Coriolis term is treated differently depending on time
      ! integration algorithm

      if ( trim( config_time_integrator ) == 'RK4') then
         ! For RK4, coriolis tendency term includes f: (eta+f)/h.
         RK4Mask = 1.0_RKIND
      elseif ( trim( config_time_integrator ) == 'split_explicit' &
          .or. trim( config_time_integrator ) == 'unsplit_explicit') then
         ! For split explicit, Coriolis tendency uses eta/h because the 
         ! Coriolis term is added separately to the momentum tendencies.
         RK4Mask = 0.0_RKIND
      end if

   !----------------------------------------------------------------------------

   end subroutine ocn_vel_hadv_coriolis_init!}}}

!*******************************************************************************

end module ocn_vel_hadv_coriolis

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
