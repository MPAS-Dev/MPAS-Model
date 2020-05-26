! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_hmix_leith
!
!> \brief Ocean horizontal mixing - Leith parameterization
!> \author Mark Petersen
!> \date   22 October 2012, updated April 2020
!> \details
!>  This module contains routines for computing horizontal mixing
!>  tendencies using the Leith parameterization.
!
!-----------------------------------------------------------------------

module ocn_vel_hmix_leith

   use mpas_timer
   use mpas_constants
   use ocn_constants
   use ocn_config
   use ocn_mesh

   implicit none
   private
   save

   !--------------------------------------------------------------------
   ! Public parameters
   !--------------------------------------------------------------------

   !--------------------------------------------------------------------
   ! Public member functions
   !--------------------------------------------------------------------

   public :: ocn_vel_hmix_leith_tend, &
             ocn_vel_hmix_leith_init

   !-------------------------------------------------------------------
   ! Private module variables
   !--------------------------------------------------------------------

   logical :: hmixLeithOn  !< flag to determine whether Leith chosen

   real (kind=RKIND) :: &
      hmixLeithFactor,  &!< precomputed Leith viscosity factor
      hmixLeithVisc2max  !< Leith viscosity coefficient

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_vel_hmix_leith_tend
!
!> \brief  Computes tendency term for horizontal momentum mixing with Leith parameterization
!> \author Mark Petersen, Todd Ringler
!> \date   22 October 2012, updated April 2020
!> \details
!> This routine computes the horizontal mixing tendency for momentum
!> based on the Leith closure.  The Leith closure is the
!> enstrophy-cascade analogy to the Smagorinsky (1963) energy-cascade
!> closure, i.e. Leith (1996) assumes an inertial range of enstrophy 
!> flux moving toward the mesh scale. The assumption of an enstrophy 
!> cascade and dimensional analysis produces right-hand-side 
!> dissipation, !$\bf{D}$, of velocity of the form
!> $ {\bf D} = \nabla \cdot \left( \nu_\ast \nabla {\bf u} \right)
!>    = \nabla \cdot \left( \gamma \left| \nabla \omega  \right|
!>      \left( \Delta x \right)^3 \nabla \bf{u} \right)
!> where $\omega$ is the relative vorticity and $\gamma$ is a 
!> non-dimensional, $O(1)$ parameter. We set $\gamma=1$.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_leith_tend(divergence, relVorticity, tend, &
                                      err)

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
         tend            !< [inout] velocity tendency

      !-----------------------------------------------------------------
      ! output variables
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------

      integer ::         &
         iEdge, k,       &! loop indices for edge, vertical
         cell1, cell2,   &! neighbor cell   indices across edge
         vertex1, vertex2 ! neighbor vertex indices across edge

      real (kind=RKIND) :: &
         uDiff,          &! temporary u diffusion calculation
         vortDiff,       &! temporary vorticity difference
         invLengthDc,    &! inverse dist between centers 
         invLengthDv,    &! inverse dist between vertices
         scaleFactor,    &! precomputed dx^3
         visc2            ! intermediate viscosity temp

      ! end preamble
      !-------------
      ! begin code

      ! exit if Leith mixing is not selected

      err = 0
      if (.not. hmixLeithOn) return

      call mpas_timer_start("vel leith")

      ! Compute the tendency

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(nEdgesOwned, &
      !$acc            cellsOnEdge, verticesOnEdge, &
      !$acc            dcEdge, dvEdge, meshScaling, &
      !$acc            divergence, relVorticity, &
      !$acc            maxLevelEdgeTop, tend)   &
      !$acc    private(k, cell1, cell2, vertex1, vertex2, scaleFactor, &
      !$acc         invLengthDc, invLengthDv, uDiff, visc2, vortDiff)
#else
      !$omp parallel do schedule(runtime) &
      !$omp    private(k, cell1, cell2, vertex1, vertex2, scaleFactor, &
      !$omp         invLengthDc, invLengthDv, uDiff, visc2, vortDiff)
#endif
      do iEdge = 1, nEdgesOwned
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         vertex1 = verticesOnEdge(1,iEdge)
         vertex2 = verticesOnEdge(2,iEdge)

         invLengthDc = 1.0_RKIND / dcEdge(iEdge)
         invLengthDv = 1.0_RKIND / dvEdge(iEdge)

         scaleFactor = hmixLeithFactor* &
                       (meshScaling(iEdge))**3

         do k = 1, maxLevelEdgeTop(iEdge)

            ! Here -( relVorticity(k,vertex2) - relVorticity(k,vertex1) ) / dvEdge(iEdge)
            ! is - \nabla relVorticity pointing from vertex 2 to vertex 1, or equivalently
            !    + k \times \nabla relVorticity pointing from cell1 to cell2.
            ! uDiff is \nabla^2 u (see formula for $\bf{D}$ above).

            vortDiff = relVorticity(k,vertex2) - relVorticity(k,vertex1)

            uDiff = (divergence(k,cell2) - divergence(k,cell1))*invLengthDc &
                   - vortDiff*invLengthDv

            ! Here the scale factor is (\delta x)^3 (precomputed)
            ! the second factor is |\nabla \omega|
            visc2 = scaleFactor*abs(vortDiff)*invLengthDc
            visc2 = min(visc2, hmixLeithVisc2max)

            tend(k,iEdge) = tend(k,iEdge) + visc2*uDiff

         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end parallel do
#endif

      call mpas_timer_stop("vel leith")

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_leith_tend!}}}

!***********************************************************************
!
!  routine ocn_vel_hmix_leith_init
!
!> \brief   Initializes ocean momentum horizontal mixing with Leith parameterization
!> \author Mark Petersen
!> \date   22 October 2012, updated April 2020
!> \details
!>  This routine initializes a variety of quantities related to
!>  Leith parameterization for horizontal momentum mixing in the ocean.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_leith_init(err)

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-----------------------------------------------------------------
      ! set defaults for module variables
      !-----------------------------------------------------------------

      err = 0

      hmixLeithOn       = .false.
      hmixLeithFactor   = 0.0
      hmixLeithVisc2max = 0.0

      !-----------------------------------------------------------------
      ! override defaults with input selections
      !-----------------------------------------------------------------

      hmixLeithOn       = config_use_leith_del2 
      hmixLeithVisc2max = config_leith_visc2_max

      !-----------------------------------------------------------------
      ! Leith viscosity is of the form:
      !   visc2 = 
      !      (hmixLeithParam*hmixLeithDx*meshScaling(iEdge)/pii)**3 &
      !      * abs(relVorticity(k,vertex2)-relVorticity(k,vertex1))*
      !       invLength_dc(iEdge) * sqrt(3.0_RKIND)
      ! We precompute several common constant factors here for better
      ! performance
      !-----------------------------------------------------------------

      hmixLeithFactor = sqrt(3.0_RKIND)* &
                        (config_leith_parameter*config_leith_dx/pii)**3

      !-----------------------------------------------------------------
      ! Perform some error checking on inputs
      !-----------------------------------------------------------------

      if (hmixLeithOn) then
         if (config_leith_parameter  == 0.0) then
            err = -1
            call mpas_log_write(&
            'ocn_vel_hmix_leith: leith parameter must be non-zero', &
            MPAS_LOG_CRIT)
         endif
         if (hmixLeithVisc2max == 0.0) then
            err = -1
            call mpas_log_write(&
            'ocn_vel_hmix_leith: invalid max viscosity (visc2max)', &
            MPAS_LOG_CRIT)
         endif
      endif

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_leith_init!}}}

!***********************************************************************

end module ocn_vel_hmix_leith

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
