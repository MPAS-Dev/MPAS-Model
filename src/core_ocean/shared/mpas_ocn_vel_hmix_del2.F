! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_hmix_del2
!
!> \brief Ocean horizontal mixing - Laplacian parameterization
!> \author Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date   September 2011, updated April 2020
!> \details
!>  This module contains routines for computing horizontal mixing
!>  tendencies using a Laplacian formulation.
!
!-----------------------------------------------------------------------

module ocn_vel_hmix_del2

   use mpas_timer
   use mpas_vector_operations
   use mpas_matrix_operations
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

   public :: ocn_vel_hmix_del2_tend, &
             ocn_vel_hmix_del2_init

   !-------------------------------------------------------------------
   ! Private module variables
   !--------------------------------------------------------------------

   logical :: hmixDel2On       !< flag to determine whether del2 chosen
   real (kind=RKIND) :: visc2  !< del2 viscosity coefficient

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_vel_hmix_del2_tend
!
!> \brief   Computes tendency term for Laplacian horizontal momentum mixing
!> \author  Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date    22 August 2011, updated April 2020
!> \details
!>  This routine computes the horizontal mixing tendency for momentum
!>  based on a Laplacian form for the mixing, \f$\nu_2 \nabla^2 u\f$
!>  This tendency takes the
!>  form \f$\nu( \nabla divergence + k \times \nabla relativeVorticity )\f$,
!>  where \f$\nu\f$ is a viscosity and \f$k\f$ is the vertical unit vector.
!>  This form is strictly only valid for constant \f$\nu\f$ .
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_del2_tend(divergence, relVorticity, tend, err)

      !-----------------------------------------------------------------
      ! input variables
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         divergence      !< [in] velocity divergence

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         relVorticity    !< [in] relative vorticity

      !-----------------------------------------------------------------
      ! input /output variables
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

      integer :: &
         iEdge, k,       &! edge and verical loop indices
         cell1, cell2,   &! neighbor cell   indices across edge
         vertex1, vertex2 ! neighbor vertex indices across edge

      real (kind=RKIND) :: &
         u_diffusion,      &!
         invDcEdge,        &! inverse length between centers
         invDvEdge,        &! inverse length between vertices 
         viscDel2           ! scaled viscosity coefficient

      ! End of preamble
      !----------------
      ! Begin code

      !-----------------------------------------------------------------
      ! exit if this mixing is not selected
      !-----------------------------------------------------------------

      err = 0

      if (.not. hmixDel2On) return

      call mpas_timer_start("vel del2")

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(nEdgesOwned, maxLevelEdgeTop, &
      !$acc            cellsOnEdge, verticesOnEdge, &
      !$acc            dcEdge, dvEdge, meshScalingDel2, &
      !$acc            divergence, relVorticity, tend)   &
      !$acc    private(cell1, cell2, vertex1, vertex2, k, invDcEdge, &
      !$acc            invDvEdge, u_diffusion, viscDel2)
#else
      !$omp parallel
      !$omp do schedule(runtime) &
      !$omp    private(cell1, cell2, vertex1, vertex2, k, invDcEdge, &
      !$omp            invDvEdge, u_diffusion, viscDel2)
#endif
      do iEdge = 1, nEdgesOwned
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         vertex1 = verticesOnEdge(1,iEdge)
         vertex2 = verticesOnEdge(2,iEdge)

         invDcEdge = 1.0_RKIND / dcEdge(iEdge)
         invDvEdge = 1.0_RKIND / dvEdge(iEdge)

         viscDel2 =  visc2 * meshScalingDel2(iEdge)

         do k = 1, maxLevelEdgeTop(iEdge)

            ! Here -( relVorticity(k,vertex2) - 
            !         relVorticity(k,vertex1) )/ dvEdge(iEdge)
            ! is - \nabla relVorticity pointing from vertex 2 to 
            !       vertex 1, or equivalently
            !    + k \times \nabla relVorticity pointing from 
            !       cell1 to cell2.

            u_diffusion = & 
               (divergence(k,cell2) - divergence(k,cell1))*invDcEdge - &
               (relVorticity(k,vertex2) - relVorticity(k,vertex1))*invDvEdge

            tend(k,iEdge) = tend(k,iEdge) + viscDel2*u_diffusion

         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end do
      !$omp end parallel
#endif

      call mpas_timer_stop("vel del2")

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_del2_tend!}}}

!***********************************************************************
!
!  routine ocn_vel_hmix_del2_init
!
!> \brief   Initializes ocean momentum Laplacian horizontal mixing
!> \author  Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date    September 2011, updated April 2020
!> \details
!>  This routine initializes a variety of quantities related to
!>  Laplacian horizontal momentum mixing in the ocean.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_del2_init(err)

   !--------------------------------------------------------------------
   ! Output variables
   !--------------------------------------------------------------------

   integer, intent(out) :: err !< [out] error flag

   !{{{
   !--------------------------------------------------------------------
   ! local variables
   !--------------------------------------------------------------------

   ! None

   ! End preamble
   !-------------
   ! Begin code

   !--------------------------------------------------------------------
   ! set default values for module variables and constants
   !--------------------------------------------------------------------

   err = 0

   hmixDel2On       = .false.
   visc2            = 0.0_RKIND

   !--------------------------------------------------------------------
   ! override defaults with input choices
   !--------------------------------------------------------------------

   hmixDel2On       = config_use_mom_del2
   visc2            = config_mom_del2

   !--------------------------------------------------------------------
   ! perform some error checks on inputs
   !--------------------------------------------------------------------

   if (hmixDel2On .and. visc2 <= 0.0_RKIND) then
      err = -1
      call mpas_log_write(&
         'ocn_vel_hmix_del2: invalid viscosity coeff for del2', &
         MPAS_LOG_CRIT)
   endif

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_del2_init!}}}

!***********************************************************************

end module ocn_vel_hmix_del2

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
