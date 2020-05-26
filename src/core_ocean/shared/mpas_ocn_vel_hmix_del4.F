! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_hmix_del4
!
!> \brief Ocean horizontal mixing - biharmonic parameterization
!> \author Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date   September 2011, updated April 2020
!> \details
!>  This module contains routines and variables for computing
!>  horizontal momentum mixing tendencies using a biharmonic formulation.
!
!-----------------------------------------------------------------------

module ocn_vel_hmix_del4

   use mpas_timer
   use mpas_log
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

   public :: ocn_vel_hmix_del4_tend, &
             ocn_vel_hmix_del4_init

   !--------------------------------------------------------------------
   ! Private module variables
   !--------------------------------------------------------------------

   logical :: hmixDel4On       !< flag to determine whether del4 chosen
   real (kind=RKIND) :: visc4     !< base momentum del4 diffusion coeff
   real (kind=RKIND) :: divFactor !< factor to use for divergence term

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_vel_hmix_del4_tend
!
!> \brief   Computes tendency term for biharmonic horizontal momentum mixing
!> \author  Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date    September 2011, updated April 2020
!> \details
!>  This routine computes the horizontal mixing tendency for momentum
!>  based on a biharmonic form for the mixing.  This mixing tendency
!>  takes the form  \f$-\nu_4 \nabla^4 u\f$
!>  but is computed as
!>  \f$\nabla^2 u = \nabla divergence + k \times \nabla relativeVorticity\f$
!>  applied recursively.
!>  This formulation is only valid for constant \f$\nu_4\f$ .
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_del4_tend(divergence, relVorticity, tend, err)

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

      integer :: i,k,iEdge,iCell,iVertex               ! loop iterators
      integer :: imax, kmax, nEdges, nCells, nVertices ! loop limits
      integer :: cell1, cell2, vertex1, vertex2        ! nbr addresses

      real (kind=RKIND) :: &
         invAreaCell1, invAreaCell2, &! temp reciprocals
         invAreaTri1,  invAreaTri2,  &! temp reciprocals
         invDcEdge,    invDvEdge,    &! temp reciprocals
         del2Factor,                 &! temp for holding common factors
         delsqSum,                   &! temp for holding local sums
         viscTmp                      ! scaled visc

      real (kind=RKIND), dimension(:,:), allocatable :: &
         delsqDiv,          &! Laplacian on divergence field
         delsqRelVort,      &! Laplacian on relative vorticity
         delsqU              ! Laplacian on velocity

      ! end preamble
      !-----------------------------------------------------------------
      ! begin code

      err = 0

      if(.not.hmixDel4On) return

      call mpas_timer_start("vel del4")

      allocate(delsqDiv    (nVertLevels,nCellsAll   ), &
               delsqRelVort(nVertLevels,nVerticesAll), &
               delsqU      (nVertLevels,nEdgesAll   ))
      !$acc enter data &
      !$acc  create(delsqDiv, delsqRelVort, delsqU)

      !Compute delsq_u

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(delsqU, nEdgesHalo, &
      !$acc            cellsOnEdge, verticesOnEdge, &
      !$acc            dcEdge, dvEdge, divergence, &
      !$acc            maxLevelEdgeTop, relVorticity)   &
      !$acc    private(k, kmax, cell1, cell2, vertex1, vertex2, &
      !$acc            invDcEdge, invDvEdge)
#else
      !$omp parallel
      !$omp do schedule(runtime) &
      !$omp    private(k, kmax, cell1, cell2, vertex1, vertex2, &
      !$omp            invDcEdge, invDvEdge)
#endif
      do iEdge = 1, nEdgesHalo(2)
         delsqU(:, iEdge) = 0.0_RKIND
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)

         vertex1 = verticesOnEdge(1,iEdge)
         vertex2 = verticesOnEdge(2,iEdge)

         invDcEdge = 1.0_RKIND / dcEdge(iEdge)
         invDvEdge = 1.0_RKIND / max(dvEdge(iEdge), &
                          0.25_RKIND*dcEdge(iEdge))
         kmax = maxLevelEdgeTop(iEdge)

         do k=1,kmax
            ! Compute \nabla^2 u = \nabla divergence + 
            !                      k \times \nabla relativeVorticity
            delsqU(k, iEdge) = &
               (divergence(k,cell2) - divergence(k,cell1))*invDcEdge - &
               (relVorticity(k,vertex2) - relVorticity(k,vertex1))*invDvEdge
         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end do
#endif

      ! Compute delsq of relativeVorticity

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(delsqRelVort, delsqU,  &
      !$acc            nVerticesHalo, &
      !$acc            maxLevelVertexTop, &
      !$acc            dcEdge, areaTriangle,  &
      !$acc            vertexDegree, edgesOnVertex, &
      !$acc            edgeSignOnVertex) &
      !$acc    private(i, iEdge, k, kmax, invAreaTri1, del2Factor)
#else
      !$omp do schedule(runtime)
      !$omp    private(i, iEdge, k, kmax, invAreaTri1, del2Factor)
#endif
      do iVertex = 1, nVerticesHalo(1) ! compute out to one halo level

         delsqRelVort(:, iVertex) = 0.0_RKIND
         invAreaTri1 = 1.0_RKIND / areaTriangle(iVertex)
         kmax = maxLevelVertexTop(iVertex)
         do i = 1, vertexDegree
            iEdge = edgesOnVertex(i, iVertex)
            del2Factor = edgeSignOnVertex(i,iVertex) * &
                         dcEdge(iEdge)*invAreaTri1
            do k = 1, kmax
               delsqRelVort(k, iVertex) = &
               delsqRelVort(k, iVertex) + del2Factor*delsqU(k,iEdge)
            end do
         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end do
#endif

      ! Compute delsq_divergence

#ifdef MPAS_OPENACC
      !$acc parallel loop &
      !$acc    present(delsqDiv, delsqU,  &
      !$acc            nCellsHalo, maxLevelCell, &
      !$acc            dvEdge, areaCell, &
      !$acc            nEdgesOnCell, edgesOnCell, &
      !$acc            edgeSignOnCell) &
      !$acc    private(i, imax, iEdge, k, kmax, invAreaCell1, del2Factor)
#else
      !$omp do schedule(runtime)
      !$omp    private(i, imax, iEdge, k, kmax, invAreaCell1, del2Factor)
#endif
      do iCell = 1, nCellsHalo(1)

         delsqDiv(:, iCell) = 0.0_RKIND
         invAreaCell1 = 1.0_RKIND / areaCell(iCell)
         kmax = maxLevelCell(iCell)
         imax = nEdgesOnCell(iCell)

         do i = 1,imax
            iEdge = edgesOnCell(i, iCell)
            del2Factor = edgeSignOnCell(i, iCell) * &
                         dvEdge(iEdge)*invAreaCell1
            do k = 1, kmax
               delsqDiv(k, iCell) = &
               delsqDiv(k, iCell) - del2Factor*delsqU(k,iEdge)
            end do
         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end do
#endif

      ! Compute - \kappa \nabla^4 u
      ! as  \nabla div(\nabla^2 u) + k \times \nabla ( k \cross curl(\nabla^2 u) )

#ifdef MPAS_OPENACC
      !$acc parallel loop                          &
      !$acc    present(delsqDiv, delsqRelVort, tend,        &
      !$acc            cellsOnEdge, verticesOnEdge, &
      !$acc            maxLevelEdgeTop,                     &
      !$acc            dcEdge, dvEdge,              &
      !$acc            nEdgesOwned, meshScalingDel4)&
      !$acc    private(k, kmax, cell2, cell2, vertex1, vertex2,     &
      !$acc            invDcEdge, invDvEdge, viscTmp)
#else
      !$omp do schedule(runtime)
      !$omp    private(k, kmax, cell2, cell2, vertex1, vertex2, &
      !$omp            invDcEdge, invDvEdge, viscTmp)
#endif
      do iEdge = 1, nEdgesOwned

         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         vertex1 = verticesOnEdge(1,iEdge)
         vertex2 = verticesOnEdge(2,iEdge)
         kmax    = maxLevelEdgeTop(iEdge)

         invDcEdge = divFactor / dcEdge(iEdge)
         invDvEdge = 1.0_RKIND / dvEdge(iEdge)
         viscTmp  = visc4*meshScalingDel4(iEdge)

         do k=1,kmax
            tend(k,iEdge) = &
            tend(k,iEdge) - viscTmp*   &
                            ((delsqDiv(k,cell2) -                &
                              delsqDiv(k,cell1))*invDcEdge  -    &
                             (delsqRelVort(k,vertex2) -          &
                              delsqRelVort(k,vertex1))*invDvEdge)
         end do
      end do
#ifndef MPAS_OPENACC
      !$omp end do
      !$omp end parallel
#endif

      !$acc exit data &
      !$acc  delete(delsqDiv, delsqRelVort, delsqU)
      deallocate(delsqDiv    , &
                 delsqRelVort, &
                 delsqU        )

      call mpas_timer_stop("vel del4")

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_del4_tend !}}}

!***********************************************************************
!
!  routine ocn_vel_hmix_del4_init
!
!> \brief   Initializes ocean momentum biharmonic horizontal mixing
!> \author  Mark Petersen, Doug Jacobsen, Todd Ringler, Phil Jones
!> \date    September 2011, updated April 2020
!> \details
!>  This routine initializes a variety of quantities related to
!>  biharmonic horizontal momentum mixing in the ocean.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_hmix_del4_init(err)

   !--------------------------------------------------------------------
   ! output variables
   !--------------------------------------------------------------------

   integer, intent(out) :: err !< Output: error flag

   !{{{
   !--------------------------------------------------------------------
   ! set default values for module variables and constants
   !--------------------------------------------------------------------

   err = 0

   hmixDel4On       = .false.
   visc4            = 0.0_RKIND
   divFactor        = 1.0_RKIND

   !--------------------------------------------------------------------
   ! override any variables with choices from input configuration file
   !--------------------------------------------------------------------

   hmixDel4On       = config_use_mom_del4

   visc4            = config_mom_del4
   divFactor        = config_mom_del4_div_factor

   !--------------------------------------------------------------------
   ! Perform some error checks on input choices
   !--------------------------------------------------------------------

   if (hmixDel4On .and. visc4 <= 0.0_RKIND) then
      err = -1
      call mpas_log_write(&
         'ocn_vel_hmix_del4: invalid viscosity coeff for del4', &
         MPAS_LOG_CRIT)
   endif

   !--------------------------------------------------------------------

   end subroutine ocn_vel_hmix_del4_init!}}}

!***********************************************************************

end module ocn_vel_hmix_del4

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
