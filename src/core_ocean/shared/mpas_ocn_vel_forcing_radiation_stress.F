! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_forcing_radiation_stress
!
!> \brief  MPAS ocean wave radiation stress
!> \author Steven Brus 
!> \date   April 2019
!> \details
!>  This module contains the routine for computing
!>  tendencies from wave radiation stress.
!
!-----------------------------------------------------------------------

module ocn_vel_forcing_radiation_stress

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_timer

   use ocn_constants
   use ocn_forcing

   implicit none
   private
   save

   !--------------------------------------------------------------------
   !
   ! Public parameters
   !
   !--------------------------------------------------------------------

   !--------------------------------------------------------------------
   !
   ! Public member functions
   !
   !--------------------------------------------------------------------

   public :: ocn_vel_forcing_radiation_stress_tend, &
             ocn_vel_forcing_radiation_stress_init

   !--------------------------------------------------------------------
   !
   ! Private module variables
   !
   !--------------------------------------------------------------------

   logical :: radiationStressOn
   real (kind=RKIND), dimension(:), allocatable :: normalRadiationStress

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_vel_forcing_radiation_stress_tend
!
!> \brief   Computes tendency term from wave radiation stress
!> \author  Steven Brus 
!> \date    April 2019
!> \details
!>  This routine computes the wave radiation stress tendency for momentum
!>  based on current state.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_forcing_radiation_stress_tend(meshPool, & 
                                                    diagnosticsPool, &
                                                    waveRadiationStressXX, &
                                                    waveRadiationStressXY, &
                                                    waveRadiationStressYY, & 
                                                    layerThicknessEdge, &
                                                    tend, err) !{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         waveRadiationStressXX, &
         waveRadiationStressXY, &
         waveRadiationStressYY

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         layerThicknessEdge     !< Input: thickness at edge

      type (mpas_pool_type), intent(in) :: &
         meshPool, &          !< Input: mesh information
         diagnosticsPool      !< Input: Diagnostic information

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(inout) :: &
         tend          !< Input/Output: velocity tendency

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      integer, dimension(:), pointer :: nEdgesArray
      integer, dimension(:), pointer :: maxLevelEdgeTop
      integer, dimension(:,:), pointer :: edgeMask
      real (kind=RKIND), pointer :: daysSinceStartOfSim 

      integer :: i,j,k,l
      integer :: iEdge, iVertex, iCell 
      integer :: nEdges, vertex, cell
      real (kind=RKIND) :: waterColumnHeight 
      real (kind=RKIND) :: ramp,forcing_ramp

      !-----------------------------------------------------------------
      !
      ! call relevant routines for computing tendencies
      ! note that the user can choose multiple options and the
      !   tendencies will be added together
      !
      !-----------------------------------------------------------------

      err = 0
      if ( .not. radiationStressOn ) return

      call mpas_timer_start('vel surface stress')

      call mpas_pool_get_dimension(meshPool, 'nEdgesArray', nEdgesArray)
      call mpas_pool_get_array(meshPool, 'maxLevelEdgeTop', maxLevelEdgeTop)
      call mpas_pool_get_array(meshPool, 'edgeMask', edgeMask)
      call MPAS_pool_get_array(diagnosticsPool, "daysSinceStartOfSim",daysSinceStartOfSim)

      ! Compute radiation stress forcing
      ! (right now, this is only called first timestep, 
      ! when module variable normalRadiationStress hasn't been allocated yet.)
      call compute_radiation_stress_gradient(meshPool, &
                                             diagnosticsPool, &
                                             waveRadiationStressXX, &
                                             waveRadiationStressXY, &
                                             waveRadiationStressYY, &
                                             normalRadiationStress)

      nEdges = nEdgesArray ( 1 )

      ! Compute ramp factor for radiation stress forcing
      forcing_ramp = 1.0_RKIND
      ramp = tanh((2.0_RKIND*daysSinceStartOfSim)/forcing_ramp)

edgeLoop: do iEdge = 1, nEdges

        ! Compute water column height at edge
        waterColumnHeight = 0.0_RKIND 
        do k = 1, maxLevelEdgeTop(iEdge)
          waterColumnHeight = waterColumnHeight + layerThicknessEdge(k,iEdge)
        enddo

        ! Sum into tendency 
        do k = 1, maxLevelEdgeTop(iEdge)
           tend(k,iEdge) =  tend(k,iEdge) - ramp * edgeMask(k, iEdge) * normalRadiationStress(iEdge) &
                         / rho_sw / waterColumnHeight
        enddo

      enddo edgeLoop ! iEdge

      call mpas_timer_stop('vel surface stress')

   !--------------------------------------------------------------------

   end subroutine ocn_vel_forcing_radiation_stress_tend!}}}

!***********************************************************************
!
!  routine  compute_radiation_stress_gradient 
!
!> \brief   Computes wave radiation stress gradients
!> \author  Steven Brus
!> \date    April 2019 
!> \details
!>  This routine computes radiation stress gradients on edges from 
!>  cell-centered radiation stresses
!
!-----------------------------------------------------------------------

   subroutine compute_radiation_stress_gradient(meshPool, &
                                                diagnosticsPool, &
                                                waveRadiationStressXX, &
                                                waveRadiationStressXY, &
                                                waveRadiationStressYY, &
                                                normalRadiationStress) !{{{

   implicit none

   
      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         waveRadiationStressXX, &
         waveRadiationStressXY, &
         waveRadiationStressYY

      type (mpas_pool_type), intent(in) :: &
         meshPool, &          !< Input: mesh information
         diagnosticsPool      !< Input: Diagnostic information

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), allocatable, intent(inout) :: normalRadiationStress

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      integer, pointer :: nCells
      integer, dimension(:), pointer :: nEdgesArray
      integer, dimension(:,:), pointer :: verticesOnEdge, cellsOnVertex, cellsOnEdge
      real (kind=RKIND), dimension(:), pointer :: lonCell, latCell
      real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
      real (kind=RKIND), dimension(:), pointer :: xVertex, yVertex, zVertex
      real (kind=RKIND), dimension(:), pointer :: waveRadiationStressXXdx
      real (kind=RKIND), dimension(:,:), pointer :: edgeNormalVectors
      logical, pointer :: on_a_sphere

      integer :: i,j,k,l
      integer :: iEdge, iVertex, iCell 
      integer :: nEdges, vertex, cell
      integer :: boundaryVertex
      real (kind=RKIND), parameter :: sphereRadius = 6371229.0_RKIND
      real (kind=RKIND) :: radiationStressTensor(3,3,3)
      real (kind=RKIND) :: dxdlon(3), dxdlat(3)
      real (kind=RKIND) :: dpdr(3,3), dxdr(3,3), drdx(3,3), detJ
      real (kind=RKIND) :: dpdx(3,3)
      real (kind=RKIND) :: x1, x2, x3
      real (kind=RKIND) :: y1, y2, y3
      real (kind=RKIND) :: z1, z2, z3
      real (kind=RKIND) :: lat, lon
      real (kind=RKIND) :: tauAvgDual(3,2), tauAvgEdge(3)
      real (kind=RKIND) :: areaWeights(2)

      !-----------------------------------------------------------------
      !
      ! call relevant routines for computing tendencies
      ! note that the user can choose multiple options and the
      !   tendencies will be added together
      !
      !-----------------------------------------------------------------

      call mpas_pool_get_dimension(meshPool, 'nEdgesArray', nEdgesArray)

      nEdges = nEdgesArray ( 1 )
      if ( allocated(normalRadiationStress)) then
        return
      else
        allocate(normalRadiationStress(nEdges))
      end if
 
      call mpas_pool_get_dimension(meshPool, 'nCells', nCells)
      call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)
      call mpas_pool_get_array(meshPool, 'cellsOnVertex', cellsOnVertex)
      call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)
      call mpas_pool_get_config(meshPool, 'on_a_sphere', on_a_sphere)
      call mpas_pool_get_array(meshPool, 'lonCell', lonCell )
      call mpas_pool_get_array(meshPool, 'latCell', latCell )
      call mpas_pool_get_array(meshPool, 'xCell', xCell )
      call mpas_pool_get_array(meshPool, 'yCell', yCell )
      call mpas_pool_get_array(meshPool, 'zCell', zCell )
      call mpas_pool_get_array(meshPool, 'xVertex', xVertex )
      call mpas_pool_get_array(meshPool, 'yVertex', yVertex )
      call mpas_pool_get_array(meshPool, 'zVertex', zVertex )
      call mpas_pool_get_array(meshPool, 'edgeNormalVectors', edgeNormalVectors)
      call mpas_pool_get_array(diagnosticsPool, 'waveRadiationStressXXdx',waveRadiationStressXXdx, 1 )


      ! Basis function derivatives with respect to local coordinates
      dpdr(1,1) = -0.5_RKIND ; dpdr(1,2) = 0.5_RKIND ; dpdr(1,3) = 0.0_RKIND 
      dpdr(2,1) = -0.5_RKIND ; dpdr(2,2) = 0.0_RKIND ; dpdr(2,3) = 0.5_RKIND

edgeLoop: do iEdge = 1, nEdges
        tauAvgDual = 0.0_RKIND

        if (cellsOnEdge(2,iEdge) == nCells+1) then
           cycle edgeLoop
         end if
 
         boundaryVertex = 0
vertexLoop: do iVertex = 1,2
          vertex = verticesOnEdge(iVertex,iEdge)
          do iCell = 1,3
            cell = cellsOnVertex(iCell,vertex)
           
            if (cell == nCells+1) then
              boundaryVertex = iVertex
              cycle vertexLoop
            end if

            if (on_a_sphere) then
              lon = lonCell(cell)
              lat = latCell(cell)
      
              ! Form rows of Jacobian matrix bewteen Cartesian and sperical coordinates (dxdrad not needed)
              dxdlon(1) = -sphereRadius*cos(lat)*sin(lon)
              dxdlon(2) =  sphereRadius*cos(lat)*cos(lat)
              dxdlon(3) =  0.0_RKIND

              dxdlat(1) = -sphereRadius*sin(lat)*cos(lon)
              dxdlat(2) = -sphereRadius*sin(lat)*sin(lon)
              dxdlat(3) =  sphereRadius*cos(lat)

              ! Transform radiation stress tensor to Cartesian coordinates
              do j = 1,3
                do i = 1,3
                  radiationStressTensor(i,j,iCell) = dxdlon(j)*(dxdlon(i)*waveRadiationStressXX(cell)  + &
                                                                dxdlat(i)*waveRadiationStressXY(cell)) + &
                                                     dxdlat(j)*(dxdlon(i)*waveRadiationStressXY(cell)  + &
                                                                dxdlat(i)*waveRadiationStressYY(cell))
                enddo
              enddo
            else              
              radiationStressTensor(1,1,iCell) = waveRadiationStressXX(cell)
              radiationStressTensor(1,2,iCell) = waveRadiationStressXY(cell)
              radiationStressTensor(1,3,iCell) = 0.0_RKIND 
              radiationStressTensor(2,1,iCell) = waveRadiationStressXY(cell)
              radiationStressTensor(2,2,iCell) = waveRadiationStressYY(cell)
              radiationStressTensor(2,3,iCell) = 0.0_RKIND
              radiationStressTensor(3,1,iCell) = 0.0_RKIND
              radiationStressTensor(3,2,iCell) = 0.0_RKIND
              radiationStressTensor(3,3,iCell) = 0.0_RKIND
            endif

          enddo ! iCell

          ! Get coordinates of dual-grid triangle
          x1 = xCell(cellsOnVertex(1,vertex))
          y1 = yCell(cellsOnVertex(1,vertex))
          z1 = zCell(cellsOnVertex(1,vertex))

          x2 = xCell(cellsOnVertex(2,vertex))
          y2 = yCell(cellsOnVertex(2,vertex))
          z2 = zCell(cellsOnVertex(2,vertex))

          x3 = xCell(cellsOnVertex(3,vertex))
          y3 = yCell(cellsOnVertex(3,vertex)) 
          z3 = zCell(cellsOnVertex(3,vertex))

          ! Avoid Jacobian singularity for planar meshes
          if (abs(z1) < 1e-10_RKIND .and. abs(z2) < 1e-10_RKIND .and. abs(z3) < 1e-10_RKIND) then
            z1 = 1.0_RKIND
            z2 = 1.0_RKIND
            z3 = 1.0_RKIND
          endif

          ! Compute derivatives of interpolating functions on dual-grid triangle
          detJ = x1*(y2*z3-y3*z2)-y1*(x2*z3-x3*z2)+z1*(x2*y3-x3*y2)
          dpdx(1,1) = (y2*z3-y3*z2)/detJ; dpdx(1,2) = (z1*y3-z3*y1)/detJ; dpdx(1,3) = (y1*z2-z1*y2)/detJ
          dpdx(2,1) = (x3*z2-x2*z3)/detJ; dpdx(2,2) = (x1*z3-x3*z1)/detJ; dpdx(2,3) = (z1*x2-x1*z2)/detJ
          dpdx(3,1) = (x2*y3-x3*y2)/detJ; dpdx(3,2) = (y1*x3-y3*x1)/detJ; dpdx(3,3) = (x1*y2-y1*x2)/detJ

          ! Compute average wave radiation stress divergence on dual mesh triangle 
          do i = 1,3
            do l = 1,3
              do j = 1,3 
                tauAvgDual(i,iVertex) = tauAvgDual(i,iVertex) + radiationStressTensor(i,j,l)*dpdx(j,l)
              enddo
            enddo
          enddo

          ! Compute area weights for calculating area average edge values
          ! (Probably not needed, areas should be the same)
          x1 = xVertex(vertex)
          y1 = yVertex(vertex)
          z1 = zVertex(vertex)

          x2 = xCell(cellsOnEdge(1,iEdge))
          y2 = yCell(cellsOnEdge(1,iEdge))
          z2 = zCell(cellsOnEdge(1,iEdge))

          x3 = xCell(cellsOnEdge(2,iEdge))
          y3 = yCell(cellsOnEdge(2,iEdge))
          z3 = zCell(cellsOnEdge(2,iEdge))

          areaWeights(iVertex) = 0.5_RKIND*(sqrt(((y2-y1)*(z3-z1)-(y3-y1)*(z2-z1))**2+ &
                                                ((x3-x1)*(z2-z1)-(x2-x1)*(z3-z1))**2+ &
                                                ((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1))**2))

        enddo vertexLoop

        ! Set boundary vertex = to interior vertex on edge
        ! (could probably do something better)
        if (boundaryVertex > 0) then
          do i = 1,3
            tauAvgDual(i,boundaryVertex) = tauAvgDual(i,mod(boundaryVertex,2)+1)
          enddo
        endif

        ! Compute area agerage edge value        
        do i = 1,3
          tauAvgEdge(i) = (tauAvgDual(i,1)*areaWeights(1) + tauAvgDual(i,2)*areaWeights(2))/(areaWeights(1) + areaWeights(2))
        enddo

        waveRadiationStressXXdx(iEdge) = tauAvgEdge(1)
 
        ! Compute dot product of average edge value with edge normal
        normalRadiationStress(iEdge) = 0.0_RKIND
        do i = 1,3
          normalRadiationStress(iEdge) = normalRadiationStress(iEdge) + tauAvgEdge(i)*edgeNormalVectors(i,iEdge)
        enddo

      enddo edgeLoop ! iEdge


   !--------------------------------------------------------------------


   end subroutine compute_radiation_stress_gradient!}}}

!***********************************************************************
!
!  routine ocn_vel_forcing_radiation_stress_init
!
!> \brief   Initializes ocean wave radiation stress forcing
!> \author  Steven Brus
!> \date    April 2019 
!> \details
!>  This routine initializes quantities related to wave radiation stress
!>  in the ocean.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_forcing_radiation_stress_init(err)!{{{

   !--------------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! call individual init routines for each parameterization
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      logical, pointer :: config_use_wave_radiation_stress_forcing

      call mpas_pool_get_config(ocnConfigs, 'config_use_wave_radiation_stress_forcing', config_use_wave_radiation_stress_forcing)

      radiationStressOn = .false.
      if(config_use_wave_radiation_stress_forcing) radiationStressOn = .true.

      err = 0

   !--------------------------------------------------------------------

   end subroutine ocn_vel_forcing_radiation_stress_init!}}}

!***********************************************************************

end module ocn_vel_forcing_radiation_stress

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
