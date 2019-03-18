! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_time_integration_forward_euler
!
!> \brief MPAS ocean Forward Euler Time integration scheme
!> \author Mark Petersen, Doug Jacobsen, Todd Ringler
!> \date   September 2011
!> \details
!>  This module contains the Forward Euler time integration routine.
!
!-----------------------------------------------------------------------

module ocn_time_integration_forward_euler

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_constants
   use mpas_dmpar
   use mpas_threading
   use mpas_vector_reconstruction
   use mpas_spline_interpolation
   use mpas_timer

   use ocn_constants
   use ocn_tendency
   use ocn_diagnostics
   use ocn_gm

   use ocn_equation_of_state
   use ocn_vmix
   use ocn_time_average_coupled

   use ocn_effective_density_in_land_ice

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

   public :: ocn_time_integrator_forward_euler

   contains

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_time_integrator_forward_euler
!
!> \brief MPAS ocean Forward Euler Time integration scheme
!> \author Mark Petersen, Doug Jacobsen, Todd Ringler
!> \date   September 2011
!> \details
!>  This routine integrates one timestep (dt) using an Forward Euler time integrator.
!
!-----------------------------------------------------------------------

   subroutine ocn_time_integrator_forward_euler(domain, dt)!{{{
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Advance model state forward in time by the specified time step using
   !   4th order Runge-Kutta
   !
   ! Input: domain - current model state in time level 1 (e.g., time_levs(1)state%h(:,:))
   !                 plus mesh meta-data
   ! Output: domain - upon exit, time level 2 (e.g., time_levs(2)%state%h(:,:)) contains
   !                  model state advanced forward in time by dt seconds
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type (domain_type), intent(inout) :: domain !< Input/Output: domain information
      real (kind=RKIND), intent(in) :: dt !< Input: timestep

      type (block_type), pointer :: block

      integer, pointer :: nCells, nEdges
      integer :: iCell, iEdge, k, i, err

      type (mpas_pool_type), pointer :: tendPool
      type (mpas_pool_type), pointer :: tracersTendPool
      type (mpas_pool_type), pointer :: statePool
      type (mpas_pool_type), pointer :: tracersPool
      type (mpas_pool_type), pointer :: meshPool
      type (mpas_pool_type), pointer :: diagnosticsPool
      type (mpas_pool_type), pointer :: verticalMeshPool
      type (mpas_pool_type), pointer :: forcingPool
      type (mpas_pool_type), pointer :: scratchPool
      type (mpas_pool_type), pointer :: swForcingPool

      real (kind=RKIND), dimension(:, :), pointer :: normalVelocityNew, normalVelocityTend
      real (kind=RKIND), dimension(:, :), pointer :: layerThicknessNew, layerThicknessTend
      real (kind=RKIND), dimension(:, :), pointer :: layerThicknessCur, normalVelocityCur
      real (kind=RKIND), dimension(:, :, :), pointer :: tracersGroupCur, tracersGroupNew, tracersGroupTend
      real (kind=RKIND), dimension(:), pointer :: sshCur

      integer, dimension(:), pointer :: maxLevelCell

!      ! Diagnostics Array Pointers
      real (kind=RKIND), dimension(:,:), pointer :: layerThicknessEdge
      real (kind=RKIND), dimension(:,:), pointer :: vertAleTransportTop
      real (kind=RKIND), dimension(:,:), pointer :: normalTransportVelocity, normalGMBolusVelocity
      real (kind=RKIND), dimension(:,:), pointer :: velocityX, velocityY, velocityZ
      real (kind=RKIND), dimension(:,:), pointer :: velocityZonal, velocityMeridional
      real (kind=RKIND), dimension(:), pointer :: gradSSH
      real (kind=RKIND), dimension(:), pointer :: gradSSHX, gradSSHY, gradSSHZ
      real (kind=RKIND), dimension(:), pointer :: gradSSHZonal, gradSSHMeridional
      real (kind=RKIND), dimension(:,:), pointer :: surfaceVelocity, sshGradient

!      ! Diagnostics Indices
      integer, pointer :: indexSurfaceVelocityZonal, indexSurfaceVelocityMeridional
      integer, pointer :: indexSSHGradientZonal, indexSSHGradientMeridional

!      ! Config options
      logical, pointer :: config_prescribe_velocity, config_prescribe_thickness
      logical, pointer :: config_use_standardGM
      logical, pointer :: config_use_cvmix_kpp
      logical, pointer :: config_use_tracerGroup
      logical, pointer :: config_disable_thick_all_tend
      logical, pointer :: config_disable_vel_all_tend
      logical, pointer :: config_disable_tr_all_tend
      real (kind=RKIND), pointer :: config_mom_del4
      character (len=StrKIND), pointer :: config_land_ice_flux_mode

      type (mpas_pool_iterator_type) :: groupItr
      character (len=StrKIND) :: modifiedGroupName
      character (len=StrKIND) :: configName

      err = 0

      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_dimension(block % dimensions, 'nCells', nCells)
         call mpas_pool_get_dimension(block % dimensions, 'nEdges', nEdges)
   
         call mpas_pool_get_subpool(block % structs, 'state', statePool)
         call mpas_pool_get_subpool(block % structs, 'tend', tendPool)
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
   
         call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)
         call mpas_pool_get_subpool(tendPool, 'tracersTend', tracersTendPool)
   
         call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocityCur, 1)
         call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocityNew, 2)
         call mpas_pool_get_array(statePool, 'layerThickness', layerThicknessCur, 1)
         call mpas_pool_get_array(statePool, 'layerThickness', layerThicknessNew, 2)
   
         call mpas_pool_get_array(tendPool, 'normalVelocity', normalVelocityTend)
         call mpas_pool_get_array(tendPool, 'layerThickness', layerThicknessTend)
   
         call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
   
         call mpas_pool_get_subpool(block % structs, 'verticalMesh', verticalMeshPool)
         call mpas_pool_get_subpool(block % structs, 'diagnostics', diagnosticsPool)
         call mpas_pool_get_subpool(block % structs, 'forcing', forcingPool)
         call mpas_pool_get_subpool(block % structs, 'shortwave', swForcingPool)
         call mpas_pool_get_subpool(block % structs, 'scratch', scratchPool)
         call mpas_pool_get_array(statePool, 'ssh', sshCur, 1)
   
         call mpas_pool_get_array(diagnosticsPool, 'layerThicknessEdge', layerThicknessEdge)
         call mpas_pool_get_array(diagnosticsPool, 'vertAleTransportTop', vertAleTransportTop)
         call mpas_pool_get_array(diagnosticsPool, 'normalTransportVelocity', normalTransportVelocity)
   
         !--------------------------------------------------------------------
         ! Compute tendencies for velocity
         !--------------------------------------------------------------------
         call ocn_vert_transport_velocity_top(meshPool, verticalMeshPool, scratchPool, &
            layerThicknessCur,layerThicknessEdge, normalVelocityCur, &
            sshCur, dt, &
            vertAleTransportTop, err)
         call mpas_threading_barrier()
   
         call ocn_tend_vel(tendPool, statePool, forcingPool, diagnosticsPool, meshPool, scratchPool, 1)
         call mpas_threading_barrier()
   
         call ocn_vert_transport_velocity_top(meshPool, verticalMeshPool, scratchPool, &
            layerThicknessCur, layerThicknessEdge, normalTransportVelocity, &
            sshCur, dt, &
            vertAleTransportTop, err)
         call mpas_threading_barrier()
   
         call ocn_tend_thick(tendPool, forcingPool, diagnosticsPool, meshPool)
         call mpas_threading_barrier()
   
         call ocn_tend_tracer(tendPool, statePool, forcingPool, diagnosticsPool, meshPool, swForcingPool, &
                              scratchPool, dt, activeTracersOnlyIn=.false., timeLevelIn=1)
         call mpas_threading_barrier()
   
         !$omp do schedule(runtime) private(k)
         do iCell = 1, nCells
            do k = 1, maxLevelCell(iCell)
               layerThicknessNew(k, iCell) = layerThicknessCur(k, iCell) + dt * layerThicknessTend(k, iCell)
            end do
         end do
         !$omp end do
   
         call ocn_tend_vel(tendPool, statePool, forcingPool, diagnosticsPool, meshPool, scratchPool, 1)
   
         !$omp do schedule(runtime)
         do iEdge = 1, nEdges
            normalVelocityNew(:, iEdge) = normalVelocityCur(:, iEdge) + dt * normalVelocityTend(:, iEdge)
         end do
         !$omp end do
   
         call mpas_pool_begin_iteration(tracersPool)
         do while ( mpas_pool_get_next_member(tracersPool, groupItr) )
            if ( groupItr % memberType == MPAS_POOL_FIELD ) then
               configName = 'config_use_' // trim(groupItr % memberName)
               call mpas_pool_get_config(block % configs, configName, config_use_tracerGroup)
   
               if ( config_use_tracerGroup ) then
                  call mpas_pool_get_array(tracersPool, groupItr % memberName, tracersGroupCur, 1)
                  call mpas_pool_get_array(tracersPool, groupItr % memberName, tracersGroupNew, 2)
   
                  modifiedGroupName = trim(groupItr % memberName) // 'Tend'
                  call mpas_pool_get_array(tracersTendPool, modifiedGroupName, tracersGroupTend)
                  if ( associated(tracersGroupNew) .and. associated(tracersGroupTend) ) then
                     !$omp do schedule(runtime) private(k)
                     do iCell = 1, nCells
                        do k = 1, maxLevelCell(iCell)
                           tracersGroupNew(:, k, iCell) = tracersGroupCur(:, k, iCell) + dt &
                                                   * tracersGroupTend(:, k, iCell)
                        end do
                     end do
                     !$omp end do
                  end if
               end if
            end if
         end do

         block => block % next
      end do

      !
      !  A little clean up at the end: rescale tracer fields and compute diagnostics for new state
      !
      call mpas_timer_start("Forward Euler-cleaup phase")

      ! Rescale tracers
      block => domain % blocklist
      do while(associated(block))
        call ocn_time_integrator_forward_euler_cleanup(block, dt, err)

        block => block % next
      end do

      call mpas_timer_start("Forward Euler-implicit vert mix")
      ! Update halo on u and tracers, which were just updated for implicit vertical mixing.  If not done,
      ! this leads to lack of volume conservation.  It is required because halo updates in Forward Euler are only
      ! conducted on tendencies, not on the velocity and tracer fields.  So this update is required to
      ! communicate the change due to implicit vertical mixing across the boundary.
      call mpas_timer_start("Forward Euler-implicit vert mix halos")

      call mpas_pool_get_subpool(domain % blocklist % structs, 'state', statePool)
      call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)

      call mpas_dmpar_field_halo_exch(domain, 'normalVelocity', timeLevel=2)

      call mpas_pool_begin_iteration(tracersPool)
      do while ( mpas_pool_get_next_member(tracersPool, groupItr) )
         if ( groupItr % memberType == MPAS_POOL_FIELD ) then
            call mpas_dmpar_field_halo_exch(domain, groupItr % memberName, timeLevel=2)
         end if
      end do

      call mpas_timer_stop("Forward Euler-implicit vert mix halos")

      call mpas_timer_stop("Forward Euler-implicit vert mix")
      call mpas_threading_barrier()

      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_subpool(block % structs, 'state', statePool)
         call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)
         call mpas_pool_get_subpool(block % structs, 'forcing', forcingPool)
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
         call mpas_pool_get_subpool(block % structs, 'diagnostics', diagnosticsPool)
         call mpas_pool_get_subpool(block % structs, 'scratch', scratchPool)

         call mpas_pool_get_dimension(meshPool, 'nCells', nCells)
         call mpas_pool_get_dimension(meshPool, 'nEdges', nEdges)

         call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocityCur, 1)
         call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocityNew, 2)
         call mpas_pool_get_array(statePool, 'layerThickness', layerThicknessCur, 1)
         call mpas_pool_get_array(statePool, 'layerThickness', layerThicknessNew, 2)

         call mpas_pool_get_dimension(diagnosticsPool, 'index_surfaceVelocityZonal', indexSurfaceVelocityZonal)
         call mpas_pool_get_dimension(diagnosticsPool, 'index_surfaceVelocityMeridional', indexSurfaceVelocityMeridional)
         call mpas_pool_get_dimension(diagnosticsPool, 'index_SSHGradientZonal', indexSSHGradientZonal)
         call mpas_pool_get_dimension(diagnosticsPool, 'index_SSHGradientMeridional', indexSSHGradientMeridional)

         call mpas_pool_get_array(diagnosticsPool, 'normalTransportVelocity', normalTransportVelocity)
         call mpas_pool_get_array(diagnosticsPool, 'normalGMBolusVelocity', normalGMBolusVelocity)
         call mpas_pool_get_array(diagnosticsPool, 'velocityX', velocityX)
         call mpas_pool_get_array(diagnosticsPool, 'velocityY', velocityY)
         call mpas_pool_get_array(diagnosticsPool, 'velocityZ', velocityZ)
         call mpas_pool_get_array(diagnosticsPool, 'velocityZonal', velocityZonal)
         call mpas_pool_get_array(diagnosticsPool, 'velocityMeridional', velocityMeridional)
         call mpas_pool_get_array(diagnosticsPool, 'gradSSH', gradSSH)
         call mpas_pool_get_array(diagnosticsPool, 'gradSSHX', gradSSHX)
         call mpas_pool_get_array(diagnosticsPool, 'gradSSHY', gradSSHY)
         call mpas_pool_get_array(diagnosticsPool, 'gradSSHZ', gradSSHZ)
         call mpas_pool_get_array(diagnosticsPool, 'gradSSHZonal', gradSSHZonal)
         call mpas_pool_get_array(diagnosticsPool, 'gradSSHMeridional', gradSSHMeridional)
         call mpas_pool_get_array(diagnosticsPool, 'surfaceVelocity', surfaceVelocity)
         call mpas_pool_get_array(diagnosticsPool, 'SSHGradient', SSHGradient)

         call mpas_pool_get_config(block % configs, 'config_prescribe_velocity', config_prescribe_velocity)
         if (config_prescribe_velocity) then
            !$omp do schedule(runtime)
            do iEdge = 1, nEdges
               normalVelocityNew(:, iEdge) = normalVelocityCur(:, iEdge)
            end do
            !$omp end do
         end if

         call mpas_pool_get_config(block % configs, 'config_prescribe_thickness', config_prescribe_thickness)
         if (config_prescribe_thickness) then
            !$omp do schedule(runtime)
            do iCell = 1, nCells
               layerThicknessNew(:, iCell) = layerThicknessCur(:, iCell)
            end do
            !$omp end do
         end if

         call ocn_diagnostic_solve(dt, statePool, forcingPool, meshPool, diagnosticsPool, scratchPool, tracersPool, 2)
         call mpas_threading_barrier()

         ! Update the effective desnity in land ice if we're coupling to land ice
         call ocn_effective_density_in_land_ice_update(meshPool, forcingPool, statePool, scratchPool, err)

         ! ------------------------------------------------------------------
         ! Accumulating various parameterizations of the transport velocity
         ! ------------------------------------------------------------------
         !$omp do schedule(runtime)
         do iEdge = 1, nEdges
            normalTransportVelocity(:, iEdge) = normalVelocityNew(:, iEdge)
         end do
         !$omp end do
         call mpas_threading_barrier()

         ! Compute normalGMBolusVelocity and the tracer transport velocity
         call mpas_pool_get_config(block % configs, 'config_use_standardGM', config_use_standardGM)
         if (config_use_standardGM) then
             call ocn_gm_compute_Bolus_velocity(diagnosticsPool, meshPool, scratchPool)
         end if
         call mpas_threading_barrier()

         if (config_use_standardGM) then
            !$omp do schedule(runtime)
            do iEdge = 1, nEdges
               normalTransportVelocity(:, iEdge) = normalTransportVelocity(:, iEdge) + normalGMBolusVelocity(:, iEdge)
            end do
            !$omp end do
         end if
         ! ------------------------------------------------------------------
         ! End: Accumulating various parameterizations of the transport velocity
         ! ------------------------------------------------------------------

         call mpas_reconstruct(meshPool,  normalVelocityNew, &
                          velocityX, velocityY, velocityZ,   &
                          velocityZonal, velocityMeridional, &
                          includeHalos = .true.)

         call mpas_reconstruct(meshPool, gradSSH,          &
                          gradSSHX, gradSSHY, gradSSHZ,    &
                          gradSSHZonal, gradSSHMeridional, &
                          includeHalos = .true.)
         call mpas_threading_barrier()

         !$omp do schedule(runtime)
         do iCell = 1, nCells
            surfaceVelocity(indexSurfaceVelocityZonal, iCell) = velocityZonal(1, iCell)
            surfaceVelocity(indexSurfaceVelocityMeridional, iCell) = velocityMeridional(1, iCell)

            SSHGradient(indexSSHGradientZonal, iCell) = gradSSHZonal(iCell)
            SSHGradient(indexSSHGradientMeridional, iCell) = gradSSHMeridional(iCell)
         end do
         !$omp end do

         call ocn_time_average_coupled_accumulate(diagnosticsPool, statePool, forcingPool, 2)

         if (config_use_standardGM) then
            call ocn_reconstruct_gm_vectors(diagnosticsPool, meshPool)
         end if
         call mpas_threading_barrier()

         block => block % next
      end do

      call mpas_timer_stop("Forward Euler-cleaup phase")

      call mpas_threading_barrier()

   end subroutine ocn_time_integrator_forward_euler!}}}

   subroutine ocn_time_integrator_forward_euler_cleanup(block, dt, err)!{{{
      type (block_type), intent(in) :: block
      real (kind=RKIND), intent(in) :: dt
      integer, intent(out) :: err

      integer, pointer :: nCells, nEdges, indexTemperature, indexSalinity
      integer :: iCell, iEdge, k

      type (mpas_pool_type), pointer :: statePool, meshPool, forcingPool
      type (mpas_pool_type), pointer :: diagnosticsPool, scratchPool
      type (mpas_pool_type), pointer :: tracersPool

      real (kind=RKIND), dimension(:, :), pointer :: layerThicknessNew, normalVelocityNew
      real (kind=RKIND), dimension(:, :), pointer :: normalTransportVelocity, normalGMBolusVelocity
      real (kind=RKIND), dimension(:, :, :), pointer :: tracersGroupNew, tracersGroupCur

      integer, dimension(:), pointer :: maxLevelCell

      logical, pointer :: config_use_tracerGroup
      type (mpas_pool_iterator_type) :: groupItr
      character (len=StrKIND) :: modifiedGroupName
      character (len=StrKIND) :: configName

      logical, pointer :: config_use_standardGM

      err = 0

      call mpas_pool_get_config(block % configs, 'config_use_standardGM', config_use_standardGM)

      call mpas_pool_get_dimension(block % dimensions, 'nCells', nCells)
      call mpas_pool_get_dimension(block % dimensions, 'nEdges', nEdges)

      call mpas_pool_get_subpool(block % structs, 'state', statePool)
      call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
      call mpas_pool_get_subpool(block % structs, 'forcing', forcingPool)
      call mpas_pool_get_subpool(block % structs, 'diagnostics', diagnosticsPool)
      call mpas_pool_get_subpool(block % structs, 'scratch', scratchPool)

      call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)

      call mpas_pool_get_array(statePool, 'layerThickness', layerThicknessNew, 2)
      call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocityNew, 2)

      call mpas_pool_get_array(diagnosticsPool, 'normalTransportVelocity', normalTransportVelocity)
      call mpas_pool_get_array(diagnosticsPool, 'normalGMBolusVelocity', normalGMBolusVelocity)

      call mpas_pool_get_dimension(tracersPool, 'index_temperature', indexTemperature)
      call mpas_pool_get_dimension(tracersPool, 'index_salinity', indexSalinity)

      call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)

      call mpas_pool_begin_iteration(tracersPool)
      do while ( mpas_pool_get_next_member(tracersPool, groupItr) )
         if ( groupItr % memberType == MPAS_POOL_FIELD ) then
            call mpas_pool_get_array(tracersPool, groupItr % memberName, tracersGroupNew, 2)
            if ( associated(tracersGroupNew) ) then
               !$omp do schedule(runtime) private(k)
               do iCell = 1, nCells
                 do k = 1, maxLevelCell(iCell)
                   tracersGroupNew(:, k, iCell) = tracersGroupNew(:, k, iCell) / layerThicknessNew(k, iCell)
                 end do
               end do
               !$omp end do
            end if
         end if
      end do

      call ocn_diagnostic_solve(dt, statePool, forcingPool, meshPool, diagnosticsPool, scratchPool, tracersPool, 2)
      call mpas_threading_barrier()

      call ocn_vmix_implicit(dt, meshPool, diagnosticsPool, statePool, forcingPool, scratchPool, err, 2)
      call mpas_threading_barrier()

   end subroutine ocn_time_integrator_forward_euler_cleanup!}}}

end module ocn_time_integration_forward_euler

! vim: foldmethod=marker
