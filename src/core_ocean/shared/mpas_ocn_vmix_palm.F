!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vmix_LES
!
!> \brief MPAS ocean vertical mixing interface to PALM les 
!> \author Luke Van Roekel
!> \date   09 January 2019
!> \details
!>  This module contains the routines for calls into LES
!>
!
!-----------------------------------------------------------------------

module ocn_vmix_LES

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_timer
   use mpas_constants
   use mpas_log
   use mpas_vector_reconstruction

   use ocn_constants
   use ocn_surface_bulk_forcing
   use ocn_surface_land_ice_fluxes

   implicit none
   private
   save

   !--------------------------------------------------------------------
   !
   ! Public parameters
   !
   !-------------------------------------------------------------------   

   integer, parameter :: num_water_types = 5
   real (kind=RKIND), dimension(num_water_types) ::                       &
      rfac   = (/ 0.58_RKIND, 0.62_RKIND, 0.67_RKIND, 0.77_RKIND, 0.78_RKIND /), &
      depth1 = (/ 0.35_RKIND, 0.60_RKIND, 1.00_RKIND, 1.50_RKIND, 1.40_RKIND /), &
      depth2 = (/ 23.0_RKIND, 20.0_RKIND, 17.0_RKIND, 14.0_RKIND, 7.90_RKIND /)

   !--------------------------------------------------------------------
   !
   ! Public member functions
   !
   !--------------------------------------------------------------------

   public :: ocn_vmix_LES_interface, &
             ocn_vmix_LES_apply_tend

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_vmix_LES_interface
!
!> \brief   Compute mixing tendency due to LES 
!> \author  Luke Van Roekel
!> \date    09 January 2019
!> \details
!>  This routine computes the sub grid mixing tendencies
!
!-----------------------------------------------------------------------

   subroutine ocn_vmix_LES_interface(meshPool, statePool, forcingPool, diagnosticsPool, dt, err, timeLevelIn)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      type (mpas_pool_type), intent(in) :: &
         meshPool          !< Input: mesh information

      integer, intent(in), optional :: timeLevelIn !< Input: time level for state pool

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (mpas_pool_type), intent(inout) :: &
         statePool         !< Input/Output: state information

      type (mpas_pool_type), intent(inout) :: &
         diagnosticsPool   !< Input/Output: diagnostic information

      type (mpas_pool_type), intent(inout) :: &
         forcingPool   !< Input/Output: forcing information

      type (mpas_pool_type), pointer :: tracersPool, tracersSurfaceFluxPool

      real(KIND=RKIND), intent(in) :: dt

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

      integer, dimension(:), pointer :: maxLevelCell

      real (kind=RKIND), dimension(:), pointer :: &
        stressX, stressY, stressZ, stressZonal, stressMeridional, fCell, &
        surfaceStress, surfaceStressMagnitude,   &
        penetrativeTemperatureFlux, angleEdge, surfaceThicknessFlux, &
        surfaceThicknessFluxRunoff

      real (kind=RKIND), dimension(:,:), pointer :: &
        layerThickness, velocityZonal, velocityMeridional, &
        normalVelocity, uIncrementLES, vIncrementLES, tIncrementLES, &
        sIncrementLES, normalVelocityIncrementLES,     &
        tracerGroupSurfaceFlux, tracerGroupSurfaceFluxRunoff, &
        tracerGroupSurfaceFluxRemoved

      real (kind=RKIND), dimension(:,:,:), pointer :: activeTracers
      integer, pointer :: indexTemperature, indexSalinity, config_jerlov_water_type
      integer, dimension(:,:), pointer :: cellsOnEdge

      integer :: k, i, iCell, timeLevel, iEdge, nCells, nEdges
      integer :: nLevs, edgeCount, cell1, cell2
      integer, pointer :: nVertLevels
      integer, dimension(:), pointer :: nCellsArray, nEdgesArray

      logical, pointer :: config_use_les_for_vmix, config_use_ecosysTracers
      real (kind=RKIND), pointer :: config_les_dz, config_les_nz, config_les_dt_data_output,   &
                                    config_les_dt_disturb, config_les_dt_data_output_av,       &
                                    config_les_dt_dopr, config_les_end_time,                   &
                                    config_les_dt_dots, config_flux_attenuation_coefficient,   &
                                    config_flux_attenuation_coefficient_runoff

      real (kind=RKIND), dimension(:), allocatable :: zonalAverage, meridionalAverage 
      real (kind=RKIND) :: fac, dep1, dep2, fracAbsorbed, fracAbsorbedRunoff,  &
                           wt_sfc, uw_sfc, vw_sfc, ws_sfc

      err=0

      if (present(timeLevelIn)) then
         timeLevel = timeLevelIn
      else
         timeLevel = 2 
      end if

      call mpas_pool_get_config(ocnConfigs, 'config_use_les_for_vmix', config_use_les_for_vmix)
      call mpas_pool_get_config(ocnConfigs, 'config_use_ecosysTracers', config_use_ecosysTracers)
      if ( .not. config_use_les_for_vmix ) return
      if (config_use_ecosysTracers) then
         call mpas_log_write('Error, superparameterization is not configured for use with BGC',MPAS_LOG_CRIT)
      endif

      call mpas_pool_get_config(ocnConfigs, 'config_flux_attenuation_coefficient',  &
                               config_flux_attenuation_coefficient)
      call mpas_pool_get_config(ocnConfigs, 'config_flux_attenuation_coefficient_runoff',  &
                               config_flux_attenuation_coefficient_runoff)
      call mpas_pool_get_config(ocnConfigs, 'config_les_dz', config_les_dz)
      call mpas_pool_get_config(ocnConfigs, 'config_les_nz', config_les_nz)
      call mpas_pool_get_config(ocnConfigs, 'config_les_dt_data_output', config_les_dt_data_output)
      call mpas_pool_get_config(ocnConfigs, 'config_les_dt_disturb', config_les_dt_disturb)
      call mpas_pool_get_config(ocnConfigs, 'config_les_dt_data_output_av', config_les_dt_data_output_av)
      call mpas_pool_get_config(ocnConfigs, 'config_les_dt_dopr', config_les_dt_dopr)
      call mpas_pool_get_config(ocnConfigs, 'config_les_end_time', config_les_end_time)
      call mpas_pool_get_config(ocnConfigs, 'config_les_dt_dots', config_les_dt_dots)
      call mpas_pool_get_config(ocnConfigs, 'config_jerlov_water_type', config_jerlov_water_type)

      call mpas_pool_get_dimension(meshPool, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(meshPool, 'nCellsArray', nCellsArray)
      call mpas_pool_get_dimension(meshPool, 'nEdgesArray', nEdgesArray)

      call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocity, timeLevel)
      call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(meshPool, 'angleEdge', angleEdge)

      call mpas_pool_get_subpool(forcingPool, 'tracersSurfaceFlux', tracersSurfaceFluxPool)
      call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)
      call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, timeLevel)
      call mpas_pool_get_dimension(tracersPool, 'index_temperature', indexTemperature)
      call mpas_pool_get_dimension(tracersPool, 'index_salinity', indexSalinity)

      call mpas_pool_get_array(tracersSurfaceFluxPool, "activeTracersSurfaceFlux", tracerGroupSurfaceFlux)
      call mpas_pool_get_array(tracersSurfaceFluxPool, "activeTracersSurfaceFluxRunoff", tracerGroupSurfaceFluxRunoff)
      call mpas_pool_get_array(tracersSurfaceFluxPool, "activeTracersSurfaceFluxRemoved", tracerGroupSurfaceFluxRunoff)
      call mpas_pool_get_array(forcingPool, "surfaceThicknessFlux", surfaceThicknessFlux)
      call mpas_pool_get_array(forcingPool, "surfaceThicknessFluxRunoff", surfaceThicknessFluxRunoff)
      call mpas_pool_get_array(meshPool, 'fCell', fCell)
      call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
      call mpas_pool_get_array(statePool, 'layerThickness', layerThickness, timeLevel)

      call mpas_pool_get_array(diagnosticsPool, 'tIncrementLES', tIncrementLES)
      call mpas_pool_get_array(diagnosticsPool, 'sIncrementLES', sIncrementLES)
      call mpas_pool_get_array(diagnosticsPool, 'uIncrementLES', uIncrementLES)
      call mpas_pool_get_array(diagnosticsPool, 'vIncrementLES', vIncrementLES)
      call mpas_pool_get_array(diagnosticsPool, 'normalVelocityIncrementLES', normalVelocityIncrementLES)
      call mpas_pool_get_array(diagnosticsPool, 'velocityZonal', velocityZonal)
      call mpas_pool_get_array(diagnosticsPool, 'velocityMeridional', velocityMeridional)

      ! need to use RBFs to get stress at cell centers
      call mpas_pool_get_array(diagnosticsPool, 'stressZonal', stressZonal)
      call mpas_pool_get_array(diagnosticsPool, 'stressMeridional', stressMeridional)
      call mpas_pool_get_array(diagnosticsPool, 'stressX', stressX)
      call mpas_pool_get_array(diagnosticsPool, 'stressY', stressY)
      call mpas_pool_get_array(diagnosticsPool, 'stressZ', stressZ)

      call mpas_pool_get_array(forcingPool, 'surfaceStress', surfaceStress)
      call mpas_pool_get_array(forcingPool, 'surfaceStressMagnitude', surfaceStressMagnitude)
      
      allocate(zonalAverage(nVertLevels),meridionalAverage(nVertLevels))

      nCells = nCellsArray( 2 )
      nEdges = nEdgesArray( 1 )

      !$omp do schedule(runtime)
      do iEdge=1,nEdges
         surfaceStress(iEdge) = 0.0_RKIND
      enddo
      !$omp end do

      !$omp do schedule(runtime)
      do iCell = 1, nCells
         surfaceStressMagnitude(iCell) = 0.0_RKIND
         tracerGroupSurfaceFluxRunoff(:, iCell) = 0.0_RKIND
         tracerGroupSurfaceFlux(:, iCell) = 0.0_RKIND
         tracerGroupSurfaceFluxRemoved(:, iCell) = 0.0_RKIND
      end do
      !$omp end do

      call ocn_surface_bulk_forcing_vel(meshPool, forcingPool, surfaceStress, surfaceStressMagnitude, err)

      call ocn_surface_land_ice_fluxes_vel(meshPool, diagnosticsPool, surfaceStress, surfaceStressMagnitude, err)

      call ocn_surface_bulk_forcing_tracers(meshPool, "activeTracers", forcingPool, activeTracers,  &
                                            tracerGroupSurfaceFlux, tracerGroupSurfaceFluxRunoff, &
                                            tracerGroupSurfaceFluxRemoved, dt, layerThickness, err)

      call ocn_surface_land_ice_fluxes_tracers(meshPool, "activeTracers", forcingPool, tracerGroupSurfaceFlux, err)

      call ocn_surface_bulk_forcing_thick(meshPool, forcingPool, surfaceThicknessFlux, surfaceThicknessFluxRunoff, err) 

      !need surface thickness flux too

      call mpas_pool_get_array(forcingPool, 'penetrativeTemperatureFlux', penetrativeTemperatureFlux)

      !interpolate edge stresses to cell centers to get X and Y directions
      call mpas_reconstruct(meshPool, surfaceStress, stressX, stressY, stressZ,  &
                            stressZonal, stressMeridional, includeHalos = .true.)

      fac = rfac(config_jerlov_water_type)
      dep1 = depth1(config_jerlov_water_type)
      dep2 = depth2(config_jerlov_water_type)

      !$omp do schedule(runtime)
      do iCell = 1, nCells
         fracAbsorbed = 1.0_RKIND - exp( max(-layerThickness(1, iCell) / config_flux_attenuation_coefficient, -100.0_RKIND) )
         fracAbsorbedRunoff = 1.0_RKIND - exp( max(-layerThickness(1, iCell) / config_flux_attenuation_coefficient_runoff, &
                              -100.0_RKIND) )

         wt_sfc = tracerGroupSurfaceFlux(indexTemperature, iCell) - fracAbsorbed * surfaceThicknessFlux(iCell) * &
                        activeTracers(indexTemperature,1,iCell) + tracerGroupSurfaceFluxRunoff(indexTemperature,iCell) * &
                        fracAbsorbedRunoff

         ws_sfc = tracerGroupSurfaceFlux(indexSalinity, iCell) - fracAbsorbed * surfaceThicknessFlux(iCell) *  &
                        activeTracers(indexSalinity,1,iCell)

         uw_sfc = sign(sqrt(stressZonal(iCell) / rho_sw), stressZonal(iCell))
         
         vw_sfc = sign(sqrt(stressMeridional(iCell) / rho_sw), stressMeridional(iCell))

         nLevs = maxLevelCell(iCell)

         call palm(activeTracers(1,:nLevs,iCell), activeTracers(2,:nLevs,iCell),               &
                velocityZonal(:nLevs,iCell), velocityMeridional(:nLevs,iCell),                 &
                layerThickness(:nLevs,iCell), fCell(iCell), nLevs,                             &
                wt_sfc, penetrativeTemperatureFlux(iCell), ws_sfc, uw_sfc, vw_sfc,             &
                fac, dep1, dep2, config_les_dz, config_les_nz, config_les_dt_data_output,      &
                config_les_dt_disturb, config_les_dt_data_output_av, config_les_dt_dopr,       &
                config_les_end_time, config_les_dt_dots, tIncrementLES(:nLevs,iCell),          &
                sIncrementLES(:nLevs,iCell), uIncrementLES(:nLevs,iCell), vIncrementLES(:nLevs,iCell))

      enddo
      !$ome end do

      !average U,V increments to edges to increment normal Velocity

      !$omp do schedule(runtime)
      do iEdge = 1, nEdges
         cell1 = cellsOnEdge(1, iEdge)
         cell2 = cellsOnEdge(2, iEdge)

         zonalAverage = 0.5_RKIND * (uIncrementLES(:,cell1) + uIncrementLES(:,cell2))
         meridionalAverage = 0.5_RKIND * (vIncrementLES(:,cell1) + vIncrementLES(:,cell2))

         normalVelocityIncrementLES(:,iEdge) = normalVelocityIncrementLES(:,iEdge) + cos(angleEdge(iEdge)) * zonalAverage + &
                                               + sin(angleEdge(iEdge)) * meridionalAverage
      end do
      !$omp end do

      deallocate(zonalAverage)
      deallocate(meridionalAverage)

   end subroutine ocn_vmix_LES_interface!}}}

!***********************************************************************
!
!  routine ocn_vmix_LES_apply_tend
!
!> \brief   Updates fields due to tends from LES
!> \author  Luke Van Roekel
!> \date    09 January 2019
!> \details
!>  This routine updates state fields due to LES tendencies.
!
!-----------------------------------------------------------------------

   subroutine ocn_vmix_LES_apply_tend(meshPool, statePool, forcingPool, diagnosticsPool, timeLevelIn)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      type (mpas_pool_type), intent(in) :: &
         meshPool          !< Input: mesh information

      integer, intent(in), optional :: timeLevelIn !< Input: time level for state pool

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (mpas_pool_type), intent(inout) :: &
         statePool         !< Input/Output: state information

      type (mpas_pool_type), intent(inout) :: &
         diagnosticsPool   !< Input/Output: diagnostic information

      type (mpas_pool_type), intent(inout) :: &
         forcingPool   !< Input/Output: forcing information

      type (mpas_pool_type), pointer :: tracersPool

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), pointer :: &
              normalVelocity, tIncrementLES, sIncrementLES, normalVelocityIncrementLES

      real (kind=RKIND), dimension(:,:,:), pointer :: activeTracers

      integer, pointer :: indexTemperature, indexSalinity

      integer, dimension(:), pointer :: nCellsArray, nEdgesArray

      integer, dimension(:), pointer :: maxLevelCell, maxLevelEdge

      integer :: iCell, iEdge, k, nEdges, nCells

      logical,pointer :: config_use_les_for_vmix, config_use_ecosysTracers

      call mpas_pool_get_config(ocnConfigs, 'config_use_les_for_vmix', config_use_les_for_vmix)
      call mpas_pool_get_config(ocnConfigs, 'config_use_ecosysTracers', config_use_ecosysTracers)
      if ( .not. config_use_les_for_vmix ) return
      if (config_use_ecosysTracers) then
         call mpas_log_write('Error, superparameterization is not configured for use with BGC',MPAS_LOG_CRIT)
      endif

      call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)
      call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, timeLevelIn)
      call mpas_pool_get_dimension(tracersPool, 'index_temperature', indexTemperature)
      call mpas_pool_get_dimension(tracersPool, 'index_salinity', indexSalinity)

      call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
      call mpas_pool_get_array(meshPool, 'maxLevelEdge', maxLevelEdge)

      call mpas_pool_get_array(diagnosticsPool, 'tIncrementLES', tIncrementLES)
      call mpas_pool_get_array(diagnosticsPool, 'sIncrementLES', sIncrementLES)
      call mpas_pool_get_array(diagnosticsPool, 'normalVelocityIncrementLES', normalVelocityIncrementLES)

      call mpas_pool_get_dimension(meshPool, 'nCellsArray', nCellsArray)
      call mpas_pool_get_dimension(meshPool, 'nEdgesArray', nEdgesArray)
      call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocity, timeLevelIn)

      nEdges = nEdgesArray(1)
      nCells = nCellsArray(1)

      !$omp do schedule(runtime)
      do iCell=1,nCells
         do k=1,maxLevelCell(iCell)
            activeTracers(indexTemperature,k,iCell) = activeTracers(indexTemperature,k,iCell) + &
                                                      tIncrementLES(k,iCell)
            activeTracers(indexSalinity,k,iCell) = activeTracers(indexSalinity,k,iCell) +       &
                                                   sIncrementLES(k,iCell)
         end do
      end do
      !$omp end do

      !$omp do schedule(runtime)
      do iEdge=1,nEdges
         do k=1,maxLevelEdge(iEdge)
            normalVelocity(k,iEdge) = normalVelocity(k,iEdge) + normalVelocityIncrementLES(k,iEdge)
         end do
      end do
      !$omp end do

   end subroutine ocn_vmix_LES_apply_tend!}}}
   
end module ocn_vmix_LES
 
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
