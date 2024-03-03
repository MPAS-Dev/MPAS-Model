module TileDrainageSimpleMod

!!! Calculate tile drainage discharge [mm] based on simple model

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine TileDrainageSimple(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TILE_DRAIN
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndSoil                           ! soil layer loop index
    real(kind=kind_noahmp)           :: DrainWatVolTot                    ! temporary variable for drainage volume [mm]
    real(kind=kind_noahmp)           :: DrainCoeffTmp                     ! temporary variable for drainage
    real(kind=kind_noahmp)           :: DrainWatTmp                       ! temporary variable for drainage
    real(kind=kind_noahmp), allocatable, dimension(:) :: WatExcFieldCap   ! temp variable for volume of water above field capacity
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilFieldCapLiq  ! Available field capacity = field capacity - SoilIce [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DrainFracTmp     ! tile drainage fraction

! --------------------------------------------------------------------
    associate(                                                                 &
              NumSoilLayer         => noahmp%config%domain%NumSoilLayer       ,& ! in,    number of soil layers
              DepthSoilLayer       => noahmp%config%domain%DepthSoilLayer     ,& ! in,    depth [m] of layer-bottom from soil surface
              SoilTimeStep         => noahmp%config%domain%SoilTimeStep       ,& ! in,    noahmp soil timestep [s]
              ThicknessSoilLayer   => noahmp%config%domain%ThicknessSoilLayer ,& ! in,    soil layer thickness [m]
              TileDrainCoeffSp     => noahmp%water%param%TileDrainCoeffSp     ,& ! in,    drainage coefficient [mm/d]
              DrainSoilLayerInd    => noahmp%water%param%DrainSoilLayerInd    ,& ! in,    starting soil layer for drainage
              TileDrainTubeDepth   => noahmp%water%param%TileDrainTubeDepth   ,& ! in,    depth of drain tube from the soil surface
              DrainFacSoilWat      => noahmp%water%param%DrainFacSoilWat      ,& ! in,    drainage factor for soil moisture
              SoilMoistureFieldCap => noahmp%water%param%SoilMoistureFieldCap ,& ! in,    reference soil moisture (field capacity) [m3/m3]
              SoilIce              => noahmp%water%state%SoilIce              ,& ! in,    soil ice content [m3/m3]
              SoilLiqWater         => noahmp%water%state%SoilLiqWater         ,& ! inout, soil water content [m3/m3]
              SoilMoisture         => noahmp%water%state%SoilMoisture         ,& ! inout, total soil moisture [m3/m3]
              TileDrain            => noahmp%water%flux%TileDrain              & ! out,   tile drainage [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(DrainFracTmp)   ) allocate(DrainFracTmp   (1:NumSoilLayer))
    if (.not. allocated(SoilFieldCapLiq)) allocate(SoilFieldCapLiq(1:NumSoilLayer))
    if (.not. allocated(WatExcFieldCap) ) allocate(WatExcFieldCap (1:NumSoilLayer))
    DrainFracTmp       = 0.0
    SoilFieldCapLiq    = 0.0
    DrainWatVolTot     = 0.0
    WatExcFieldCap     = 0.0
    TileDrain          = 0.0
    ThicknessSoilLayer = 0.0
    DrainWatTmp        = 0.0
    DrainFracTmp       = 0.0
    DrainCoeffTmp      = TileDrainCoeffSp * SoilTimeStep / (24.0 * 3600.0)

    do IndSoil = 1, NumSoilLayer
       if ( IndSoil == 1 ) then
          ThicknessSoilLayer(IndSoil) = -1.0 * DepthSoilLayer(IndSoil)
       else
          ThicknessSoilLayer(IndSoil) = DepthSoilLayer(IndSoil-1) - DepthSoilLayer(IndSoil)
       endif
    enddo
    if ( DrainSoilLayerInd == 0 ) then ! drainage from one specified layer in NoahmpTable.TBL
       IndSoil                  = TileDrainTubeDepth
       SoilFieldCapLiq(IndSoil) = SoilMoistureFieldCap(IndSoil) - SoilIce (IndSoil)
       WatExcFieldCap(IndSoil)  = (SoilLiqWater(IndSoil) - (DrainFacSoilWat*SoilFieldCapLiq(IndSoil))) * &
                                  ThicknessSoilLayer(IndSoil) * 1000.0 ! mm
       if ( WatExcFieldCap(IndSoil) > 0.0 ) then
          if ( WatExcFieldCap(IndSoil) > DrainCoeffTmp ) WatExcFieldCap(IndSoil) = DrainCoeffTmp
          DrainWatVolTot        = DrainWatVolTot  + WatExcFieldCap(IndSoil)
          SoilLiqWater(IndSoil) = SoilLiqWater(IndSoil) - &
                                  (WatExcFieldCap(IndSoil) / (ThicknessSoilLayer(IndSoil) * 1000.0))
          SoilMoisture(IndSoil) = SoilLiqWater(IndSoil) + SoilIce (IndSoil)
       endif
    else if ( DrainSoilLayerInd == 1 ) then
       do IndSoil = 1, 2
          SoilFieldCapLiq(IndSoil) = SoilMoistureFieldCap(IndSoil) - SoilIce (IndSoil)
          WatExcFieldCap(IndSoil)  = (SoilLiqWater(IndSoil) - (DrainFacSoilWat*SoilFieldCapLiq(IndSoil))) * &
                                     ThicknessSoilLayer(IndSoil) * 1000.0 ! mm
          if ( WatExcFieldCap(IndSoil) < 0.0 ) WatExcFieldCap(IndSoil) = 0.0
          DrainWatTmp = DrainWatTmp + WatExcFieldCap(IndSoil)
       enddo
       do IndSoil = 1, 2
          if ( WatExcFieldCap(IndSoil) /= 0.0 ) then
             DrainFracTmp(IndSoil) = WatExcFieldCap(IndSoil) / DrainWatTmp
          endif
       enddo
       if ( DrainWatTmp > 0.0 ) then
          if ( DrainWatTmp > DrainCoeffTmp ) DrainWatTmp = DrainCoeffTmp
          DrainWatVolTot = DrainWatVolTot + DrainWatTmp
          do IndSoil = 1, 2
             WatExcFieldCap(IndSoil) = DrainFracTmp(IndSoil) * DrainWatTmp
             SoilLiqWater(IndSoil)   = SoilLiqWater(IndSoil) - &
                                       (WatExcFieldCap(IndSoil) / (ThicknessSoilLayer(IndSoil) * 1000.0))
             SoilMoisture(IndSoil)   = SoilLiqWater(IndSoil) + SoilIce (IndSoil)
          enddo
       endif
    else if ( DrainSoilLayerInd == 2 ) then
       do IndSoil = 1, 3
          SoilFieldCapLiq(IndSoil) = SoilMoistureFieldCap(IndSoil) - SoilIce (IndSoil)
          WatExcFieldCap(IndSoil)  = (SoilLiqWater(IndSoil) - (DrainFacSoilWat*SoilFieldCapLiq(IndSoil))) * &
                                     ThicknessSoilLayer(IndSoil) * 1000.0
          if ( WatExcFieldCap(IndSoil) < 0.0 ) WatExcFieldCap(IndSoil) = 0.0
          DrainWatTmp = DrainWatTmp + WatExcFieldCap(IndSoil)
       enddo
       do IndSoil = 1, 3
          if ( WatExcFieldCap(IndSoil) /= 0.0 ) then
             DrainFracTmp(IndSoil) = WatExcFieldCap(IndSoil) / DrainWatTmp
          endif
       enddo
       if ( DrainWatTmp > 0.0 ) then
          if ( DrainWatTmp > DrainCoeffTmp ) DrainWatTmp = DrainCoeffTmp
          DrainWatVolTot = DrainWatVolTot + DrainWatTmp
          do IndSoil = 1, 3
             WatExcFieldCap(IndSoil) = DrainFracTmp(IndSoil) * DrainWatTmp
             SoilLiqWater(IndSoil)   = SoilLiqWater(IndSoil) - &
                                       (WatExcFieldCap(IndSoil) / (ThicknessSoilLayer(IndSoil) * 1000.0))
             SoilMoisture(IndSoil)   = SoilLiqWater(IndSoil) + SoilIce (IndSoil)
          enddo
       endif
    else if ( DrainSoilLayerInd == 3 ) then
       do IndSoil = 2, 3
          SoilFieldCapLiq(IndSoil) = SoilMoistureFieldCap(IndSoil) - SoilIce (IndSoil)
          WatExcFieldCap(IndSoil)  = (SoilLiqWater(IndSoil) - (DrainFacSoilWat*SoilFieldCapLiq(IndSoil))) * &
                                     ThicknessSoilLayer(IndSoil) * 1000.0
          if ( WatExcFieldCap(IndSoil) < 0.0 ) WatExcFieldCap(IndSoil) = 0.0
          DrainWatTmp = DrainWatTmp + WatExcFieldCap(IndSoil)
       enddo
       do IndSoil = 2, 3
          if ( WatExcFieldCap(IndSoil) /= 0.0 ) then
             DrainFracTmp(IndSoil) = WatExcFieldCap(IndSoil) / DrainWatTmp
          endif
       enddo
       if ( DrainWatTmp > 0.0 ) then
          if ( DrainWatTmp > DrainCoeffTmp ) DrainWatTmp = DrainCoeffTmp
          DrainWatVolTot = DrainWatVolTot + DrainWatTmp
          do IndSoil = 2, 3
             WatExcFieldCap(IndSoil) = DrainFracTmp(IndSoil) * DrainWatTmp
             SoilLiqWater(IndSoil)   = SoilLiqWater(IndSoil) - &
                                       (WatExcFieldCap(IndSoil) / (ThicknessSoilLayer(IndSoil) * 1000.0))
             SoilMoisture(IndSoil)   = SoilLiqWater(IndSoil) + SoilIce (IndSoil)
          enddo
       endif
    else if ( DrainSoilLayerInd == 4 ) then
       do IndSoil = 3, 4
          SoilFieldCapLiq(IndSoil) = SoilMoistureFieldCap(IndSoil) - SoilIce (IndSoil)
          WatExcFieldCap(IndSoil)  = (SoilLiqWater(IndSoil) - (DrainFacSoilWat*SoilFieldCapLiq(IndSoil))) * &
                                     ThicknessSoilLayer(IndSoil) * 1000.0
          if ( WatExcFieldCap(IndSoil) < 0.0 ) WatExcFieldCap(IndSoil) = 0.0
          DrainWatTmp = DrainWatTmp + WatExcFieldCap(IndSoil)
       enddo
       do IndSoil = 3, 4
          if ( WatExcFieldCap(IndSoil) /= 0.0 ) then
             DrainFracTmp(IndSoil) = WatExcFieldCap(IndSoil) / DrainWatTmp
          endif
       enddo
       if ( DrainWatTmp > 0.0 ) then
          if ( DrainWatTmp > DrainCoeffTmp ) DrainWatTmp = DrainCoeffTmp
          DrainWatVolTot = DrainWatVolTot + DrainWatTmp
          do IndSoil = 3, 4
             WatExcFieldCap(IndSoil) = DrainFracTmp(IndSoil) * DrainWatTmp
             SoilLiqWater(IndSoil)   = SoilLiqWater(IndSoil) - (WatExcFieldCap(IndSoil) / &
                                       (ThicknessSoilLayer(IndSoil) * 1000.0))
             SoilMoisture(IndSoil)   = SoilLiqWater(IndSoil) + SoilIce (IndSoil)
          enddo
       endif
    else if ( DrainSoilLayerInd == 5 ) then ! from all the four layers
       do IndSoil = 1, 4
          SoilFieldCapLiq(IndSoil) = SoilMoistureFieldCap(IndSoil) - SoilIce (IndSoil)
          WatExcFieldCap(IndSoil)  = (SoilLiqWater(IndSoil) - (DrainFacSoilWat*SoilFieldCapLiq(IndSoil))) * &
                                     ThicknessSoilLayer(IndSoil) * 1000.0
          if ( WatExcFieldCap(IndSoil) < 0.0 ) WatExcFieldCap(IndSoil) = 0.0
          DrainWatTmp = DrainWatTmp + WatExcFieldCap(IndSoil)
       enddo
       do IndSoil = 1, 4
          if ( WatExcFieldCap(IndSoil) /= 0.0 ) then
             DrainFracTmp(IndSoil) = WatExcFieldCap(IndSoil) / DrainWatTmp
          endif
       enddo
       if ( DrainWatTmp > 0.0 ) then
          if ( DrainWatTmp > DrainCoeffTmp ) DrainWatTmp = DrainCoeffTmp
          DrainWatVolTot = DrainWatVolTot + DrainWatTmp
          do IndSoil = 1, 4
             WatExcFieldCap(IndSoil) = DrainFracTmp(IndSoil) * DrainWatTmp
             SoilLiqWater(IndSoil)   = SoilLiqWater(IndSoil) - (WatExcFieldCap(IndSoil) / &
                                       (ThicknessSoilLayer(IndSoil) * 1000.0))
             SoilMoisture(IndSoil)   = SoilLiqWater(IndSoil) + SoilIce (IndSoil)
          enddo
       endif
    endif

    TileDrain = DrainWatVolTot / SoilTimeStep

    ! deallocate local arrays to avoid memory leaks
    deallocate(DrainFracTmp   )
    deallocate(SoilFieldCapLiq)
    deallocate(WatExcFieldCap )

    end associate

  end subroutine TileDrainageSimple

end module TileDrainageSimpleMod
