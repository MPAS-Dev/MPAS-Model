module SnowpackHydrologyMod

!!! Snowpack hydrology processes (sublimation/frost, evaporation/dew, meltwater)
!!! Update snowpack ice and liquid water content

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowLayerCombineMod, only : SnowLayerCombine

  implicit none

contains

  subroutine SnowpackHydrology(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWH2O
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: LoopInd                       ! do loop/array indices
    real(kind=kind_noahmp)           :: InflowSnowLayer               ! water flow into each snow layer [mm/s]
    real(kind=kind_noahmp)           :: OutflowSnowLayer              ! water flow out of each snow layer [mm/s]
    real(kind=kind_noahmp)           :: SnowIceTmp                    ! ice mass after minus sublimation
    real(kind=kind_noahmp)           :: SnowWaterRatio                ! ratio of SWE after frost & sublimation to original SWE
    real(kind=kind_noahmp)           :: SnowWaterTmp                  ! temporary SWE
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowLiqVol   ! partial volume of liquid water in layer
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceVol   ! partial volume of ice lens in layer

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSnowLayerMax        => noahmp%config%domain%NumSnowLayerMax        ,& ! in,    maximum number of snow layers
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,    noahmp main time step [s]
              FrostSnowSfcIce        => noahmp%water%flux%FrostSnowSfcIce           ,& ! in,    snow surface frost rate [mm/s]
              SublimSnowSfcIce       => noahmp%water%flux%SublimSnowSfcIce          ,& ! in,    snow surface sublimation rate [mm/s]
              RainfallGround         => noahmp%water%flux%RainfallGround            ,& ! in,    ground surface rain rate [mm/s]
              SnowLiqFracMax         => noahmp%water%param%SnowLiqFracMax           ,& ! in,    maximum liquid water fraction in snow
              SnowLiqHoldCap         => noahmp%water%param%SnowLiqHoldCap           ,& ! in,    liquid water holding capacity for snowpack [m3/m3]
              SnowLiqReleaseFac      => noahmp%water%param%SnowLiqReleaseFac        ,& ! in,    snowpack water release timescale factor [1/s]
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! inout, actual number of snow layers (negative)
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! inout, thickness of snow/soil layers [m]
              SnowDepth              => noahmp%water%state%SnowDepth                ,& ! inout, snow depth [m]
              SnowWaterEquiv         => noahmp%water%state%SnowWaterEquiv           ,& ! inout, snow water equivalent [mm]
              SnowIce                => noahmp%water%state%SnowIce                  ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater             ,& ! inout, snow layer liquid water [mm]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater             ,& ! inout, soil liquid moisture [m3/m3]
              SoilIce                => noahmp%water%state%SoilIce                  ,& ! inout, soil ice moisture [m3/m3]
              SnowEffPorosity        => noahmp%water%state%SnowEffPorosity          ,& ! out,   snow effective porosity [m3/m3]
              SnowBotOutflow         => noahmp%water%flux%SnowBotOutflow             & ! out,   total water (snowmelt + rain through pack) out of snowpack bottom [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(SnowLiqVol)) allocate(SnowLiqVol(-NumSnowLayerMax+1:0))
    if (.not. allocated(SnowIceVol)) allocate(SnowIceVol(-NumSnowLayerMax+1:0))
    SnowLiqVol(:)      = 0.0
    SnowIceVol(:)      = 0.0
    SnowEffPorosity(:) = 0.0
    SnowBotOutflow     = 0.0
    InflowSnowLayer    = 0.0
    OutflowSnowLayer   = 0.0

    ! for the case when SnowWaterEquiv becomes '0' after 'COMBINE'
    if ( SnowWaterEquiv == 0.0 ) then
       SoilIce(1) = SoilIce(1) + (FrostSnowSfcIce-SublimSnowSfcIce) * MainTimeStep / &
                                 (ThicknessSnowSoilLayer(1)*1000.0)  ! Barlage: SoilLiqWater->SoilIce v3.6
       if ( SoilIce(1) < 0.0 ) then
          SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
          SoilIce(1)      = 0.0
       endif
    endif

    ! for shallow snow without a layer
    ! snow surface sublimation may be larger than existing snow mass. To conserve water,
    ! excessive sublimation is used to reduce soil water. Smaller time steps would tend to aviod this problem.
    if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) ) then
       SnowWaterTmp   = SnowWaterEquiv
       SnowWaterEquiv = SnowWaterEquiv - SublimSnowSfcIce*MainTimeStep + FrostSnowSfcIce*MainTimeStep
       SnowWaterRatio = SnowWaterEquiv / SnowWaterTmp
       SnowDepth      = max(0.0, SnowWaterRatio*SnowDepth )
       SnowDepth      = min(max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0)    ! limit adjustment to a reasonable density
       if ( SnowWaterEquiv < 0.0 ) then
          SoilIce(1)     = SoilIce(1) + SnowWaterEquiv / (ThicknessSnowSoilLayer(1)*1000.0)
          SnowWaterEquiv = 0.0
          SnowDepth      = 0.0
       endif
       if ( SoilIce(1) < 0.0 ) then
          SoilLiqWater(1) = SoilLiqWater(1) + SoilIce(1)
          SoilIce(1)      = 0.0
       endif
    endif

    if ( (SnowDepth <= 1.0e-8) .or. (SnowWaterEquiv <= 1.0e-6) ) then
       SnowDepth      = 0.0
       SnowWaterEquiv = 0.0
    endif

    ! for multi-layer (>=1) snow
    if ( NumSnowLayerNeg < 0 ) then
      SnowIceTmp = SnowIce(NumSnowLayerNeg+1) - SublimSnowSfcIce*MainTimeStep + FrostSnowSfcIce*MainTimeStep
      SnowIce(NumSnowLayerNeg+1) = SnowIceTmp
      if ( (SnowIceTmp < 1.0e-6) .and. (NumSnowLayerNeg < 0) ) call SnowLayerCombine(noahmp)
      if ( NumSnowLayerNeg < 0 ) then
         SnowLiqWater(NumSnowLayerNeg+1) = SnowLiqWater(NumSnowLayerNeg+1) + RainfallGround * MainTimeStep
         SnowLiqWater(NumSnowLayerNeg+1) = max(0.0, SnowLiqWater(NumSnowLayerNeg+1))
      endif
    endif

    ! Porosity and partial volume
    do LoopInd = NumSnowLayerNeg+1, 0
       SnowIceVol(LoopInd)      = min(1.0, SnowIce(LoopInd)/(ThicknessSnowSoilLayer(LoopInd)*ConstDensityIce))
       SnowEffPorosity(LoopInd) = 1.0 - SnowIceVol(LoopInd)
    enddo

    ! compute inter-layer snow water flow
    do LoopInd = NumSnowLayerNeg+1, 0
       SnowLiqWater(LoopInd) = SnowLiqWater(LoopInd) + InflowSnowLayer
       SnowLiqVol(LoopInd)   = SnowLiqWater(LoopInd) / (ThicknessSnowSoilLayer(LoopInd)*ConstDensityWater)
       OutflowSnowLayer      = max(0.0, (SnowLiqVol(LoopInd)-SnowLiqHoldCap*SnowEffPorosity(LoopInd)) * &
                                        ThicknessSnowSoilLayer(LoopInd))
       if ( LoopInd == 0 ) then
          OutflowSnowLayer   = max((SnowLiqVol(LoopInd)-SnowEffPorosity(LoopInd)) * ThicknessSnowSoilLayer(LoopInd), &
                                 SnowLiqReleaseFac * MainTimeStep * OutflowSnowLayer)
       endif
       OutflowSnowLayer      = OutflowSnowLayer * ConstDensityWater
       SnowLiqWater(LoopInd) = SnowLiqWater(LoopInd) - OutflowSnowLayer
       if ( (SnowLiqWater(LoopInd)/(SnowIce(LoopInd)+SnowLiqWater(LoopInd))) > SnowLiqFracMax ) then
          OutflowSnowLayer   = OutflowSnowLayer + (SnowLiqWater(LoopInd) - &
                                                   SnowLiqFracMax / (1.0-SnowLiqFracMax) * SnowIce(LoopInd))
          SnowLiqWater(LoopInd) = SnowLiqFracMax / (1.0 - SnowLiqFracMax) * SnowIce(LoopInd)
       endif
       InflowSnowLayer = OutflowSnowLayer
    enddo

    ! update snow depth
    do LoopInd = NumSnowLayerNeg+1, 0
       ThicknessSnowSoilLayer(LoopInd) = max(ThicknessSnowSoilLayer(LoopInd), &
                                             SnowLiqWater(LoopInd)/ConstDensityWater+SnowIce(LoopInd)/ConstDensityIce)
    enddo

    ! Liquid water from snow bottom to soil [mm/s]
    SnowBotOutflow = OutflowSnowLayer / MainTimeStep

    ! deallocate local arrays to avoid memory leaks
    deallocate(SnowLiqVol)
    deallocate(SnowIceVol)

    end associate

  end subroutine SnowpackHydrology

end module SnowpackHydrologyMod
