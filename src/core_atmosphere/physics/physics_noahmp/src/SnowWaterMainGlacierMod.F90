module SnowWaterMainGlacierMod

!!! Main glacier snow water module including all snowpack processes
!!! Snowfall -> Snowpack compaction -> Snow layer combination -> Snow layer division -> Snow Hydrology

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowfallBelowCanopyMod,      only : SnowfallAfterCanopyIntercept
  use SnowpackCompactionMod,       only : SnowpackCompaction
  use SnowLayerCombineMod,         only : SnowLayerCombine
  use SnowLayerDivideMod,          only : SnowLayerDivide
  use SnowpackHydrologyGlacierMod, only : SnowpackHydrologyGlacier

  implicit none

contains

  subroutine SnowWaterMainGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWWATER_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variables
    integer                          :: LoopInd         ! do loop/array indices
    real(kind=kind_noahmp)           :: SnowDensBulk    ! bulk density of snow [kg/m3]

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSnowLayerMax        => noahmp%config%domain%NumSnowLayerMax        ,& ! in,    maximum number of snow layers
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,    number of soil layers
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,    noahmp main time step [s]
              DepthSoilLayer         => noahmp%config%domain%DepthSoilLayer         ,& ! in,    depth [m] of layer-bottom from soil surface
              SnoWatEqvMaxGlacier    => noahmp%water%param%SnoWatEqvMaxGlacier      ,& ! in,    Maximum SWE allowed at glaciers [mm]
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! inout, thickness of snow/soil layers [m]
              DepthSnowSoilLayer     => noahmp%config%domain%DepthSnowSoilLayer     ,& ! inout, depth of snow/soil layer-bottom [m]
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! inout, actual number of snow layers (negative)
              SnowDepth              => noahmp%water%state%SnowDepth                ,& ! inout, snow depth [m]
              SnowWaterEquiv         => noahmp%water%state%SnowWaterEquiv           ,& ! inout, snow water equivalent [mm]
              SnowIce                => noahmp%water%state%SnowIce                  ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater             ,& ! inout, snow layer liquid water [mm]
              TemperatureSoilSnow    => noahmp%energy%state%TemperatureSoilSnow     ,& ! inout, snow and soil layer temperature [K]
              GlacierExcessFlow      => noahmp%water%flux%GlacierExcessFlow         ,& ! out,   glacier excess flow [mm/s]
              PondSfcThinSnwComb     => noahmp%water%state%PondSfcThinSnwComb       ,& ! out,   surface ponding [mm] from liquid in thin snow layer combination
              PondSfcThinSnwTrans    => noahmp%water%state%PondSfcThinSnwTrans       & ! out,   surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
             )
! ----------------------------------------------------------------------

    ! initialize out-only variables
    GlacierExcessFlow   = 0.0
    PondSfcThinSnwComb  = 0.0
    PondSfcThinSnwTrans = 0.0

    ! snowfall
    call SnowfallAfterCanopyIntercept(noahmp)

    ! do following snow layer compaction, combination, and division only for multi-layer snowpack

    ! snowpack compaction
    if ( NumSnowLayerNeg < 0 ) call SnowpackCompaction(noahmp)

    ! snow layer combination
    if ( NumSnowLayerNeg < 0 ) call SnowLayerCombine(noahmp)

    ! snow layer division
    if ( NumSnowLayerNeg < 0 ) call SnowLayerDivide(noahmp)

    ! snow hydrology for all snow cases
    call SnowpackHydrologyGlacier(noahmp)

    ! set empty snow layer properties to zero
    do LoopInd = -NumSnowLayerMax+1, NumSnowLayerNeg
       SnowIce(LoopInd)                = 0.0
       SnowLiqWater(LoopInd)           = 0.0
       TemperatureSoilSnow(LoopInd)    = 0.0
       ThicknessSnowSoilLayer(LoopInd) = 0.0
       DepthSnowSoilLayer(LoopInd)     = 0.0
    enddo

    ! to obtain equilibrium state of snow in glacier region
    if ( SnowWaterEquiv > SnoWatEqvMaxGlacier ) then 
       SnowDensBulk              = SnowIce(0) / ThicknessSnowSoilLayer(0)
       GlacierExcessFlow         = SnowWaterEquiv - SnoWatEqvMaxGlacier
       SnowIce(0)                = SnowIce(0)  - GlacierExcessFlow
       ThicknessSnowSoilLayer(0) = ThicknessSnowSoilLayer(0) - GlacierExcessFlow / SnowDensBulk
       GlacierExcessFlow         = GlacierExcessFlow / MainTimeStep
    endif

    ! sum up snow mass for layered snow
    if ( NumSnowLayerNeg < 0 ) then  ! MB: only do for multi-layer
       SnowWaterEquiv = 0.0
       do LoopInd = NumSnowLayerNeg+1, 0
          SnowWaterEquiv = SnowWaterEquiv + SnowIce(LoopInd) + SnowLiqWater(LoopInd)
       enddo
    endif

    ! Reset DepthSnowSoilLayer and ThicknessSnowSoilLayer
    do LoopInd = NumSnowLayerNeg+1, 0
       ThicknessSnowSoilLayer(LoopInd) = -ThicknessSnowSoilLayer(LoopInd)
    enddo

    ThicknessSnowSoilLayer(1) = DepthSoilLayer(1)
    do LoopInd = 2, NumSoilLayer
       ThicknessSnowSoilLayer(LoopInd) = DepthSoilLayer(LoopInd) - DepthSoilLayer(LoopInd-1)
    enddo

    DepthSnowSoilLayer(NumSnowLayerNeg+1) = ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
    do LoopInd = NumSnowLayerNeg+2, NumSoilLayer
       DepthSnowSoilLayer(LoopInd) = DepthSnowSoilLayer(LoopInd-1) + ThicknessSnowSoilLayer(LoopInd)
    enddo

    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       ThicknessSnowSoilLayer(LoopInd) = -ThicknessSnowSoilLayer(LoopInd)
    enddo

    ! Update SnowDepth for multi-layer snow
    if ( NumSnowLayerNeg < 0 ) then
       SnowDepth = 0.0
       do LoopInd = NumSnowLayerNeg+1, 0
          SnowDepth = SnowDepth + ThicknessSnowSoilLayer(LoopInd)
       enddo
    endif

    ! update snow quantity
    if ( (SnowDepth <= 1.0e-6) .or. (SnowWaterEquiv <= 1.0e-6) ) then
       SnowDepth      = 0.0
       SnowWaterEquiv = 0.0
    endif

    end associate

  end subroutine SnowWaterMainGlacier

end module SnowWaterMainGlacierMod
