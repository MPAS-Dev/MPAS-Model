module CanopyHydrologyMod

!!! Canopy Hydrology processes for intercepted rain and snow water
!!! Canopy liquid water evaporation and dew; canopy ice water sublimation and frost
  
  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CanopyHydrology(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CANWATER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                            &
              MainTimeStep      => noahmp%config%domain%MainTimeStep     ,& ! in,    noahmp main time step [s]
              HeatLatentCanopy  => noahmp%energy%flux%HeatLatentCanopy   ,& ! in,    canopy latent heat flux [W/m2] (+ to atm)
              HeatLatentTransp  => noahmp%energy%flux%HeatLatentTransp   ,& ! in,    latent heat flux from transpiration [W/m2] (+ to atm)
              LeafAreaIndEff    => noahmp%energy%state%LeafAreaIndEff    ,& ! in,    leaf area index, after burying by snow
              StemAreaIndEff    => noahmp%energy%state%StemAreaIndEff    ,& ! in,    stem area index, after burying by snow
              FlagFrozenCanopy  => noahmp%energy%state%FlagFrozenCanopy  ,& ! in,    used to define latent heat pathway
              VegFrac           => noahmp%energy%state%VegFrac           ,& ! in,    greeness vegetation fraction
              SnowfallDensity   => noahmp%water%state%SnowfallDensity    ,& ! in,    bulk density of snowfall [kg/m3]
              CanopyLiqHoldCap  => noahmp%water%param%CanopyLiqHoldCap   ,& ! in,    maximum intercepted liquid water per unit veg area index [mm]
              CanopyLiqWater    => noahmp%water%state%CanopyLiqWater     ,& ! inout, intercepted canopy liquid water [mm]
              CanopyIce         => noahmp%water%state%CanopyIce          ,& ! inout, intercepted canopy ice [mm]
              TemperatureCanopy => noahmp%energy%state%TemperatureCanopy ,& ! inout, vegetation temperature [K]
              CanopyTotalWater  => noahmp%water%state%CanopyTotalWater   ,& ! out,   total canopy intercepted water [mm]
              CanopyWetFrac     => noahmp%water%state%CanopyWetFrac      ,& ! out,   wetted or snowed fraction of the canopy
              CanopyIceMax      => noahmp%water%state%CanopyIceMax       ,& ! out,   canopy capacity for snow interception [mm]
              CanopyLiqWaterMax => noahmp%water%state%CanopyLiqWaterMax  ,& ! out,   canopy capacity for rain interception [mm]
              EvapCanopyNet     => noahmp%water%flux%EvapCanopyNet       ,& ! out,   evaporation of intercepted total water [mm/s]
              Transpiration     => noahmp%water%flux%Transpiration       ,& ! out,   transpiration rate [mm/s]
              EvapCanopyLiq     => noahmp%water%flux%EvapCanopyLiq       ,& ! out,   canopy liquid water evaporation rate [mm/s]
              DewCanopyLiq      => noahmp%water%flux%DewCanopyLiq        ,& ! out,   canopy liquid water dew rate [mm/s]
              FrostCanopyIce    => noahmp%water%flux%FrostCanopyIce      ,& ! out,   canopy ice frost rate [mm/s]
              SublimCanopyIce   => noahmp%water%flux%SublimCanopyIce     ,& ! out,   canopy ice sublimation rate [mm/s]
              MeltCanopyIce     => noahmp%water%flux%MeltCanopyIce       ,& ! out,   canopy ice melting rate [mm/s]
              FreezeCanopyLiq   => noahmp%water%flux%FreezeCanopyLiq      & ! out,   canopy water freezing rate [mm/s]
             )
! --------------------------------------------------------------------

    ! initialization for out-only variables
    EvapCanopyNet     = 0.0
    Transpiration     = 0.0
    EvapCanopyLiq     = 0.0
    DewCanopyLiq      = 0.0
    FrostCanopyIce    = 0.0
    SublimCanopyIce   = 0.0
    MeltCanopyIce     = 0.0
    FreezeCanopyLiq   = 0.0
    CanopyLiqWaterMax = 0.0
    CanopyIceMax      = 0.0
    CanopyWetFrac     = 0.0
    CanopyTotalWater  = 0.0

    ! canopy liquid water
    ! maximum canopy intercepted water
    CanopyLiqWaterMax =  VegFrac * CanopyLiqHoldCap * (LeafAreaIndEff + StemAreaIndEff)

    ! canopy evaporation, transpiration, and dew
    if ( FlagFrozenCanopy .eqv. .false. ) then    ! Barlage: change to FlagFrozenCanopy
       Transpiration   = max( HeatLatentTransp/ConstLatHeatEvap, 0.0 )
       EvapCanopyLiq   = max( HeatLatentCanopy/ConstLatHeatEvap, 0.0 )
       DewCanopyLiq    = abs( min( HeatLatentCanopy/ConstLatHeatEvap, 0.0 ) )
       SublimCanopyIce = 0.0
       FrostCanopyIce  = 0.0
    else
       Transpiration   = max( HeatLatentTransp/ConstLatHeatSublim, 0.0 )
       EvapCanopyLiq   = 0.0
       DewCanopyLiq    = 0.0
       SublimCanopyIce = max( HeatLatentCanopy/ConstLatHeatSublim, 0.0 )
       FrostCanopyIce  = abs( min( HeatLatentCanopy/ConstLatHeatSublim, 0.0 ) )
    endif

    ! canopy water balance. for convenience allow dew to bring CanopyLiqWater above
    ! maxh2o or else would have to re-adjust drip
    EvapCanopyLiq  = min( CanopyLiqWater/MainTimeStep, EvapCanopyLiq )
    CanopyLiqWater = max( 0.0, CanopyLiqWater+(DewCanopyLiq-EvapCanopyLiq)*MainTimeStep )
    if ( CanopyLiqWater <= 1.0e-06 ) CanopyLiqWater = 0.0

    ! canopy ice 
    ! maximum canopy intercepted ice
    CanopyIceMax = VegFrac * 6.6 * (0.27 + 46.0/SnowfallDensity) * (LeafAreaIndEff + StemAreaIndEff)

    ! canopy sublimation and frost
    SublimCanopyIce = min( CanopyIce/MainTimeStep, SublimCanopyIce )
    CanopyIce       = max( 0.0, CanopyIce+(FrostCanopyIce-SublimCanopyIce)*MainTimeStep )
    if ( CanopyIce <= 1.0e-6 ) CanopyIce = 0.0

    ! wetted fraction of canopy
    if ( (CanopyIce > 0.0) .and. (CanopyIce >= CanopyLiqWater) ) then
       CanopyWetFrac = max(0.0,CanopyIce) / max(CanopyIceMax,1.0e-06)
    else
       CanopyWetFrac = max(0.0,CanopyLiqWater) / max(CanopyLiqWaterMax,1.0e-06)
    endif
    CanopyWetFrac    = min(CanopyWetFrac, 1.0) ** 0.667
    CanopyTotalWater = CanopyLiqWater + CanopyIce

    ! phase change
    ! canopy ice melting
    if ( (CanopyIce > 1.0e-6) .and. (TemperatureCanopy > ConstFreezePoint) ) then
       MeltCanopyIce     = min( CanopyIce/MainTimeStep, (TemperatureCanopy-ConstFreezePoint) * ConstHeatCapacIce * &
                                CanopyIce / ConstDensityIce / (MainTimeStep*ConstLatHeatFusion) )
       CanopyIce         = max( 0.0, CanopyIce - MeltCanopyIce*MainTimeStep )
       CanopyLiqWater    = max( 0.0, CanopyTotalWater - CanopyIce )
       TemperatureCanopy = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TemperatureCanopy
    endif

    ! canopy water refreeezing
    if ( (CanopyLiqWater > 1.0e-6) .and. (TemperatureCanopy < ConstFreezePoint) ) then
       FreezeCanopyLiq   = min( CanopyLiqWater/MainTimeStep, (ConstFreezePoint-TemperatureCanopy) * ConstHeatCapacWater * &
                                CanopyLiqWater / ConstDensityWater / (MainTimeStep*ConstLatHeatFusion) )
       CanopyLiqWater    = max( 0.0, CanopyLiqWater - FreezeCanopyLiq*MainTimeStep )
       CanopyIce         = max( 0.0, CanopyTotalWater - CanopyLiqWater )
       TemperatureCanopy = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TemperatureCanopy
    endif

    ! update total canopy water
    CanopyTotalWater = CanopyLiqWater + CanopyIce

    ! total canopy net evaporation
    EvapCanopyNet    = EvapCanopyLiq + SublimCanopyIce - DewCanopyLiq - FrostCanopyIce

    end associate

  end subroutine CanopyHydrology

end module CanopyHydrologyMod
