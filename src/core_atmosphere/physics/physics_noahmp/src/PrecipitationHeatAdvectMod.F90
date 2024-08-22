module PrecipitationHeatAdvectMod

!!! Estimate heat flux advected from precipitation to vegetation and ground

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PrecipitationHeatAdvect(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PRECIP_HEAT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! The water and heat portions of PRECIP_HEAT are separated in refactored code
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: HeatPrcpAirToCan    ! precipitation advected heat - air to canopy [W/m2]
    real(kind=kind_noahmp)           :: HeatPrcpCanToGrd    ! precipitation advected heat - canopy to ground [W/m2]
    real(kind=kind_noahmp)           :: HeatPrcpAirToGrd    ! precipitation advected heat - air to ground [W/m2]

! --------------------------------------------------------------------
    associate(                                                                    &
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight  ,& ! in,  air temperature [K] at reference height
              TemperatureCanopy       => noahmp%energy%state%TemperatureCanopy   ,& ! in,  vegetation temperature [K]
              TemperatureGrd          => noahmp%energy%state%TemperatureGrd      ,& ! in,  ground temperature [K]
              VegFrac                 => noahmp%energy%state%VegFrac             ,& ! in,  greeness vegetation fraction
              RainfallRefHeight       => noahmp%water%flux%RainfallRefHeight     ,& ! in,  total liquid rainfall [mm/s] before interception
              SnowfallRefHeight       => noahmp%water%flux%SnowfallRefHeight     ,& ! in,  total snowfall [mm/s] before interception
              DripCanopyRain          => noahmp%water%flux%DripCanopyRain        ,& ! in,  drip rate for intercepted rain [mm/s]
              ThroughfallRain         => noahmp%water%flux%ThroughfallRain       ,& ! in,  throughfall for rain [mm/s]
              DripCanopySnow          => noahmp%water%flux%DripCanopySnow        ,& ! in,  drip (unloading) rate for intercepted snow [mm/s]
              ThroughfallSnow         => noahmp%water%flux%ThroughfallSnow       ,& ! in,  throughfall of snowfall [mm/s]
              HeatPrecipAdvCanopy     => noahmp%energy%flux%HeatPrecipAdvCanopy  ,& ! out, precipitation advected heat - vegetation net [W/m2]
              HeatPrecipAdvVegGrd     => noahmp%energy%flux%HeatPrecipAdvVegGrd  ,& ! out, precipitation advected heat - under canopy net [W/m2]
              HeatPrecipAdvBareGrd    => noahmp%energy%flux%HeatPrecipAdvBareGrd  & ! out, precipitation advected heat - bare ground net [W/m2]
             )
! ----------------------------------------------------------------------

    ! initialization
    HeatPrcpAirToCan     = 0.0
    HeatPrcpCanToGrd     = 0.0
    HeatPrcpAirToGrd     = 0.0
    HeatPrecipAdvCanopy  = 0.0
    HeatPrecipAdvVegGrd  = 0.0
    HeatPrecipAdvBareGrd = 0.0

    ! Heat advection for liquid rainfall
    HeatPrcpAirToCan = VegFrac * RainfallRefHeight * (ConstHeatCapacWater/1000.0) * (TemperatureAirRefHeight-TemperatureCanopy)
    HeatPrcpCanToGrd = DripCanopyRain * (ConstHeatCapacWater/1000.0) * (TemperatureCanopy-TemperatureGrd)
    HeatPrcpAirToGrd = ThroughfallRain * (ConstHeatCapacWater/1000.0) * (TemperatureAirRefHeight-TemperatureGrd)

    ! Heat advection for snowfall
    HeatPrcpAirToCan = HeatPrcpAirToCan + &
                       VegFrac * SnowfallRefHeight * (ConstHeatCapacIce/1000.0) * (TemperatureAirRefHeight-TemperatureCanopy)
    HeatPrcpCanToGrd = HeatPrcpCanToGrd + &
                       DripCanopySnow * (ConstHeatCapacIce/1000.0) * (TemperatureCanopy-TemperatureGrd)
    HeatPrcpAirToGrd = HeatPrcpAirToGrd + &
                       ThroughfallSnow * (ConstHeatCapacIce/1000.0) * (TemperatureAirRefHeight-TemperatureGrd)

    ! net heat advection
    HeatPrecipAdvCanopy  = HeatPrcpAirToCan - HeatPrcpCanToGrd
    HeatPrecipAdvVegGrd  = HeatPrcpCanToGrd
    HeatPrecipAdvBareGrd = HeatPrcpAirToGrd

    ! adjust for VegFrac
    if ( (VegFrac > 0.0) .and. (VegFrac < 1.0) ) then
       HeatPrecipAdvVegGrd  = HeatPrecipAdvVegGrd / VegFrac                  ! these will be multiplied by fraction later
       HeatPrecipAdvBareGrd = HeatPrecipAdvBareGrd / (1.0-VegFrac)
    elseif ( VegFrac <= 0.0 ) then
       HeatPrecipAdvBareGrd = HeatPrecipAdvVegGrd + HeatPrecipAdvBareGrd     ! for case of canopy getting buried
       HeatPrecipAdvVegGrd  = 0.0
       HeatPrecipAdvCanopy  = 0.0
    elseif ( VegFrac >= 1.0 ) then
       HeatPrecipAdvBareGrd = 0.0
    endif

    ! Put some artificial limits here for stability
    HeatPrecipAdvCanopy  = max(HeatPrecipAdvCanopy , -20.0)
    HeatPrecipAdvCanopy  = min(HeatPrecipAdvCanopy ,  20.0)
    HeatPrecipAdvVegGrd  = max(HeatPrecipAdvVegGrd , -20.0)
    HeatPrecipAdvVegGrd  = min(HeatPrecipAdvVegGrd ,  20.0)
    HeatPrecipAdvBareGrd = max(HeatPrecipAdvBareGrd, -20.0)
    HeatPrecipAdvBareGrd = min(HeatPrecipAdvBareGrd,  20.0)

    end associate

  end subroutine PrecipitationHeatAdvect

end module PrecipitationHeatAdvectMod
