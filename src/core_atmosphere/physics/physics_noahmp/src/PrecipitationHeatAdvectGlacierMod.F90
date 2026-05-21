module PrecipitationHeatAdvectGlacierMod

!!! Estimate heat flux advected from precipitation to glacier ground

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PrecipitationHeatAdvectGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: none (adapted from PRECIP_HEAT)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: HeatPrcpAirToGrd    ! precipitation advected heat - air to ground [W/m2]

! --------------------------------------------------------------------
    associate(                                                                    &
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight  ,& ! in,  air temperature [K] at reference height
              TemperatureGrd          => noahmp%energy%state%TemperatureGrd      ,& ! in,  ground temperature [K]
              RainfallRefHeight       => noahmp%water%flux%RainfallRefHeight     ,& ! in,  total liquid rainfall [mm/s] before interception
              SnowfallRefHeight       => noahmp%water%flux%SnowfallRefHeight     ,& ! in,  total snowfall [mm/s] before interception
              SnowfallGround          => noahmp%water%flux%SnowfallGround        ,& ! out, snowfall at ground surface [mm/s]
              RainfallGround          => noahmp%water%flux%RainfallGround        ,& ! out, rainfall at ground surface [mm/s]
              HeatPrecipAdvBareGrd    => noahmp%energy%flux%HeatPrecipAdvBareGrd  & ! out, precipitation advected heat - bare ground net [W/m2]
             )
! ----------------------------------------------------------------------

    ! initialization
    HeatPrcpAirToGrd     = 0.0
    HeatPrecipAdvBareGrd = 0.0
    RainfallGround       = RainfallRefHeight
    SnowfallGround       = SnowfallRefHeight

    ! Heat advection for liquid rainfall
    HeatPrcpAirToGrd     = RainfallGround * (ConstHeatCapacWater/1000.0) * (TemperatureAirRefHeight - TemperatureGrd)

    ! Heat advection for snowfall
    HeatPrcpAirToGrd     = HeatPrcpAirToGrd + &
                           SnowfallGround * (ConstHeatCapacIce/1000.0) * (TemperatureAirRefHeight - TemperatureGrd)

    ! net heat advection
    HeatPrecipAdvBareGrd = HeatPrcpAirToGrd

    ! Put some artificial limits here for stability
    HeatPrecipAdvBareGrd = max(HeatPrecipAdvBareGrd, -20.0)
    HeatPrecipAdvBareGrd = min(HeatPrecipAdvBareGrd,  20.0)

    end associate

  end subroutine PrecipitationHeatAdvectGlacier

end module PrecipitationHeatAdvectGlacierMod
