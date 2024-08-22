module EnergyMainGlacierMod

!!! Main energy module for glacier points including all energy relevant processes
!!! snow thermal property -> radiation -> ground heat flux -> snow temperature solver -> snow/ice phase change

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowCoverGlacierMod,                   only : SnowCoverGlacier
  use GroundRoughnessPropertyGlacierMod,     only : GroundRoughnessPropertyGlacier
  use GroundThermalPropertyGlacierMod,       only : GroundThermalPropertyGlacier
  use SurfaceAlbedoGlacierMod,               only : SurfaceAlbedoGlacier
  use SurfaceRadiationGlacierMod,            only : SurfaceRadiationGlacier
  use SurfaceEmissivityGlacierMod,           only : SurfaceEmissivityGlacier
  use ResistanceGroundEvaporationGlacierMod, only : ResistanceGroundEvaporationGlacier
  use PsychrometricVariableGlacierMod,       only : PsychrometricVariableGlacier
  use SurfaceEnergyFluxGlacierMod,           only : SurfaceEnergyFluxGlacier
  use GlacierTemperatureMainMod,             only : GlacierTemperatureMain
  use GlacierPhaseChangeMod,                 only : GlacierPhaseChange

  implicit none

contains

  subroutine EnergyMainGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ENERGY_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

! --------------------------------------------------------------------
    associate(                                                                    &
              RadLwDownRefHeight     => noahmp%forcing%RadLwDownRefHeight        ,& ! in,    downward longwave radiation [W/m2] at reference height
              RadSwDownRefHeight     => noahmp%forcing%RadSwDownRefHeight        ,& ! in,    downward shortwave radiation [W/m2] at reference height
              OptSnowSoilTempTime    => noahmp%config%nmlist%OptSnowSoilTempTime ,& ! in,    options for snow/soil temperature time scheme
              HeatPrecipAdvBareGrd   => noahmp%energy%flux%HeatPrecipAdvBareGrd  ,& ! in,    precipitation advected heat - bare ground net [W/m2]
              TemperatureSfc         => noahmp%energy%state%TemperatureSfc       ,& ! inout, surface temperature [K]
              TemperatureGrd         => noahmp%energy%state%TemperatureGrd       ,& ! inout, ground temperature [K]
              SpecHumiditySfc        => noahmp%energy%state%SpecHumiditySfc      ,& ! inout, specific humidity at bare surface
              SpecHumiditySfcMean    => noahmp%energy%state%SpecHumiditySfcMean  ,& ! inout, specific humidity at surface grid mean
              ExchCoeffMomSfc        => noahmp%energy%state%ExchCoeffMomSfc      ,& ! inout, exchange coefficient [m/s] for momentum, surface, grid mean
              ExchCoeffShSfc         => noahmp%energy%state%ExchCoeffShSfc       ,& ! inout, exchange coefficient [m/s] for heat, surface, grid mean
              SnowDepth              => noahmp%water%state%SnowDepth             ,& ! inout, snow depth [m]
              RoughLenMomSfcToAtm    => noahmp%energy%state%RoughLenMomSfcToAtm  ,& ! out,   roughness length, momentum, surface, sent to coupled model
              WindStressEwSfc        => noahmp%energy%state%WindStressEwSfc      ,& ! out,   wind stress: east-west [N/m2] grid mean
              WindStressNsSfc        => noahmp%energy%state%WindStressNsSfc      ,& ! out,   wind stress: north-south [N/m2] grid mean
              TemperatureRadSfc      => noahmp%energy%state%TemperatureRadSfc    ,& ! out,   radiative temperature [K]
              TemperatureAir2m       => noahmp%energy%state%TemperatureAir2m     ,& ! out,   grid mean 2-m air temperature [K]
              TemperatureAir2mBare   => noahmp%energy%state%TemperatureAir2mBare ,& ! out,   2 m height air temperature [K] bare ground
              EmissivitySfc          => noahmp%energy%state%EmissivitySfc        ,& ! out,   surface emissivity
              RoughLenMomGrd         => noahmp%energy%state%RoughLenMomGrd       ,& ! out,   roughness length, momentum, ground [m]
              WindStressEwBare       => noahmp%energy%state%WindStressEwBare     ,& ! out,   wind stress: east-west [N/m2] bare ground
              WindStressNsBare       => noahmp%energy%state%WindStressNsBare     ,& ! out,   wind stress: north-south [N/m2] bare ground
              SpecHumidity2mBare     => noahmp%energy%state%SpecHumidity2mBare   ,& ! out,   bare ground 2-m water vapor mixing ratio
              SpecHumidity2m         => noahmp%energy%state%SpecHumidity2m       ,& ! out,   grid mean 2-m water vapor mixing ratio
              TemperatureGrdBare     => noahmp%energy%state%TemperatureGrdBare   ,& ! out,   bare ground temperature [K]
              ExchCoeffMomBare       => noahmp%energy%state%ExchCoeffMomBare     ,& ! out,   exchange coeff [m/s] for momentum, above ZeroPlaneDisp, bare ground
              ExchCoeffShBare        => noahmp%energy%state%ExchCoeffShBare      ,& ! out,   exchange coeff [m/s] for heat, above ZeroPlaneDisp, bare ground
              AlbedoSfc              => noahmp%energy%state%AlbedoSfc            ,& ! out,   total shortwave surface albedo
              RadSwReflSfc           => noahmp%energy%flux%RadSwReflSfc          ,& ! out,   total reflected solar radiation [W/m2]
              RadLwNetSfc            => noahmp%energy%flux%RadLwNetSfc           ,& ! out,   total net longwave rad [W/m2] (+ to atm)
              HeatSensibleSfc        => noahmp%energy%flux%HeatSensibleSfc       ,& ! out,   total sensible heat [W/m2] (+ to atm)
              HeatLatentGrd          => noahmp%energy%flux%HeatLatentGrd         ,& ! out,   total ground latent heat [W/m2] (+ to atm)
              HeatGroundTot          => noahmp%energy%flux%HeatGroundTot         ,& ! out,   total ground heat flux [W/m2] (+ to soil/snow)
              HeatPrecipAdvSfc       => noahmp%energy%flux%HeatPrecipAdvSfc      ,& ! out,   precipitation advected heat - total [W/m2]
              RadLwEmitSfc           => noahmp%energy%flux%RadLwEmitSfc          ,& ! out,   emitted outgoing IR [W/m2]
              RadLwNetBareGrd        => noahmp%energy%flux%RadLwNetBareGrd       ,& ! out,   net longwave rad [W/m2] bare ground (+ to atm)
              HeatSensibleBareGrd    => noahmp%energy%flux%HeatSensibleBareGrd   ,& ! out,   sensible heat flux [W/m2] bare ground (+ to atm)
              HeatLatentBareGrd      => noahmp%energy%flux%HeatLatentBareGrd     ,& ! out,   latent heat flux [W/m2] bare ground (+ to atm)
              HeatGroundBareGrd      => noahmp%energy%flux%HeatGroundBareGrd      & ! out,   bare ground heat flux [W/m2] (+ to soil/snow)
             )
! ----------------------------------------------------------------------

    ! glaicer snow cover fraction
    call SnowCoverGlacier(noahmp)

    ! ground and surface roughness length and reference height
    call GroundRoughnessPropertyGlacier(noahmp)

    ! Thermal properties of snow and glacier ice
    call GroundThermalPropertyGlacier(noahmp)

    ! Glacier surface shortwave abeldo
    call SurfaceAlbedoGlacier(noahmp)

    ! Glacier surface shortwave radiation
    call SurfaceRadiationGlacier(noahmp)

    ! longwave emissivity for glacier surface
    call SurfaceEmissivityGlacier(noahmp)

    ! glacier surface resistance for ground evaporation/sublimation
    call ResistanceGroundEvaporationGlacier(noahmp)

    ! set psychrometric variable/constant
    call PsychrometricVariableGlacier(noahmp)

    ! temperatures and energy fluxes of glacier ground
    TemperatureGrdBare = TemperatureGrd
    ExchCoeffMomBare   = ExchCoeffMomSfc
    ExchCoeffShBare    = ExchCoeffShSfc
    call SurfaceEnergyFluxGlacier(noahmp)

    ! assign glacier bare ground quantity to grid-level quantity
    ! Energy balance at glacier (bare) ground: 
    ! RadSwAbsGrd + HeatPrecipAdvBareGrd = RadLwNetBareGrd + HeatSensibleBareGrd + HeatLatentBareGrd + HeatGroundBareGrd
    WindStressEwSfc     = WindStressEwBare
    WindStressNsSfc     = WindStressNsBare
    RadLwNetSfc         = RadLwNetBareGrd
    HeatSensibleSfc     = HeatSensibleBareGrd
    HeatLatentGrd       = HeatLatentBareGrd
    HeatGroundTot       = HeatGroundBareGrd
    TemperatureGrd      = TemperatureGrdBare
    TemperatureAir2m    = TemperatureAir2mBare
    HeatPrecipAdvSfc    = HeatPrecipAdvBareGrd
    TemperatureSfc      = TemperatureGrd
    ExchCoeffMomSfc     = ExchCoeffMomBare
    ExchCoeffShSfc      = ExchCoeffShBare
    SpecHumiditySfcMean = SpecHumiditySfc
    SpecHumidity2m      = SpecHumidity2mBare
    RoughLenMomSfcToAtm = RoughLenMomGrd

    ! emitted longwave radiation and physical check
    RadLwEmitSfc = RadLwDownRefHeight + RadLwNetSfc
    if ( RadLwEmitSfc <= 0.0 ) then
       write(*,*) "emitted longwave <0; skin T may be wrong due to inconsistent"
       write(*,*) "RadLwDownRefHeight = ", RadLwDownRefHeight, "RadLwNetSfc = ", RadLwNetSfc, "SnowDepth = ", SnowDepth
       stop "Error: Longwave radiation budget problem in NoahMP LSM"
    endif

    ! radiative temperature: subtract from the emitted IR the
    ! reflected portion of the incoming longwave radiation, so just
    ! considering the IR originating/emitted in the ground system.
    ! Old TemperatureRadSfc calculation not taking into account Emissivity:
    ! TemperatureRadSfc = (RadLwEmitSfc/ConstStefanBoltzmann)**0.25
    TemperatureRadSfc = ((RadLwEmitSfc - (1.0 - EmissivitySfc)*RadLwDownRefHeight) / &
                         (EmissivitySfc * ConstStefanBoltzmann)) ** 0.25

    ! compute snow and glacier ice temperature
    call GlacierTemperatureMain(noahmp)

    ! adjusting suface temperature based on snow condition
    if ( OptSnowSoilTempTime == 2 ) then
       if ( (SnowDepth > 0.05) .and. (TemperatureGrd > ConstFreezePoint) ) then
          TemperatureGrdBare = ConstFreezePoint
          TemperatureGrd     = TemperatureGrdBare
          TemperatureSfc     = TemperatureGrdBare
       endif
    endif

    ! Phase change and Energy released or consumed by snow & glacier ice
    call GlacierPhaseChange(noahmp)

    ! update total surface albedo
    if ( RadSwDownRefHeight > 0.0 ) then
       AlbedoSfc = RadSwReflSfc / RadSwDownRefHeight
    else
       AlbedoSfc = undefined_real
    endif

    end associate

  end subroutine EnergyMainGlacier

end module EnergyMainGlacierMod
