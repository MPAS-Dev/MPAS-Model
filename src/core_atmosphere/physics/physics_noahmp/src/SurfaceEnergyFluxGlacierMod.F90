module SurfaceEnergyFluxGlacierMod

!!! Compute surface energy fluxes and budget for bare ground (glacier)
!!! Use newton-raphson iteration to solve for ground temperatures
!!! Surface energy balance (bare soil):
!!! Ground level: -RadSwAbsGrd - HeatPrecipAdvBareGrd + RadLwNetBareGrd + HeatSensibleBareGrd + HeatLatentBareGrd + HeatGroundBareGrd = 0

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use VaporPressureSaturationMod,    only : VaporPressureSaturation
  use ResistanceBareGroundMostMod,   only : ResistanceBareGroundMOST

  implicit none

contains

  subroutine SurfaceEnergyFluxGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GLACIER_FLUX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! local variables
    integer                               :: IndIter                 ! iteration index
    integer                               :: MoStabParaSgn           ! number of times MoStabParaBare changes sign
    integer, parameter                    :: NumIter = 5             ! number of iterations for surface temperature
    real(kind=kind_noahmp)                :: TemperatureGrdChg       ! change in ground temperature [K], last iteration
    real(kind=kind_noahmp)                :: LwRadCoeff              ! coefficients for longwave radiation as function of ts**4
    real(kind=kind_noahmp)                :: ShCoeff                 ! coefficients for sensible heat as function of ts
    real(kind=kind_noahmp)                :: LhCoeff                 ! coefficients for latent heat as function of ts
    real(kind=kind_noahmp)                :: GrdHeatCoeff            ! coefficients for st as function of ts
    real(kind=kind_noahmp)                :: ExchCoeffShTmp          ! temporary sensible heat exchange coefficient [m/s]
    real(kind=kind_noahmp)                :: ExchCoeffMomTmp         ! temporary momentum heat exchange coefficient [m/s]
    real(kind=kind_noahmp)                :: MoistureFluxSfc         ! moisture flux
    real(kind=kind_noahmp)                :: VapPresSatWatTmp        ! saturated vapor pressure for water
    real(kind=kind_noahmp)                :: VapPresSatIceTmp        ! saturated vapor pressure for ice
    real(kind=kind_noahmp)                :: VapPresSatWatTmpD       ! saturated vapor pressure gradient with ground temp. [Pa/K] for water
    real(kind=kind_noahmp)                :: VapPresSatIceTmpD       ! saturated vapor pressure gradient with ground temp. [Pa/K] for ice
    real(kind=kind_noahmp)                :: FluxTotCoeff            ! temporary total coefficients for all energy flux
    real(kind=kind_noahmp)                :: EnergyResTmp            ! temporary energy residual
    real(kind=kind_noahmp)                :: HeatSensibleTmp         ! temporary sensible heat flux [W/m2]
    real(kind=kind_noahmp)                :: TempTmp                 ! temporary temperature
    real(kind=kind_noahmp)                :: TempUnitConv            ! Kelvin to degree Celsius with limit -50 to +50
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIceTmp  ! temporary glacier ice content [m3/m3]
! local statement function
    TempUnitConv(TempTmp) = min(50.0, max(-50.0, (TempTmp-ConstFreezePoint)))

! --------------------------------------------------------------------
    associate(                                                                        &
              NumSoilLayer            => noahmp%config%domain%NumSoilLayer           ,& ! in,    number of glacier/soil layers
              NumSnowLayerNeg         => noahmp%config%domain%NumSnowLayerNeg        ,& ! in,    actual number of snow layers (negative)
              ThicknessSnowSoilLayer  => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,    thickness of snow/soil layers [m]
              OptSnowSoilTempTime     => noahmp%config%nmlist%OptSnowSoilTempTime    ,& ! in,    options for snow/soil temperature time scheme (only layer 1)
              OptGlacierTreatment     => noahmp%config%nmlist%OptGlacierTreatment    ,& ! in,    options for glacier treatment 
              RadLwDownRefHeight      => noahmp%forcing%RadLwDownRefHeight           ,& ! in,    downward longwave radiation [W/m2] at reference height
              WindEastwardRefHeight   => noahmp%forcing%WindEastwardRefHeight        ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight  => noahmp%forcing%WindNorthwardRefHeight       ,& ! in,    wind speed [m/s] in northward direction at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight      ,& ! in,    air temperature [K] at reference height
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight         ,& ! in,    air pressure [Pa] at reference height
              SnowDepth               => noahmp%water%state%SnowDepth                ,& ! in,    snow depth [m]
              SoilMoisture            => noahmp%water%state%SoilMoisture             ,& ! in,    total glacier/soil water content [m3/m3]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,    glacier/soil water content [m3/m3]
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! in,    solar radiation absorbed by ground [W/m2]
              HeatPrecipAdvBareGrd    => noahmp%energy%flux%HeatPrecipAdvBareGrd     ,& ! in,    precipitation advected heat - bare ground net [W/m2]
              WindSpdRefHeight        => noahmp%energy%state%WindSpdRefHeight        ,& ! in,    wind speed [m/s] at reference height
              PressureVaporRefHeight  => noahmp%energy%state%PressureVaporRefHeight  ,& ! in,    vapor pressure air [Pa] at reference height
              SpecHumidityRefHeight   => noahmp%forcing%SpecHumidityRefHeight        ,& ! in,    specific humidity [kg/kg] at reference height
              DensityAirRefHeight     => noahmp%energy%state%DensityAirRefHeight     ,& ! in,    density air [kg/m3]
              RelHumidityGrd          => noahmp%energy%state%RelHumidityGrd          ,& ! in,    raltive humidity in surface soil/snow air space
              EmissivityGrd           => noahmp%energy%state%EmissivityGrd           ,& ! in,    ground emissivity
              TemperatureSoilSnow     => noahmp%energy%state%TemperatureSoilSnow     ,& ! in,    snow and soil layer temperature [K]
              ThermConductSoilSnow    => noahmp%energy%state%ThermConductSoilSnow    ,& ! in,    thermal conductivity [W/m/K] for all soil & snow
              ResistanceGrdEvap       => noahmp%energy%state%ResistanceGrdEvap       ,& ! in,    ground surface resistance [s/m] to evaporation
              RoughLenMomGrd          => noahmp%energy%state%RoughLenMomGrd          ,& ! in,    roughness length, momentum, ground [m]
              LatHeatVapGrd           => noahmp%energy%state%LatHeatVapGrd           ,& ! in,    latent heat of vaporization/subli [J/kg], ground
              PsychConstGrd           => noahmp%energy%state%PsychConstGrd           ,& ! in,    psychrometric constant [Pa/K], ground
              SpecHumiditySfc         => noahmp%energy%state%SpecHumiditySfc         ,& ! inout, specific humidity at surface
              TemperatureGrdBare      => noahmp%energy%state%TemperatureGrdBare      ,& ! inout, bare ground temperature [K]
              ExchCoeffMomBare        => noahmp%energy%state%ExchCoeffMomBare        ,& ! inout, momentum exchange coeff [m/s], above ZeroPlaneDisp, bare ground
              ExchCoeffShBare         => noahmp%energy%state%ExchCoeffShBare         ,& ! inout, heat exchange coeff [m/s], above ZeroPlaneDisp, bare ground
              WindStressEwBare        => noahmp%energy%state%WindStressEwBare        ,& ! out,   wind stress: east-west [N/m2] bare ground
              WindStressNsBare        => noahmp%energy%state%WindStressNsBare        ,& ! out,   wind stress: north-south [N/m2] bare ground
              TemperatureAir2mBare    => noahmp%energy%state%TemperatureAir2mBare    ,& ! out,   2 m height air temperature [K] bare ground
              SpecHumidity2mBare      => noahmp%energy%state%SpecHumidity2mBare      ,& ! out,   bare ground 2-m specific humidity [kg/kg]
              ExchCoeffSh2mBare       => noahmp%energy%state%ExchCoeffSh2mBare       ,& ! out,   bare ground 2-m sensible heat exchange coefficient [m/s]
              FrictionVelBare         => noahmp%energy%state%FrictionVelBare         ,& ! out,   friction velocity [m/s], vegetated
              RoughLenShBareGrd       => noahmp%energy%state%RoughLenShBareGrd       ,& ! out,   roughness length [m], sensible heat, bare ground
              ResistanceLhBareGrd     => noahmp%energy%state%ResistanceLhBareGrd     ,& ! out,   aerodynamic resistance for water vapor [s/m], bare ground
              ResistanceShBareGrd     => noahmp%energy%state%ResistanceShBareGrd     ,& ! out,   aerodynamic resistance for sensible heat [s/m], bare ground
              ResistanceMomBareGrd    => noahmp%energy%state%ResistanceMomBareGrd    ,& ! out,   aerodynamic resistance for momentum [s/m], bare ground
              VapPresSatGrdBare       => noahmp%energy%state%VapPresSatGrdBare       ,& ! out,   bare ground saturation vapor pressure at TemperatureGrd [Pa]
              VapPresSatGrdBareTempD  => noahmp%energy%state%VapPresSatGrdBareTempD  ,& ! out,   bare ground d(VapPresSatGrdBare)/dt at TemperatureGrd [Pa/K]
              MoStabParaBare          => noahmp%energy%state%MoStabParaBare          ,& ! out,   Monin-Obukhov stability (z/L), above ZeroPlaneDisp, bare ground
              MoStabCorrShBare2m      => noahmp%energy%state%MoStabCorrShBare2m      ,& ! out,   M-O sen heat stability correction, 2m, bare ground
              RadLwNetBareGrd         => noahmp%energy%flux%RadLwNetBareGrd          ,& ! out,   net longwave rad [W/m2] bare ground (+ to atm)
              HeatSensibleBareGrd     => noahmp%energy%flux%HeatSensibleBareGrd      ,& ! out,   sensible heat flux [W/m2] bare ground (+ to atm)
              HeatLatentBareGrd       => noahmp%energy%flux%HeatLatentBareGrd        ,& ! out,   latent heat flux [W/m2] bare ground (+ to atm)
              HeatGroundBareGrd       => noahmp%energy%flux%HeatGroundBareGrd         & ! out,   bare ground heat flux [W/m2] (+ to soil/snow)
             )
! ----------------------------------------------------------------------

    ! initialization (including variables that do not depend on stability iteration)
    if (.not. allocated(SoilIceTmp)) allocate(SoilIceTmp(1:NumSoilLayer))
    SoilIceTmp         = 0.0
    TemperatureGrdChg  = 0.0
    MoStabParaBare     = 0.0
    MoStabParaSgn      = 0
    MoStabCorrShBare2m = 0.0
    HeatSensibleTmp    = 0.0
    MoistureFluxSfc    = 0.0
    FrictionVelBare    = 0.1
    LwRadCoeff         = EmissivityGrd * ConstStefanBoltzmann
    GrdHeatCoeff       = 2.0*ThermConductSoilSnow(NumSnowLayerNeg+1)/ThicknessSnowSoilLayer(NumSnowLayerNeg+1)

    ! begin stability iteration for ground temperature and flux
    loop3: do IndIter = 1, NumIter

       ! ground roughness length
       RoughLenShBareGrd = RoughLenMomGrd

       ! aerodyn resistances between heights reference height and d+z0v
       call ResistanceBareGroundMOST(noahmp, IndIter, HeatSensibleTmp, MoStabParaSgn)

       ! conductance variables for diagnostics         
       ExchCoeffMomTmp = 1.0 / ResistanceMomBareGrd
       ExchCoeffShTmp  = 1.0 / ResistanceShBareGrd

       ! ES and d(ES)/dt evaluated at TemperatureGrd
       TempTmp = TempUnitConv(TemperatureGrdBare)
       call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, VapPresSatIceTmpD)
       if ( TempTmp > 0.0 ) then
          VapPresSatGrdBare      = VapPresSatWatTmp
          VapPresSatGrdBareTempD = VapPresSatWatTmpD
       else
          VapPresSatGrdBare      = VapPresSatIceTmp
          VapPresSatGrdBareTempD = VapPresSatIceTmpD
       endif

       ! ground fluxes and temperature change
       ShCoeff = DensityAirRefHeight * ConstHeatCapacAir / ResistanceShBareGrd
       if ( (SnowDepth > 0.0) .or. (OptGlacierTreatment == 1) ) then
          LhCoeff = DensityAirRefHeight * ConstHeatCapacAir / PsychConstGrd / (ResistanceGrdEvap+ResistanceLhBareGrd)
       else
          LhCoeff = 0.0   ! don't allow any sublimation of glacier in OptGlacierTreatment=2
       endif
       RadLwNetBareGrd     = LwRadCoeff * TemperatureGrdBare**4 - EmissivityGrd * RadLwDownRefHeight
       HeatSensibleBareGrd = ShCoeff * (TemperatureGrdBare - TemperatureAirRefHeight)
       HeatLatentBareGrd   = LhCoeff * (VapPresSatGrdBare*RelHumidityGrd - PressureVaporRefHeight)
       HeatGroundBareGrd   = GrdHeatCoeff * (TemperatureGrdBare - TemperatureSoilSnow(NumSnowLayerNeg+1))
       EnergyResTmp        = RadSwAbsGrd - RadLwNetBareGrd - HeatSensibleBareGrd - &
                             HeatLatentBareGrd - HeatGroundBareGrd + HeatPrecipAdvBareGrd
       FluxTotCoeff        = 4.0*LwRadCoeff*TemperatureGrdBare**3 + ShCoeff + LhCoeff*VapPresSatGrdBareTempD + GrdHeatCoeff
       TemperatureGrdChg   = EnergyResTmp / FluxTotCoeff
       RadLwNetBareGrd     = RadLwNetBareGrd + 4.0 * LwRadCoeff * TemperatureGrdBare**3 * TemperatureGrdChg
       HeatSensibleBareGrd = HeatSensibleBareGrd + ShCoeff * TemperatureGrdChg
       HeatLatentBareGrd   = HeatLatentBareGrd + LhCoeff * VapPresSatGrdBareTempD * TemperatureGrdChg
       HeatGroundBareGrd   = HeatGroundBareGrd + GrdHeatCoeff * TemperatureGrdChg
       TemperatureGrdBare  = TemperatureGrdBare + TemperatureGrdChg  ! update ground temperature

       ! for computing M-O length
       HeatSensibleTmp     = ShCoeff * (TemperatureGrdBare - TemperatureAirRefHeight)

       ! update specific humidity
       TempTmp = TempUnitConv(TemperatureGrdBare)
       call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, VapPresSatIceTmpD)
       if ( TempTmp > 0.0 ) then
          VapPresSatGrdBare = VapPresSatWatTmp
       else
          VapPresSatGrdBare = VapPresSatIceTmp
       endif
       SpecHumiditySfc      = 0.622 * (VapPresSatGrdBare*RelHumidityGrd) / &
                              (PressureAirRefHeight - 0.378 * (VapPresSatGrdBare*RelHumidityGrd))
       MoistureFluxSfc      = (SpecHumiditySfc - SpecHumidityRefHeight) * LhCoeff * PsychConstGrd / ConstHeatCapacAir

    enddo loop3 ! end stability iteration

    ! if snow on ground and TemperatureGrdBare > freezing point: reset TemperatureGrdBare = freezing point. reevaluate ground fluxes.
    SoilIceTmp = SoilMoisture - SoilLiqWater
    if ( (OptSnowSoilTempTime == 1) .or. (OptSnowSoilTempTime == 3) ) then
       if ( (maxval(SoilIceTmp) > 0.0 .or. SnowDepth > 0.05) .and. &
            (TemperatureGrdBare > ConstFreezePoint) .and. (OptGlacierTreatment == 1) ) then
          TemperatureGrdBare  = ConstFreezePoint
          TempTmp             = TempUnitConv(TemperatureGrdBare) ! MB: recalculate VapPresSatGrdBare
          call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, VapPresSatIceTmpD)
          VapPresSatGrdBare   = VapPresSatIceTmp
          SpecHumiditySfc     = 0.622 * (VapPresSatGrdBare*RelHumidityGrd) / &
                                (PressureAirRefHeight - 0.378 * (VapPresSatGrdBare*RelHumidityGrd))
          MoistureFluxSfc     = (SpecHumiditySfc - SpecHumidityRefHeight) * LhCoeff * PsychConstGrd / ConstHeatCapacAir
          RadLwNetBareGrd     = LwRadCoeff * TemperatureGrdBare**4 - EmissivityGrd * RadLwDownRefHeight
          HeatSensibleBareGrd = ShCoeff * (TemperatureGrdBare - TemperatureAirRefHeight)
          HeatLatentBareGrd   = LhCoeff * (VapPresSatGrdBare*RelHumidityGrd - PressureVaporRefHeight)
          HeatGroundBareGrd   = RadSwAbsGrd + HeatPrecipAdvBareGrd - &
                                (RadLwNetBareGrd + HeatSensibleBareGrd + HeatLatentBareGrd)
       endif
    endif

    ! wind stresses
    WindStressEwBare = -DensityAirRefHeight * ExchCoeffMomBare * WindSpdRefHeight * WindEastwardRefHeight
    WindStressNsBare = -DensityAirRefHeight * ExchCoeffMomBare * WindSpdRefHeight * WindNorthwardRefHeight

    ! 2m air temperature
    ExchCoeffSh2mBare = FrictionVelBare * ConstVonKarman / &
                        (log((2.0+RoughLenShBareGrd)/RoughLenShBareGrd) - MoStabCorrShBare2m)
    if ( ExchCoeffSh2mBare < 1.0e-5 ) then
       TemperatureAir2mBare = TemperatureGrdBare
       SpecHumidity2mBare   = SpecHumiditySfc
    else
       TemperatureAir2mBare = TemperatureGrdBare - HeatSensibleBareGrd / &
                              (DensityAirRefHeight*ConstHeatCapacAir) * 1.0 / ExchCoeffSh2mBare
       SpecHumidity2mBare   = SpecHumiditySfc - HeatLatentBareGrd /  &
                              (LatHeatVapGrd*DensityAirRefHeight) * (1.0/ExchCoeffSh2mBare + ResistanceGrdEvap)
    endif

    ! update ExchCoeffShBare 
    ExchCoeffShBare = ExchCoeffShTmp

    ! deallocate local arrays to avoid memory leaks
    deallocate(SoilIceTmp)

    end associate

  end subroutine SurfaceEnergyFluxGlacier

end module SurfaceEnergyFluxGlacierMod
