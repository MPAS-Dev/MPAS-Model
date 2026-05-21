module SurfaceEnergyFluxVegetatedMod

!!! Compute surface energy fluxes and budget for vegetated surface
!!! Use newton-raphson iteration to solve for vegetation and ground temperatures
!!! Surface energy balance:
!!! Canopy level: -RadSwAbsVeg - HeatPrecipAdvCanopy + RadLwNetCanopy + HeatSensibleCanopy + HeatLatentCanEvap + HeatLatentCanTransp + HeatCanStorageChg = 0
!!! Ground level: -RadSwAbsGrd - HeatPrecipAdvVegGrd + RadLwNetVegGrd + HeatSensibleVegGrd + HeatLatentVegGrd + HeatGroundVegGrd = 0

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use VaporPressureSaturationMod,          only : VaporPressureSaturation
  use ResistanceAboveCanopyMostMod,        only : ResistanceAboveCanopyMOST
  use ResistanceAboveCanopyChen97Mod,      only : ResistanceAboveCanopyChen97
  use ResistanceLeafToGroundMod,           only : ResistanceLeafToGround
  use ResistanceCanopyStomataBallBerryMod, only : ResistanceCanopyStomataBallBerry
  use ResistanceCanopyStomataJarvisMod,    only : ResistanceCanopyStomataJarvis

  implicit none

contains

  subroutine SurfaceEnergyFluxVegetated(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: VEGE_FLUX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                               :: IndIter                 ! iteration index
    integer                               :: LastIter                ! Last iteration
    integer                               :: MoStabParaSgn           ! number of times MoStabParaAbvCan changes sign
    integer                               :: IndexShade              ! index for sunlit/shaded (0=sunlit;1=shaded)
    integer, parameter                    :: NumIterC = 20           ! number of iterations for surface temperature (5~20)
    integer, parameter                    :: NumIterG = 5            ! number of iterations for ground temperature (3~5)
    real(kind=kind_noahmp)                :: ExchCoeffShAbvCanTmp    ! sensible heat conductance, canopy air to reference height air [m/s]
    real(kind=kind_noahmp)                :: TemperatureCanChg       ! change in tv, last iteration [K]
    real(kind=kind_noahmp)                :: TemperatureGrdChg       ! change in tg, last iteration [K]
    real(kind=kind_noahmp)                :: LwCoeffAir              ! coefficients for longwave emission as function of ts**4
    real(kind=kind_noahmp)                :: LwCoeffCan              ! coefficients for longwave emission as function of ts**4
    real(kind=kind_noahmp)                :: ShCoeff                 ! coefficients for sensible heat as function of ts
    real(kind=kind_noahmp)                :: LhCoeff                 ! coefficients for latent heat as function of ts
    real(kind=kind_noahmp)                :: GrdHeatCoeff            ! coefficients for ground heat as function of ts
    real(kind=kind_noahmp)                :: TranspHeatCoeff         ! coefficients for transpiration heat as function of ts
    real(kind=kind_noahmp)                :: TempShGhTmp             ! partial temperature by sensible and ground heat
    real(kind=kind_noahmp)                :: ExchCoeffShFrac         ! exchange coefficient fraction for sensible heat 
    real(kind=kind_noahmp)                :: VapPresLhTot            ! vapor pressure related to total latent heat
    real(kind=kind_noahmp)                :: ExchCoeffEtFrac         ! exchange coefficient fraction for evapotranspiration heat
    real(kind=kind_noahmp)                :: VapPresSatWatTmp        ! saturated vapor pressure for water
    real(kind=kind_noahmp)                :: VapPresSatIceTmp        ! saturated vapor pressure for ice
    real(kind=kind_noahmp)                :: VapPresSatWatTmpD       ! saturated vapor pressure gradient with ground temp. [Pa/K] for water
    real(kind=kind_noahmp)                :: VapPresSatIceTmpD       ! saturated vapor pressure gradient with ground temp. [Pa/K] for ice
    real(kind=kind_noahmp)                :: FluxTotCoeff            ! temporary total coefficients for all energy flux
    real(kind=kind_noahmp)                :: EnergyResTmp            ! temporary energy residual
    real(kind=kind_noahmp)                :: ExchCoeffShLeafTmp      ! sensible heat conductance, leaf surface to canopy air [m/s]
    real(kind=kind_noahmp)                :: ExchCoeffTot            ! sum of conductances [m/s]
    real(kind=kind_noahmp)                :: ShCanTmp                ! temporary sensible heat flux [W/m2]
    real(kind=kind_noahmp)                :: ShGrdTmp                ! temporary sensible heat flux [W/m2]
    real(kind=kind_noahmp)                :: MoistureFluxSfc         ! moisture flux
    real(kind=kind_noahmp)                :: VegAreaIndTmp           ! total leaf area index + stem area index,effective
    real(kind=kind_noahmp)                :: LeafAreaIndSunEff       ! sunlit leaf area index, one-sided [m2/m2],effective
    real(kind=kind_noahmp)                :: LeafAreaIndShdEff       ! shaded leaf area index, one-sided [m2/m2],effective
    real(kind=kind_noahmp)                :: TempTmp                 ! temporary temperature
    real(kind=kind_noahmp)                :: TempUnitConv            ! Kelvin to degree Celsius with limit -50 to +50
    real(kind=kind_noahmp)                :: HeatCapacCan            ! canopy heat capacity [J/m2/K]
! local statement function
    TempUnitConv(TempTmp) = min(50.0, max(-50.0, (TempTmp - ConstFreezePoint)))

! --------------------------------------------------------------------
    associate(                                                                        &
              MainTimeStep            => noahmp%config%domain%MainTimeStep           ,& ! in,    main noahmp timestep [s]
              GridIndexI              => noahmp%config%domain%GridIndexI             ,& ! in,    grid index in x-direction
              GridIndexJ              => noahmp%config%domain%GridIndexJ             ,& ! in,    grid index in y-direction
              NumSnowLayerNeg         => noahmp%config%domain%NumSnowLayerNeg        ,& ! in,    actual number of snow layers (negative)
              ThicknessSnowSoilLayer  => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,    thickness of snow/soil layers [m]
              OptSurfaceDrag          => noahmp%config%nmlist%OptSurfaceDrag         ,& ! in,    options for surface layer drag/exchange coefficient
              OptStomataResistance    => noahmp%config%nmlist%OptStomataResistance   ,& ! in,    options for canopy stomatal resistance
              OptSnowSoilTempTime     => noahmp%config%nmlist%OptSnowSoilTempTime    ,& ! in,    options for snow/soil temperature time scheme (only layer 1)
              WindEastwardRefHeight   => noahmp%forcing%WindEastwardRefHeight        ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight  => noahmp%forcing%WindNorthwardRefHeight       ,& ! in,    wind speed [m/s] in northward direction at reference height
              RadLwDownRefHeight      => noahmp%forcing%RadLwDownRefHeight           ,& ! in,    downward longwave radiation [W/m2] at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight      ,& ! in,    air temperature [K] at reference height
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight         ,& ! in,    air pressure [Pa] at reference height
              PressureAirSurface      => noahmp%forcing%PressureAirSurface           ,& ! in,    air pressure [Pa] at surface-atmos interface
              SnowDepth               => noahmp%water%state%SnowDepth                ,& ! in,    snow depth [m]
              SnowCoverFrac           => noahmp%water%state%SnowCoverFrac            ,& ! in,    snow cover fraction
              CanopyWetFrac           => noahmp%water%state%CanopyWetFrac            ,& ! in,    wetted or snowed fraction of the canopy
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! in,    canopy intercepted liquid water [mm]
              CanopyIce               => noahmp%water%state%CanopyIce                ,& ! in,    canopy intercepted ice [mm]
              HeightCanopyTop         => noahmp%energy%param%HeightCanopyTop         ,& ! in,    top of canopy [m]
              ZilitinkevichCoeff      => noahmp%energy%param%ZilitinkevichCoeff      ,& ! in,    Zilitinkevich Coefficient for exchange coefficient calculation
              HeatCapacCanFac         => noahmp%energy%param%HeatCapacCanFac         ,& ! in,    canopy biomass heat capacity parameter [m]
              RadSwAbsVeg             => noahmp%energy%flux%RadSwAbsVeg              ,& ! in,    solar radiation absorbed by vegetation [W/m2]
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! in,    solar radiation absorbed by ground [W/m2]
              HeatPrecipAdvCanopy     => noahmp%energy%flux%HeatPrecipAdvCanopy      ,& ! in,    precipitation advected heat - vegetation net [W/m2]
              HeatPrecipAdvVegGrd     => noahmp%energy%flux%HeatPrecipAdvVegGrd      ,& ! in,    precipitation advected heat - under canopy net [W/m2]
              RefHeightAboveGrd       => noahmp%energy%state%RefHeightAboveGrd       ,& ! in,    surface reference height [m]
              VegFrac                 => noahmp%energy%state%VegFrac                 ,& ! in,    greeness vegetation fraction
              WindSpdRefHeight        => noahmp%energy%state%WindSpdRefHeight        ,& ! in,    wind speed [m/s] at reference height
              PressureVaporRefHeight  => noahmp%energy%state%PressureVaporRefHeight  ,& ! in,    vapor pressure air [Pa] at reference height
              SpecHumidityRefHeight   => noahmp%forcing%SpecHumidityRefHeight        ,& ! in,    specific humidity [kg/kg] at reference height
              DensityAirRefHeight     => noahmp%energy%state%DensityAirRefHeight     ,& ! in,    density air [kg/m3]
              VegAreaIndEff           => noahmp%energy%state%VegAreaIndEff           ,& ! in,    one-sided leaf+stem area index [m2/m2]
              LeafAreaIndSunlit       => noahmp%energy%state%LeafAreaIndSunlit       ,& ! in,    sunlit leaf area index, one-sided [m2/m2]
              LeafAreaIndShade        => noahmp%energy%state%LeafAreaIndShade        ,& ! in,    shaded leaf area index, one-sided [m2/m2]
              ZeroPlaneDispSfc        => noahmp%energy%state%ZeroPlaneDispSfc        ,& ! in,    zero plane displacement [m]
              RoughLenMomSfc          => noahmp%energy%state%RoughLenMomSfc          ,& ! in,    roughness length [m], momentum, surface
              RoughLenMomGrd          => noahmp%energy%state%RoughLenMomGrd          ,& ! in,    roughness length [m], momentum, ground
              EmissivityVeg           => noahmp%energy%state%EmissivityVeg           ,& ! in,    vegetation emissivity
              EmissivityGrd           => noahmp%energy%state%EmissivityGrd           ,& ! in,    ground emissivity
              TemperatureSoilSnow     => noahmp%energy%state%TemperatureSoilSnow     ,& ! in,    snow and soil layer temperature [K]
              ThermConductSoilSnow    => noahmp%energy%state%ThermConductSoilSnow    ,& ! in,    thermal conductivity [W/m/K] for all soil & snow
              ResistanceGrdEvap       => noahmp%energy%state%ResistanceGrdEvap       ,& ! in,    ground surface resistance [s/m] to evaporation
              PsychConstCanopy        => noahmp%energy%state%PsychConstCanopy        ,& ! in,    psychrometric constant [Pa/K], canopy
              LatHeatVapCanopy        => noahmp%energy%state%LatHeatVapCanopy        ,& ! in,    latent heat of vaporization/subli [J/kg], canopy
              PsychConstGrd           => noahmp%energy%state%PsychConstGrd           ,& ! in,    psychrometric constant [Pa/K], ground
              RelHumidityGrd          => noahmp%energy%state%RelHumidityGrd          ,& ! in,    raltive humidity in surface soil/snow air space
              SpecHumiditySfc         => noahmp%energy%state%SpecHumiditySfc         ,& ! inout, specific humidity at vegetated surface
              PressureVaporCanAir     => noahmp%energy%state%PressureVaporCanAir     ,& ! inout, canopy air vapor pressure [Pa]
              TemperatureCanopyAir    => noahmp%energy%state%TemperatureCanopyAir    ,& ! inout, canopy air temperature [K]
              TemperatureCanopy       => noahmp%energy%state%TemperatureCanopy       ,& ! inout, vegetation temperature [K]
              TemperatureGrdVeg       => noahmp%energy%state%TemperatureGrdVeg       ,& ! inout, vegetated ground (below-canopy) temperature [K]
              ExchCoeffMomAbvCan      => noahmp%energy%state%ExchCoeffMomAbvCan      ,& ! inout, momentum exchange coeff [m/s], above ZeroPlaneDisp, vegetated
              ExchCoeffShAbvCan       => noahmp%energy%state%ExchCoeffShAbvCan       ,& ! inout, heat exchange coeff [m/s], above ZeroPlaneDisp, vegetated
              WindStressEwVeg         => noahmp%energy%state%WindStressEwVeg         ,& ! out,   wind stress: east-west [N/m2] above canopy
              WindStressNsVeg         => noahmp%energy%state%WindStressNsVeg         ,& ! out,   wind stress: north-south [N/m2] above canopy
              TemperatureAir2mVeg     => noahmp%energy%state%TemperatureAir2mVeg     ,& ! out,   2 m height air temperature [K], vegetated
              ExchCoeffShLeaf         => noahmp%energy%state%ExchCoeffShLeaf         ,& ! out,   sensible heat exchange coeff [m/s],leaf surface to canopy air
              ExchCoeffShUndCan       => noahmp%energy%state%ExchCoeffShUndCan       ,& ! out,   under canopy sensible heat exchange coefficient [m/s]
              ExchCoeffSh2mVeg        => noahmp%energy%state%ExchCoeffSh2mVeg        ,& ! out,   2m sensible heat exchange coefficient [m/s]
              SpecHumidity2mVeg       => noahmp%energy%state%SpecHumidity2mVeg       ,& ! out,   specific humidity [kg/kg] at 2m vegetated
              ResistanceStomataSunlit => noahmp%energy%state%ResistanceStomataSunlit ,& ! out,   sunlit leaf stomatal resistance [s/m]
              ResistanceStomataShade  => noahmp%energy%state%ResistanceStomataShade  ,& ! out,   shaded leaf stomatal resistance [s/m]
              FrictionVelVeg          => noahmp%energy%state%FrictionVelVeg          ,& ! out,   friction velocity [m/s], vegetated
              RoughLenShCanopy        => noahmp%energy%state%RoughLenShCanopy        ,& ! out,   roughness length [m], sensible heat, vegetated
              RoughLenShVegGrd        => noahmp%energy%state%RoughLenShVegGrd        ,& ! out,   roughness length [m], sensible heat ground, below canopy
              ResistanceLeafBoundary  => noahmp%energy%state%ResistanceLeafBoundary  ,& ! out,   bulk leaf boundary layer resistance [s/m]
              ResistanceShAbvCan      => noahmp%energy%state%ResistanceShAbvCan      ,& ! out,   aerodynamic resistance for sensible heat [s/m], above canopy
              ResistanceLhAbvCan      => noahmp%energy%state%ResistanceLhAbvCan      ,& ! out,   aerodynamic resistance for water vapor [s/m], above canopy
              ResistanceShUndCan      => noahmp%energy%state%ResistanceShUndCan      ,& ! out,   ground aerodynamic resistance for sensible heat [s/m]
              ResistanceLhUndCan      => noahmp%energy%state%ResistanceLhUndCan      ,& ! out,   ground aerodynamic resistance for water vapor [s/m]
              ExchCoeffLhAbvCan       => noahmp%energy%state%ExchCoeffLhAbvCan       ,& ! out,   latent heat conductance, canopy air to reference height [m/s]
              ExchCoeffLhTransp       => noahmp%energy%state%ExchCoeffLhTransp       ,& ! out,   transpiration conductance, leaf to canopy air [m/s]
              ExchCoeffLhEvap         => noahmp%energy%state%ExchCoeffLhEvap         ,& ! out,   evaporation conductance, leaf to canopy air [m/s]
              ExchCoeffLhUndCan       => noahmp%energy%state%ExchCoeffLhUndCan       ,& ! out,   latent heat conductance, ground to canopy air [m/s]
              VapPresSatCanopy        => noahmp%energy%state%VapPresSatCanopy        ,& ! out,   saturation vapor pressure at TemperatureCanopy [Pa]
              VapPresSatGrdVeg        => noahmp%energy%state%VapPresSatGrdVeg        ,& ! out,   saturation vapor pressure at TemperatureGrd [Pa]
              VapPresSatCanTempD      => noahmp%energy%state%VapPresSatCanTempD      ,& ! out,   d(VapPresSatCanopy)/dt at TemperatureCanopy [Pa/K]
              VapPresSatGrdVegTempD   => noahmp%energy%state%VapPresSatGrdVegTempD   ,& ! out,   d(VapPresSatGrdVeg)/dt at TemperatureGrd [Pa/K]
              CanopyHeight            => noahmp%energy%state%CanopyHeight            ,& ! out,   canopy height [m]
              WindSpdCanopyTop        => noahmp%energy%state%WindSpdCanopyTop        ,& ! out,   wind speed at top of canopy [m/s]
              MoStabParaAbvCan        => noahmp%energy%state%MoStabParaAbvCan        ,& ! out,   Monin-Obukhov stability (z/L), above ZeroPlaneDispSfc, vegetated
              MoStabCorrShVeg2m       => noahmp%energy%state%MoStabCorrShVeg2m       ,& ! out,   M-O sen heat stability correction, 2m, vegetated
              RadLwNetCanopy          => noahmp%energy%flux%RadLwNetCanopy           ,& ! out,   canopy net longwave radiation [W/m2] (+ to atm)
              HeatSensibleCanopy      => noahmp%energy%flux%HeatSensibleCanopy       ,& ! out,   canopy sensible heat flux [W/m2] (+ to atm)
              HeatLatentCanEvap       => noahmp%energy%flux%HeatLatentCanEvap        ,& ! out,   canopy evaporation heat flux [W/m2] (+ to atm)
              RadLwNetVegGrd          => noahmp%energy%flux%RadLwNetVegGrd           ,& ! out,   ground net longwave radiation [W/m2] (+ to atm)
              HeatSensibleVegGrd      => noahmp%energy%flux%HeatSensibleVegGrd       ,& ! out,   vegetated ground sensible heat flux [W/m2] (+ to atm)
              HeatLatentVegGrd        => noahmp%energy%flux%HeatLatentVegGrd         ,& ! out,   ground evaporation heat flux [W/m2] (+ to atm)
              HeatLatentCanTransp     => noahmp%energy%flux%HeatLatentCanTransp      ,& ! out,   canopy transpiration heat flux [W/m2] (+ to atm)
              HeatCanStorageChg       => noahmp%energy%flux%HeatCanStorageChg        ,& ! out,   canopy heat storage change [W/m2]
              HeatGroundVegGrd        => noahmp%energy%flux%HeatGroundVegGrd          & ! out,   vegetated ground heat [W/m2] (+ to soil/snow)
             )
! ----------------------------------------------------------------------

    ! initialization (including variables that do not depend on stability iteration)
    LastIter          = 0
    FrictionVelVeg    = 0.1
    TemperatureCanChg = 0.0
    TemperatureGrdChg = 0.0
    MoStabParaAbvCan  = 0.0
    MoStabParaSgn     = 0
    MoStabCorrShVeg2m = 0.0
    ShGrdTmp          = 0.0
    ShCanTmp          = 0.0
    MoistureFluxSfc   = 0.0
    ! limit LeafAreaIndex
    VegAreaIndTmp     = min(6.0, VegAreaIndEff)
    LeafAreaIndSunEff = min(6.0, LeafAreaIndSunlit)
    LeafAreaIndShdEff = min(6.0, LeafAreaIndShade)

    ! saturation vapor pressure at ground temperature
    TempTmp = TempUnitConv(TemperatureGrdVeg)
    call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, VapPresSatIceTmpD)
    if ( TempTmp > 0.0 ) then
       VapPresSatGrdVeg = VapPresSatWatTmp
    else
       VapPresSatGrdVeg = VapPresSatIceTmp
    endif

    ! canopy height
    CanopyHeight = HeightCanopyTop
    ! wind speed at canopy height
   !WindSpdCanopyTop = WindSpdRefHeight * log(CanopyHeight/RoughLenMomSfc) / log(RefHeightAboveGrd/RoughLenMomSfc)
    WindSpdCanopyTop = WindSpdRefHeight * log((CanopyHeight - ZeroPlaneDispSfc + RoughLenMomSfc)/RoughLenMomSfc) / &
                       log(RefHeightAboveGrd/RoughLenMomSfc)                                           ! MB: add ZeroPlaneDispSfc v3.7
    if ( (CanopyHeight-ZeroPlaneDispSfc) <= 0.0 ) then
       print*, "CRITICAL PROBLEM: CanopyHeight <= ZeroPlaneDispSfc"
       print*, "GridIndexI,GridIndexJ = ", GridIndexI, GridIndexJ
       print*, "CanopyHeight = "         , CanopyHeight
       print*, "ZeroPlaneDispSfc = "     , ZeroPlaneDispSfc
       print*, "SnowDepth = "            , SnowDepth
       stop "Error: ZeroPlaneDisp problem in NoahMP LSM"
    endif

    ! prepare for longwave rad.
    LwCoeffAir = -EmissivityVeg * (1.0 + (1.0-EmissivityVeg)*(1.0-EmissivityGrd)) * RadLwDownRefHeight - &
                  EmissivityVeg * EmissivityGrd * ConstStefanBoltzmann * TemperatureGrdVeg**4
    LwCoeffCan = (2.0 - EmissivityVeg * (1.0-EmissivityGrd)) * EmissivityVeg * ConstStefanBoltzmann

    ! begin stability iteration for canopy temperature and flux
    loop1: do IndIter = 1, NumIterC

       ! ground and surface roughness length
       if ( IndIter == 1 ) then
          RoughLenShCanopy = RoughLenMomSfc
          RoughLenShVegGrd = RoughLenMomGrd
       else
          RoughLenShCanopy = RoughLenMomSfc  !* exp(-ZilitinkevichCoeff*0.4*258.2*sqrt(FrictionVelVeg*RoughLenMomSfc))
          RoughLenShVegGrd = RoughLenMomGrd  !* exp(-ZilitinkevichCoeff*0.4*258.2*sqrt(FrictionVelVeg*RoughLenMomGrd))
       endif

       ! aerodyn resistances between RefHeightAboveGrd and d+z0v
       if ( OptSurfaceDrag == 1 ) call ResistanceAboveCanopyMOST(noahmp, IndIter, ShCanTmp, MoStabParaSgn)
       if ( OptSurfaceDrag == 2 ) call ResistanceAboveCanopyChen97(noahmp, IndIter)

       ! aerodyn resistance between z0g and d+z0v, and leaf boundary layer resistance
       call ResistanceLeafToGround(noahmp, IndIter, VegAreaIndTmp, ShGrdTmp)

       ! ES and d(ES)/dt evaluated at TemperatureCanopy
       TempTmp = TempUnitConv(TemperatureCanopy)
       call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, VapPresSatIceTmpD)
       if ( TempTmp > 0.0 ) then
          VapPresSatCanopy   = VapPresSatWatTmp
          VapPresSatCanTempD = VapPresSatWatTmpD
       else
          VapPresSatCanopy   = VapPresSatIceTmp
          VapPresSatCanTempD = VapPresSatIceTmpD
       endif

       ! stomatal resistance
       if ( IndIter == 1 ) then
          if ( OptStomataResistance == 1 ) then  ! Ball-Berry
             IndexShade = 0 ! sunlit case
             call ResistanceCanopyStomataBallBerry(noahmp, IndexShade)
             IndexShade = 1 ! shaded case
             call ResistanceCanopyStomataBallBerry(noahmp, IndexShade)
          endif
          if ( OptStomataResistance == 2 ) then  ! Jarvis
             IndexShade = 0 ! sunlit case
             call ResistanceCanopyStomataJarvis(noahmp, IndexShade)
             IndexShade = 1 ! shaded case
             call ResistanceCanopyStomataJarvis(noahmp, IndexShade)
          endif
       endif

       ! sensible heat conductance and coeff above veg.
       ExchCoeffShAbvCanTmp = 1.0 / ResistanceShAbvCan
       ExchCoeffShLeafTmp   = 2.0 * VegAreaIndTmp / ResistanceLeafBoundary
       GrdHeatCoeff         = 1.0 / ResistanceShUndCan
       ExchCoeffTot         = ExchCoeffShAbvCanTmp + ExchCoeffShLeafTmp + GrdHeatCoeff
       TempShGhTmp          = (TemperatureAirRefHeight*ExchCoeffShAbvCanTmp + TemperatureGrdVeg*GrdHeatCoeff) / ExchCoeffTot
       ExchCoeffShFrac      = ExchCoeffShLeafTmp / ExchCoeffTot
       ShCoeff              = (1.0 - ExchCoeffShFrac) * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffShLeafTmp

       ! latent heat conductance and coeff above veg.
       ExchCoeffLhAbvCan = 1.0 / ResistanceLhAbvCan
       ExchCoeffLhEvap   = CanopyWetFrac * VegAreaIndTmp / ResistanceLeafBoundary
       ExchCoeffLhTransp = (1.0 - CanopyWetFrac) * (LeafAreaIndSunEff/(ResistanceLeafBoundary+ResistanceStomataSunlit) + &
                                                    LeafAreaIndShdEff/(ResistanceLeafBoundary+ResistanceStomataShade))
       ExchCoeffLhUndCan = 1.0 / (ResistanceLhUndCan + ResistanceGrdEvap)
       ExchCoeffTot      = ExchCoeffLhAbvCan + ExchCoeffLhEvap + ExchCoeffLhTransp + ExchCoeffLhUndCan
       VapPresLhTot      = (PressureVaporRefHeight*ExchCoeffLhAbvCan + VapPresSatGrdVeg*ExchCoeffLhUndCan ) / ExchCoeffTot
       ExchCoeffEtFrac   = (ExchCoeffLhEvap + ExchCoeffLhTransp) / ExchCoeffTot
       LhCoeff           = (1.0 - ExchCoeffEtFrac) * ExchCoeffLhEvap * DensityAirRefHeight * &
                           ConstHeatCapacAir / PsychConstCanopy                                        ! Barlage: change to vegetation v3.6
       TranspHeatCoeff   = (1.0 - ExchCoeffEtFrac) * ExchCoeffLhTransp * DensityAirRefHeight * &
                           ConstHeatCapacAir / PsychConstCanopy

       ! evaluate surface fluxes with current temperature and solve for temperature change
       TemperatureCanopyAir = TempShGhTmp + ExchCoeffShFrac * TemperatureCanopy                        ! canopy air T.
       PressureVaporCanAir  = VapPresLhTot + ExchCoeffEtFrac * VapPresSatCanopy                        ! canopy air e
       RadLwNetCanopy       = VegFrac * (LwCoeffAir + LwCoeffCan * TemperatureCanopy**4)
       HeatSensibleCanopy   = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * &
                              ExchCoeffShLeafTmp * (TemperatureCanopy - TemperatureCanopyAir)
       HeatLatentCanEvap    = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffLhEvap * &
                              (VapPresSatCanopy - PressureVaporCanAir) / PsychConstCanopy              ! Barlage: change to v in v3.6
       HeatLatentCanTransp  = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffLhTransp * &
                              (VapPresSatCanopy - PressureVaporCanAir) / PsychConstCanopy
       if ( TemperatureCanopy > ConstFreezePoint ) then
          HeatLatentCanEvap = min(CanopyLiqWater*LatHeatVapCanopy/MainTimeStep, HeatLatentCanEvap)     ! Barlage: add if block for canopy ice in v3.6
       else
          HeatLatentCanEvap = min(CanopyIce*LatHeatVapCanopy/MainTimeStep, HeatLatentCanEvap)
       endif
       ! canopy heat capacity
       HeatCapacCan         = HeatCapacCanFac*VegAreaIndTmp*ConstHeatCapacWater + CanopyLiqWater*ConstHeatCapacWater/ConstDensityWater + &
                              CanopyIce*ConstHeatCapacIce/ConstDensityIce      ! [J/m2/K]
       ! compute vegetation temperature change
       EnergyResTmp         = RadSwAbsVeg - RadLwNetCanopy - HeatSensibleCanopy - &
                              HeatLatentCanEvap - HeatLatentCanTransp + HeatPrecipAdvCanopy
       FluxTotCoeff         = VegFrac * (4.0*LwCoeffCan*TemperatureCanopy**3 + ShCoeff + &
                                        (LhCoeff+TranspHeatCoeff)*VapPresSatCanTempD + HeatCapacCan/MainTimeStep)     ! volumetric heat capacity
       TemperatureCanChg    = EnergyResTmp / FluxTotCoeff
       ! update fluxes with temperature change
       RadLwNetCanopy       = RadLwNetCanopy      + VegFrac * 4.0 * LwCoeffCan * TemperatureCanopy**3 * TemperatureCanChg
       HeatSensibleCanopy   = HeatSensibleCanopy  + VegFrac * ShCoeff * TemperatureCanChg
       HeatLatentCanEvap    = HeatLatentCanEvap   + VegFrac * LhCoeff * VapPresSatCanTempD * TemperatureCanChg
       HeatLatentCanTransp  = HeatLatentCanTransp + VegFrac * TranspHeatCoeff * VapPresSatCanTempD * TemperatureCanChg
       HeatCanStorageChg    = VegFrac * HeatCapacCan / MainTimeStep * TemperatureCanChg  ! canopy heat storage change [W/m2]
       ! update vegetation temperature
       TemperatureCanopy    = TemperatureCanopy + TemperatureCanChg
      !TemperatureCanopyAir = TempShGhTmp + ExchCoeffShFrac * TemperatureCanopy                        ! canopy air T; update here for consistency

       ! for computing M-O length in the next iteration
       ShCanTmp = DensityAirRefHeight * ConstHeatCapacAir * (TemperatureCanopyAir-TemperatureAirRefHeight) / ResistanceShAbvCan
       ShGrdTmp = DensityAirRefHeight * ConstHeatCapacAir * (TemperatureGrdVeg-TemperatureCanopyAir) / ResistanceShUndCan

       ! consistent specific humidity from canopy air vapor pressure
       SpecHumiditySfc = (0.622 * PressureVaporCanAir) / (PressureAirRefHeight - 0.378 * PressureVaporCanAir)
       if ( LastIter == 1 ) then
          exit loop1
       endif
       if ( (IndIter >= 5) .and. (abs(TemperatureCanChg) <= 0.01) .and. (LastIter == 0) ) then
          LastIter = 1
       endif
    enddo loop1  ! end stability iteration

    ! under-canopy fluxes and ground temperature
    LwCoeffAir   = -EmissivityGrd * (1.0 - EmissivityVeg) * RadLwDownRefHeight - &
                    EmissivityGrd * EmissivityVeg * ConstStefanBoltzmann * TemperatureCanopy**4
    LwCoeffCan   = EmissivityGrd * ConstStefanBoltzmann
    ShCoeff      = DensityAirRefHeight * ConstHeatCapacAir / ResistanceShUndCan
    LhCoeff      = DensityAirRefHeight * ConstHeatCapacAir / (PsychConstGrd * (ResistanceLhUndCan+ResistanceGrdEvap))  ! Barlage: change to ground v3.6
    GrdHeatCoeff = 2.0 * ThermConductSoilSnow(NumSnowLayerNeg+1) / ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
    ! begin stability iteration
    loop2: do IndIter = 1, NumIterG
       TempTmp = TempUnitConv(TemperatureGrdVeg)
       call VaporPressureSaturation(TempTmp, VapPresSatWatTmp, VapPresSatIceTmp, VapPresSatWatTmpD, VapPresSatIceTmpD)
       if ( TempTmp > 0.0 ) then
          VapPresSatGrdVeg      = VapPresSatWatTmp
          VapPresSatGrdVegTempD = VapPresSatWatTmpD
       else
          VapPresSatGrdVeg      = VapPresSatIceTmp
          VapPresSatGrdVegTempD = VapPresSatIceTmpD
       endif
       RadLwNetVegGrd     = LwCoeffCan * TemperatureGrdVeg**4 + LwCoeffAir
       HeatSensibleVegGrd = ShCoeff * (TemperatureGrdVeg - TemperatureCanopyAir)
       HeatLatentVegGrd   = LhCoeff * (VapPresSatGrdVeg*RelHumidityGrd - PressureVaporCanAir)
       HeatGroundVegGrd   = GrdHeatCoeff * (TemperatureGrdVeg - TemperatureSoilSnow(NumSnowLayerNeg+1))
       EnergyResTmp       = RadSwAbsGrd - RadLwNetVegGrd - HeatSensibleVegGrd - &
                            HeatLatentVegGrd - HeatGroundVegGrd + HeatPrecipAdvVegGrd
       FluxTotCoeff       = 4.0 * LwCoeffCan * TemperatureGrdVeg**3 + ShCoeff + LhCoeff*VapPresSatGrdVegTempD + GrdHeatCoeff
       TemperatureGrdChg  = EnergyResTmp / FluxTotCoeff
       RadLwNetVegGrd     = RadLwNetVegGrd + 4.0 * LwCoeffCan * TemperatureGrdVeg**3 * TemperatureGrdChg
       HeatSensibleVegGrd = HeatSensibleVegGrd + ShCoeff * TemperatureGrdChg
       HeatLatentVegGrd   = HeatLatentVegGrd + LhCoeff * VapPresSatGrdVegTempD * TemperatureGrdChg
       HeatGroundVegGrd   = HeatGroundVegGrd + GrdHeatCoeff * TemperatureGrdChg
       TemperatureGrdVeg  = TemperatureGrdVeg + TemperatureGrdChg
    enddo loop2
    !TemperatureCanopyAir = (ExchCoeffShAbvCanTmp*TemperatureAirRefHeight + ExchCoeffShLeafTmp*TemperatureCanopy + &
    !                        GrdHeatCoeff*TemperatureGrdVeg)/(ExchCoeffShAbvCanTmp + ExchCoeffShLeafTmp + GrdHeatCoeff)

    ! if snow on ground and TemperatureGrdVeg > freezing point: reset TemperatureGrdVeg = freezing point. reevaluate ground fluxes.
    if ( (OptSnowSoilTempTime == 1) .or. (OptSnowSoilTempTime == 3) ) then
       if ( (SnowDepth > 0.05) .and. (TemperatureGrdVeg > ConstFreezePoint) ) then
          if ( OptSnowSoilTempTime == 1 ) &
             TemperatureGrdVeg = ConstFreezePoint
          if ( OptSnowSoilTempTime == 3 ) &
             TemperatureGrdVeg = (1.0 - SnowCoverFrac) * TemperatureGrdVeg + SnowCoverFrac * ConstFreezePoint   ! MB: allow TemperatureGrdVeg>0C during melt v3.7

          RadLwNetVegGrd     = LwCoeffCan * TemperatureGrdVeg**4 - EmissivityGrd * (1.0-EmissivityVeg) * RadLwDownRefHeight - &
                               EmissivityGrd * EmissivityVeg * ConstStefanBoltzmann * TemperatureCanopy**4
          HeatSensibleVegGrd = ShCoeff * (TemperatureGrdVeg - TemperatureCanopyAir)
          HeatLatentVegGrd   = LhCoeff * (VapPresSatGrdVeg*RelHumidityGrd - PressureVaporCanAir)
          HeatGroundVegGrd   = RadSwAbsGrd + HeatPrecipAdvVegGrd - (RadLwNetVegGrd + HeatSensibleVegGrd + HeatLatentVegGrd)
       endif
    endif

    ! wind stresses
    WindStressEwVeg = -DensityAirRefHeight * ExchCoeffMomAbvCan * WindSpdRefHeight * WindEastwardRefHeight
    WindStressNsVeg = -DensityAirRefHeight * ExchCoeffMomAbvCan * WindSpdRefHeight * WindNorthwardRefHeight

    ! consistent vegetation air temperature and vapor pressure 
    ! since TemperatureGrdVeg is not consistent with the TemperatureCanopyAir/PressureVaporCanAir calculation.
    !TemperatureCanopyAir = TemperatureAirRefHeight + (HeatSensibleVegGrd + HeatSensibleCanopy) / &
    !                       (DensityAirRefHeight*ConstHeatCapacAir*ExchCoeffShAbvCanTmp) 
    !TemperatureCanopyAir = TemperatureAirRefHeight + (HeatSensibleVegGrd * VegFrac + HeatSensibleCanopy) / &
    !                       (DensityAirRefHeight*ConstHeatCapacAir*ExchCoeffShAbvCanTmp)                     ! ground flux need fveg
    !PressureVaporCanAir  = PressureVaporRefHeight + (HeatLatentCanEvap+VegFrac*(HeatLatentCanTransp+HeatLatentVegGrd)) / &
    !                       (DensityAirRefHeight*ExchCoeffLhAbvCan*ConstHeatCapacAir/PsychConstGrd)
    !MoistureFluxSfc      = (SpecHumiditySfc - SpecHumidityRefHeight) * DensityAirRefHeight * ExchCoeffLhAbvCan !*ConstHeatCapacAir/PsychConstGrd

    ! 2m temperature over vegetation ( corrected for low LH exchange coeff values )
    if ( (OptSurfaceDrag == 1) .or. (OptSurfaceDrag == 2) ) then
      !ExchCoeffSh2mVeg = FrictionVelVeg * 1.0 / ConstVonKarman * log((2.0+RoughLenShCanopy)/RoughLenShCanopy)
      !ExchCoeffSh2mVeg = FrictionVelVeg * ConstVonKarman / log((2.0+RoughLenShCanopy)/RoughLenShCanopy)
       ExchCoeffSh2mVeg = FrictionVelVeg * ConstVonKarman / (log((2.0+RoughLenShCanopy)/RoughLenShCanopy) - MoStabCorrShVeg2m)
       if ( ExchCoeffSh2mVeg < 1.0e-5 ) then
          TemperatureAir2mVeg = TemperatureCanopyAir
         !SpecHumidity2mVeg   = (PressureVaporCanAir*0.622/(PressureAirRefHeight - 0.378*PressureVaporCanAir))
          SpecHumidity2mVeg   = SpecHumiditySfc
       else
          TemperatureAir2mVeg = TemperatureCanopyAir - (HeatSensibleVegGrd + HeatSensibleCanopy/VegFrac) / &
                                (DensityAirRefHeight * ConstHeatCapacAir) * 1.0 / ExchCoeffSh2mVeg
         !SpecHumidity2mVeg   = (PressureVaporCanAir*0.622/(PressureAirRefHeight - 0.378*PressureVaporCanAir)) - &
         !                      MoistureFluxSfc/(DensityAirRefHeight*FrictionVelVeg)* 1.0/ConstVonKarman * &
         !                      log((2.0+RoughLenShCanopy)/RoughLenShCanopy)
          SpecHumidity2mVeg   = SpecHumiditySfc - ((HeatLatentCanEvap+HeatLatentCanTransp)/VegFrac + HeatLatentVegGrd) / &
                                                  (LatHeatVapCanopy * DensityAirRefHeight) * 1.0 / ExchCoeffSh2mVeg
       endif
    endif

    ! update ExchCoeffSh for output
    ExchCoeffShAbvCan = ExchCoeffShAbvCanTmp
    ExchCoeffShLeaf   = ExchCoeffShLeafTmp
    ExchCoeffShUndCan = 1.0 / ResistanceShUndCan

    end associate

  end subroutine SurfaceEnergyFluxVegetated

end module SurfaceEnergyFluxVegetatedMod
