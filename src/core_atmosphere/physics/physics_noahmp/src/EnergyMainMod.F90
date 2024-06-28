module EnergyMainMod

!!! Main energy module including all energy relevant processes
!!! soil/snow thermal property -> radiation -> ground/vegtation heat flux -> snow/soil temperature solver -> soil/snow phase change
!
! --------------------------------------------------------------------------------------------------
! NoahMP uses different approaches to deal with subgrid features of radiation transfer and turbulent
! transfer. It uses 'tile' approach to compute turbulent fluxes, while it uses two-stream approx.
! to compute radiation transfer. Tile approach, assemblying vegetation canopies together,
! may expose too much ground surfaces (either covered by snow or grass) to solar radiation. The
! modified two-stream assumes vegetation covers fully the gridcell but with gaps between tree crowns.
! --------------------------------------------------------------------------------------------------
! turbulence transfer : 'tile' approach to compute energy fluxes in vegetated fraction and
!                         bare fraction separately and then sum them up weighted by fraction
!                     --------------------------------------
!                    / O  O  O  O  O  O  O  O  /          / 
!                   /  |  |  |  |  |  |  |  | /          /
!                  / O  O  O  O  O  O  O  O  /          /
!                 /  |  |  |tile1|  |  |  | /  tile2   /
!                / O  O  O  O  O  O  O  O  /  bare    /
!               /  |  |  | vegetated |  | /          /
!              / O  O  O  O  O  O  O  O  /          /
!             /  |  |  |  |  |  |  |  | /          /
!            --------------------------------------
! --------------------------------------------------------------------------------------------------
! radiation transfer : modified two-stream (Yang and Friedl, 2003, JGR; Niu ang Yang, 2004, JGR)
!                     --------------------------------------  two-stream treats leaves as
!                    /   O   O   O   O   O   O   O   O    /  cloud over the entire grid-cell,
!                   /    |   |   |   |   |   |   |   |   / while the modified two-stream 
!                  /   O   O   O   O   O   O   O   O    / aggregates cloudy leaves into  
!                 /    |   |   |   |   |   |   |   |   / tree crowns with gaps (as shown in
!                /   O   O   O   O   O   O   O   O    / the left figure). We assume these
!               /    |   |   |   |   |   |   |   |   / tree crowns are evenly distributed
!              /   O   O   O   O   O   O   O   O    / within the gridcell with 100% veg
!             /    |   |   |   |   |   |   |   |   / fraction, but with gaps. The 'tile'
!            -------------------------------------- approach overlaps too much shadows.
! --------------------------------------------------------------------------------------------------

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowCoverGroundNiu07Mod,        only : SnowCoverGroundNiu07
  use GroundRoughnessPropertyMod,     only : GroundRoughnessProperty
  use GroundThermalPropertyMod,       only : GroundThermalProperty
  use SurfaceAlbedoMod,               only : SurfaceAlbedo
  use SurfaceRadiationMod,            only : SurfaceRadiation
  use SurfaceEmissivityMod,           only : SurfaceEmissivity
  use SoilWaterTranspirationMod,      only : SoilWaterTranspiration
  use ResistanceGroundEvaporationMod, only : ResistanceGroundEvaporation
  use PsychrometricVariableMod,       only : PsychrometricVariable
  use SurfaceEnergyFluxVegetatedMod,  only : SurfaceEnergyFluxVegetated
  use SurfaceEnergyFluxBareGroundMod, only : SurfaceEnergyFluxBareGround
  use SoilSnowTemperatureMainMod,     only : SoilSnowTemperatureMain
  use SoilSnowWaterPhaseChangeMod,    only : SoilSnowWaterPhaseChange

  implicit none

contains

  subroutine EnergyMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ENERGY
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    logical                          :: FlagVegSfc    ! flag: true if vegetated surface

! --------------------------------------------------------------------
    associate(                                                                        &
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight         ,& ! in,    air pressure [Pa] at reference height
              RadLwDownRefHeight      => noahmp%forcing%RadLwDownRefHeight           ,& ! in,    downward longwave radiation [W/m2] at reference height
              RadSwDownRefHeight      => noahmp%forcing%RadSwDownRefHeight           ,& ! in,    downward shortwave radiation [W/m2] at reference height
              OptSnowSoilTempTime     => noahmp%config%nmlist%OptSnowSoilTempTime    ,& ! in,    options for snow/soil temperature time scheme
              FlagCropland            => noahmp%config%domain%FlagCropland           ,& ! in,    flag to identify croplands
              FlagSoilProcess         => noahmp%config%domain%FlagSoilProcess        ,& ! in,    flag to determine if calculating soil processes
              NumSoilTimeStep         => noahmp%config%domain%NumSoilTimeStep        ,& ! in,    number of time step for calculating soil processes
              SoilTimeStep            => noahmp%config%domain%SoilTimeStep           ,& ! in,    soil process timestep [s]
              IrriFracThreshold       => noahmp%water%param%IrriFracThreshold        ,& ! in,    irrigation fraction parameter
              IrrigationFracGrid      => noahmp%water%state%IrrigationFracGrid       ,& ! in,    total input irrigation fraction
              LeafAreaIndEff          => noahmp%energy%state%LeafAreaIndEff          ,& ! in,    leaf area index, after burying by snow
              StemAreaIndEff          => noahmp%energy%state%StemAreaIndEff          ,& ! in,    stem area index, after burying by snow
              VegFrac                 => noahmp%energy%state%VegFrac                 ,& ! in,    greeness vegetation fraction
              HeatLatentIrriEvap      => noahmp%energy%flux%HeatLatentIrriEvap       ,& ! in,    latent heating due to sprinkler evaporation [W/m2]
              HeatPrecipAdvCanopy     => noahmp%energy%flux%HeatPrecipAdvCanopy      ,& ! in,    precipitation advected heat - vegetation net [W/m2]
              HeatPrecipAdvVegGrd     => noahmp%energy%flux%HeatPrecipAdvVegGrd      ,& ! in,    precipitation advected heat - under canopy net [W/m2]
              HeatPrecipAdvBareGrd    => noahmp%energy%flux%HeatPrecipAdvBareGrd     ,& ! in,    precipitation advected heat - bare ground net [W/m2]
              TemperatureSfc          => noahmp%energy%state%TemperatureSfc          ,& ! inout, surface temperature [K]
              TemperatureGrd          => noahmp%energy%state%TemperatureGrd          ,& ! inout, ground temperature [K]
              TemperatureCanopy       => noahmp%energy%state%TemperatureCanopy       ,& ! inout, vegetation temperature [K]
              SpecHumiditySfc         => noahmp%energy%state%SpecHumiditySfc         ,& ! inout, specific humidity [kg/kg] at bare/veg/urban surface
              SpecHumiditySfcMean     => noahmp%energy%state%SpecHumiditySfcMean     ,& ! inout, specific humidity [kg/kg] at surface grid mean
              PressureVaporCanAir     => noahmp%energy%state%PressureVaporCanAir     ,& ! inout, canopy air vapor pressure [Pa]
              ExchCoeffMomSfc         => noahmp%energy%state%ExchCoeffMomSfc         ,& ! inout, exchange coefficient [m/s] for momentum, surface, grid mean
              ExchCoeffShSfc          => noahmp%energy%state%ExchCoeffShSfc          ,& ! inout, exchange coefficient [m/s] for heat, surface, grid mean
              HeatGroundTotAcc        => noahmp%energy%flux%HeatGroundTotAcc         ,& ! inout, accumulated total ground heat flux per soil timestep [W/m2 * dt_soil/dt_main]
              SnowDepth               => noahmp%water%state%SnowDepth                ,& ! inout, snow depth [m]
              RoughLenMomSfcToAtm     => noahmp%energy%state%RoughLenMomSfcToAtm     ,& ! out,   roughness length, momentum, surface, sent to coupled model
              WindStressEwSfc         => noahmp%energy%state%WindStressEwSfc         ,& ! out,   wind stress: east-west [N/m2] grid mean
              WindStressNsSfc         => noahmp%energy%state%WindStressNsSfc         ,& ! out,   wind stress: north-south [N/m2] grid mean
              TemperatureRadSfc       => noahmp%energy%state%TemperatureRadSfc       ,& ! out,   surface radiative temperature [K]
              TemperatureAir2m        => noahmp%energy%state%TemperatureAir2m        ,& ! out,   grid mean 2-m air temperature [K]
              ResistanceStomataSunlit => noahmp%energy%state%ResistanceStomataSunlit ,& ! out,   sunlit leaf stomatal resistance [s/m]
              ResistanceStomataShade  => noahmp%energy%state%ResistanceStomataShade  ,& ! out,   shaded leaf stomatal resistance [s/m]
              TemperatureAir2mVeg     => noahmp%energy%state%TemperatureAir2mVeg     ,& ! out,   2 m height air temperature [K], vegetated
              TemperatureAir2mBare    => noahmp%energy%state%TemperatureAir2mBare    ,& ! out,   2 m height air temperature [K] bare ground
              LeafAreaIndSunlit       => noahmp%energy%state%LeafAreaIndSunlit       ,& ! out,   sunlit leaf area index, one-sided [m2/m2]
              LeafAreaIndShade        => noahmp%energy%state%LeafAreaIndShade        ,& ! out,   shaded leaf area index, one-sided [m2/m2]
              EmissivitySfc           => noahmp%energy%state%EmissivitySfc           ,& ! out,   surface emissivity
              VegAreaIndEff           => noahmp%energy%state%VegAreaIndEff           ,& ! out,   one-sided leaf+stem area index [m2/m2]
              RoughLenMomSfc          => noahmp%energy%state%RoughLenMomSfc          ,& ! out,   roughness length [m], momentum, surface
              RoughLenMomGrd          => noahmp%energy%state%RoughLenMomGrd          ,& ! out,   roughness length [m], momentum, ground
              WindStressEwVeg         => noahmp%energy%state%WindStressEwVeg         ,& ! out,   wind stress: east-west [N/m2] above canopy
              WindStressNsVeg         => noahmp%energy%state%WindStressNsVeg         ,& ! out,   wind stress: north-south [N/m2] above canopy
              WindStressEwBare        => noahmp%energy%state%WindStressEwBare        ,& ! out,   wind stress: east-west [N/m2] bare ground
              WindStressNsBare        => noahmp%energy%state%WindStressNsBare        ,& ! out,   wind stress: north-south [N/m2] bare ground
              SpecHumidity2mVeg       => noahmp%energy%state%SpecHumidity2mVeg       ,& ! out,   water vapor mixing ratio at 2m vegetated
              SpecHumidity2mBare      => noahmp%energy%state%SpecHumidity2mBare      ,& ! out,   bare ground 2-m water vapor mixing ratio
              SpecHumidity2m          => noahmp%energy%state%SpecHumidity2m          ,& ! out,   grid mean 2-m water vapor mixing ratio
              TemperatureGrdVeg       => noahmp%energy%state%TemperatureGrdVeg       ,& ! out,   vegetated ground (below-canopy) temperature [K]
              TemperatureGrdBare      => noahmp%energy%state%TemperatureGrdBare      ,& ! out,   bare ground temperature [K]
              ExchCoeffMomAbvCan      => noahmp%energy%state%ExchCoeffMomAbvCan      ,& ! out,   exchange coeff [m/s] for momentum, above ZeroPlaneDisp, vegetated
              ExchCoeffMomBare        => noahmp%energy%state%ExchCoeffMomBare        ,& ! out,   exchange coeff [m/s] for momentum, above ZeroPlaneDisp, bare ground
              ExchCoeffShAbvCan       => noahmp%energy%state%ExchCoeffShAbvCan       ,& ! out,   exchange coeff [m/s] for heat, above ZeroPlaneDisp, vegetated
              ExchCoeffShBare         => noahmp%energy%state%ExchCoeffShBare         ,& ! out,   exchange coeff [m/s] for heat, above ZeroPlaneDisp, bare ground
              ExchCoeffShLeaf         => noahmp%energy%state%ExchCoeffShLeaf         ,& ! out,   leaf sensible heat exchange coeff [m/s], leaf to canopy air
              ExchCoeffShUndCan       => noahmp%energy%state%ExchCoeffShUndCan       ,& ! out,   under canopy sensible heat exchange coefficient [m/s]
              ExchCoeffSh2mVeg        => noahmp%energy%state%ExchCoeffSh2mVeg        ,& ! out,   2m sensible heat exchange coefficient [m/s] vegetated
              AlbedoSfc               => noahmp%energy%state%AlbedoSfc               ,& ! out,   total shortwave surface albedo
              RadSwReflSfc            => noahmp%energy%flux%RadSwReflSfc             ,& ! out,   total reflected solar radiation [W/m2]
              RadLwNetSfc             => noahmp%energy%flux%RadLwNetSfc              ,& ! out,   total net longwave rad [W/m2] (+ to atm)
              HeatSensibleSfc         => noahmp%energy%flux%HeatSensibleSfc          ,& ! out,   total sensible heat [W/m2] (+ to atm)
              HeatLatentGrd           => noahmp%energy%flux%HeatLatentGrd            ,& ! out,   total ground latent heat [W/m2] (+ to atm)
              HeatLatentCanopy        => noahmp%energy%flux%HeatLatentCanopy         ,& ! out,   canopy latent heat flux [W/m2] (+ to atm)
              HeatLatentTransp        => noahmp%energy%flux%HeatLatentTransp         ,& ! out,   latent heat flux from transpiration [W/m2] (+ to atm)
              RadPhotoActAbsCan       => noahmp%energy%flux%RadPhotoActAbsCan        ,& ! out,   total photosyn. active energy [W/m2) absorbed by canopy
              RadPhotoActAbsSunlit    => noahmp%energy%flux%RadPhotoActAbsSunlit     ,& ! out,   average absorbed par for sunlit leaves [W/m2]
              RadPhotoActAbsShade     => noahmp%energy%flux%RadPhotoActAbsShade      ,& ! out,   average absorbed par for shaded leaves [W/m2]
              HeatGroundTot           => noahmp%energy%flux%HeatGroundTot            ,& ! out,   total ground heat flux [W/m2] (+ to soil/snow)
              HeatPrecipAdvSfc        => noahmp%energy%flux%HeatPrecipAdvSfc         ,& ! out,   precipitation advected heat - total [W/m2]
              RadLwEmitSfc            => noahmp%energy%flux%RadLwEmitSfc             ,& ! out,   emitted outgoing IR [W/m2]
              RadLwNetCanopy          => noahmp%energy%flux%RadLwNetCanopy           ,& ! out,   canopy net longwave radiation [W/m2] (+ to atm)
              RadLwNetVegGrd          => noahmp%energy%flux%RadLwNetVegGrd           ,& ! out,   ground net longwave radiation [W/m2] (+ to atm)
              RadLwNetBareGrd         => noahmp%energy%flux%RadLwNetBareGrd          ,& ! out,   net longwave rad [W/m2] bare ground (+ to atm)
              HeatSensibleCanopy      => noahmp%energy%flux%HeatSensibleCanopy       ,& ! out,   canopy sensible heat flux [W/m2] (+ to atm)
              HeatSensibleVegGrd      => noahmp%energy%flux%HeatSensibleVegGrd       ,& ! out,   vegetated ground sensible heat flux [W/m2] (+ to atm)
              HeatSensibleBareGrd     => noahmp%energy%flux%HeatSensibleBareGrd      ,& ! out,   sensible heat flux [W/m2] bare ground (+ to atm)
              HeatLatentVegGrd        => noahmp%energy%flux%HeatLatentVegGrd         ,& ! out,   ground evaporation heat flux [W/m2] (+ to atm)
              HeatLatentBareGrd       => noahmp%energy%flux%HeatLatentBareGrd        ,& ! out,   latent heat flux [W/m2] bare ground (+ to atm)
              HeatLatentCanEvap       => noahmp%energy%flux%HeatLatentCanEvap        ,& ! out,   canopy evaporation heat flux [W/m2] (+ to atm)
              HeatLatentCanTransp     => noahmp%energy%flux%HeatLatentCanTransp      ,& ! out,   canopy transpiration heat flux [W/m2] (+ to atm)
              HeatGroundVegGrd        => noahmp%energy%flux%HeatGroundVegGrd         ,& ! out,   vegetated ground heat [W/m2] (+ to soil/snow)
              HeatGroundBareGrd       => noahmp%energy%flux%HeatGroundBareGrd        ,& ! out,   bare ground heat flux [W/m2] (+ to soil/snow)
              HeatCanStorageChg       => noahmp%energy%flux%HeatCanStorageChg        ,& ! out,   canopy heat storage change [W/m2]
              HeatFromSoilBot         => noahmp%energy%flux%HeatFromSoilBot          ,& ! out,   energy influx from soil bottom [J/m2] during soil timestep
              HeatGroundTotMean       => noahmp%energy%flux%HeatGroundTotMean        ,& ! out,   mean ground heat flux during soil timestep [W/m2]
              PhotosynTotal           => noahmp%biochem%flux%PhotosynTotal           ,& ! out,   total leaf photosynthesis [umol co2 /m2 /s]
              PhotosynLeafSunlit      => noahmp%biochem%flux%PhotosynLeafSunlit      ,& ! out,   sunlit leaf photosynthesis [umol co2 /m2 /s]
              PhotosynLeafShade       => noahmp%biochem%flux%PhotosynLeafShade        & ! out,   shaded leaf photosynthesis [umol co2 /m2 /s]
             )
! ----------------------------------------------------------------------

    ! initialization
    WindStressEwVeg     = 0.0
    WindStressNsVeg     = 0.0
    RadLwNetCanopy      = 0.0
    HeatSensibleCanopy  = 0.0
    RadLwNetVegGrd      = 0.0
    HeatSensibleVegGrd  = 0.0
    HeatLatentVegGrd    = 0.0
    HeatLatentCanEvap   = 0.0
    HeatLatentCanTransp = 0.0
    HeatGroundVegGrd    = 0.0
    PhotosynLeafSunlit  = 0.0
    PhotosynLeafShade   = 0.0
    TemperatureAir2mVeg = 0.0
    SpecHumidity2mVeg   = 0.0
    ExchCoeffShAbvCan   = 0.0
    ExchCoeffShLeaf     = 0.0
    ExchCoeffShUndCan   = 0.0
    ExchCoeffSh2mVeg    = 0.0
    HeatPrecipAdvSfc    = 0.0
    HeatCanStorageChg   = 0.0

    ! vegetated or non-vegetated
    VegAreaIndEff = LeafAreaIndEff + StemAreaIndEff
    FlagVegSfc    = .false.
    if ( VegAreaIndEff > 0.0 ) FlagVegSfc = .true.

    ! ground snow cover fraction [Niu and Yang, 2007, JGR]
    call SnowCoverGroundNiu07(noahmp)

    ! ground and surface roughness length and reference height
    call GroundRoughnessProperty(noahmp, FlagVegSfc)

    ! Thermal properties of soil, snow, lake, and frozen soil
    call GroundThermalProperty(noahmp)

    ! Surface shortwave albedo: ground and canopy radiative transfer
    call SurfaceAlbedo(noahmp)

    ! Surface shortwave radiation: absorbed & reflected by the ground and canopy
    call SurfaceRadiation(noahmp)

    ! longwave emissivity for vegetation, ground, total net surface
    call SurfaceEmissivity(noahmp)

    ! soil water transpiration factor controlling stomatal resistance and evapotranspiration
    call SoilWaterTranspiration(noahmp)

    ! soil surface resistance for ground evaporation/sublimation
    call ResistanceGroundEvaporation(noahmp)

    ! set psychrometric variable/constant
    call PsychrometricVariable(noahmp)

    ! temperatures and energy fluxes of canopy and below-canopy ground
    if ( (FlagVegSfc .eqv. .true.) .and. (VegFrac > 0) ) then ! vegetated portion of the grid
       TemperatureGrdVeg  = TemperatureGrd
       ExchCoeffMomAbvCan = ExchCoeffMomSfc
       ExchCoeffShAbvCan  = ExchCoeffShSfc
       call SurfaceEnergyFluxVegetated(noahmp)
    endif

    ! temperatures and energy fluxes of bare ground
    TemperatureGrdBare = TemperatureGrd
    ExchCoeffMomBare   = ExchCoeffMomSfc
    ExchCoeffShBare    = ExchCoeffShSfc
    call SurfaceEnergyFluxBareGround(noahmp)

    ! compute grid mean quantities by weighting vegetated and bare portions
    ! Energy balance at vege canopy: 
    ! RadSwAbsVeg = (RadLwNetCanopy + HeatSensibleCanopy + HeatLatentCanEvap + HeatLatentCanTransp) * VegFrac at VegFrac 
    ! Energy balance at vege ground: 
    ! RadSwAbsGrd * VegFrac = (RadLwNetVegGrd + HeatSensibleVegGrd + HeatLatentVegGrd + HeatGroundVegGrd) * VegFrac at VegFrac
    ! Energy balance at bare ground: 
    ! RadSwAbsGrd * (1-VegFrac) = (RadLwNetBareGrd + HeatSensibleBareGrd + HeatLatentBareGrd + HeatGroundBareGrd) * (1-VegFrac) at 1-VegFrac
    if ( (FlagVegSfc .eqv. .true.) .and. (VegFrac > 0) ) then
       WindStressEwSfc     = VegFrac * WindStressEwVeg     + (1.0 - VegFrac) * WindStressEwBare
       WindStressNsSfc     = VegFrac * WindStressNsVeg     + (1.0 - VegFrac) * WindStressNsBare
       RadLwNetSfc         = VegFrac * RadLwNetVegGrd      + (1.0 - VegFrac) * RadLwNetBareGrd     + RadLwNetCanopy
       HeatSensibleSfc     = VegFrac * HeatSensibleVegGrd  + (1.0 - VegFrac) * HeatSensibleBareGrd + HeatSensibleCanopy
       HeatLatentGrd       = VegFrac * HeatLatentVegGrd    + (1.0 - VegFrac) * HeatLatentBareGrd
       HeatGroundTot       = VegFrac * HeatGroundVegGrd    + (1.0 - VegFrac) * HeatGroundBareGrd
       HeatLatentCanopy    = HeatLatentCanEvap
       HeatLatentTransp    = HeatLatentCanTransp
       HeatPrecipAdvSfc    = VegFrac * HeatPrecipAdvVegGrd + (1.0 - VegFrac) * HeatPrecipAdvBareGrd + HeatPrecipAdvCanopy
       TemperatureGrd      = VegFrac * TemperatureGrdVeg   + (1.0 - VegFrac) * TemperatureGrdBare
       TemperatureAir2m    = VegFrac * TemperatureAir2mVeg + (1.0 - VegFrac) * TemperatureAir2mBare
       TemperatureSfc      = VegFrac * TemperatureCanopy   + (1.0 - VegFrac) * TemperatureGrdBare
       ExchCoeffMomSfc     = VegFrac * ExchCoeffMomAbvCan  + (1.0 - VegFrac) * ExchCoeffMomBare     ! better way to average?
       ExchCoeffShSfc      = VegFrac * ExchCoeffShAbvCan   + (1.0 - VegFrac) * ExchCoeffShBare
       SpecHumidity2m      = VegFrac * SpecHumidity2mVeg   + (1.0 - VegFrac) * SpecHumidity2mBare 
       SpecHumiditySfcMean = VegFrac * (PressureVaporCanAir * 0.622 / &
                             (PressureAirRefHeight - 0.378*PressureVaporCanAir)) + (1.0 - VegFrac) * SpecHumiditySfc
       RoughLenMomSfcToAtm = RoughLenMomSfc
    else
       WindStressEwSfc         = WindStressEwBare
       WindStressNsSfc         = WindStressNsBare
       RadLwNetSfc             = RadLwNetBareGrd
       HeatSensibleSfc         = HeatSensibleBareGrd
       HeatLatentGrd           = HeatLatentBareGrd
       HeatGroundTot           = HeatGroundBareGrd
       TemperatureGrd          = TemperatureGrdBare
       TemperatureAir2m        = TemperatureAir2mBare
       HeatLatentCanopy        = 0.0
       HeatLatentTransp        = 0.0
       HeatPrecipAdvSfc        = HeatPrecipAdvBareGrd
       TemperatureSfc          = TemperatureGrd
       ExchCoeffMomSfc         = ExchCoeffMomBare
       ExchCoeffShSfc          = ExchCoeffShBare
       SpecHumiditySfcMean     = SpecHumiditySfc
       SpecHumidity2m          = SpecHumidity2mBare
       ResistanceStomataSunlit = 0.0
       ResistanceStomataShade  = 0.0
       TemperatureGrdVeg       = TemperatureGrdBare
       ExchCoeffShAbvCan       = ExchCoeffShBare
       RoughLenMomSfcToAtm     = RoughLenMomGrd
    endif

    ! emitted longwave radiation and physical check
    RadLwEmitSfc = RadLwDownRefHeight + RadLwNetSfc
    if ( RadLwEmitSfc <= 0.0 ) then
       write(*,*) "emitted longwave <0; skin T may be wrong due to inconsistent"
       write(*,*) "input of VegFracGreen with LeafAreaIndex"
       write(*,*) "VegFrac = ", VegFrac, "VegAreaIndEff = ", VegAreaIndEff, &
                  "TemperatureCanopy = ", TemperatureCanopy, "TemperatureGrd = ", TemperatureGrd
       write(*,*) "RadLwDownRefHeight = ", RadLwDownRefHeight, "RadLwNetSfc = ", RadLwNetSfc, "SnowDepth = ", SnowDepth
       stop "Error: Longwave radiation budget problem in NoahMP LSM"
    endif

    ! radiative temperature: subtract from the emitted IR the
    ! reflected portion of the incoming longwave radiation, so just
    ! considering the IR originating/emitted in the canopy/ground system.
    ! Old TemperatureRadSfc calculation not taking into account Emissivity:
    ! TemperatureRadSfc = (RadLwEmitSfc/ConstStefanBoltzmann)**0.25
    TemperatureRadSfc = ((RadLwEmitSfc - (1.0-EmissivitySfc)*RadLwDownRefHeight) / (EmissivitySfc*ConstStefanBoltzmann))**0.25

    ! other photosynthesis related quantities for biochem process
    RadPhotoActAbsCan = RadPhotoActAbsSunlit * LeafAreaIndSunlit + RadPhotoActAbsShade * LeafAreaIndShade
    PhotosynTotal     = PhotosynLeafSunlit   * LeafAreaIndSunlit + PhotosynLeafShade   * LeafAreaIndShade

    ! compute snow and soil layer temperature at soil timestep
    HeatFromSoilBot = 0.0
    HeatGroundTotAcc = HeatGroundTotAcc + HeatGroundTot
    if ( FlagSoilProcess .eqv. .true. ) then
       HeatGroundTotMean = HeatGroundTotAcc / NumSoilTimeStep
       call SoilSnowTemperatureMain(noahmp)
    endif ! FlagSoilProcess

    ! adjusting suface temperature based on snow condition
    if ( OptSnowSoilTempTime == 2 ) then
       if ( (SnowDepth > 0.05) .and. (TemperatureGrd > ConstFreezePoint) ) then
          TemperatureGrdVeg  = ConstFreezePoint
          TemperatureGrdBare = ConstFreezePoint
          if ( (FlagVegSfc .eqv. .true.) .and. (VegFrac > 0) ) then
             TemperatureGrd  = VegFrac * TemperatureGrdVeg + (1.0 - VegFrac) * TemperatureGrdBare
             TemperatureSfc  = VegFrac * TemperatureCanopy + (1.0 - VegFrac) * TemperatureGrdBare
          else
             TemperatureGrd  = TemperatureGrdBare
             TemperatureSfc  = TemperatureGrdBare
          endif
       endif
    endif

    ! Phase change and Energy released or consumed by snow & frozen soil
    call SoilSnowWaterPhaseChange(noahmp)

    ! update sensible heat flux due to sprinkler irrigation evaporation
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) ) &
       HeatSensibleSfc = HeatSensibleSfc - HeatLatentIrriEvap

    ! update total surface albedo
    if ( RadSwDownRefHeight > 0.0 ) then
       AlbedoSfc = RadSwReflSfc / RadSwDownRefHeight
    else
       AlbedoSfc = undefined_real
    endif

    end associate

  end subroutine EnergyMain

end module EnergyMainMod
