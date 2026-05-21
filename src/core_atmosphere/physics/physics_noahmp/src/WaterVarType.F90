module WaterVarType

!!! Define column (1-D) Noah-MP Water variables
!!! Water variable initialization is done in WaterVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

!=== define "flux" sub-type of water (water%flux%variable)
  type :: flux_type

    real(kind=kind_noahmp) :: RainfallRefHeight          ! liquid rainfall rate [mm/s] at reference height
    real(kind=kind_noahmp) :: SnowfallRefHeight          ! snowfall rate [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipTotRefHeight         ! total precipitation [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipConvTotRefHeight     ! total convective precipitation [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipLargeSclRefHeight    ! large-scale precipitation [mm/s] at reference height
    real(kind=kind_noahmp) :: EvapCanopyNet              ! net evaporation of canopy intercepted total water [mm/s]
    real(kind=kind_noahmp) :: Transpiration              ! transpiration rate [mm/s]
    real(kind=kind_noahmp) :: EvapCanopyLiq              ! canopy liquid water evaporation rate [mm/s]
    real(kind=kind_noahmp) :: DewCanopyLiq               ! canopy water dew rate [mm/s]
    real(kind=kind_noahmp) :: FrostCanopyIce             ! canopy ice frost rate [mm/s]
    real(kind=kind_noahmp) :: SublimCanopyIce            ! canopy ice sublimation rate [mm/s]
    real(kind=kind_noahmp) :: MeltCanopyIce              ! canopy ice melting rate [mm/s]
    real(kind=kind_noahmp) :: FreezeCanopyLiq            ! canopy water freezing rate [mm/s]
    real(kind=kind_noahmp) :: SnowfallGround             ! snowfall on the ground (below canopy) [mm/s]
    real(kind=kind_noahmp) :: SnowDepthIncr              ! snow depth increasing rate [m/s] due to snowfall
    real(kind=kind_noahmp) :: FrostSnowSfcIce            ! snow surface ice frost rate[mm/s]
    real(kind=kind_noahmp) :: SublimSnowSfcIce           ! snow surface ice sublimation rate[mm/s]
    real(kind=kind_noahmp) :: RainfallGround             ! ground surface rain rate [mm/s]
    real(kind=kind_noahmp) :: SnowBotOutflow             ! total water (snowmelt + rain through pack) out of snowpack bottom [mm/s]
    real(kind=kind_noahmp) :: GlacierExcessFlow          ! glacier excess flow [mm/s]
    real(kind=kind_noahmp) :: IrrigationRateFlood        ! flood irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IrrigationRateMicro        ! micro irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IrrigationRateSprinkler    ! sprinkler irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IrriEvapLossSprinkler      ! loss of irrigation water to evaporation,sprinkler [m/timestep]
    real(kind=kind_noahmp) :: SoilSfcInflow              ! water input on soil surface [m/s]
    real(kind=kind_noahmp) :: RunoffSurface              ! surface runoff [mm/s]
    real(kind=kind_noahmp) :: RunoffSubsurface           ! subsurface runoff [mm/s]
    real(kind=kind_noahmp) :: InfilRateSfc               ! infiltration rate at surface [m/s]
    real(kind=kind_noahmp) :: EvapSoilSfcLiq             ! soil surface water evaporation [m/s]
    real(kind=kind_noahmp) :: DrainSoilBot               ! soil bottom drainage [mm/s]
    real(kind=kind_noahmp) :: TileDrain                  ! tile drainage [mm/s]
    real(kind=kind_noahmp) :: RechargeGw                 ! groundwater recharge rate [mm/s]
    real(kind=kind_noahmp) :: DischargeGw                ! groundwater discharge rate [mm/s]
    real(kind=kind_noahmp) :: VaporizeGrd                ! ground vaporize rate total (evap+sublim) [mm/s]
    real(kind=kind_noahmp) :: CondenseVapGrd             ! ground vapor condense rate total (dew+frost) [mm/s]
    real(kind=kind_noahmp) :: DewSoilSfcLiq              ! soil surface water dew rate [mm/s]
    real(kind=kind_noahmp) :: EvapIrriSprinkler          ! evaporation of irrigation water, sprinkler [mm/s]
    real(kind=kind_noahmp) :: InterceptCanopyRain        ! interception rate for rain [mm/s]
    real(kind=kind_noahmp) :: DripCanopyRain             ! drip rate for intercepted rain [mm/s]
    real(kind=kind_noahmp) :: ThroughfallRain            ! throughfall for rain [mm/s]
    real(kind=kind_noahmp) :: InterceptCanopySnow        ! interception (loading) rate for snowfall [mm/s]
    real(kind=kind_noahmp) :: DripCanopySnow             ! drip (unloading) rate for intercepted snow [mm/s]
    real(kind=kind_noahmp) :: ThroughfallSnow            ! throughfall of snowfall [mm/s]
    real(kind=kind_noahmp) :: EvapGroundNet              ! net ground (soil/snow) evaporation [mm/s]
    real(kind=kind_noahmp) :: MeltGroundSnow             ! ground snow melting rate [mm/s]
    real(kind=kind_noahmp) :: WaterToAtmosTotal          ! total surface water vapor flux to atmosphere [mm/s]
    real(kind=kind_noahmp) :: EvapSoilSfcLiqAcc          ! accumulated soil surface water evaporation per soil timestep [m/s * dt_soil/dt_main]
    real(kind=kind_noahmp) :: SoilSfcInflowAcc           ! accumulated water input on soil surface per soil timestep [m/s * dt_soil/dt_main]
    real(kind=kind_noahmp) :: SfcWaterTotChgAcc          ! accumulated snow,soil,canopy water change per soil timestep [mm]
    real(kind=kind_noahmp) :: PrecipTotAcc               ! accumulated precipitation per soil timestep [mm]
    real(kind=kind_noahmp) :: EvapCanopyNetAcc           ! accumulated net evaporation of canopy intercepted water per soil timestep [mm]
    real(kind=kind_noahmp) :: TranspirationAcc           ! accumulated transpiration per soil timestep [mm]
    real(kind=kind_noahmp) :: EvapGroundNetAcc           ! accumulated net ground (soil/snow) evaporation per soil timestep [mm]
    real(kind=kind_noahmp) :: EvapSoilSfcLiqMean         ! mean soil surface water evaporation during soil timestep [m/s]
    real(kind=kind_noahmp) :: SoilSfcInflowMean          ! mean water input on soil surface during soil timestep [m/s]

    real(kind=kind_noahmp), allocatable, dimension(:) :: TranspWatLossSoil     ! transpiration water loss from soil layers [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: TranspWatLossSoilAcc  ! accumulated transpiration water loss from soil per soil timestep [m/s * dt_soil/dt_main]
    real(kind=kind_noahmp), allocatable, dimension(:) :: TranspWatLossSoilMean ! mean transpiration water loss from soil during soil timestep [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowAging   ! rate of snow compaction due to destructive metamorphism/aging [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowBurden  ! rate of snow compaction due to overburden [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowMelt    ! rate of snow compaction due to melt [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowTot     ! rate of total snow compaction [fraction/timestep]

  end type flux_type


!=== define "state" sub-type of water (water%state%variable)
  type :: state_type

    integer                :: IrrigationCntSprinkler     ! irrigation event number, Sprinkler
    integer                :: IrrigationCntMicro         ! irrigation event number, Micro
    integer                :: IrrigationCntFlood         ! irrigation event number, Flood
    real(kind=kind_noahmp) :: CanopyTotalWater           ! total (liquid+ice) canopy intercepted water [mm]
    real(kind=kind_noahmp) :: CanopyWetFrac              ! wetted or snowed fraction of the canopy
    real(kind=kind_noahmp) :: SnowfallDensity            ! bulk density of snowfall (kg/m3)
    real(kind=kind_noahmp) :: CanopyLiqWater             ! intercepted canopy liquid water [mm]
    real(kind=kind_noahmp) :: CanopyIce                  ! intercepted canopy ice [mm]
    real(kind=kind_noahmp) :: CanopyIceMax               ! canopy capacity for snow interception [mm]
    real(kind=kind_noahmp) :: CanopyLiqWaterMax          ! canopy capacity for rain interception [mm]
    real(kind=kind_noahmp) :: SnowDepth                  ! snow depth [m]
    real(kind=kind_noahmp) :: SnowWaterEquiv             ! snow water equivalent (ice+liquid) [mm]
    real(kind=kind_noahmp) :: SnowWaterEquivPrev         ! snow water equivalent at previous time step (mm)
    real(kind=kind_noahmp) :: PondSfcThinSnwMelt         ! surface ponding [mm] from snowmelt when snow has no layer
    real(kind=kind_noahmp) :: PondSfcThinSnwComb         ! surface ponding [mm] from liquid in thin snow layer combination
    real(kind=kind_noahmp) :: PondSfcThinSnwTrans        ! surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
    real(kind=kind_noahmp) :: IrrigationFracFlood        ! fraction of grid under flood irrigation (0 to 1)
    real(kind=kind_noahmp) :: IrrigationAmtFlood         ! flood irrigation water amount [m]
    real(kind=kind_noahmp) :: IrrigationFracMicro        ! fraction of grid under micro irrigation (0 to 1)
    real(kind=kind_noahmp) :: IrrigationAmtMicro         ! micro irrigation water amount [m]
    real(kind=kind_noahmp) :: IrrigationFracSprinkler    ! fraction of grid under sprinkler irrigation (0 to 1)
    real(kind=kind_noahmp) :: IrrigationAmtSprinkler     ! sprinkler irrigation water amount [m]
    real(kind=kind_noahmp) :: WaterTableDepth            ! water table depth [m]
    real(kind=kind_noahmp) :: SoilIceMax                 ! maximum soil ice content [m3/m3]
    real(kind=kind_noahmp) :: SoilLiqWaterMin            ! minimum soil liquid water content [m3/m3]
    real(kind=kind_noahmp) :: SoilSaturateFrac           ! fractional saturated area for soil moisture
    real(kind=kind_noahmp) :: SoilImpervFracMax          ! maximum soil imperviousness fraction
    real(kind=kind_noahmp) :: SoilMoistureToWT           ! soil moisture between bottom of the soil and the water table
    real(kind=kind_noahmp) :: RechargeGwDeepWT           ! groundwater recharge to or from the water table when deep [m]
    real(kind=kind_noahmp) :: RechargeGwShallowWT        ! groundwater recharge to or from shallow water table [m]
    real(kind=kind_noahmp) :: SoilSaturationExcess       ! saturation excess of the total soil [m]
    real(kind=kind_noahmp) :: WaterTableHydro            ! water table depth estimated in WRF-Hydro fine grids [m]
    real(kind=kind_noahmp) :: TileDrainFrac              ! tile drainage fraction
    real(kind=kind_noahmp) :: WaterStorageAquifer        ! water storage in aquifer [mm]
    real(kind=kind_noahmp) :: WaterStorageSoilAqf        ! water storage in aquifer + saturated soil [mm]
    real(kind=kind_noahmp) :: WaterStorageLake           ! water storage in lake (can be negative) [mm] 
    real(kind=kind_noahmp) :: WaterHeadSfc               ! surface water head [mm]
    real(kind=kind_noahmp) :: IrrigationFracGrid         ! total irrigation fraction from input for a grid
    real(kind=kind_noahmp) :: PrecipAreaFrac             ! fraction of the gridcell that receives precipitation
    real(kind=kind_noahmp) :: SnowCoverFrac              ! snow cover fraction
    real(kind=kind_noahmp) :: SoilTranspFacAcc           ! accumulated soil water transpiration factor (0 to 1)
    real(kind=kind_noahmp) :: FrozenPrecipFrac           ! fraction of frozen precip in total precipitation
    real(kind=kind_noahmp) :: SoilWaterRootZone          ! root zone soil water
    real(kind=kind_noahmp) :: SoilWaterStress            ! soil water stress
    real(kind=kind_noahmp) :: WaterStorageTotBeg         ! total water storage [mm] at the begining before NoahMP process
    real(kind=kind_noahmp) :: WaterBalanceError          ! water balance error [mm]
    real(kind=kind_noahmp) :: WaterStorageTotEnd         ! total water storage [mm] at the end of NoahMP process

    integer               , allocatable, dimension(:) :: IndexPhaseChange      ! phase change index (0-none;1-melt;2-refreeze)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIce               ! snow layer ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowLiqWater          ! snow layer liquid water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceFracPrev       ! ice fraction in snow layers at previous timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceFrac           ! ice fraction in snow layers at current timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIceFrac           ! ice fraction in soil layers at current timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowEffPorosity       ! snow effective porosity [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilLiqWater          ! soil liquid moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIce               ! soil ice moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoisture          ! total soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilImpervFrac        ! fraction of imperviousness due to frozen soil
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatConductivity   ! soil hydraulic/water conductivity [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatDiffusivity    ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilEffPorosity       ! soil effective porosity [m3/m3] 
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoistureEqui      ! equilibrium soil water  content [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilTranspFac         ! soil water transpiration factor (0 to 1)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceVol            ! partial volume of snow ice [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowLiqWaterVol       ! partial volume of snow liquid water [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilSupercoolWater    ! supercooled water in soil [kg/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMatPotential      ! soil matric potential [m]

  end type state_type


!=== define "parameter" sub-type of water (water%param%variable)
  type :: parameter_type

    integer                :: DrainSoilLayerInd          ! starting soil layer for drainage
    integer                :: TileDrainTubeDepth         ! depth [m] of drain tube from the soil surface for simple scheme
    integer                :: NumSoilLayerRoot           ! number of soil layers with root present
    integer                :: IrriStopDayBfHarvest       ! number of days before harvest date to stop irrigation
    real(kind=kind_noahmp) :: CanopyLiqHoldCap           ! maximum canopy intercepted liquid water per unit veg area index [mm]
    real(kind=kind_noahmp) :: SnowCompactBurdenFac       ! overburden snow compaction parameter [m3/kg]
    real(kind=kind_noahmp) :: SnowCompactAgingFac1       ! snow desctructive metamorphism compaction parameter1 [1/s]
    real(kind=kind_noahmp) :: SnowCompactAgingFac2       ! snow desctructive metamorphism compaction parameter2 [1/k]
    real(kind=kind_noahmp) :: SnowCompactAgingFac3       ! snow desctructive metamorphism compaction parameter3 
    real(kind=kind_noahmp) :: SnowCompactAgingMax        ! upper Limit on destructive metamorphism compaction [kg/m3]
    real(kind=kind_noahmp) :: SnowViscosityCoeff         ! snow viscosity coefficient [kg-s/m2], Anderson1979: 0.52e6~1.38e6
    real(kind=kind_noahmp) :: SnowLiqFracMax             ! maximum liquid water fraction in snow
    real(kind=kind_noahmp) :: SnowLiqHoldCap             ! liquid water holding capacity for snowpack [m3/m3]
    real(kind=kind_noahmp) :: SnowLiqReleaseFac          ! snowpack water release timescale factor [1/s]
    real(kind=kind_noahmp) :: IrriFloodRateFac           ! flood irrigation application rate factor
    real(kind=kind_noahmp) :: IrriMicroRate              ! micro irrigation rate [mm/hr]
    real(kind=kind_noahmp) :: SoilInfilMaxCoeff          ! parameter to calculate maximum soil infiltration rate
    real(kind=kind_noahmp) :: SoilImpervFracCoeff        ! parameter to calculate frozen soil impermeable fraction
    real(kind=kind_noahmp) :: InfilFacVic                ! VIC model infiltration parameter
    real(kind=kind_noahmp) :: TensionWatDistrInfl        ! Tension water distribution inflection parameter
    real(kind=kind_noahmp) :: TensionWatDistrShp         ! Tension water distribution shape parameter
    real(kind=kind_noahmp) :: FreeWatDistrShp            ! Free water distribution shape parameter
    real(kind=kind_noahmp) :: InfilHeteroDynVic          ! DVIC heterogeniety parameter for infiltration
    real(kind=kind_noahmp) :: InfilCapillaryDynVic       ! DVIC Mean Capillary Drive (m) for infiltration models
    real(kind=kind_noahmp) :: InfilFacDynVic             ! DVIC model infiltration parameter
    real(kind=kind_noahmp) :: SoilDrainSlope             ! slope index for soil drainage
    real(kind=kind_noahmp) :: TileDrainCoeffSp           ! drainage coefficient [mm d^-1] for simple scheme
    real(kind=kind_noahmp) :: DrainFacSoilWat            ! drainage factor for soil moisture
    real(kind=kind_noahmp) :: TileDrainCoeff             ! drainage coefficent [m d^-1] for Hooghoudt scheme
    real(kind=kind_noahmp) :: DrainDepthToImperv         ! Actual depth of tile drainage to impermeable layer form surface
    real(kind=kind_noahmp) :: LateralWatCondFac          ! multiplication factor to determine lateral hydraulic conductivity
    real(kind=kind_noahmp) :: TileDrainDepth             ! Depth of drain [m] for Hooghoudt scheme
    real(kind=kind_noahmp) :: DrainTubeDist              ! distance between two drain tubes or tiles [m]
    real(kind=kind_noahmp) :: DrainTubeRadius            ! effective radius of drain tubes [m]
    real(kind=kind_noahmp) :: DrainWatDepToImperv        ! depth to impervious layer from drain water level [m]
    real(kind=kind_noahmp) :: RunoffDecayFac             ! runoff decay factor [m^-1]
    real(kind=kind_noahmp) :: BaseflowCoeff              ! baseflow coefficient [mm/s]
    real(kind=kind_noahmp) :: GridTopoIndex              ! gridcell mean topgraphic index (global mean)
    real(kind=kind_noahmp) :: SoilSfcSatFracMax          ! maximum surface soil saturated fraction (global mean)
    real(kind=kind_noahmp) :: SpecYieldGw                ! specific yield [-] for Niu et al. 2007 groundwater scheme
    real(kind=kind_noahmp) :: MicroPoreContent           ! microprore content (0.0-1.0), 0.0: close to free drainage
    real(kind=kind_noahmp) :: WaterStorageLakeMax        ! maximum lake water storage [mm]
    real(kind=kind_noahmp) :: SnoWatEqvMaxGlacier        ! Maximum SWE allowed at glaciers [mm]
    real(kind=kind_noahmp) :: SoilConductivityRef        ! Reference Soil Conductivity parameter (used in runoff formulation)
    real(kind=kind_noahmp) :: SoilInfilFacRef            ! Reference Soil Infiltration Parameter (used in runoff formulation)
    real(kind=kind_noahmp) :: GroundFrzCoeff             ! Frozen ground parameter to compute frozen soil impervious fraction
    real(kind=kind_noahmp) :: IrriTriggerLaiMin          ! minimum lai to trigger irrigation
    real(kind=kind_noahmp) :: SoilWatDeficitAllow        ! management allowable deficit (0-1)
    real(kind=kind_noahmp) :: IrriFloodLossFrac          ! factor of flood irrigation loss
    real(kind=kind_noahmp) :: IrriSprinklerRate          ! sprinkler irrigation rate [mm/h]
    real(kind=kind_noahmp) :: IrriFracThreshold          ! irrigation Fraction threshold in a grid
    real(kind=kind_noahmp) :: IrriStopPrecipThr          ! precipitation threshold [mm/hr] to stop irrigation trigger
    real(kind=kind_noahmp) :: SnowfallDensityMax         ! maximum fresh snowfall density [kg/m3]
    real(kind=kind_noahmp) :: SnowMassFullCoverOld       ! new snow mass to fully cover old snow [mm]
    real(kind=kind_noahmp) :: SoilMatPotentialWilt       ! soil metric potential for wilting point [m]
    real(kind=kind_noahmp) :: SnowMeltFac                ! snowmelt m parameter in snow cover fraction calculation
    real(kind=kind_noahmp) :: SnowCoverFac               ! snow cover factor [m] (originally hard-coded 2.5*z0 in SCF formulation)

    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoistureSat        ! saturated value of soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoistureWilt       ! wilting point soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoistureFieldCap   ! reference soil moisture (field capacity) [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoistureDry        ! dry soil moisture threshold [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatDiffusivitySat  ! saturated soil hydraulic diffusivity [m2/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatConductivitySat ! saturated soil hydraulic conductivity [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilExpCoeffB          ! soil exponent B paramete
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMatPotentialSat    ! saturated soil matric potential [m]

  end type parameter_type


!=== define water type that includes 3 subtypes (flux,state,parameter)
  type, public :: water_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param

  end type water_type

end module WaterVarType
